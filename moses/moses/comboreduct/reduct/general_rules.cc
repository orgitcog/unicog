/*
 * moses/comboreduct/reduct/general_rules.cc
 *
 * Copyright (C) 2002-2008 Novamente LLC
 * All Rights Reserved
 *
 * Written by Moshe Looks
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
#include "general_rules.h"
#include "../interpreter/eval.h"
#include <moses/comboreduct/combo/assumption.h>
#include "../crutil/exception.h"

namespace opencog { namespace reduct {
typedef combo_tree::sibling_iterator sib_it;

/// Flattens all associative functions: f(a,f(b,c)) -> f(a,b,c)
/// Note that level is recursive that is f(a,f(b,f(c,d))) -> f(a,b,c,d)
void level::operator()(combo_tree& tr, combo_tree::iterator it) const
{
    if (is_associative(*it)) {
        for (sib_it sib = it.begin(); sib != it.end(); )
            if (*sib == *it)
                sib = tr.erase(tr.flatten(sib));
            else
                ++sib;
    }
}

/// Evaluates sub-expressions when possible.
/// If an operator is commutative, op(const,var,const) will become
/// op(op(const,const),var), e.g., +(2,x,1)->+(3,x)
void eval_constants::operator()(combo_tree& tr, combo_tree::iterator it) const
{
    if (it.is_childless()) {
        if (is_indefinite_object(*it)) {
            // not sure we want that when indefinite_object is random
            vertex_seq empty;
            *it = eval_throws_binding(empty, it);
        }
        return;
    }

    sib_it to;
    if (is_associative(*it)) {
        if (is_commutative(*it)) {
            to = tr.partition(it.begin(), it.end(), is_constant<vertex>);
            int n_consts = std::distance(it.begin(), to);
            if (n_consts < 2 && (!(n_consts == 1 && it.has_one_child())))
                return;
            if (to != it.end()) {
                tr.reparent(tr.append_child(it, *it), it.begin(), to);
                it = it.last_child();
            }
        }
        else {
            // Handle non-commutative associative operators
            // For these, we can still evaluate constant sub-expressions in order
            // but we need to be careful about maintaining operator order
            
            // Find consecutive constant sequences from left to right
            auto start_const = it.begin();
            while (start_const != it.end()) {
                if (is_constant(*start_const)) {
                    auto end_const = start_const;
                    ++end_const;
                    
                    // Find the end of this constant sequence
                    while (end_const != it.end() && is_constant(*end_const)) {
                        ++end_const;
                    }
                    
                    // If we have at least 2 constants in sequence, evaluate them
                    if (std::distance(start_const, end_const) >= 2) {
                        // Create a sub-expression with the operator and constants
                        combo_tree sub_tree;
                        auto sub_root = sub_tree.set_head(*it);
                        
                        // Copy constants to sub-tree
                        for (auto const_it = start_const; const_it != end_const; ++const_it) {
                            sub_tree.append_child(sub_root, *const_it);
                        }
                        
                        // Evaluate the sub-expression
                        vertex_seq empty;
                        vertex result = eval_throws_binding(empty, sub_root);
                        
                        // Replace the constant sequence with the result
                        auto replacement = tr.replace(start_const, result);
                        tr.erase(++replacement, end_const);
                        
                        // Continue from after the replacement
                        start_const = replacement;
                        ++start_const;
                    } else {
                        ++start_const;
                    }
                } else {
                    ++start_const;
                }
            }
        }
    }
    else {
        for (sib_it sib = it.begin(); sib != it.end(); ++sib)
            if (!is_constant(*sib))
                return;	
    }

    // We pass an empty vertex sequence, as there should be no
    // arguments in the tree below 'it'.  Viz, the only things we
    // expect to evaluate are things like 'not(true)', '*(-1.75 1)'
    // '+(2 0 6)' and so on.  If there are args, then hopefully an
    // exception will be thrown :-)
    //
    // Err, well, its common for knob-building to generate things
    // log(0) or /(1 0) and so we want to catch that inf, and 
    // propagate it along.  (later on, there may be a divde-by-inf
    // or maybe a 0<(-inf) predicate, etc. all of which can be legally
    // evaluated to return valid results.
    vertex_seq empty;
    try {
        *it = eval_throws_binding(empty, it);
    } catch (OverflowException& e) {
        *it = e.get_vertex();
    };
    tr.erase_children(it);
}

// Reorder children of commutative operators (should be applied upwards)
void reorder_commutative::operator()(combo_tree& tr,combo_tree::iterator it) const
{
    if (is_commutative(*it))
        tr.sort_on_subtrees(it.begin(), it.end(),
                            opencog::lexicographic_subtree_order<vertex>(),false);
}

// Get rid of subtrees marked with a null_vertex in their roots
void remove_null_vertices::operator()(combo_tree& tr, combo_tree::iterator it) const
{
    // Most nodes take simple lists; but not cond. Cond takes clauses,
    // which are pairs. If we remove the condition, we must also remove
    // the consequent.
    if (*it != id::cond) {
        for (sib_it sib = it.begin(); sib != it.end(); )
            if (*sib == id::null_vertex)
                sib = tr.erase(sib);
            else
                ++sib;
    } else {
        for (sib_it sib = it.begin(); sib != it.end(); )
            if (*sib == id::null_vertex) {
                sib = tr.erase(sib);
                sib = tr.erase(sib);
            }
            else {
                ++sib;
                ++sib;
            }
    }
}

void remove_all_assumptions::operator()(combo_tree& tr,combo_tree::iterator it) const
{
    delete_all_assumptions(tr);
}

} // ~namespace reduct
} // ~namespace opencog

