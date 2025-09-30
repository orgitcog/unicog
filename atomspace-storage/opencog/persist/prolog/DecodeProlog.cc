/*
 * DecodeProlog.cc
 *
 * Copyright (C) 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  September 2022
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>

#include "Prolog.h"

using namespace opencog;

/// Parse one or more clauses, e.g.
/// foo(X) :- bar(X). bing(bang,bong). food(pizza).
HandleSeq Prolog::parse(const std::string& sexpr, size_t& l, size_t& r)
{
	HandleSeq clauses;
	while (std::string::npos != l)
	{
		Handle h(get_next_expr(sexpr, l, r));
		clauses.emplace_back(h);

		if (std::string::npos == l)
			throw SyntaxException(TRACE_INFO, "Expecting period at end.");

		l++;
		l = sexpr.find_first_not_of(" \t\n", l);
	}

	return clauses;
}

// ---------------------------------------------------------------

// Lower-case tokens, or quoted tokens, are concept nodes.
// Upper-case tokens are Variables.
static Handle make_tok(const std::string& tok)
{
	char c = tok[0];

	// Variables begin with an underscore, or uppercase letter.
	if ('_' == c or isupper(c))
		return HandleCast(createNode(VARIABLE_NODE, tok));

	return HandleCast(createNode(CONCEPT_NODE, tok));
}

// Forward declaration
static Handle replace_wildcards_with_vars(const Handle& h, HandleSeq& vars);

// Parse factual assertions such as
// likes(john, mary) or food(pizza)
// but also fragments of clauses, such as
// likes(X,Y)
static Handle get_fact(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	r = sexpr.find_first_of("( \t\n", l+1);
	const std::string& spred = sexpr.substr(l, r-l);
	Handle pred = createNode(PREDICATE_NODE, std::move(spred));

	l = sexpr.find_first_not_of(" \t\n", r);
	if (std::string::npos == l or '(' != sexpr[l])
		throw SyntaxException(TRACE_INFO, "Expecting open-paren");

	l++; // step past open paren

	HandleSeq clist;
	while (std::string::npos != l)
	{
		// Matching quotes, if quoted.
		if ('\'' == sexpr[l])
		{
			r = sexpr.find('\'', l+1);
			if (std::string::npos != r) r++;
			else
				throw SyntaxException(TRACE_INFO, "Unbalanced quotes");
		}
		else
			r = sexpr.find_first_of(",) \t\n", l);
		const std::string& litrl = sexpr.substr(l, r-l);

		if ('\'' == litrl[0] or std::string::npos == litrl.find('('))
			clist.emplace_back(make_tok(litrl));
		else
		{
			// The literal is yet another fact. Yuck.
			// Go and nest them. (For now? Maybe fix later?)
			clist.emplace_back(get_fact(sexpr, l, r));
			r = l;
		}

		if (')' == sexpr[r]) break;
		l = sexpr.find_first_not_of(", \t\n", r);
	}
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Expecting close-paren");

	l = sexpr.find_first_not_of(" \t\n", r+1);
	r = std::string::npos;

	Handle evl = createLink(EVALUATION_LINK,
		pred, createLink(std::move(clist), LIST_LINK));
	return evl;
}

// ---------------------------------------------------------------
//
// Handle Prolog queries.
// There are several basic types:
// 1) ?- likes(alice,john).
//    Return true of false; tests for presence. (SatisfactionLink)
// 2) ?- likes(alice, _).
//    Return true of false; alice likes someone, but its indeterminate.
// 3) ?- likes(alice, Who).
//    Return grounding of Who. Standard GetLink.
//    Caution: this may require chaining to solve.
// 4) Unification, chaining, ... etc.

static Handle formulate_query(const std::string& sexpr, size_t& l, size_t &r)
{
	// Skip past the ?- at the start.
	l = sexpr.find_first_not_of(" \t\n", l+2);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Get the query fact
	Handle fac = get_fact(sexpr, l, r);
	
	// Extract all variables from the fact
	HandleSet vars;
	std::function<void(const Handle&)> extract_vars = [&](const Handle& h) {
		if (h->is_type(VARIABLE_NODE))
		{
			vars.insert(h);
		}
		else if (h->is_link())
		{
			for (const Handle& ho : h->getOutgoingSet())
				extract_vars(ho);
		}
	};
	extract_vars(fac);
	
	// Case 1: No variables - simple presence test (SatisfactionLink)
	if (vars.empty())
	{
		// Check if there are wildcards (underscores)
		bool has_wildcards = false;
		std::function<bool(const Handle&)> check_wildcards = [&](const Handle& h) -> bool {
			if (h->is_node() && h->get_name() == "_")
			{
				has_wildcards = true;
				return true;
			}
			if (h->is_link())
			{
				for (const Handle& ho : h->getOutgoingSet())
					if (check_wildcards(ho)) return true;
			}
			return false;
		};
		check_wildcards(fac);
		
		if (has_wildcards)
		{
			// Case 2: Has wildcards - convert to pattern with anonymous variables
			HandleSeq pattern_vars;
			Handle pattern = replace_wildcards_with_vars(fac, pattern_vars);
			
			// Create a SatisfactionLink with anonymous variables
			Handle varlist = createLink(std::move(pattern_vars), VARIABLE_LIST);
			return createLink(SATISFACTION_LINK, varlist, pattern);
		}
		else
		{
			// Simple presence test
			return createLink(SATISFACTION_LINK, fac);
		}
	}
	else
	{
		// Case 3: Has variables - create GetLink
		HandleSeq varlist(vars.begin(), vars.end());
		Handle vardecl;
		
		if (varlist.size() == 1)
			vardecl = varlist[0];
		else
			vardecl = createLink(std::move(varlist), VARIABLE_LIST);
		
		return createLink(GET_LINK, vardecl, fac);
	}
}

// Helper function to replace wildcards with anonymous variables
static Handle replace_wildcards_with_vars(const Handle& h, HandleSeq& vars)
{
	static int anon_var_counter = 0;
	
	if (h->is_node())
	{
		if (h->get_name() == "_")
		{
			// Create anonymous variable
			std::string varname = "$anon_" + std::to_string(anon_var_counter++);
			Handle var = createNode(VARIABLE_NODE, varname);
			vars.push_back(var);
			return var;
		}
		return h;
	}
	
	if (h->is_link())
	{
		HandleSeq new_outgoing;
		bool changed = false;
		
		for (const Handle& ho : h->getOutgoingSet())
		{
			Handle new_ho = replace_wildcards_with_vars(ho, vars);
			new_outgoing.push_back(new_ho);
			if (new_ho != ho) changed = true;
		}
		
		if (changed)
			return createLink(std::move(new_outgoing), h->get_type());
		else
			return h;
	}
	
	return h;
}

// ---------------------------------------------------------------
// Parse clauses such as
// likes(john, mary).
// or
// child(X,Y) :- parent(Y,X).
// Clauses must be terminated by a period.
Handle Prolog::get_next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Maybe it's a query?
	if ('?' == sexpr[l])
		return formulate_query(sexpr, l, r);

	// Not a query. Get first part of clause.
	Handle fac = get_fact(sexpr, l, r);

	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Expecting terminating period");

	// Just a single fact.
	if ('.' == sexpr[l])
		return fac;

	// Is it a Horn clause?
	if (sexpr.substr(l, 2) != ":-")
		throw SyntaxException(TRACE_INFO, "Badly formed expression");

	l += 2;

	// Loop over conjunctions.
	HandleSeq premis;
	while (std::string::npos != l)
	{
		Handle fac = get_fact(sexpr, l, r);
		premis.emplace_back(fac);
		if (std::string::npos == l)
			throw SyntaxException(TRACE_INFO, "Badly formed expression");
		if ('.' == sexpr[l]) break;
		if (',' != sexpr[l])
			throw SyntaxException(TRACE_INFO, "Badly formed expression");
		l++;
	}

	// Not a conjunction, just a single term.
	if (1 == premis.size())
		return HandleCast(createLink(IMPLICATION_LINK, premis[0], fac));

	// Conjunction of multiple terms.
	Handle imp = createLink(IMPLICATION_LINK,
		createLink(std::move(premis), AND_LINK), fac);
	return imp;
}

/* ===================== END OF FILE ===================== */
