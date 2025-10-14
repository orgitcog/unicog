# OpenCog Unified FIXME Implementation Catalog

## Executive Summary

This document provides a comprehensive categorization of all **605 FIXME instances** found in the OpenCog Unified repository, sorted by implementation difficulty to guide development priorities and resource allocation.

### Summary Statistics

| Difficulty Level | Count | Percentage | Estimated Total Effort |
|-----------------|-------|------------|------------------------|
| **VERY_HARD** | 32 | 5.3% | 6-18 months |
| **HARD** | 55 | 9.1% | 2-8 months |  
| **MEDIUM** | 479 | 79.2% | 3-12 months |
| **EASY** | 39 | 6.4% | 1-4 weeks |

**Total files affected**: 288

---

## Implementation Priority Recommendations

### 🔥 Critical Priority (VERY_HARD)
These items require specialized expertise and significant architectural work. Consider outsourcing to domain experts or dedicating senior developers full-time.

### ⚡ High Priority (HARD) 
These items impact core functionality, performance, and reliability. Should be addressed by experienced developers with relevant domain knowledge.

### 📋 Medium Priority (MEDIUM)
Standard implementation work that can be distributed among team members based on component expertise.

### ✅ Easy Wins (EASY)
Quick fixes suitable for new contributors or as warm-up tasks.

---


## 🚨 Very Hard Priority (32 items)

### Atomspace Component (9 items)


**1.** `atomspace/opencog/query/InitiateSearchMixin.cc:126`

**Issue:** XXX FIXMEwe should be using ptm->isVariable() instead !?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
if (_nameserver.isNode(t))
{
// XXX FIXME; we should be using ptm->isVariable() instead !?
if (VARIABLE_NODE != t and GLOB_NODE != t and SIGN_NODE != t)
{
```
</details>


**2.** `atomspace/opencog/query/PatternMatchEngine.cc:1502`

**Issue:** its definition. XXX TODO. Hmm. Should we do this at runtime,

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

// If the pattern is a DefinedSchemaNode, we need to substitute
// its definition. XXX TODO. Hmm. Should we do this at runtime,
// i.e. here, or at static-analysis time, when creating the PatternLink?
if (DEFINED_SCHEMA_NODE == tp and not ptm->isQuoted())
```
</details>


**3.** `atomspace/opencog/atomspace/AtomSpace.h:524`

**Issue:** XXX FIXME Users should call StorageNode::add_nocheck() instead.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
/* ----------------------------------------------------------- */
// Not for public use! Only StorageNodes get to call this!
// XXX FIXME Users should call StorageNode::add_nocheck() instead.
Handle storage_add_nocheck(const Handle& h) { return add(h); }
};
```
</details>


**4.** `atomspace/opencog/atoms/pattern/PatternUtils.cc:55`

**Issue:** XXX FIXME Are the below needed?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
EVALUATABLE_LINK)))

// XXX FIXME Are the below needed?
or contains_atomtype(clause, DEFINED_PREDICATE_NODE)
or contains_atomtype(clause, DEFINED_SCHEMA_NODE)
```
</details>


**5.** `atomspace/opencog/atoms/core/TypedVariableLink.cc:50`

**Issue:** which should be using a SignatureLink for this case. XXX FIXME.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

// Allow VARIABLE_NODE, although this is a bug in the URE,
// which should be using a SignatureLink for this case. XXX FIXME.
Type dtype = _outgoing[1]->get_type();
if (not nameserver().isA(dtype, TYPE_NODE) and
```
</details>


**6.** `atomspace/opencog/atoms/core/RandomChoice.cc:143`

**Issue:** XXX TODO if execute() above returns FloatValue, use that!

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
hw = HandleCast(hw->execute(as, silent));

// XXX TODO if execute() above returns FloatValue, use that!
NumberNodePtr nn(NumberNodeCast(hw));
if (nullptr == nn) // goto uniform;
```
</details>


**7.** `atomspace/opencog/atoms/core/Checkers.cc:79`

**Issue:** XXX FIXME, this is to be removed, because UnionLink,

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
// union and complement. Since it cannot inherit from
// EVALUATABLE_LINK (cause it's a Node) we have to add it here.
// XXX FIXME, this is to be removed, because UnionLink,
// IntersectionLink takes the place of OrLink, AndLink.
if (h->is_type(CONCEPT_NODE)) continue;
```
</details>


**8.** `atomspace/opencog/atoms/reduct/DecimateLink.cc:65`

**Issue:** XXX FIXME ... both the NumberNode and the FloatValue variations

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME ... both the NumberNode and the FloatValue variations
// below make a copy of the mask.  Instead of making a copy, create
// something more efficient/faster. It is, after all, a simple
```
</details>


**9.** `atomspace/tests/cython/atomspace/test_atomspace.py:362`

**Issue:** XXX FIXME is testing the name of the bottom type

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
self.assertEqual(get_type_name(types.Node), "Node")
self.assertEqual(get_type_name(2231), "")
# XXX FIXME is testing the name of the bottom type
# a sane thing to do?
self.assertEqual(get_type_name(types.NO_TYPE), "*** Bottom Type! ***")
```
</details>


### Atomspace-Storage Component (2 items)


**1.** `atomspace-storage/opencog/persist/flow/StoreValueOfLink.cc:61`

**Issue:** XXX TODO FIXME ... if either of these are executable, then

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
StorageNodePtr stnp = StorageNodeCast(_outgoing[2]);

// XXX TODO FIXME ... if either of these are executable, then
// they need to be executed, first, right? Because that's the
// usual intent. Else they'd be wrapped in a DontExecLink, right?
```
</details>


**2.** `atomspace-storage/opencog/persist/flow/FetchValueOfLink.cc:63`

**Issue:** XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
StorageNodePtr stnp = StorageNodeCast(_outgoing[2]);

// XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]
// are executable, then they need to be executed, first, right?
// Yes, they do. But, for just right now, we don't, to stay
```
</details>


### Components Component (18 items)


**1.** `components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:51`

**Issue:** XXX FIXME This is wrongthis has been replaced by the eva-model

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
(LemmaLink (VariableNode "$subj-inst") (WordNode "you"))

; XXX FIXME This is wrong; this has been replaced by the eva-model
; code Unfortunately, it does not offer any easy way of querying.
; (State (Anchor "*-gaze-direction-*") (Variable "$direction"))
```
</details>


**2.** `components/integration/opencog/opencog/nlp/scm/type-definitions.scm:8`

**Issue:** XXX This is currently not used anywhere, but if it was fixed up,

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
; for new atom types, such as "GenderNode" or "TenseNode", etc.
;
; XXX This is currently not used anywhere, but if it was fixed up,
; it could be, I guess ...
;
```
</details>


**3.** `components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm:222`

**Issue:** XXX FIXME -- sentiment analysis should not be done here.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
(nlp-stimulate sent-node nlp-stimulation-value))

; XXX FIXME -- sentiment analysis should not be done here.
; (perform-sentiment-analysis sent-node)

```
</details>


**4.** `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:26`

**Issue:** XXX FIXME this method is really bad because for each new type of

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
; DefinedLinguisticPredicateNode created by R2L helper.
;
; XXX FIXME this method is really bad because for each new type of
; node that R2L uses, it needs to be added here.  There needs some
; different way for linking R2L nodes to WordInstanceNodes other
```
</details>


**5.** `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:361`

**Issue:** ;XXX FIXME using the hacky word-get-r2l-node, bad idea!

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
- #t or #f to signal the update of etv in the abstracted r2l outputs returned
"
;XXX FIXME using the hacky word-get-r2l-node, bad idea!
; get all the nodes linked by this link
(define old-oset (cog-outgoing-set ilink))
```
</details>


**6.** `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:386`

**Issue:** FIXME: Why create a node with new-instance name? Is this for

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
)
)
;  ; FIXME: Why create a node with new-instance name? Is this for
; anaphora-resolution?
;  ; If node needed to be cloned with new instance name
```
</details>


**7.** `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:480`

**Issue:** ;FIXME: Why occurence of a node in more than one relation

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
(and
(is-r2l-inst? n)
;FIXME: Why occurence of a node in more than one relation
; matter? One reason is if the instance-node is renamed
; then one wouldn't want to rename the same instance to
```
</details>


**8.** `components/integration/opencog/opencog/nlp/sureal/SuRealSCM.cc:171`

**Issue:** XXX perhaps it's better to write a eval_q in SchemeEval to convert

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

// get all the nodes to be treated as variable in the Pattern Matcher
// XXX perhaps it's better to write a eval_q in SchemeEval to convert
//     a scm list to HandleSeq, so can just use the scheme utilities?
UnorderedHandleSet allNodes;
```
</details>


**9.** `components/integration/opencog/opencog/nlp/scm/oc/relex-utils.scm:176`

**Issue:** FIXME: this is a dumb way to get other type

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
(let ((wlist (cog-chase-link 'LemmaLink 'WordNode word-inst)))
(if (nil? wlist)
; FIXME: this is a dumb way to get other type
(let ((nlist (cog-chase-link 'LemmaLink 'NumberNode word-inst)))
(if (nil? nlist)
```
</details>


**10.** `components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:211`

**Issue:** XXX FIXME: really, if a or b are vars, then they are WordInstanceNodes.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

; Return the variables and clauses in an association list
; XXX FIXME: really, if a or b are vars, then they are WordInstanceNodes.
; XXX However, to fix this, we will need to modify the varscope code to
; merge together lits of possibly duplicate var decls!
```
</details>


**11.** `components/integration/opencog/opencog/nlp/chatbot-old/wordnet-import/wni.c:340`

**Issue:** XXX ?? is there some reason these are not "DefinedLinguisticConceptNode" ??

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
#endif

// XXX ?? is there some reason these are not "DefinedLinguisticConceptNode" ??
// I'd think they should be, right ... ? Is this a bug ??
printf("scm\n");
```
</details>


**12.** `components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:361`

**Issue:** XXX this needs to be replaced in the end, for now its just a cheesy

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

// And now for a cheesy hack to report the solution
// XXX this needs to be replaced in the end, for now its just a cheesy
// hack to pass data back to scheme.
Handle hq = atom_space->addNode(ANCHOR_NODE, "# QUERY SOLUTION");
```
</details>


**13.** `components/language/learn/learn-lang-diary/utils/ortho-compute.scm:57`

**Issue:** XXX todo store mean-rms on any-node.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
(gor 'mean-rms)

; XXX todo store mean-rms on any-node.

(define gos (add-similarity-api ami #f "goe"))
```
</details>


**14.** `components/language/learn/scm/gram-class/goe-similarity.scm:57`

**Issue:** XXX todo store mean-rms on any-node.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
(goe 'mean-rms)

; XXX todo store mean-rms on any-node.

(define gos (add-similarity-api ami #f "goe"))
```
</details>


**15.** `components/language/lg-atomese/opencog/nlp/lg-parse/LGParseLink.cc:229`

**Issue:** XXX FIXME. This should be part of the LgDictNode but since

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
// This must happen before ldn->get_dictionary() because the
// setup is stateful. This seems buggy, but is adequate for now.
// XXX FIXME. This should be part of the LgDictNode but since
// LgDictNode is a node, not a link, its hard to pass args.
// We would need to wrap it with a StateLink, or maybe use the
```
</details>


**16.** `components/learning/moses/moses/moses/scoring/bscores.cc:930`

**Issue:** /XXX this should probably be removed! TODO FIXME

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
/// formula (the edges, with are printed by hand, below).
///
/// XXX this should probably be removed! TODO FIXME

cluster_bscore::cluster_bscore(const ITable& itable)
```
</details>


**17.** `components/learning/moses/moses/moses/moses/mpi_moses.cc:420`

**Issue:** XXX is mp.best_score thread safe !???? since another thread might be updating this as we

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
// If we are here, then we are the root node.  The root will act
// as a dispatcher to all of the worker nodes.
// XXX is mp.best_score thread safe !???? since another thread might be updating this as we
// come around ...

```
</details>


**18.** `components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:130`

**Issue:** //* $Nxxx are ann_nodes and $Ixxx are ann_inputs

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
//*
//* ANN strings must begin with $N or $I
//* $Nxxx are ann_nodes and $Ixxx are ann_inputs
//
bool ann_str_to_vertex(const std::string& str, vertex& v)
```
</details>


### Moses Component (3 items)


**1.** `moses/moses/moses/scoring/bscores.cc:930`

**Issue:** /XXX this should probably be removed! TODO FIXME

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
/// formula (the edges, with are printed by hand, below).
///
/// XXX this should probably be removed! TODO FIXME

cluster_bscore::cluster_bscore(const ITable& itable)
```
</details>


**2.** `moses/moses/moses/moses/mpi_moses.cc:417`

**Issue:** XXX is mp.best_score thread safe !???? since another thread might be updating this as we

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
// If we are here, then we are the root node.  The root will act
// as a dispatcher to all of the worker nodes.
// XXX is mp.best_score thread safe !???? since another thread might be updating this as we
// come around ...

```
</details>


**3.** `moses/moses/comboreduct/combo/iostream_combo.cc:130`

**Issue:** //* $Nxxx are ann_nodes and $Ixxx are ann_inputs

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
//*
//* ANN strings must begin with $N or $I
//* $Nxxx are ann_nodes and $Ixxx are ann_inputs
//
bool ann_str_to_vertex(const std::string& str, vertex& v)
```
</details>



## ⚡ Hard Priority (55 items)

### Atomspace Component (10 items)


**1.** `atomspace/opencog/query/TermMatchMixin.cc:197`

**Issue:** XXX The assert below -- if we hit this, then we have nested

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
if (ptm->isQuoted()) return true;

// XXX The assert below -- if we hit this, then we have nested
// scoped links. The correct fix would be to push these onto a
// stack, and then alter scope_match() to walk the stack,
```
</details>


**2.** `atomspace/opencog/query/PatternMatchEngine.cc:1116`

**Issue:** XXX The current implementation is a brute-force search, and is highly

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// "sparse".)
//
// XXX The current implementation is a brute-force search, and is highly
// inefficient for truly sparse searches. A (vastly) superior search
// woudld be to obtain the connected components in the search set, and
```
</details>


**3.** `atomspace/opencog/query/PatternMatchEngine.cc:2047`

**Issue:** /XXX FIXME: Right now, this code handles graphs that have only one

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// is not a part of the functional group.
///
/// XXX FIXME: Right now, this code handles graphs that have only one
/// single sparse search.   Nested sparse searches are not supported;
/// to implement those, its "easy": implement the same flow control as
```
</details>


**4.** `atomspace/opencog/query/PatternMatchEngine.cc:2739`

**Issue:** * XXX TODO -- if the algo is working correctly, then all

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/**
* Unconditionally clear all graph traversal stacks
* XXX TODO -- if the algo is working correctly, then all
* of these should already be empty, when this method is
* called. So really, we should check the stack size, and
```
</details>


**5.** `atomspace/opencog/atomspace/AtomSpace.cc:283`

**Issue:** XXX FIXME -- The recursive design of the depth() routine below makes

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

// ====================================================================
// XXX FIXME -- The recursive design of the depth() routine below makes
// it into a bottleneck, when the stack of AtomSpaces exceeds a few
// hundred. In particular, the recursion is on the C stack, and I don't
```
</details>


**6.** `atomspace/opencog/atomspace/Transient.cc:43`

**Issue:** /XXX FIXME. Performance has not been recently measuredthere

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// and ready to go. The code in this section implements this.
///
/// XXX FIXME. Performance has not been recently measured; there
/// have been a lot of redesigns since when this utility was created.
/// It is not at all clear that the code here takes less CPU/RAM than
```
</details>


**7.** `atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:59`

**Issue:** /XXX TODO: We could have a non-blocking version of this atom. We

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// threads is the smaller of the NumberNode and the seize of the Set.
///
/// XXX TODO: We could have a non-blocking version of this atom. We
/// could just return the QueueValue immediately; the user could check
/// to see if the queue is closed, to find out if the threads have
```
</details>


**8.** `atomspace/opencog/atoms/pattern/PatternJit.cc:47`

**Issue:** as well.  XXX Except that this is wrong, if any of the

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// Now is the time to look up the definitions!
// We loop here, so that all recursive definitions are expanded
// as well.  XXX Except that this is wrong, if any of the
// definitions are actually recursive. That is, this will be
// an infinite loop if a definition is self-referencing; so
```
</details>


**9.** `atomspace/opencog/atoms/execution/EvaluationLink.cc:824`

**Issue:** XXX Is there a more efficient way to do this copy?

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
{
// Copy all but the first.
// XXX Is there a more efficient way to do this copy?
size_t sz = sna.size();
for (size_t i=1; i<sz; i++) args.push_back(sna[i]);
```
</details>


**10.** `atomspace/opencog/atoms/base/Valuation.cc:50`

**Issue:** XXX TODO -- C++ smart pointers are not atomicwe really

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
void Valuation::setValue(const ValuePtr& v)
{
// XXX TODO -- C++ smart pointers are not atomic; we really
// need to use a lock here, to avoid thread-races.
_value = v;
```
</details>


### Atomspace-Storage Component (1 items)


**1.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:164`

**Issue:** XXX this should be nuked, and replaced by appropriate kind of proxy.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// (cog-execute-cache! (GetLink ...) (Predicate "key") ...)
// This is complicated, and subject to change...
// XXX this should be nuked, and replaced by appropriate kind of proxy.
// FIXME read above comment.
std::string Commands::cog_execute_cache(const std::string& cmd)
```
</details>


### Cogserver Component (1 items)


**1.** `cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc:114`

**Issue:** which seems inefficient. But still ... XXX FIXME ?

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// Sexpr::decode_slist to return a list of keys,
// and then we'd have to store one key at a time,
// which seems inefficient. But still ... XXX FIXME ?
for (const StorageNodePtr& snp : _targets)
snp->store_atom(atom);
```
</details>


### Components Component (27 items)


**1.** `components/integration/opencog/opencog/openpsi/rule.scm:211`

**Issue:** ;faster if cog-chase-link was used instead. FIXME.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
"
;; Using a GetLink here is quite inefficient; this would run much
;; faster if cog-chase-link was used instead. FIXME.
(cog-outgoing-set (cog-execute!
(GetLink
```
</details>


**2.** `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:258`

**Issue:** XXX FIXME -- Implement-me, actually -- need to do the above, but for

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
(ConceptNode "schema-thing")))

; XXX FIXME -- Implement-me, actually -- need to do the above, but for
; the self-model, rather than the direct robot action.  The self-model
; is not being updated by these imperatives.
```
</details>


**3.** `components/integration/opencog/opencog/nlp/wsd/MihalceaEdge.cc:248`

**Issue:** cache (if it exists). XXX This appears to be a loosing strategy,

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
#ifdef USE_LOCAL_CACHE
// Get the similarity between the two word senses out of the
// cache (if it exists). XXX This appears to be a loosing strategy,
// See the README file for details.  The core problem is that the
// cache is using the atomspace in a very inefficient way. XXX
```
</details>


**4.** `components/integration/opencog/opencog/nlp/wsd/MihalceaEdge.cc:250`

**Issue:** cache is using the atomspace in a very inefficient way. XXX

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// cache (if it exists). XXX This appears to be a loosing strategy,
// See the README file for details.  The core problem is that the
// cache is using the atomspace in a very inefficient way. XXX
SimpleTruthValue stv(0.5,0.5);
stv = sense_cache.similarity(first_word_sense, second_word_sense_h);
```
</details>


**5.** `components/integration/opencog/opencog/nlp/sureal/SuRealCache.h:38`

**Issue:** * XXX THIS IS A BROKEN DESIGN! -- FIXME! (The fix is easy) The cache

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* A Cache between SuReal and PatternMatcher. This class is a Singleton.
*
* XXX THIS IS A BROKEN DESIGN! -- FIXME! (The fix is easy) The cache
* needs to be kept in the atomspace; just put the atoms there, keep
* them there, put them in well-known locations!  The problem with this
```
</details>


**6.** `components/integration/opencog/opencog/nlp/sureal/SuRealCache.h:43`

**Issue:** * FIXME by getting rid of this class!!!

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* cache is that it does NOT work with multiple atomspaces; see for
* example SuRealUTest, which bombs when multiple atomspces are used!!
* FIXME by getting rid of this class!!!
*
* This cache stores the results of calls to differents methods of PatternMatcherCallBack
```
</details>


**7.** `components/language/learn/scm/utils/utilities.scm:377`

**Issue:** FIXME: use a thread-safe test-n-set instead.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
(define new (append old (list val)))

; FIXME: use a thread-safe test-n-set instead.
(cog-set-value! ATOM KEY (cog-new-value typ new)))
)
```
</details>


**8.** `components/language/learn/scm/attic/cluster/gram-pairwise.scm:592`

**Issue:** XXX TODO once make-merge-majority is done, this can be reimplemented

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
; ---------------------------------------------------------------

; XXX TODO once make-merge-majority is done, this can be reimplemented
; as a special case of that. That means that the above three functions
; can be discarded. It also means that `gram-class-api` 'make-cluster
```
</details>


**9.** `components/language/learn/attic/run-ull-2019/SchemeEval.cc:93`

**Issue:** XXX FIXME This lock is not needed, because in guile-2.2,

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

// Lock to prevent racey setting of the output port.
// XXX FIXME This lock is not needed, because in guile-2.2,
// at least, every thread has its own output port, and so its
// impossible for two different threads to compete to set the
```
</details>


**10.** `components/language/lg-atomese/opencog/nlp/lg-dict/LGDictExpContainer.cc:268`

**Issue:** XXX FIXME ... using an std::map would be more efficient.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
if (m_type == OR_type)
{
// XXX FIXME ... using an std::map would be more efficient.
std::sort(outgoing.begin(), outgoing.end());
outgoing.erase(std::unique(outgoing.begin(),
```
</details>


**11.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc:240`

**Issue:** XXX TODO: we should probably cache the results, instead of

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
std::map<uint64_t, Handle>& order)
{
// XXX TODO: we should probably cache the results, instead of
// recomputing every time!?
// As long as there's a stack of Frames, just loop.
```
</details>


**12.** `components/learning/moses/moses/moses/metapopulation/metapopulation.h:195`

**Issue:** /minor though in terms of performance gain. FIXME.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// scored_combo_tree_ptr_set and not having to copy and
/// reallocate candidates once they are selected. It might be
/// minor though in terms of performance gain. FIXME.
void merge_candidates(scored_combo_tree_set& candidates);

```
</details>


**13.** `components/learning/moses/moses/moses/metapopulation/merging.cc:579`

**Issue:** XXX this lock probably doesn't have to be the same one

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

// Make this routine thread-safe.
// XXX this lock probably doesn't have to be the same one
// that merge uses.  I think.
std::lock_guard<std::mutex> lock(_merge_mutex);
```
</details>


**14.** `components/learning/moses/moses/moses/scoring/scoring_base.cc:108`

**Issue:** XXX FIXME complexity_t should be a double not an int ...

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME complexity_t should be a double not an int ...
return (complexity_t) floor (cpxy / norm + 0.5);
}
```
</details>


**15.** `components/learning/moses/moses/moses/moses/neighborhood_sampling.h:249`

**Issue:** * XXX TODO: the current algo could be speeded up a fair bit, cutting

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* This function is the main work-horse for generate_all_in_neighborhood().
*
* XXX TODO: the current algo could be speeded up a fair bit, cutting
* out some of the if tests, and the final recursive call.
*
```
</details>


**16.** `components/learning/moses/moses/moses/moses/neighborhood_sampling.h:494`

**Issue:** * XXX/TODO: the performance of this thing can be strongly improved

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* is exceeded.
*
* XXX/TODO: the performance of this thing can be strongly improved
* by looping on the tail-call, just as in the xxx routine...
*
```
</details>


**17.** `components/learning/moses/moses/moses/moses/complexity.cc:37`

**Issue:** if we did decide to count the operators.  (XXX But why  not count

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// and, in that case, the complexity is "almost" a linear function
// of the number of arguments -- well, at least, its sub-quadratic,
// if we did decide to count the operators.  (XXX But why  not count
// the operators?  For ENF (elegant normal form) there should be even
// fewer operators than either CNF or DNF, so counting operators
```
</details>


**18.** `components/learning/moses/moses/moses/moses/complexity.cc:42`

**Issue:** XXX What is the complexity of contin expressions?

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// doesn't seem wrong to me ...)
//
// XXX What is the complexity of contin expressions?
// Expressions containining greater_than_zero, impulse, cond?  I'm
// somwhat confused about how thigs are being measured.   Note that
```
</details>


**19.** `components/learning/moses/moses/moses/representation/build_knobs.cc:312`

**Issue:** /XXX There's a deep problem here: this probing and complexity measuring

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// knob will be discarded.
///
/// XXX There's a deep problem here: this probing and complexity measuring
/// can be rather incredibly costly, especially when the exemplars start
/// getting large.  So the real question is: is the performance cost of
```
</details>


**20.** `components/learning/moses/moses/moses/representation/build_knobs.cc:343`

**Issue:** /_exemplar simpler (??? XXX ??? huh?)

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// there is a strange thing with kb.complexity_bound()
/// because apparently when it is 0 it actually makes
/// _exemplar simpler (??? XXX ??? huh?)

// We halt complexity searches underneath contins, since anything
```
</details>


**21.** `components/learning/moses/moses/moses/representation/knobs.h:123`

**Issue:** /turned to.   XXX This method is never called by anyone, at this

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

/// Expected complexity based on whatever the knob is currently
/// turned to.   XXX This method is never called by anyone, at this
/// time ...
virtual complexity_t complexity_bound() const = 0;
```
</details>


**22.** `components/learning/moses/moses/feature-selection/algo/simple.cc:43`

**Issue:** XXX: fsc(all_features) is skipped because that algorithm is

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// Nothing happened, return all features by default

// XXX: fsc(all_features) is skipped because that algorithm is
// used in combination with contin MI in
// feature_selectionUTest.cxxtest and contin MI does not
```
</details>


**23.** `components/learning/moses/moses/feature-selection/algo/simple.cc:55`

**Issue:** XXX: fsc(all_features) is skipped because that algorithm is

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
fs_params.exp_distrib,
fs_params.threshold);
// XXX: fsc(all_features) is skipped because that algorithm is
// used in combination with contin MI in
// feature_selectionUTest.cxxtest and contin MI does not support
```
</details>


**24.** `components/learning/moses/moses/comboreduct/table/table.h:98`

**Issue:** XXX FIXME TODO: change the implementation, per the above note.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// design here should be changed, so that the space-savings is still
// realized, while also allowing different types for different columns.
// XXX FIXME TODO: change the implementation, per the above note.

typedef std::vector<builtin> builtin_seq;
```
</details>


**25.** `components/learning/moses/moses/comboreduct/table/table.h:1075`

**Issue:** XXX TODO to implement enum support, cut-n-paste from CTable

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
double mutualInformation(const ITable& it, const OTable& ot, const FeatureSet& fs)
{
// XXX TODO to implement enum support, cut-n-paste from CTable
// mutual info code, below.
type_node otype = ot.get_type();
```
</details>


**26.** `components/learning/moses/moses/comboreduct/table/table.h:1287`

**Issue:** XXX TODO remove this print, for better performance.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// I thought that IC was supposed to max out at 1.0 !?
contin_t ic = - KLD(p,q);
// XXX TODO remove this print, for better performance.
unsigned idx = *(fs.begin());
logger().debug() <<"Contin MI for feat=" << idx << " ic=" << ic;
```
</details>


**27.** `components/learning/moses/moses/comboreduct/reduct/logical_reduction.cc:115`

**Issue:** Arghh .. XXX should use reduct_effort==3 for the complexe rule.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
downwards(reduce_ors(), id::boolean_type));

// Arghh .. XXX should use reduct_effort==3 for the complexe rule.
int reduct_effort = 2;

```
</details>


### Moses Component (16 items)


**1.** `moses/moses/moses/metapopulation/metapopulation.h:195`

**Issue:** /minor though in terms of performance gain. FIXME.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// scored_combo_tree_ptr_set and not having to copy and
/// reallocate candidates once they are selected. It might be
/// minor though in terms of performance gain. FIXME.
void merge_candidates(scored_combo_tree_set& candidates);

```
</details>


**2.** `moses/moses/moses/metapopulation/merging.cc:579`

**Issue:** XXX this lock probably doesn't have to be the same one

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

// Make this routine thread-safe.
// XXX this lock probably doesn't have to be the same one
// that merge uses.  I think.
std::lock_guard<std::mutex> lock(_merge_mutex);
```
</details>


**3.** `moses/moses/moses/scoring/scoring_base.cc:142`

**Issue:** XXX FIXME complexity_t should be a double not an int ...

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME complexity_t should be a double not an int ...
return (complexity_t) floor (cpxy / norm + 0.5);
}
```
</details>


**4.** `moses/moses/moses/moses/neighborhood_sampling.h:249`

**Issue:** * XXX TODO: the current algo could be speeded up a fair bit, cutting

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* This function is the main work-horse for generate_all_in_neighborhood().
*
* XXX TODO: the current algo could be speeded up a fair bit, cutting
* out some of the if tests, and the final recursive call.
*
```
</details>


**5.** `moses/moses/moses/moses/neighborhood_sampling.h:477`

**Issue:** * XXX/TODO: the performance of this thing can be strongly improved

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* is exceeded.
*
* XXX/TODO: the performance of this thing can be strongly improved
* by looping on the tail-call, just as in the xxx routine...
*
```
</details>


**6.** `moses/moses/moses/moses/complexity.cc:37`

**Issue:** if we did decide to count the operators.  (XXX But why  not count

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// and, in that case, the complexity is "almost" a linear function
// of the number of arguments -- well, at least, its sub-quadratic,
// if we did decide to count the operators.  (XXX But why  not count
// the operators?  For ENF (elegant normal form) there should be even
// fewer operators than either CNF or DNF, so counting operators
```
</details>


**7.** `moses/moses/moses/moses/complexity.cc:42`

**Issue:** XXX What is the complexity of contin expressions?

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// doesn't seem wrong to me ...)
//
// XXX What is the complexity of contin expressions?
// Expressions containining greater_than_zero, impulse, cond?  I'm
// somwhat confused about how thigs are being measured.   Note that
```
</details>


**8.** `moses/moses/moses/representation/build_knobs.cc:312`

**Issue:** /XXX There's a deep problem here: this probing and complexity measuring

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// knob will be discarded.
///
/// XXX There's a deep problem here: this probing and complexity measuring
/// can be rather incredibly costly, especially when the exemplars start
/// getting large.  So the real question is: is the performance cost of
```
</details>


**9.** `moses/moses/moses/representation/build_knobs.cc:343`

**Issue:** /_exemplar simpler (??? XXX ??? huh?)

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// there is a strange thing with kb.complexity_bound()
/// because apparently when it is 0 it actually makes
/// _exemplar simpler (??? XXX ??? huh?)

// We halt complexity searches underneath contins, since anything
```
</details>


**10.** `moses/moses/moses/representation/knobs.h:123`

**Issue:** /turned to.   XXX This method is never called by anyone, at this

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

/// Expected complexity based on whatever the knob is currently
/// turned to.   XXX This method is never called by anyone, at this
/// time ...
virtual complexity_t complexity_bound() const = 0;
```
</details>


**11.** `moses/moses/feature-selection/algo/simple.cc:43`

**Issue:** XXX: fsc(all_features) is skipped because that algorithm is

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// Nothing happened, return all features by default

// XXX: fsc(all_features) is skipped because that algorithm is
// used in combination with contin MI in
// feature_selectionUTest.cxxtest and contin MI does not
```
</details>


**12.** `moses/moses/feature-selection/algo/simple.cc:55`

**Issue:** XXX: fsc(all_features) is skipped because that algorithm is

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
fs_params.exp_distrib,
fs_params.threshold);
// XXX: fsc(all_features) is skipped because that algorithm is
// used in combination with contin MI in
// feature_selectionUTest.cxxtest and contin MI does not support
```
</details>


**13.** `moses/moses/comboreduct/table/table.h:99`

**Issue:** XXX FIXME TODO: change the implementation, per the above note.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// design here should be changed, so that the space-savings is still
// realized, while also allowing different types for different columns.
// XXX FIXME TODO: change the implementation, per the above note.

typedef std::vector<builtin> builtin_seq;
```
</details>


**14.** `moses/moses/comboreduct/table/table.h:1076`

**Issue:** XXX TODO to implement enum support, cut-n-paste from CTable

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
double mutualInformation(const ITable& it, const OTable& ot, const FeatureSet& fs)
{
// XXX TODO to implement enum support, cut-n-paste from CTable
// mutual info code, below.
type_node otype = ot.get_type();
```
</details>


**15.** `moses/moses/comboreduct/table/table.h:1288`

**Issue:** XXX TODO remove this print, for better performance.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// I thought that IC was supposed to max out at 1.0 !?
contin_t ic = - KLD(p,q);
// XXX TODO remove this print, for better performance.
unsigned idx = *(fs.begin());
logger().debug() <<"Contin MI for feat=" << idx << " ic=" << ic;
```
</details>


**16.** `moses/moses/comboreduct/reduct/logical_reduction.cc:115`

**Issue:** Arghh .. XXX should use reduct_effort==3 for the complexe rule.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
downwards(reduce_ors(), id::boolean_type));

// Arghh .. XXX should use reduct_effort==3 for the complexe rule.
int reduct_effort = 2;

```
</details>



## 📋 Medium Priority (479 items)

### Atomspace Component (97 items)


**1.** `atomspace/examples/pattern-matcher/deduction-engine.scm:9`

**Issue:** ;XXX under construction, incomplete. The correct fix is to remove

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; goal of this example is to show how to do that.
;
;; XXX under construction, incomplete. The correct fix is to remove
;; BindLink everywhere below, and use UnifierLink instead, according
;; to the examples demoing the unifier.
```
</details>


**2.** `atomspace/opencog/guile/SchemeModule.cc:60`

**Issue:** XXX we should also allow opt-args to be a list of handles

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
Handle FunctionWrap::as_wrapper_h_h(Handle h)
{
// XXX we should also allow opt-args to be a list of handles
const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
AtomSpace* as = asp.get();
```
</details>


**3.** `atomspace/opencog/guile/SchemeModule.cc:68`

**Issue:** XXX we should also allow opt-args to be a list of handles

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
Handle FunctionWrap::as_wrapper_h_hz(Handle h, size_t sz)
{
// XXX we should also allow opt-args to be a list of handles
const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
AtomSpace* as = asp.get();
```
</details>


**4.** `atomspace/opencog/guile/SchemeModule.cc:76`

**Issue:** XXX we should also allow opt-args to be a list of handles

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
TruthValuePtr FunctionWrap::as_wrapper_p_h(Handle h)
{
// XXX we should also allow opt-args to be a list of handles
const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
AtomSpace* as = asp.get();
```
</details>


**5.** `atomspace/opencog/guile/SchemeModule.cc:84`

**Issue:** XXX we should also allow opt-args to be a list of handles

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
ValuePtr FunctionWrap::as_wrapper_v_h(Handle h)
{
// XXX we should also allow opt-args to be a list of handles
const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
AtomSpace* as = asp.get();
```
</details>


**6.** `atomspace/opencog/guile/SchemeSmobAtom.cc:84`

**Issue:** XXX FIXME. Work around the despicable, horrible guile UTF8 handling.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/* ============================================================== */

// XXX FIXME. Work around the despicable, horrible guile UTF8 handling.
// I am flabbergasted. The guile people are smart, but they could not have
// possibly picked a crappier string handling design. Fuck me. See
```
</details>


**7.** `atomspace/opencog/ocaml/CamlWrap.cc:57`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
opstbl.finalize = finalize;

// XXX FIXME
opstbl.compare = custom_compare_default;
opstbl.hash = custom_hash_default;
```
</details>


**8.** `atomspace/opencog/ocaml/CamlWrap.cc:206`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (vp->is_atom()) return oc_to_caml_str(HandleCast(vp), indent);

// XXX FIXME
return vp->to_short_string();
}
```
</details>


**9.** `atomspace/opencog/query/InitiateSearchMixin.cc:169`

**Issue:** XXX TODO We could start inside an evaluatable, but it would

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Ignore all dynamically-evaluatable links up front.
// However, we are allowed to start inside of IdenticalLinks.
// XXX TODO We could start inside an evaluatable, but it would
// be better to try elsewhere, first. Special-case Identical.
if (ptm->hasEvaluatable() and not ptm->isIdentical())
```
</details>


**10.** `atomspace/opencog/query/InitiateSearchMixin.cc:268`

**Issue:** So we are good to go. XXX FIXME -- we could try again, to find

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// If we encountered choices, then we have enumerated all of them.
// So we are good to go. XXX FIXME -- we could try again, to find
// some thinner set of choices. Later, some other time.
if (0 < _start_choices.size()) break;
```
</details>


**11.** `atomspace/opencog/query/InitiateSearchMixin.cc:368`

**Issue:** XXX ?? Why incoming set ???

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (_starter_term->getHandle()->is_link())
{
// XXX ?? Why incoming set ???
ch.search_set = get_incoming_set(best_start,
_starter_term->getHandle()->get_type());
```
</details>


**12.** `atomspace/opencog/query/InitiateSearchMixin.cc:730`

**Issue:** no unit test triggers this, but its not clear why. XXX FIXME??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// If this is used in a way such that the handle appears under a
// ChoiceLink, then an incomplete search will result!  Right now,
// no unit test triggers this, but its not clear why. XXX FIXME??
// For this case, use the `term_choices_of_handle` below.
PatternTermPtr InitiateSearchMixin::term_of_handle(const Handle& h,
```
</details>


**13.** `atomspace/opencog/query/NextSearchMixin.cc:167`

**Issue:** XXX TODO ... Rather than counting the number of variables, we

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// "unit propagation" in the DPLL algorithm.
//
// XXX TODO ... Rather than counting the number of variables, we
// should instead look for one with the smallest incoming set.
// That is because the very next thing that we do will be to
```
</details>


**14.** `atomspace/opencog/query/SatisfyMixin.cc:583`

**Issue:** XXX FIXME terrible hack.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (is_pure_absent)
{
// XXX FIXME terrible hack.
TermMatchMixin* intu =
dynamic_cast<TermMatchMixin*>(this);
```
</details>


**15.** `atomspace/opencog/query/PatternMatchEngine.h:189`

**Issue:** with it. XXX Needs to move to the Mixin class... XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// State that manages the next PresentLink subterm to be grounded.
// Similar to the next-clause, above, and someday should be unified
// with it. XXX Needs to move to the Mixin class... XXX FIXME.

bool next_untried_present(const PatternTermPtr&,
```
</details>


**16.** `atomspace/opencog/query/TermMatchMixin.cc:551`

**Issue:** XXX TODO as discussed on the mailing list, we should perhaps first

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//       Arg2Atom
//
// XXX TODO as discussed on the mailing list, we should perhaps first
// see if the following can be found in the atomspace:
//
```
</details>


**17.** `atomspace/opencog/query/TermMatchMixin.cc:702`

**Issue:** XXX ... This might be buggyI'm confused. Deep in the bowels

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// must be present" is implemented by PresentLink.
//
// XXX ... This might be buggy; I'm confused. Deep in the bowels
// of the pattern matcher, we make an explicit promise to explore
// all possible choices.  Here, we are making no such promise;
```
</details>


**18.** `atomspace/opencog/query/TermMatchMixin.cc:710`

**Issue:** XXX FIXME: worse: this cannot possibly be right when

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// anything about that here? Seems like we can't do anything...
//
// XXX FIXME: worse: this cannot possibly be right when
// the ChoiceLink contains presentLinks.
for (const Handle& h : oset)
```
</details>


**19.** `atomspace/opencog/query/RewriteMixin.cc:118`

**Issue:** See issue #950 and pull req #962. XXX FIXME later.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// nothing.  In practice it is difficult to insure, so
// meanwhile this try-catch is used.
// See issue #950 and pull req #962. XXX FIXME later.
// Tested by BuggyBindLinkUTest and NoExceptionUTest.
// Well, given that URE is dead meat, maybe we can remove this?
```
</details>


**20.** `atomspace/opencog/query/RewriteMixin.cc:164`

**Issue:** /XXX FIXME now I see how it can be done. The groupings should

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// clever way to find groupings in a single batch; but for now, I don't
/// see how this could be done.
/// XXX FIXME now I see how it can be done. The groupings should
/// be converted to marginals, and handled the same way. So this
/// needs a rewrite. Good thing that almost no one uses this ...
```
</details>


**21.** `atomspace/opencog/query/PatternMatchEngine.cc:284`

**Issue:** /XXX FIXME: this is currently a weak stop-gap measure to handle

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// proposed grounding. The term `ptm` points at the Present term.
///
/// XXX FIXME: this is currently a weak stop-gap measure to handle
/// the special case of Present terms embedded in Choice terms.
/// Present terms that are NOT in a Choice are handled by the
```
</details>


**22.** `atomspace/opencog/query/PatternMatchEngine.cc:335`

**Issue:** XXX This is almost surely wrong... if there are two

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
<< ", choose_next=" << _choose_next;})

// XXX This is almost surely wrong... if there are two
// nested choice links, then this will hog the steps,
// and the deeper choice will fail.
```
</details>


**23.** `atomspace/opencog/query/PatternMatchEngine.cc:805`

**Issue:** XXX should we be clearing ... or popping this flag?

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
POPSTK(_perm_breakout_stack, _perm_breakout);

// XXX should we be clearing ... or popping this flag?
_perm_go_around = false;

```
</details>


**24.** `atomspace/opencog/query/PatternMatchEngine.cc:956`

**Issue:** XXX why are we not doing any checks to see if the

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
else
{
// XXX why are we not doing any checks to see if the
// grounding meets the variable constraints?
glob_pos_stack.push({glob, {ip, jg}});
```
</details>


**25.** `atomspace/opencog/query/PatternMatchEngine.cc:1005`

**Issue:** XXX Huh ???

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Just in case, if the upper bound is zero...
// XXX Huh ???
if (not _variables->is_upper_bound(ohp, 1))
{
```
</details>


**26.** `atomspace/opencog/query/PatternMatchEngine.cc:1159`

**Issue:** XXX TODO The logic here should be updated to resemble that

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
<< ptm->to_string();})

// XXX TODO The logic here should be updated to resemble that
// in curr_perm(), which deals correctly with nested permutations
// of unordered patterns. For just right now, we are not
```
</details>


**27.** `atomspace/opencog/query/PatternMatchEngine.cc:1355`

**Issue:** I think this is correct. But it's untested! XXX verify!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
else if (pto->hasChoice())
{
// I think this is correct. But it's untested! XXX verify!
_choose_next = true;
if (-1 ==  ig) ig++;
```
</details>


**28.** `atomspace/opencog/query/PatternMatchEngine.cc:1533`

**Issue:** XXX FIXME - this is not very elegant. We should probably

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// search pattern, i.e. if is is a scoped variable, then
// accept a match to any other alpha-equivalent variable.
// XXX FIXME - this is not very elegant. We should probably
// have a distinct `scoped_link_compare()` function to handle
// this. Right now, the scope_match() callback uses a rather
```
</details>


**29.** `atomspace/opencog/query/PatternMatchEngine.cc:1628`

**Issue:** XXX I'm not convinced this is right, if there are mixtures

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Check if the pattern has globs in it.
// XXX I'm not convinced this is right, if there are mixtures
// of unordered and globby links in different places...
if (parent->hasAnyGlobbyVar())
```
</details>


**30.** `atomspace/opencog/query/PatternMatchEngine.cc:1980`

**Issue:** XXX FIXME: Issue #3016 - Unification with unordered AndLinks

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
///
//
// XXX FIXME: Issue #3016 - Unification with unordered AndLinks
// The current implementation of unordered link permutation exploration
// in IdenticalLinks stops after finding the first valid permutation
```
</details>


**31.** `atomspace/opencog/query/PatternMatchEngine.cc:2063`

**Issue:** XXX TODO FIXME. The ptm needs to be decomposed into connected

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
logmsg("Explore sparse: Start exploration");

// XXX TODO FIXME. The ptm needs to be decomposed into connected
// components. Then only the connected components need to be walked
// over.  That would be much more efficient.
```
</details>


**32.** `atomspace/opencog/query/PatternMatchEngine.cc:2173`

**Issue:** XXX This `need_choice_push` thing is probably wrongit probably

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
logmsg("Begin choice branchpoint iteration loop");
do {
// XXX This `need_choice_push` thing is probably wrong; it probably
// should resemble the perm_push() used for unordered links.
// However, currently, no test case trips this up. so .. OK.
```
</details>


**33.** `atomspace/opencog/query/PatternMatchEngine.cc:2221`

**Issue:** /XXX FIXME -- do the above.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// -- build a clause_variables struct, but just for this term
/// -- search for the thinnest joint, just like `get_next_clause`
/// XXX FIXME -- do the above.
///
bool PatternMatchEngine::next_untried_present(const PatternTermPtr& parent,
```
</details>


**34.** `atomspace/opencog/query/PatternMatchEngine.cc:2440`

**Issue:** XXX TODO make sure that all variables in the clause have

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
{
OC_ASSERT(false, "Hit some dead code!");
// XXX TODO make sure that all variables in the clause have
// been grounded!  If they're not, something is badly wrong!
logmsg("Term inside evaluatable, move up to it's top:",
```
</details>


**35.** `atomspace/opencog/cython/PythonEval.cc:1427`

**Issue:** XXX FIXME this does a lot of wasteful string copying.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void PythonEval::eval_expr(const std::string& partial_expr)
{
// XXX FIXME this does a lot of wasteful string copying.
std::string expr = partial_expr;
size_t nl = expr.find_first_of("\n\r");
```
</details>


**36.** `atomspace/opencog/cython/PyIncludeWrapper.h:4`

**Issue:** XXX Cython currently conflicts with standard C library definitions.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#ifdef HAVE_CYTHON

// XXX Cython currently conflicts with standard C library definitions.
// The push/pop below should hush it, for now. (needed for cython
// 0.15.1 and maybe other versions)  FIXME someday...
```
</details>


**37.** `atomspace/opencog/cython/PyIncludeWrapper.h:6`

**Issue:** 0.15.1 and maybe other versions)  FIXME someday...

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// XXX Cython currently conflicts with standard C library definitions.
// The push/pop below should hush it, for now. (needed for cython
// 0.15.1 and maybe other versions)  FIXME someday...
#ifdef _GNU_SOURCE
#pragma push_macro("_POSIX_C_SOURCE")
```
</details>


**38.** `atomspace/opencog/atomspace/AtomSpace.cc:272`

**Issue:** Fixme maybe later someday, if/when this is needed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// except that we don't have any viable mechanisms for such multiple
// membership, and so I don't know how to treat this right now.
// Fixme maybe later someday, if/when this is needed.
if (not (nullptr == _atom_space or as == nullptr))
throw RuntimeException(TRACE_INFO,
```
</details>


**39.** `atomspace/opencog/atomspace/AtomTable.cc:782`

**Issue:** atom in the parent. What??? XXX NOT TRUE FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// If an atom is already in the set, it will hide any duplicate
// atom in the parent. What??? XXX NOT TRUE FIXME
if (parent) {
for (const AtomSpacePtr& base : _environ)
```
</details>


**40.** `atomspace/opencog/atomspace/TypeIndex.h:54`

**Issue:** #if HAVE_FOLLY_XXX

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//    one failure is enough to say "not recommended." I don't need
//    to be chasing obscure bugs.
#if HAVE_FOLLY_XXX
typedef folly::F14ValueSet<Handle> AtomSet;
#else
```
</details>


**41.** `atomspace/opencog/haskell/AtomSpace_CWrapper.h:112`

**Issue:** * XXX FIXME no one should be using Handle's to work with atoms,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/**
* AtomSpace_getAtomByHandle Gets an atom back from the atomspace.
* XXX FIXME no one should be using Handle's to work with atoms,
* except for the database and communications back-ends.  The
* Handle's were never intended as a user interface to atoms, and,
```
</details>


**42.** `atomspace/opencog/haskell/PatternMatcher_CWrapper.h:8`

**Issue:** * XXX FIXME: atoms must never be accessed by UUID except by the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* An interface necessary for haskell bindings.
* (ghc supports FFI for c libraries)
* XXX FIXME: atoms must never be accessed by UUID except by the
* communication and database layers. The UUID is not meant to be
* a public interface.
```
</details>


**43.** `atomspace/opencog/guile/modules/TypeUtilsSCM.cc:58`

**Issue:** XXX HACK ALERT This needs to be static, in order for python to

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
```
</details>


**44.** `atomspace/opencog/guile/modules/ExecSCM.cc:70`

**Issue:** XXX HACK ALERT This needs to be static, in order for python to

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
```
</details>


**45.** `atomspace/opencog/guile/modules/ExecSCM.cc:73`

**Issue:** XXX FIXME: can we fix cython to not do this, already?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// XXX FIXME: can we fix cython to not do this, already?
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the process, anyway.
```
</details>


**46.** `atomspace/opencog/cython/opencog/load-file.cc:86`

**Issue:** XXX This is fairly tacky/broken, and needs a better fix.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Sometimes paths are given without the "opencog" part.
// Also check the build directory for autogen'ed files.
// XXX This is fairly tacky/broken, and needs a better fix.
for (auto p : default_paths) {
search_paths.push_back(p);
```
</details>


**47.** `atomspace/opencog/atoms/flow/FormulaPredicateLink.cc:41`

**Issue:** /XXX FIXME - in the future, some user is going to want to include

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// component of the final truth value...
///
/// XXX FIXME - in the future, some user is going to want to include
/// variable declarations, and/or an explicit Lambda in the body, for
/// some reason that I cannot imagine.  The code below will then fail.
```
</details>


**48.** `atomspace/opencog/atoms/flow/FormulaPredicateLink.cc:72`

**Issue:** XXX This is buggy. If the formula contains a VariableList,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// and a CountTruthVaue, if there are three.
//
// XXX This is buggy. If the formula contains a VariableList,
// and any of the two sub-parts of it use only some of the variables,
// but not all of them, then the reduction will go wrong. The solution
```
</details>


**49.** `atomspace/opencog/atoms/flow/StringOfLink.cc:118`

**Issue:** a stream or something dynamic ... XXX ???

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Recase to an explicit (concrete) StringValue,
// to handle the case where the from-value is
// a stream or something dynamic ... XXX ???
// Or maybe we want to cast *to* something dynamic?
// XXX FIXME this is unclear, under-specified and
```
</details>


**50.** `atomspace/opencog/atoms/flow/StringOfLink.cc:120`

**Issue:** XXX FIXME this is unclear, under-specified and

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// a stream or something dynamic ... XXX ???
// Or maybe we want to cast *to* something dynamic?
// XXX FIXME this is unclear, under-specified and
// under-used at the moment.
return createStringValue(
```
</details>


**51.** `atomspace/opencog/atoms/flow/FilterLink.cc:278`

**Issue:** /XXX Is executing the ground a good design choice? I dunno. It's the

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
///
/// Any executable terms in `ground` are executed prior to comparison.
/// XXX Is executing the ground a good design choice? I dunno. It's the
/// historical choice. So it goes.
///
```
</details>


**52.** `atomspace/opencog/atoms/flow/FilterLink.cc:615`

**Issue:** XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
vex = _outgoing[1]->execute(as, silent);

// XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,
// then we should construct another Queue as the return value,
// and perform filtering on-demand.
```
</details>


**53.** `atomspace/opencog/atoms/flow/TruthValueOfLink.cc:30`

**Issue:** XXX why isn't this centralized somewhere?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
using namespace opencog;

// XXX why isn't this centralized somewhere?
// Why am I writing this again, from scratch?
static TruthValuePtr get_the_tv(AtomSpace* as, const Handle& h, bool silent)
```
</details>


**54.** `atomspace/opencog/atoms/flow/ValueOfLink.cc:84`

**Issue:** XXX TODO FIXME ... if either of these are executable, then

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// trickle out properly in the end.
//
// XXX TODO FIXME ... if either of these are executable, then
// they need to be executed, first, right? Yes, they do! We
// can currently get away with not doing this for two reasons:
```
</details>


**55.** `atomspace/opencog/atoms/join/JoinLink.cc:550`

**Issue:** /i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
///
/// TODO: it might be faster to use hash tables instead of rb-trees
/// i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.
HandleSet JoinLink::supremum(AtomSpace* as, bool silent,
Traverse& trav) const
```
</details>


**56.** `atomspace/opencog/atoms/join/JoinLink.cc:721`

**Issue:** XXX FIXME this is really dumb, using a queue and then

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
HandleSet hs = container(as, jcb, silent);

// XXX FIXME this is really dumb, using a queue and then
// copying things into it. Whatever. Fix this.
QueueValuePtr qvp(createQueueValue());
```
</details>


**57.** `atomspace/opencog/atoms/parallel/PureExecLink.cc:64`

**Issue:** XXX Note that this leaks, if the execute throws.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// No AtomSpace provided. Use a temporary.
// XXX Note that this leaks, if the execute throws.
// The transient code will catch the leak, and complain.
// (There's no actual memleak; just a complaint about counting.)
```
</details>


**58.** `atomspace/opencog/atoms/pattern/PatternLink.cc:146`

**Issue:** XXX FIXME, more correct would be to loop over

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// The variables for that component are just the variables
// that can be found in that component.
// XXX FIXME, more correct would be to loop over
// _pat.clause_variables and add those. Probably makes
// no difference in most cases.
```
</details>


**59.** `atomspace/opencog/atoms/pattern/PatternLink.cc:165`

**Issue:** XXX FIXME, this handles `absents`, `always` and `grouping`

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Each component consists of the assorted parts.
// XXX FIXME, this handles `absents`, `always` and `grouping`
// incorrectly.
HandleSeq clseq;
```
</details>


**60.** `atomspace/opencog/atoms/pattern/PatternLink.cc:407`

**Issue:** /XXX No one, except unit tests, use these deprecated API's. These

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// since no variable type restricions are possible, and no optionals,
/// either.  This is used only for backwards-compatibility API's.
/// XXX No one, except unit tests, use these deprecated API's. These
/// old unit tests should be removed.
PatternLink::PatternLink(const HandleSet& vars,
```
</details>


**61.** `atomspace/opencog/atoms/pattern/PatternLink.cc:721`

**Issue:** BUG - XXX FIXME. This extracts PresentLink's from the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
OR_LINK, SEQUENTIAL_OR_LINK, NOT_LINK});

// BUG - XXX FIXME. This extracts PresentLink's from the
// Sequentials. This is not really correct, because the
// evaluation of the sequential might terminate *before*
```
</details>


**62.** `atomspace/opencog/atoms/pattern/PatternLink.cc:980`

**Issue:** XXX Shouldn't we be adding this to _fixed, too?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
_pat.pmandatory.push_back(ptm);

// XXX Shouldn't we be adding this to _fixed, too?
return true;
}
```
</details>


**63.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1003`

**Issue:** XXX Shouldn't we be adding this to _fixed, too?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
_pat.pmandatory.push_back(ptm);

// XXX Shouldn't we be adding this to _fixed, too?
return true;
}
```
</details>


**64.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1039`

**Issue:** /XXX FIXME: the code here assumes that the situation is indeed

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// as an ordinary clause, and searched for as if it was "present".
///
/// XXX FIXME: the code here assumes that the situation is indeed
/// simple: more complex cases are not handled correctly.  Doing this
/// correctly would require iterating again, and examining the
```
</details>


**65.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1044`

**Issue:** /XXX The situation here is also very dangerous: without any

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// contents of the left and right side of the IdenticalLink... ugh.
///
/// XXX The situation here is also very dangerous: without any
/// type constraints, we risk searching atoms created in the scratch
/// atomspace, resulting in infinite recursion and a blown stack.
```
</details>


**66.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1255`

**Issue:** XXX FIXME -- this is wrong. What we really want is to

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
ptm->addEvaluatable();

// XXX FIXME -- this is wrong. What we really want is to
// identify those clauses that bridge across multiple
// components... not everything here does so. The
```
</details>


**67.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1259`

**Issue:** identify the bridging clauses... XXX Wait, maybe this

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// components... not everything here does so. The
// get_bridged_components() should be modified to
// identify the bridging clauses... XXX Wait, maybe this
// does not need to be fixed, since the component splitter
// will not split these. So we're good, I think ...
```
</details>


**68.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1465`

**Issue:** XXX FIXME: debug_log() above is more readable than the below.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
DEFINE_LINK_FACTORY(PatternLink, PATTERN_LINK)

// XXX FIXME: debug_log() above is more readable than the below.
std::string PatternLink::to_long_string(const std::string& indent) const
{
```
</details>


**69.** `atomspace/opencog/atoms/pattern/BindLink.cc:79`

**Issue:** Shoot. XXX FIXME. Most of the unit tests require that the atom

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
// Shoot. XXX FIXME. Most of the unit tests require that the atom
// that we return is in the atomspace. But it would be nice if we
// could defer this indefinitely, until its really needed.
```
</details>


**70.** `atomspace/opencog/atoms/pattern/GetLink.cc:62`

**Issue:** Shoot. XXX FIXME. Most of the unit tests require that the atom

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
// Shoot. XXX FIXME. Most of the unit tests require that the atom
// that we return is in the atomspace. But it would be nice if we
// could defer this indefinitely, until its really needed.
```
</details>


**71.** `atomspace/opencog/atoms/truthvalue/CountTruthValue.cc:168`

**Issue:** XXX This is not the correct way to handle confidence ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
oc->get_mean() * oc->get_count()) / cnt;

// XXX This is not the correct way to handle confidence ...
// The confidence will typically hold the log probability,
// where the probability is the normalized count.  Thus
```
</details>


**72.** `atomspace/opencog/atoms/truthvalue/CountTruthValue.h:73`

**Issue:** XXX FIXME Are all of these really needed?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const MergeCtrl& mc=MergeCtrl()) const;

// XXX FIXME Are all of these really needed?
// Can we get rid of some of them?
static TruthValuePtr createTV(strength_t s, confidence_t f, count_t c)
```
</details>


**73.** `atomspace/opencog/atoms/execution/ExecutionOutputLink.cc:155`

**Issue:** XXX should be be unwrapping SetLinks here?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (not h->is_executable())
{
// XXX should be be unwrapping SetLinks here?
exargs.push_back(h);
continue;
```
</details>


**74.** `atomspace/opencog/atoms/execution/ExecutionOutputLink.cc:227`

**Issue:** pipeline. (XXX Is there a better way of doing this?)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// the case where GetLink returns a set of multiple results;
// we want to emulate that set passing through the processing
// pipeline. (XXX Is there a better way of doing this?)
// If there is more than one SetLink, then this won't work,
// and we need to make a Cartesian product of them, instead.
```
</details>


**75.** `atomspace/opencog/atoms/execution/Instantiator.cc:196`

**Issue:** /cleanly separated from each other. (XXX FIXME, these need to be

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// walk_tree() performs a kind-of eager-evaluation of function arguments.
/// The code in here is a mashup of several different ideas that are not
/// cleanly separated from each other. (XXX FIXME, these need to be
/// cleanly separated; its impeding overall clean design/implementation.)
/// Roughly, it goes like so:
```
</details>


**76.** `atomspace/opencog/atoms/execution/Instantiator.cc:488`

**Issue:** walk_tree cannot grok.  XXX This is all very kind-of hacky.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// to the instantiated tree). However, special-case the handling
// of expr being a FunctionLink - this can return a Value, which
// walk_tree cannot grok.  XXX This is all very kind-of hacky.
// A proper solution would convert walk_tree to return ValuePtr's
// instead of Handles. However, it seems this would require lots
```
</details>


**77.** `atomspace/opencog/atoms/execution/Instantiator.cc:542`

**Issue:** XXX Don't we need to plug in the vars, first!?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
{
// XXX Don't we need to plug in the vars, first!?
// Maybe this is just not tested?
Handle eolh = reduce_exout(expr, ist);
```
</details>


**78.** `atomspace/opencog/atoms/execution/Instantiator.cc:553`

**Issue:** XXX Don't we need to plug in the vars, first!?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (nameserver().isA(t, PARALLEL_LINK))
{
// XXX Don't we need to plug in the vars, first!?
// Yes, we do, but this is just not tested, right now.
return ValueCast(EvaluationLink::do_evaluate(_as, expr, silent));
```
</details>


**79.** `atomspace/opencog/atoms/execution/Instantiator.cc:561`

**Issue:** XXX Don't we need to plug in the vars, first!?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (nameserver().isA(t, DEFINED_PREDICATE_NODE))
{
// XXX Don't we need to plug in the vars, first!?
// Maybe this is just not tested?
return ValueCast(EvaluationLink::do_evaluate(_as, expr, silent));
```
</details>


**80.** `atomspace/opencog/atoms/execution/Instantiator.cc:598`

**Issue:** XXX FIXME Can we defer the addition to the atomspace to an even

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// atoms to the atomspace is an expensive process.  We can save
// some time by doing it just once, right here, in one big batch.
// XXX FIXME Can we defer the addition to the atomspace to an even
// later time??
if (_as) return _as->add_atom(grounded);
```
</details>


**81.** `atomspace/opencog/atoms/execution/Instantiator.cc:642`

**Issue:** XXX FIXME, we need to get rid of this call entirely, and just

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return expr->execute(_as, silent);

// XXX FIXME, we need to get rid of this call entirely, and just
// return expr->execute(_as, silent) instead, like above.
// However, assorted parts are still broken and don't work.
```
</details>


**82.** `atomspace/opencog/atoms/core/Variables.cc:109`

**Issue:** XXX FIXME URE calls us with broken handle!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void Variables::validate_vardecl(const Handle& hdecls)
{
// XXX FIXME URE calls us with broken handle!!
if (nullptr == hdecls) return;

```
</details>


**83.** `atomspace/opencog/atoms/core/Variables.cc:284`

**Issue:** * XXX TODO this does not currently handle type equations, as outlined

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* we have memoized.
*
* XXX TODO this does not currently handle type equations, as outlined
* on the wiki; We would need the general pattern matcher to do type
* checking, in that situation.
```
</details>


**84.** `atomspace/opencog/atoms/core/Variables.cc:437`

**Issue:** XXX TODO type-checking could be lazyif the function is not

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
varseq.size(), args.size());

// XXX TODO type-checking could be lazy; if the function is not
// actually using one of the args, it's type should not be checked.
// Viz., one of the arguments might be undefined, and that's OK,
```
</details>


**85.** `atomspace/opencog/atoms/core/RandomChoice.cc:110`

**Issue:** XXX FIXME - fix this so it can also choose a single value

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
///

// XXX FIXME - fix this so it can also choose a single value
// out of a vector of values.
ValuePtr RandomChoiceLink::execute(AtomSpace* as, bool silent)
```
</details>


**86.** `atomspace/opencog/atoms/core/RandomChoice.cc:179`

**Issue:** XXX FIXME, also allow a FloatValue!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
for (Handle h : ofirst->getOutgoingSet())
{
// XXX FIXME, also allow a FloatValue!!
if (h->is_executable())
h = HandleCast(h->execute(as, silent));
```
</details>


**87.** `atomspace/opencog/atoms/core/TypeNode.h:90`

**Issue:** XXX TODO ... Some types are defined. In this case,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
{
Type t = nameserver().getType(str);
// XXX TODO ... Some types are defined. In this case,
// verify that the string occurs as a name inside
// some DefineLink... if it does, then it's valid.
```
</details>


**88.** `atomspace/opencog/atoms/core/TypeUtils.cc:421`

**Issue:** returned. XXX FIXME -- surely this should be a throw, instead!!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// If we're here we have failed to recognize vardecl as a useful
// and well-formed variable declaration, so Handle::UNDEFINED is
// returned. XXX FIXME -- surely this should be a throw, instead!!!
return Handle::UNDEFINED;
}
```
</details>


**89.** `atomspace/opencog/atoms/core/TypeChoice.cc:254`

**Issue:** For now, just avoid throwing an exception. XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// using a SignatureLink, but its not. As a result, it
// gets undefined behavior and incorrect results. Too bad.
// For now, just avoid throwing an exception. XXX FIXME.
return;
}
```
</details>


**90.** `atomspace/opencog/atoms/core/PrenexLink.cc:325`

**Issue:** Last one wins.  XXX This is actually ambiguous, if there

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
vm[pare->first] = body;

// Last one wins.  XXX This is actually ambiguous, if there
// were multiple variables, and they weren's all LambdaLinks,
// for example. In that case, things are borked, and there's
```
</details>


**91.** `atomspace/opencog/atoms/core/FindUtils.h:146`

**Issue:** * XXX FIXME: what if it appears quoted in one place, and unquoted

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* QuoteLink.
*
* XXX FIXME: what if it appears quoted in one place, and unquoted
* in another? then what?
*/
```
</details>


**92.** `atomspace/opencog/atoms/core/Checkers.cc:42`

**Issue:** XXX FIXME Much of the onfusion below is due to a bug: if the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// it does not check deep types, nor does it check arity.

// XXX FIXME Much of the onfusion below is due to a bug: if the
// types script says something like
// FOOBAR <- FUNCTION_LINK,BOOL_INPUT_LINK,NUMBER_INPUT_LINK
```
</details>


**93.** `atomspace/opencog/atoms/core/Checkers.cc:93`

**Issue:** XXX FIXME ... Perhaps IntersectionLink, UnionLink will

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// specialized operator to explicitly map the higher order into
// the lower order but as of today it is required.
// XXX FIXME ... Perhaps IntersectionLink, UnionLink will
// resolve this?
if (h->is_type(SIMILARITY_LINK) or
```
</details>


**94.** `atomspace/opencog/atoms/reduct/AccumulateLink.cc:71`

**Issue:** XXX TODO -- we could also handle vectors of strings, by

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO -- we could also handle vectors of strings, by
// concatenating them into one long string.  However, for this
// to be generally useful, we'd want to insert whitespace in
```
</details>


**95.** `atomspace/tests/query/seq-absence.scm:70`

**Issue:** ;and right now, I'm not gonna fix it... XXX FIXME.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; Pattern matcher treats present and Absent links in a very
;; different way. This one actually does not work correctly,
;; and right now, I'm not gonna fix it... XXX FIXME.
(define and-not-present
(SatisfactionLink
```
</details>


**96.** `atomspace/tests/query/seq-absence.scm:86`

**Issue:** ;XXX FIXME ... this and the above need to get done right.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; anyway, for now.  Like the above, its currently broken. Its too
;; weird right now for me to want to fix it, so I am punting on this.
;; XXX FIXME ... this and the above need to get done right.

(define or-not-absent
```
</details>


**97.** `atomspace/tests/atoms/execution/defined-schema.scm:239`

**Issue:** XXX FIXME, this does not quite work as one might naively expect,

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; Define a recursive tree-walker. Unlike the above, this does
; not reverse the order of the edges.
; XXX FIXME, this does not quite work as one might naively expect,
; because the search results are expanded combinatorially, instead
; of being kept in branching-tree form.
```
</details>


### Atomspace-Restful Component (4 items)


**1.** `atomspace-restful/opencog/python/web/api/apiatomcollection.py:10`

**Issue:** I can't find swagger on ubuntu .. wtf!? FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
from flask_restful.utils import cors

# I can't find swagger on ubuntu .. wtf!? FIXME
from flask_restful_swagger import swagger
from opencog.bank import AttentionBank
```
</details>


**2.** `atomspace-restful/opencog/python/web/api/apiatomcollection.py:377`

**Issue:** xxxxxxxxxxxx here add atoms

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if dot_format not in ['True', 'true', '1']:
atom_list = AtomListResponse(atoms)
# xxxxxxxxxxxx here add atoms
json_data = {'result': atom_list.format()}

```
</details>


**3.** `atomspace-restful/opencog/python/web/api/utilities.py:17`

**Issue:** FIXME: Should this moved to the atomspace repo and be part

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# https://github.com/opencog/atomspace/pull/611
# NOTE: This is similar to scheme `cog-node`.
# FIXME: Should this moved to the atomspace repo and be part
# of opencog.atomspace module?
def get_atoms_by_name(z_type, name, atomspace):
```
</details>


**4.** `atomspace-restful/tests/python/restapi/test_restapi.py:449`

**Issue:** XXX Emptied because the scheme command (i.e cog-af-boundary) has been removed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# Test an arbitrary Scheme command to ensure the binding is working
# properly
# XXX Emptied because the scheme command (i.e cog-af-boundary) has been removed.
pass
def test_n_dot_export(self):
```
</details>


### Atomspace-Rocks Component (2 items)


**1.** `atomspace-rocks/opencog/persist/monospace/MonoIO.cc:1004`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void MonoStorage::storeAtomSpace(const AtomSpace* table)
{
// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


**2.** `atomspace-rocks/opencog/persist/rocks/RocksIO.cc:1371`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
convertForFrames(HandleCast(getAtomSpace()));

// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


### Atomspace-Storage Component (14 items)


**1.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:165`

**Issue:** FIXME read above comment.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// This is complicated, and subject to change...
// XXX this should be nuked, and replaced by appropriate kind of proxy.
// FIXME read above comment.
std::string Commands::cog_execute_cache(const std::string& cmd)
{
```
</details>


**2.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:181`

**Issue:** XXX Hacky .. store time in float value...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
meta = _base_space->add_atom(meta);

// XXX Hacky .. store time in float value...
_base_space->set_value(query, meta, createFloatValue((double)time(0)));
if (std::string::npos != cmd.find("#t", pos))
```
</details>


**3.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:200`

**Issue:** XXX is this correct???

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
_base_space->set_value(query, key, rslt);

// XXX is this correct???
// _proxy->store_value(query, key);

```
</details>


**4.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:334`

**Issue:** ?????? XXX Is this right? Needs review

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
Handle h = createNode(t, std::move(nam));

// ?????? XXX Is this right? Needs review
if (_proxy and _proxy->have_getAtom)
{
```
</details>


**5.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:372`

**Issue:** ?????? XXX Is this right? Needs review

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
Handle h = createLink(std::move(oset), t);

// ?????? XXX Is this right? Needs review
if (_proxy and _proxy->have_getAtom)
{
```
</details>


**6.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:545`

**Issue:** Return the new value. XXX Why? This just wastes CPU?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
_proxy->update_value(atom, key, vp);

// Return the new value. XXX Why? This just wastes CPU?
// ValuePtr vp = atom->getValue(key);
// return Sexpr::encode_value(vp);
```
</details>


**7.** `atomspace-storage/opencog/persist/proxy/DynamicDataProxy.cc:77`

**Issue:** XXX TODO ... create this in some temp atomspace...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Ah! Its a procedure! Make it executable!
// XXX TODO ... create this in some temp atomspace...
Handle exo = _atom_space->add_link(EXECUTION_OUTPUT_LINK,
HandleCast(rawvp),
```
</details>


**8.** `atomspace-storage/opencog/persist/proxy/CachingProxy.cc:48`

**Issue:** XXX TODO Add support for expiration times, limited AtomSpace

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Get our configuration from the ProxyParameterLink we live in.
// XXX TODO Add support for expiration times, limited AtomSpace
// size and whatever other whizzy caching ideas we might want.
void CachingProxy::open(void)
```
</details>


**9.** `atomspace-storage/opencog/persist/proxy/ProxyNode.cc:141`

**Issue:** XXX FIXME. Using this ProxyParametersLink thing is a kind of

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Hmm, perhaps this should be a StateLink?
//
// XXX FIXME. Using this ProxyParametersLink thing is a kind of
// cheesy hack, to pass parameters to the ProxyNode. It vaguely
// resembles the structure of an ExecutionLink, but instead of
```
</details>


**10.** `atomspace-storage/opencog/persist/proxy/WriteBufferProxy.cc:195`

**Issue:** XXX FIXME. Buffering these naively, like this, voilates the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const ValuePtr& delta)
{
// XXX FIXME. Buffering these naively, like this, voilates the
// intent of how this method should work. However, for the
// RocksStorageNode, doing this is harmless. And the
```
</details>


**11.** `atomspace-storage/opencog/persist/proxy/ProxyNode.h:62`

**Issue:** updated at some later date. XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// Virtual methods from the BackingStore API. These provide some default
// implementations. Some of these are stop-gap, and will need to be
// updated at some later date. XXX FIXME.

virtual void create(void) {} // stop-gap. FIXME
```
</details>


**12.** `atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc:82`

**Issue:** * XXX FIXME This needs to be fuzzedit is very likely to crash

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* open-paren, and the string encoding the value.
*
* XXX FIXME This needs to be fuzzed; it is very likely to crash
* and/or contain bugs if it is given strings of unexpected formats.
*/
```
</details>


**13.** `atomspace-storage/opencog/persist/tlb/UuidSCM.cc:83`

**Issue:** what the heck. I'm gonna punt. XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// atomspaces, and really, we should have nested hierarchical TLB
// resolvers for nested atomspaces. But for now, no one uses this so
// what the heck. I'm gonna punt. XXX FIXME.
const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("uuid");
_tlb.set_resolver(asp.get());
```
</details>


**14.** `atomspace-storage/opencog/persist/api/cython/PersistCython.cc:31`

**Issue:** XXX FIXME: except for the error messages, most of this code is

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
namespace opencog {

// XXX FIXME: except for the error messages, most of this code is
// mostly a cut-n-pate of what's in PersistSCM.cc

```
</details>


### Cogserver Component (3 items)


**1.** `cogserver/opencog/cogserver/server/ServerConsole.cc:261`

**Issue:** /XXX escaped quotes are not handled correctly. FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

/// Parse command line. Quotes are stripped.
/// XXX escaped quotes are not handled correctly. FIXME.
/// This passes over quotes embeded in the middle strings.
/// And that OK, because what the heck did you want to happen?
```
</details>


**2.** `cogserver/opencog/cogserver/server/CogServer.cc:125`

**Issue:** XXX FIXME. terrible terrible hack. What we should be

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
runLoopStep();

// XXX FIXME. terrible terrible hack. What we should be
// doing is running in our own thread, waiting on a semaphore,
// until some request is queued. Spinning is .. just wrong.
```
</details>


**3.** `cogserver/opencog/cogserver/server/CogServer.cc:216`

**Issue:** assumes a singleton instance, so we leave this for now. XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// interesting use cases where one might want to run multiple
// cogservers. However, at this time, too much of the code (???)
// assumes a singleton instance, so we leave this for now. XXX FIXME.

// The guile module needs to be able to delete this singleton.
```
</details>


### Cogutil Component (3 items)


**1.** `cogutil/opencog/util/tree.cc:101`

**Issue:** a parenthesis. XXX If you know how to fix the parser, please do.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// I can't figure out how to fix the parser, so I hack around it
// here: we must ignore whitespace after a function name, but before
// a parenthesis. XXX If you know how to fix the parser, please do.
// Example: "and  ($1 $2)" should parse as "and($1 $2)", but the
// former fails to parse correctly for some reason unclear to me.
```
</details>


**2.** `cogutil/opencog/util/tree.h:2846`

**Issue:** FIXME: add the other members of fixed_depth_iterator.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// FIXME: add the other members of fixed_depth_iterator.


```
</details>


**3.** `cogutil/opencog/util/sigslot.h:74`

**Issue:** XXX Something like this should work, but I can't get it to go.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#if BORKEN_FOR_SOME_REASON
// Connect member of a given object.
// XXX Something like this should work, but I can't get it to go.
//
// class Bar { public:
```
</details>


### Components Component (233 items)


**1.** `components/integration/opencog/opencog/openpsi/rule.scm:294`

**Issue:** XXX FIXME How about actually using a SequentialAndLink?

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; psi-get-context maintains, which is unlikely. What other options
; are there?
; XXX FIXME How about actually using a SequentialAndLink?
; then the code will be faster, and there won't be this problem.
; TODO: This calculation can be done in OpenPsiImplicator::grounding or
```
</details>


**2.** `components/integration/opencog/opencog/openpsi/control.scm:89`

**Issue:** FIXME -- can we have a shorter/better name for this method?

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; --------------------------------------------------------------
; FIXME -- can we have a shorter/better name for this method?
;
(define (psi-rule-set-atomese-weight psi-rule weight)
```
</details>


**3.** `components/integration/opencog/opencog/openpsi/control.scm:104`

**Issue:** FIXME - the long-term design calls for the use of an AtTimeLink

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
also have the same weight.
"
; FIXME - the long-term design calls for the use of an AtTimeLink
; instead of a StateLink, here.
;
```
</details>


**4.** `components/integration/opencog/opencog/openpsi/control.scm:107`

**Issue:** FIXME -- if there are multiple names for a rule, this will

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; instead of a StateLink, here.
;
; FIXME -- if there are multiple names for a rule, this will
; use all of those names in the StateLink.  If a rule is given
; a second or third name later on, this will cause dead StateLinks
```
</details>


**5.** `components/integration/opencog/opencog/openpsi/control.scm:110`

**Issue:** to linger in the atomspace. FIXME -- make sure that a rule can

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; use all of those names in the StateLink.  If a rule is given
; a second or third name later on, this will cause dead StateLinks
; to linger in the atomspace. FIXME -- make sure that a rule can
; have only one name.
;
```
</details>


**6.** `components/integration/opencog/opencog/openpsi/utilities.scm:14`

**Issue:** XXX TODO: does this really need to be public? change into atom.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; --------------------------------------------------------------
; XXX TODO: does this really need to be public? change into atom.
(define psi-prefix-str "OpenPsi: ")

```
</details>


**7.** `components/integration/opencog/opencog/openpsi/utilities.scm:18`

**Issue:** XXX TODO: does this really need to be public?

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; --------------------------------------------------------------
; XXX TODO: does this really need to be public?
(define (psi-suffix-str a-string)
"
```
</details>


**8.** `components/integration/opencog/opencog/openpsi/main.scm:94`

**Issue:** XXX FIXME -- right now, this assumes that a single thread, running

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; --------------------------------------------------------------
;
; XXX FIXME -- right now, this assumes that a single thread, running
; at no more than 100 steps per second, is sufficient to run all of the
; psi rules.  For now, this is OK, but at some point, this will become
```
</details>


**9.** `components/integration/opencog/opencog/openpsi/main.scm:137`

**Issue:** all CPU. FIXME -- this is obviously a hack, awaiting some sort

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
"
; Pause for 10 millisecs, so that the psi engine doesn't hog
; all CPU. FIXME -- this is obviously a hack, awaiting some sort
; of better way of scehduling psi rules.
(usleep 10000)
```
</details>


**10.** `components/integration/opencog/opencog/ghost/cs-parse.scm:75`

**Issue:** FIXME: This is not really newline.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; rules.
((has-match? "\r" str) (result:suffix 'CR location ""))
; FIXME: This is not really newline.
((string=? "" str) (cons (make-lexical-token 'NEWLINE location #f) ""))
((has-match? "urge:" str) (result:suffix 'URGE location #f))
```
</details>


**11.** `components/integration/opencog/opencog/neighbors/Neighbors.h:58`

**Issue:** * XXX FIXME -- this function is curently not used anywhere. Perhaps

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* @return      an UnorderedHandleSet of neighbors
*
* XXX FIXME -- this function is curently not used anywhere. Perhaps
* it should be deleted?
*/
```
</details>


**12.** `components/integration/opencog/opencog/eva/model/faces.scm:6`

**Issue:** Perhaps this file is not needed any more? XXX FIXME

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Assorted utilities for supporting face tracking
; XXX most of face-tracking is now in self-model.scm
; Perhaps this file is not needed any more? XXX FIXME
;
(use-modules (opencog))
```
</details>


**13.** `components/integration/opencog/opencog/eva/model/faces.scm:11`

**Issue:** ;XXX FIXME: This file defines a "Room State", which currently can

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(use-modules (opencog exec))

;; XXX FIXME: This file defines a "Room State", which currently can
;; be "empty" or "non-empty", depending on whether faces are visible
;; or not.  But this is kind-of pointless: its probably easier to just
```
</details>


**14.** `components/integration/opencog/opencog/eva/model/self-model.scm:43`

**Issue:** XXX FIXME There are a bunch of define-publics in here, they probably

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ------------------------------------------------------
; State variables
; XXX FIXME There are a bunch of define-publics in here, they probably
; should not be; they're needed only by the behavior module.

```
</details>


**15.** `components/integration/opencog/opencog/eva/model/self-model.scm:95`

**Issue:** XXX FIXME -- this should return neutral, if the timestamp is more

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Get the current facial expression.
; XXX FIXME -- this should return neutral, if the timestamp is more
; than 8 seconds in the past. Well, actually, it should probably
; reset the face expression state after 8 seconds or so --
```
</details>


**16.** `components/integration/opencog/opencog/eva/model/self-model.scm:254`

**Issue:** XXX FIXME: Note also: we currently fail to distinguish the affect

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Right now, there are only two affects: happy and not happy.
; NB the python ROS sensor code uses these defines!
; XXX FIXME: Note also: we currently fail to distinguish the affect
; that was perceived, from our own state. There is a ROS message that
; informs us about what the perceived affect was: it sets this state.
```
</details>


**17.** `components/integration/opencog/opencog/eva/model/self-model.scm:374`

**Issue:** ;XXX FIXME -- the psi subsystem should be performing this action,

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
;; a degree greater than 13, then it's considered as salient. The robot
;; should then look at the salient position and show curious expression.
;; XXX FIXME -- the psi subsystem should be performing this action,
;; instead of hard-coding it here.

```
</details>


**18.** `components/integration/opencog/opencog/eva/model/time-map.scm:9`

**Issue:** XXX FIXME -- some of the below should be handled as psi-rules,

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; of the face.
;
; XXX FIXME -- some of the below should be handled as psi-rules,
; instead of being hard-coded, here.  That is, we are interested
; in the locations of sound events in general, not just for determing
```
</details>


**19.** `components/integration/opencog/opencog/eva/model/time-map.scm:38`

**Issue:** XXX FIXME Is it wise to start this, just because the guile module got

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Run the map in a new thread. This will automatically create a new
; time-slice every 66 milliseconds.
; XXX FIXME Is it wise to start this, just because the guile module got
; loaded? or should we have a distinct "start running it now" function?
(cog-pointmem-auto-step-time-on facemap)
```
</details>


**20.** `components/integration/opencog/opencog/eva/model/time-map.scm:108`

**Issue:** ;XXX FIXME huh? this needs documentation.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; ---------------------------------------------------------------------
;; Below creates say atom for face if sound came from it
;; XXX FIXME huh? this needs documentation.
;; XXX FIXME elminiate the use of cog-execute! -- that is not how
;; this should be designed -- these need to be learnable; and
```
</details>


**21.** `components/integration/opencog/opencog/eva/model/time-map.scm:109`

**Issue:** ;XXX FIXME elminiate the use of cog-execute! -- that is not how

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
;; Below creates say atom for face if sound came from it
;; XXX FIXME huh? this needs documentation.
;; XXX FIXME elminiate the use of cog-execute! -- that is not how
;; this should be designed -- these need to be learnable; and
;; cog-execute prevents learning.
```
</details>


**22.** `components/integration/opencog/opencog/eva/model/time-map.scm:162`

**Issue:** ;XXX FIXME -- this kind of crazy angle computation should be

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;angle in radians
;
;; XXX FIXME -- this kind of crazy angle computation should be
;; happenening in the space-time server, and not here.
;;
```
</details>


**23.** `components/integration/opencog/opencog/eva/model/time-map.scm:197`

**Issue:** ;XXX FIXME -- this kind of tulity needs to be in the space-time

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; the face.  Returns the atom for the face, or the emtpy list.
;;
;; XXX FIXME -- this kind of tulity needs to be in the space-time
;; server, and not here.
(define (face-nearest-sound xx yy zz)
```
</details>


**24.** `components/integration/opencog/opencog/eva/model/time-map.scm:238`

**Issue:** ;XXX TODO -- this should eventually be a psi-rule, so that we can

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; This needs to be define-public, so that ros-bridge can send this
;; to the cogserver.
;; XXX TODO -- this should eventually be a psi-rule, so that we can
;; associate spoken sounds with speakers, but also know the locations
;; of loud sounds.  That is, the time-server needs to get sound
```
</details>


**25.** `components/integration/opencog/opencog/eva/behavior/orchestrate.scm:48`

**Issue:** XXX FIXME: this records the animation that was chosen, and a

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; and edit it to provide a minimum elapsed time predicate.
;
; XXX FIXME: this records the animation that was chosen, and a
; timestamp in some StateLinks. These need to be replaced by the
; TimeServer, instead.
```
</details>


**26.** `components/integration/opencog/opencog/eva/behavior/orchestrate.scm:137`

**Issue:** XXX FIXME, this is still broken during search for attention.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Otherwise, the behavior tree forces eye contact to be continually
; running, and the turn-look command is promptly over-ridden.
; XXX FIXME, this is still broken during search for attention.

(DefineLink
```
</details>


**27.** `components/integration/opencog/opencog/eva/behavior/primitives.scm:168`

**Issue:** automated. XXX TODO. (Can we get ECAN to do this for us?)

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ----------------------------------------------------------------------
; Sigh. Perform manual garbage collection. This really should be
; automated. XXX TODO. (Can we get ECAN to do this for us?)
(define-public (run-behavior-tree-gc)
(define (free-stuff)
```
</details>


**28.** `components/integration/opencog/opencog/eva/behavior/face-priority.scm:74`

**Issue:** FIXME: There should never be an empty set. The value should be set

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(Number face-id))))
(if (equal? (Set) result)
; FIXME: There should never be an empty set. The value should be set
; during acknowledgment.
(begin
```
</details>


**29.** `components/integration/opencog/opencog/eva/behavior/face-priority.scm:131`

**Issue:** FIXME But why have this plane instead of calculating the distance between

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; `distance` is distance from the camera. The assumption is that at the given
; distance from the camera is the plane where all the faces are at.
; FIXME But why have this plane instead of calculating the distance between
; faces, will it affect the priority?
(define distance 1.0)
```
</details>


**30.** `components/integration/opencog/opencog/eva/behavior/face-priority.scm:224`

**Issue:** FIXME: Sometimes d is larger than the width-of-yz-plane

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; the width-of-yz-plane, then something fishy is going on.
(if (> d  width-of-yz-plane)
; FIXME: Sometimes d is larger than the width-of-yz-plane
; and returning 0 is causing a problem in 'choose-next-face'
; Recalculate the width-of-yz-plane?
```
</details>


**31.** `components/integration/opencog/opencog/eva/behavior/movement-api.scm:27`

**Issue:** XXX FIXME -- someday, should probably create a distinct API for

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Must print to stdout, so that IRC chatbots can see what happened.
; XXX FIXME -- someday, should probably create a distinct API for
; the IRC text strings.
(define-public (prt-face-expr PRED NAME TIME TENS)
```
</details>


**32.** `components/integration/opencog/opencog/eva/behavior/movement-api.scm:73`

**Issue:** XXX FIXME: these record the animation that was chosen, and a

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

;
; XXX FIXME: these record the animation that was chosen, and a
; timestamp in some StateLinks. These need to be replaced by the
; TimeServer, instead.
```
</details>


**33.** `components/integration/opencog/opencog/eva/behavior/behavior.scm:179`

**Issue:** XXX FIXME this should be a part of "Show random expression"

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(DefinedPredicate "was room empty?")
; Record a new emotional state (for self-awareness)
; XXX FIXME this should be a part of "Show random expression"
; below ...
(Put (DefinedPredicate "Request Set Face Expression")
```
</details>


**34.** `components/integration/opencog/opencog/eva/behavior/behavior.scm:242`

**Issue:** ;XXX TODO -- if interacting for a while

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; Respond to a new face becoming visible.
;
;; XXX TODO -- if interacting for a while
;; this alters probability of glance...
(DefineLink
```
</details>


**35.** `components/integration/opencog/opencog/eva/behavior/behavior.scm:719`

**Issue:** ;XXX FIXME -- this should not be hard-coded here!

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

;; Actions for loud sound
;; XXX FIXME -- this should not be hard-coded here!
(DefineLink
(DefinedPredicate "Say whoa!")
```
</details>


**36.** `components/integration/opencog/opencog/eva/behavior/behavior.scm:763`

**Issue:** XXX FIXME ... this means that this rule is c alled too often!

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(SequentialAnd
; Disable printing, because it prints at 100x/second.
; XXX FIXME ... this means that this rule is c alled too often!
; Tracking should be autonomous, not piped through the behaviors.
; (True (Evaluation (GroundedPredicate "scm: print-msg")
```
</details>


**37.** `components/integration/opencog/opencog/eva/src/btree.scm:4`

**Issue:** XXX FIXME ... I think this blob of code is obsolete ... I think

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; btree.scm
;
; XXX FIXME ... I think this blob of code is obsolete ... I think
; that the current interfaces are in `btree-psi.scm`.  So remove this
; file once we are clear on this.
```
</details>


**38.** `components/integration/opencog/opencog/eva/src/old-tree.scm:50`

**Issue:** ;XXX FIXME chatbot is disengaged from everything else.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(True))

;; XXX FIXME chatbot is disengaged from everything else.
;; The room can be empty, the head is bored or even asleep,
;; but the chatbot is still smiling and yabbering.
```
</details>


**39.** `components/integration/opencog/opencog/eva/src/psi-dynamics.scm:68`

**Issue:** XXX FIXME -- this is hacky -- and has multiple design flaws.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; Functions to initiate random positive and negative expressions

; XXX FIXME -- this is hacky -- and has multiple design flaws.
; These are:
;
```
</details>


**40.** `components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:144`

**Issue:** XXX this should be moved to cog-utils. Also needs to be fixed

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;--------------------------------------------------------------------
;
; XXX this should be moved to cog-utils. Also needs to be fixed
; to not detect bound variables. We already have C++ code that
; does  the right thing, here, so we should use that.
```
</details>


**41.** `components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:208`

**Issue:** openpsi to pick one. XXX FIXME -- do this.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; There may be more than one plausible reply. We should use
; openpsi to pick one. XXX FIXME -- do this.
; Alternately, this shoud probably be done with chatscript.
(let ((reply-words (filter verbalize-reply (get-grounded-replies))))
```
</details>


**42.** `components/integration/opencog/opencog/eva/chatbot-eva/run-chatbot.scm:22`

**Issue:** XXX fixme -- we should not need to load either relex2logic or

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Must load the rulebase before running eva; see bug
; https://github.com/opencog/opencog/issues/2021 for details
; XXX fixme -- we should not need to load either relex2logic or
; the rules right here, since the code in this module does not depend
; directly on these.
```
</details>


**43.** `components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm:142`

**Issue:** XXX FIXME we need a better way of marking actions as having

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; actions to the current-action anchor. Wipe these out.
; (because we have already performed the actions).
; XXX FIXME we need a better way of marking actions as having
; been performed, already.
(for-each (lambda (x)
```
</details>


**44.** `components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm:12`

**Issue:** ;XXX FIXME this is a cut-n-paste job from process-query

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;--------------------------------------------------------------------

;; XXX FIXME this is a cut-n-paste job from process-query

(define-public (grounded-talk USER QUERY)
```
</details>


**45.** `components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm:28`

**Issue:** ;XXX FIXME -- remove the IRC debug response below.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define sent-node (car (nlp-parse QUERY)))

;; XXX FIXME -- remove the IRC debug response below.
(display "Hello ")
(display USER)
```
</details>


**46.** `components/integration/opencog/opencog/eva/chatbot-eva/imperative-rules.scm:224`

**Issue:** XXX TODO Design notes:

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
))

; XXX TODO Design notes:
; Rather than hand-crafting a bunch of rules like the above, we should
; do three things:
```
</details>


**47.** `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:79`

**Issue:** to hook this up.  XXX FIXME.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; extent to the current gaze direction.  The gaze direction can be
; gotten from the space-time server, (octree server) -- someone needs
; to hook this up.  XXX FIXME.
;
(DefineLink
```
</details>


**48.** `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:399`

**Issue:** XXX FIXME ... the list below is duplicated twice, once as adjectives

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(ReferenceLink (WordNode "worry")  (DefinedSchema "worry"))

; XXX FIXME ... the list below is duplicated twice, once as adjectives
; and once as nouns.  This is partly because relex normalization is
; not being correctly used, and/or R2L in its current form is not
```
</details>


**49.** `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:404`

**Issue:** XXX FIXME -- this list contains lots of synonymsneeds to be replaced

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; quite usable for this (it's too fragile, among other things).
;
; XXX FIXME -- this list contains lots of synonyms; needs to be replaced
; by proper synonym support.
;
```
</details>


**50.** `components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm:64`

**Issue:** XXX fixme -- we should not need to load either relex2logic or

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Must load the rulebase before running eva; see bug
; https://github.com/opencog/opencog/issues/2021 for details
; XXX fixme -- we should not need to load either relex2logic or
; the rules right here, since the code in this module does not depend
; directly on thes.
```
</details>


**51.** `components/integration/opencog/opencog/nlp/scm/oc.scm:20`

**Issue:** XXX What? nothing else anywhere needs this! FIXME, somethings broke.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Weird ... MUST say `(export)` or no define-publics are visible!
; XXX What? nothing else anywhere needs this! FIXME, somethings broke.
(export)
```
</details>


**52.** `components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:54`

**Issue:** XXX FIXME the Microplanner should use the same speech-act types as

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Find the speech act from the SetLink and use it for Microplanning
; XXX FIXME the Microplanner should use the same speech-act types as
; everyone else, so that we don't have to do this horrific string
; manginling.
```
</details>


**53.** `components/integration/opencog/opencog/nlp/aiml/aiml.scm:180`

**Issue:** XXX FIXME. This is yucky, something prettier is needed.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Either gaar or gadr is the context; the other is the action.
; We identify the action by using the `psi-action?` utility.
; XXX FIXME. This is yucky, something prettier is needed.
(define (get-ctxt-act r)
(define andy (gar r))
```
</details>


**54.** `components/integration/opencog/opencog/nlp/aiml/aiml.scm:214`

**Issue:** ;XXX TODO -- filter out the exact rules that have non-trivial

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(equal? (gdr pred) SENT)))))

;; XXX TODO -- filter out the exact rules that have non-trivial
;; THAT and TOPIC contexts.

```
</details>


**55.** `components/integration/opencog/opencog/nlp/aiml/aiml.scm:284`

**Issue:** XXX FIXME -- handle topic stars also ....

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Return #t if the topic in the RULE context is actually equal
; to the current AIML topic state.
; XXX FIXME -- handle topic stars also ....
(define (is-topical-rule? RULE)
(define pred (get-pred RULE "*-AIML-topic-*"))
```
</details>


**56.** `components/integration/opencog/opencog/nlp/aiml/aiml.scm:361`

**Issue:** ;XXX FIXME crazy hacky weight-adjusting formula. This makes

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
select one to run.
"
;; XXX FIXME crazy hacky weight-adjusting formula. This makes
;; no sense at all, but is a hacky hack designed to pick more
;; desirable rules more often.  Someone should figure out
```
</details>


**57.** `components/integration/opencog/opencog/nlp/aiml/aiml.scm:484`

**Issue:** XXX FIXME .. Maybe check a much longer list??

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; checking to see if the suggested response is the same as the
; previous response. Right now, we just check one level deep.
; XXX FIXME .. Maybe check a much longer list??
(define (same-as-before? SENT)
(define that (do-aiml-get (Concept "that")))
```
</details>


**58.** `components/integration/opencog/opencog/nlp/aiml/aiml.scm:528`

**Issue:** XXX TODO: Would be better to log and retrieve the chat

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; The robots response is the current "that".
; Store up to two previous inputs and outputs
; XXX TODO: Would be better to log and retrieve the chat
;           history using AtTimeLink and the time server
(if (valid-response? response)
```
</details>


**59.** `components/integration/opencog/opencog/nlp/wsd/WordSenseProcessor.cc:120`

**Issue:** XXX we are being called too often. this needs to be fixed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
run_no_delay();

// XXX we are being called too often. this needs to be fixed.
// in truth, should only poll on new input.
usleep(50*1000);
```
</details>


**60.** `components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.cc:151`

**Issue:** XXX however, what we should really do is to not that we have no

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// If no data, return similarity of zero!
// XXX however, what we should really do is to not that we have no
// data, and maybe try to gather some.
if (!rp.have_data)
```
</details>


**61.** `components/integration/opencog/opencog/nlp/microplanning/main.scm:83`

**Issue:** XXX FIXME utterance-type should be an atom, not a string!

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
ANAPHORA can be #t or #f.
"
; XXX FIXME utterance-type should be an atom, not a string!
; viz (DefinedLinguisticConceptNode "DeclarativeSpeechAct") etc.
; this would avoid a lot of string-matching/downcasing/appending
```
</details>


**62.** `components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:41`

**Issue:** ;XXX FIXME -- remove the IRC debug response below.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define sent-node (car (nlp-parse query)))

;; XXX FIXME -- remove the IRC debug response below.
(display "Hello ")
(display user)
```
</details>


**63.** `components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:108`

**Issue:** XXX FIXME This also definitely requires change after the backward

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Used by 'truth_query_process' to find the input for the backward
; chaining.
; XXX FIXME This also definitely requires change after the backward
; chaining is completed.
;-------------------------------------------------------------------
```
</details>


**64.** `components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm:34`

**Issue:** FIXME: maybe opencog's internal time octime should

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
"
(AtTimeLink
; FIXME: maybe opencog's internal time octime should
; be used. Will do for now, assuming a single instance
; deals with a single conversation.
```
</details>


**65.** `components/integration/opencog/opencog/nlp/relex2logic/relex2logic.scm:63`

**Issue:** FIXME: Presently, only a single interpretation is created for

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

(define (interpret PARSE-NODE)
; FIXME: Presently, only a single interpretation is created for
; each parse. Multiple interpreation should be handled, when
; word-sense-disambiguation, anaphora-resolution and other
```
</details>


**66.** `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:53`

**Issue:** XXX FIXME except that we can have (EvaluationLink "not" "run@1234") which

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ignore links that do not need to be post-processed.
;
; XXX FIXME except that we can have (EvaluationLink "not" "run@1234") which
;     appears unary but should be post-processed.  A more long term
;     solution is needed.
```
</details>


**67.** `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:140`

**Issue:** XXX FIXME should be changed to just use sha-256 -- that would make it

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
;
; Returns UUID version 4 (ie, mostly just random hex with some fixed values)
; XXX FIXME should be changed to just use sha-256 -- that would make it
; faster, better.
;
```
</details>


**68.** `components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:516`

**Issue:** FIXME: this is bad because in SV, SVO type rules the same word is

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
)

; FIXME: this is bad because in SV, SVO type rules the same word is
; ConceptNode instead
(define-public (gender-rule lemma word_inst gender)
```
</details>


**69.** `components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:634`

**Issue:** ;XXX FIXME: right now, this says ImplicationScopeLink

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
((or (string=? determiner "those") (string=? determiner "these"))
(ListLink
;; XXX FIXME: right now, this says ImplicationScopeLink
;; But I think the intended meaning is a for-all link:
;; (ForAllLink (VariableNode var_name) (ImplicationLink ...))
```
</details>


**70.** `components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:725`

**Issue:** XXX FIXME these two are not returned ???

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define n2 (cog-name n2-lemma))
(define n2_instance (cog-name n2-inst ))
; XXX FIXME these two are not returned ???
(r2l-wordinst-concept n1_instance)
(r2l-wordinst-concept n2_instance)
```
</details>


**71.** `components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:793`

**Issue:** second clause. XXX FIXME

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;
; Example: "She wants you to help us." -- assigns wrong subject in
; second clause. XXX FIXME
;
(define-public (to-do-rule-2
```
</details>


**72.** `components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1652`

**Issue:** XXX FIXME: there is no such thing as a "TruthValueGreaterThanLink",

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; functions without R2L rule, not working, unneeded, etc
; -----------------------------------------------------------------------
; XXX FIXME: there is no such thing as a "TruthValueGreaterThanLink",
; so this rule is borken.
;(define (comparative-rule w1 w1_instance w2 w2_instance adj adj_instance)
```
</details>


**73.** `components/integration/opencog/opencog/nlp/sureal/surface-realization.scm:291`

**Issue:** FIXME: This results in 'result' being 'Invalid handle' sometimes.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```

; Delete the filter-from SetLink and its encompasing MapLink.
; FIXME: This results in 'result' being 'Invalid handle' sometimes.
;(cog-extract-recursive! filter-from)

```
</details>


**74.** `components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:212`

**Issue:** XXX However, to fix this, we will need to modify the varscope code to

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; Return the variables and clauses in an association list
; XXX FIXME: really, if a or b are vars, then they are WordInstanceNodes.
; XXX However, to fix this, we will need to modify the varscope code to
; merge together lits of possibly duplicate var decls!
; XXX err, well, no, since b can sometimes be a
```
</details>


**75.** `components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:394`

**Issue:** XXX FIXME (this is same, similar problem to the other XXX above.)

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; However, this can't work if defined as simply as this: there are
; too many bogus matches for 'prep'.
; XXX FIXME (this is same, similar problem to the other XXX above.)
; # IF %ListLink("# APPLY TRIPLE RULES", $sent)
;      ^ %WordInstanceLink($var1,$sent)  ; $var1 and $var2 must be
```
</details>


**76.** `components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:469`

**Issue:** XXX FIXME, rule below is just rule above, but without the prep check.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; or more generally "X verbed Y prep Z".
;
; XXX FIXME, rule below is just rule above, but without the prep check.
; This means this rule might break one of the triple-style rules above...

```
</details>


**77.** `components/integration/opencog/opencog/nlp/chatbot-old/question/FrameQuery.cc:11`

**Issue:** * XXX todo-- should have is_query look for

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* Works, at least for basic queries.
*
* XXX todo-- should have is_query look for
*   <EvaluationLink>
*       <Element class="DefinedFrameElementNode" name="#Questioning:Message"/>
```
</details>


**78.** `components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:279`

**Issue:** and that's all good.  Err.. XXX bad if not a question, but whatever.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// If we got to here then we matched isa to isa or hypothetical_isa,
// and that's all good.  Err.. XXX bad if not a question, but whatever.
return true;
}
```
</details>


**79.** `components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:293`

**Issue:** XXX This code is never reached !!!???

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX This code is never reached !!!???
// This is a bit of dead code, which may need to be revived for more
// proper relex matching ... or maybe not ...
```
</details>


**80.** `components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:242`

**Issue:** threads listening on the scheme port.  FIXME XXX This should be fixed,

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; This interactive design is not very pretty, and it would be better
; to come up with some sort of multi-threaded design, with one of the
; threads listening on the scheme port.  FIXME XXX This should be fixed,
; because the current approach has *many* problems. However, the fix is
; hard, so we just punt for now.
```
</details>


**81.** `components/integration/opencog/opencog/nlp/wsd-post/collect-stats/disjunct-list.scm:48`

**Issue:** word order. XXX FIXME -- instead of doing the below, it would be

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Compare two link-grammar relations, and determine thier sentence
; word order. XXX FIXME -- instead of doing the below, it would be
; better and easier to use WordSequenceLink to get the correct
; word sequence.
```
</details>


**82.** `components/integration/opencog/opencog/nlp/relex2logic/rules/passive.scm:2`

**Issue:** XXX Fix relex so that we don't have to make such string searches!

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; check the tense if it is passive
; XXX Fix relex so that we don't have to make such string searches!
(define-public (check-tense tense)
(if (string-contains (cog-name tense) "passive") (stv 1 1) (stv 0 1))
```
</details>


**83.** `components/integration/opencog/opencog/ghost/procedures/pln-actions.scm:140`

**Issue:** FIXME Why doesn't the first call of (update-inferences) work?

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

(State pln-qa process-started)
; FIXME Why doesn't the first call of (update-inferences) work?
(update-inferences rb-trail-1 3 time)
(update-inferences rb-trail-1 3 time)
```
</details>


**84.** `components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm:195`

**Issue:** FIXME: This only works for trail-3

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(if (nil? candidates)
""
; FIXME: This only works for trail-3
(let ((sureal-results (get-sureal-results candidates)))
(if (nil? sureal-results)
```
</details>


**85.** `components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm:7`

**Issue:** FIXME: Doesn't return anything when confidence is low, don't use for now

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(use-modules (opencog pln))
(use-modules (opencog ure))
; FIXME: Doesn't return anything when confidence is low, don't use for now
;(load-from-path "opencog/pln/rules/implication-direct-evaluation.scm")

```
</details>


**86.** `components/integration/opencog/tests/openpsi/psi-implicator.scm:39`

**Issue:** FIXME: Using psi-goal results in the failure of OpenPsiRulesUTest

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define (component-1) (psi-component "component-1"))

; FIXME: Using psi-goal results in the failure of OpenPsiRulesUTest
; and OpenPsiImplicatorUTest
;(define goal-1 (psi-goal "goal-1" 1))
```
</details>


**87.** `components/language/learn/scm/common.scm:14`

**Issue:** XXX TODO FIXME All users of the three functions below need to be

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(use-modules (opencog) (opencog persist))

; XXX TODO FIXME All users of the three functions below need to be
; converted into users of the add-count-api, add-storage-count and
; add-marginal-count API's. The three functions below are
```
</details>


**88.** `components/language/learn/learn-lang-diary/utils/disjunct-stats.scm:39`

**Issue:** XXX FIXME add-pair-count-api is obsoleteuse add-support-api instead.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
; XXX FIXME add-pair-count-api is obsolete; use add-support-api instead.
(define psc (add-pair-count-api psa))
(define psf (add-pair-freq-api psa))
```
</details>


**89.** `components/language/learn/learn-lang-diary/utils/disjunct-cross.scm:32`

**Issue:** XXX FIXME add-pair-count-api is obsoleteuse add-support-api instead.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
; XXX FIXME add-pair-count-api is obsolete; use add-support-api instead.
(define psc (add-pair-count-api psa))
(define psf (add-pair-freq-api psa))
```
</details>


**90.** `components/language/learn/learn-lang-diary/utils/word-cosines.scm:31`

**Issue:** XXX FIXME add-pair-count-api is osoleteuse add-support-api instead.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define pseudo-cset-api (make-pseudo-cset-api))

; XXX FIXME add-pair-count-api is osolete; use add-support-api instead.
(define pseudo-cset-count-api (add-pair-count-api pseudo-cset-api))
(define pseudo-cset-freq-api (add-pair-freq-api pseudo-cset-api))
```
</details>


**91.** `components/language/learn/learn-lang-diary/utils/word-cosines.scm:51`

**Issue:** Should be this: XXX FIXME later

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(cog-incoming-by-type ITEM 'Section)

; Should be this: XXX FIXME later
; (pseudo-cset-support-api 'right-stars ITEM)
)
```
</details>


**92.** `components/language/learn/scm/parse/lg-pipe-parser.scm:17`

**Issue:** XXX FIXME. The next 30 lines of code are a cut-n-paste of the

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(use-modules (opencog matrix))

; XXX FIXME. The next 30 lines of code are a cut-n-paste of the
; pair parsing pipeline code. It is needed because the batch processor
; force-feeds use text, instead of allowing us to read on our own.
```
</details>


**93.** `components/language/learn/scm/parse/lg-pipe-parser.scm:26`

**Issue:** uncertain, so for now, assume only one global. FIXME someday, if

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; expectation that maybe this should work with multiple different storage
; nodes, maybe even a different one in each thread? This futre remains
; uncertain, so for now, assume only one global. FIXME someday, if
; needed.
(define disjunct-pipe-parser #f)
```
</details>


**94.** `components/language/learn/scm/attic/summary.scm:51`

**Issue:** XXX FIXME work on the singletons API so that we don't

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(if (eqv? 0.0 nww)
(begin
; XXX FIXME work on the singletons API so that we don't
; need to fetch the words.
(display "Start loading words ...\n")
```
</details>


**95.** `components/language/learn/scm/gram-class/gram-majority.scm:205`

**Issue:** XXX TODO this should be either

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(fold
(lambda (WRD CNT)
; XXX TODO this should be either
; (if (< 0 (LLOBJ 'pair-count WRD DJ)) ...)
; or it should be
```
</details>


**96.** `components/language/learn/scm/gram-class/shape-vec.scm:583`

**Issue:** XXX FIXME: we should give the star-wild a unique name,

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Extracting the star-wild will clobber all CrossSections.
; XXX FIXME: we should give the star-wild a unique name,
; so we don't accidentally clobber CrossSections in other
; objects.
```
</details>


**97.** `components/language/learn/scm/gram-class/gram-class-api.scm:65`

**Issue:** XXX FIXME: this won't work for some classes, which store

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```

; Recycle the right wildcard from the parent class.
; XXX FIXME: this won't work for some classes, which store
; marginals in a different format than pairs. That is, the
; 'right-element method will work correctly on pairs only,
```
</details>


**98.** `components/language/learn/scm/gram-class/gram-class-api.scm:133`

**Issue:** XXX FIXME the semantics of this thing is ugly, and should be

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; with the same arguments, then a new, unique name is generated!
; Therefore, this should never be called than once!
; XXX FIXME the semantics of this thing is ugly, and should be
; moved to the caller. We shouldn't have to second-guess the
; callers dsired behavior!
```
</details>


**99.** `components/language/learn/scm/gram-class/shape-project.scm:307`

**Issue:** XXX TODO -- generic deletion should be moved to a method

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ---------------------------------------------------------------

; XXX TODO -- generic deletion should be moved to a method
; on the base object -- probably to add-pair-stars. The extra
; stuff like deleting crosses belongs in the shape-vec API.
```
</details>


**100.** `components/language/learn/scm/gram-class/shape-project.scm:369`

**Issue:** FIXME but how?

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; anywhere, except in the marginals. We should clean those up
; too, except we don't know what the marginals are. Alas!
; FIXME but how?

; (format #t "Deleted ~A secs, ~A crosses for ~A" ns nx ROW)
```
</details>


**101.** `components/language/learn/scm/pair-count/word-pair-pipe.scm:38`

**Issue:** uncertain, so for now, assume only one global. FIXME someday, if

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; expectation that maybe this should work with multiple different storage
; nodes, maybe even a different one in each thread? This futre remains
; uncertain, so for now, assume only one global. FIXME someday, if
; needed.
(define pair-pipe-parser #f)
```
</details>


**102.** `components/language/learn/scm/attic/mpg-parse/lg-parser.scm:72`

**Issue:** XXX FIXME Both of these are global and stateful in LG and

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
XXX TODO Make above configurable.
"
; XXX FIXME Both of these are global and stateful in LG and
; should be a property of the dict. They must not change,
; once set.
```
</details>


**103.** `components/language/learn/scm/attic/lg-export/export-disjuncts.scm:454`

**Issue:** anything. XXX FIXME is this really needed ??

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; The UNKNOWN-WORD device is needed to make wild-card searches
; work (when dict debugging). The XXXBOGUS+ will not link to
; anything. XXX FIXME is this really needed ??
(dbi-query db-obj (string-append
"INSERT INTO Morphemes VALUES ("
```
</details>


**104.** `components/language/learn/scm/attic/cluster/agglo-loops.scm:381`

**Issue:** XXX FIXME: The DONE-LIST should be scrubbed for short junk. That is,

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;     forms, and thus need to go into multiple classes.
;
; XXX FIXME: The DONE-LIST should be scrubbed for short junk. That is,
; words in the DONE-LIST have a good chance of being completely
; neutered, with almost nothing left in them. They should get dropped.
```
</details>


**105.** `components/language/learn/scm/attic/cluster/agglo-loops.scm:385`

**Issue:** XXX Maybe-FIXME: There's some amount of pointless recomputation of

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; neutered, with almost nothing left in them. They should get dropped.
;
; XXX Maybe-FIXME: There's some amount of pointless recomputation of
; cosines between the word-list, and the existing grammatical classes.
; During the construction of the classes, a greedy search was formed
```
</details>


**106.** `components/language/learn/scm/attic/cluster/agglo-loops.scm:487`

**Issue:** handle this. XXX TODO.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; crud, or it might be a something that can now
; be merged into an existing class.  We should
; handle this. XXX TODO.
(greedy-grow MERGER
; The new true-list is now longer.
```
</details>


**107.** `components/language/learn/scm/attic/cluster/agglo-loops.scm:667`

**Issue:** XXX FIXME ... at the conclusion of this, we have a done list,

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(print-concluding-report)

; XXX FIXME ... at the conclusion of this, we have a done list,
; which, because of repeated merging, might possibly have been
; reduced to single senses, which can now be classified.
```
</details>


**108.** `components/language/learn/scm/attic/cluster/agglo-loops.scm:680`

**Issue:** XXX FIXME, should probably use

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
(define (load-stuff)
(define start-time (get-internal-real-time))
; XXX FIXME, should probably use
; ((make-gram-class-api) 'fetch-pairs) instead.
(display "Start loading words and word-classes\n")
```
</details>


**109.** `components/language/learn/scm/attic/cluster/gram-pairwise.scm:313`

**Issue:** XXX TODO this should not be exported, not really.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ---------------------------------------------------------------------

; XXX TODO this should not be exported, not really.
(define-public (start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON)
"
```
</details>


**110.** `components/language/learn/scm/attic/cluster/gram-pairwise.scm:409`

**Issue:** XXX TODO this should not be exported, not really.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ---------------------------------------------------------------------

; XXX TODO this should not be exported, not really.
(define-public (merge-into-cluster LLOBJ CLS WA FRAC-FN NOISE MRG-CON)
"
```
</details>


**111.** `components/language/learn/scm/attic/cluster/gram-pairwise.scm:494`

**Issue:** XXX TODO this should not be exported, not really.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; ---------------------------------------------------------------------

; XXX TODO this should not be exported, not really.
(define-public (merge-clusters LLOBJ CLA CLB MRG-CON)
"
```
</details>


**112.** `components/language/learn/scm/attic/cluster/gram-pairwise.scm:576`

**Issue:** that. XXX FIXME These need to be cleaned up!

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; cross-sections!  It might appear in Connectors that are in
; ConnectorSeqs that are in marginals, and we cannot control
; that. XXX FIXME These need to be cleaned up!
; So check the types we can control.
(if (or
```
</details>


**113.** `components/language/learn/scm/attic/pair-count-new/word-pair-count.scm:121`

**Issue:** XXX TODO: this should probably be converted to an 1xN matrix

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; update-word-counts -- update the count of the individual words
; in a parse.
; XXX TODO: this should probably be converted to an 1xN matrix
; and handled with a matrix API. The sentence count and parse
; count should be marginals on this thing.
```
</details>


**114.** `components/language/learn/scm/attic/pair-count/clique-pair-count.scm:22`

**Issue:** XXX FIXME we should probably not store this way. We should probably

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; distance between them, i.e. up to N times.
;
; XXX FIXME we should probably not store this way. We should probably
; have just one word-pair, and hold the counts in different values,
; instead. This needs a code redesign. XXX
```
</details>


**115.** `components/language/learn/attic/run-ull-2019/SchemeEval.cc:1057`

**Issue:** XXX FIXME -- idealy we should avoid catch-and-rethrow.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Rethrow.  It would be better to just allow exceptions
// to pass on through, but thus breaks some unit tests.
// XXX FIXME -- idealy we should avoid catch-and-rethrow.
// At any rate, we must not return a TV of any sort, here.
throw RuntimeException(TRACE_INFO, "%s", _error_msg.c_str());
```
</details>


**116.** `components/language/learn/attic/run-ull-2019/SchemeEval.cc:1180`

**Issue:** XXX FIXME only a subset is needed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void SchemeEval::init_scheme(void)
{
// XXX FIXME only a subset is needed.
SchemeEval sch;
}
```
</details>


**117.** `components/language/lg-atomese/opencog/nlp/lg-dict/LGDictExpContainer.cc:239`

**Issue:** XXX FIXME this does not smell rightoptionals should get

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (m_type == CONNECTOR_type)
{
// XXX FIXME this does not smell right; optionals should get
// blown up into pairs of disjuncts, one with and one without.
if (m_string == "OPTIONAL") return { optnl };
```
</details>


**118.** `components/language/lg-atomese/opencog/nlp/lg-dict/LGDictReader.cc:46`

**Issue:** FIXME XXX -- Optionals are handled incorrectly here;

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
std::vector<LGDictExpContainer> subcontainers;

// FIXME XXX -- Optionals are handled incorrectly here;
// they are denoted by a null Exp pointer in an OR_list!
// Ignoring all the nulls is just ... wrong.
```
</details>


**119.** `components/language/lg-atomese/opencog/nlp/lg-dict/LGDictReader.cc:87`

**Issue:** * XXX FIXME -- this gives incorrect results if the word has non-trivial

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* appear anywhere.
*
* XXX FIXME -- this gives incorrect results if the word has non-trivial
* morphology e.g. if its Russian, and can be split into a stem and a
* suffix.  The problem is that in LG, both stem and suffix count as
```
</details>


**120.** `components/language/lg-atomese/opencog/nlp/lg-dict/LGDictReader.cc:103`

**Issue:** XXX FIXME -- if dn_head is null, then we should check regexes.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
HandleSeq outgoing;

// XXX FIXME -- if dn_head is null, then we should check regexes.
// Currently, LG does not do this automatically, but it almost surely
// should. i.e. the LG public API needs to also handle regexes
```
</details>


**121.** `components/language/lg-atomese/opencog/nlp/lg-parse/LGParseLink.cc:338`

**Issue:** XXX FIXME -- We should fish parse options out of the atomspace.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME -- We should fish parse options out of the atomspace.
// Something like this, maybe:
//     EvaluationLink
```
</details>


**122.** `components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc:920`

**Issue:** XXX TODO - maybe load links depth-order...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
CHECK_OPEN;
// First, load all the nodes ... then the links.
// XXX TODO - maybe load links depth-order...
loadAtoms(table, "n@");
loadAtoms(table, "l@");
```
</details>


**123.** `components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc:944`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void MonoStorage::storeAtomSpace(const AtomSpace* table)
{
// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


**124.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:525`

**Issue:** XXX this is adding to wrong atomspace!?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

ValuePtr vp = getValue("k@" + sid + fid + ":" + kid);
// XXX this is adding to wrong atomspace!?
if (as and vp) vp = as->add_atoms(vp);
h->setValue(key, vp);
```
</details>


**125.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:697`

**Issue:** /Note: currently broken for multi-space usage, XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// Backend callback - find the Link. This is used ONLY to implement
/// the backend Query call, and is not otherwised used.
/// Note: currently broken for multi-space usage, XXX FIXME.
Handle RocksStorage::getLink(Type t, const HandleSeq& hs)
{
```
</details>


**126.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:1286`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
convertForFrames(HandleCast(getAtomSpace()));

// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


**127.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksPersistSCM.cc:82`

**Issue:** XXX FIXME -- are open and close actually needed for anything?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME -- are open and close actually needed for anything?
void RocksPersistSCM::do_open(const std::string& uri)
{
```
</details>


**128.** `components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py:10`

**Issue:** I can't find swagger on ubuntu .. wtf!? FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
from flask_restful.utils import cors

# I can't find swagger on ubuntu .. wtf!? FIXME
from flask_restful_swagger import swagger
from opencog.bank import AttentionBank
```
</details>


**129.** `components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py:377`

**Issue:** xxxxxxxxxxxx here add atoms

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if dot_format not in ['True', 'true', '1']:
atom_list = AtomListResponse(atoms)
# xxxxxxxxxxxx here add atoms
json_data = {'result': atom_list.format()}

```
</details>


**130.** `components/core/atomspace-restful/opencog/python/web/api/utilities.py:17`

**Issue:** FIXME: Should this moved to the atomspace repo and be part

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# https://github.com/opencog/atomspace/pull/611
# NOTE: This is similar to scheme `cog-node`.
# FIXME: Should this moved to the atomspace repo and be part
# of opencog.atomspace module?
def get_atoms_by_name(z_type, name, atomspace):
```
</details>


**131.** `components/core/atomspace-restful/tests/python/restapi/test_restapi.py:449`

**Issue:** XXX Emptied because the scheme command (i.e cog-af-boundary) has been removed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# Test an arbitrary Scheme command to ensure the binding is working
# properly
# XXX Emptied because the scheme command (i.e cog-af-boundary) has been removed.
pass
def test_n_dot_export(self):
```
</details>


**132.** `components/learning/moses/examples/example-progs/trap-bit.cc:26`

**Issue:** XXX under construction XXX

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
using boost::lexical_cast;

// XXX under construction XXX

// XXX this example is broken, and will remain so until "multivariate"
```
</details>


**133.** `components/learning/moses/examples/example-progs/trap-bit.cc:28`

**Issue:** XXX this example is broken, and will remain so until "multivariate"

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// XXX under construction XXX

// XXX this example is broken, and will remain so until "multivariate"
// is ported over/re-implemented. Basically, there is no structure
// learning at this time.
```
</details>


**134.** `components/learning/moses/examples/example-progs/trap-bit.cc:31`

**Issue:** XXX some of the documentation below may be misleading.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// is ported over/re-implemented. Basically, there is no structure
// learning at this time.
// XXX some of the documentation below may be misleading.
//
// Demonstration program for the "bit-trap" optimization problem.
```
</details>


**135.** `components/learning/moses/examples/example-progs/trap-bit.cc:42`

**Issue:** XXX which is why we need to put structure leanring back in the code XXX

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// the optimal solution cannot be found without structure learning; the
// MOSES univariate() learner is a no-op, and so cannot solve this problem.
// XXX which is why we need to put structure leanring back in the code XXX
//
// The correlation between variables is accomplished by using a
```
</details>


**136.** `components/learning/moses/examples/example-progs/nmax.cc:38`

**Issue:** XXX setting n=2 currently fails due to a bug, see

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// This is a generalization of the "onemax" problem, which is nmax with n=2.
//
// XXX setting n=2 currently fails due to a bug, see
// https://bugs.launchpad.net/moses/+bug/908230
//
```
</details>


**137.** `components/learning/moses/examples/example-progs/ontomax.cc:36`

**Issue:** XXX Someday, fix all of this!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// instead of a 2-ary tree for contin.
//
// XXX Someday, fix all of this!

void recbuild(term_tree& tr, term_tree::iterator it,
```
</details>


**138.** `components/learning/moses/examples/example-progs/continmax.cc:46`

**Issue:** XXX Currently, this doesn't really work well, or maybe at all, in

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// is how the problem becomes easy when applying calculus).
//
// XXX Currently, this doesn't really work well, or maybe at all, in
// part because the contin implementation in the field set is incomplete
// or broken or maybe both; there is a confusion between depth and arity
```
</details>


**139.** `components/learning/moses/moses/moses/optimization/star-anneal.cc:42`

**Issue:** XXX TODO the annealing temperature control code should be ported over

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/////////////////////////

// XXX TODO the annealing temperature control code should be ported over
// to the hill-climbing code, thus rendering the below obsolete.  The
// hill-climbing code is much more sophisticated in every way: correct
```
</details>


**140.** `components/learning/moses/moses/moses/optimization/optimization.h:82`

**Issue:** XXX Why n^1.05 ??? This is going to have a significant effect

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// N = p.popsize_ratio * n^1.05
// XXX Why n^1.05 ??? This is going to have a significant effect
// (as compared to n^1.00) only when n is many thousands or bigger...
unsigned pop_size(const field_set& fs) const;
```
</details>


**141.** `components/learning/moses/moses/moses/optimization/optimization.h:116`

**Issue:** problem size in info-theoretic bits.  (XXX Why 1.05 ???)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Populations are sized at N = popsize_ratio*n^1.05 where n is
// problem size in info-theoretic bits.  (XXX Why 1.05 ???)
double pop_size_ratio;

```
</details>


**142.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:45`

**Issue:** XXX PSO parameters hardcoded just for now.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Particle Swarm parameters
// XXX PSO parameters hardcoded just for now.
struct ps_parameters
{
```
</details>


**143.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:82`

**Issue:** XXX I have to find the best values for bit and disc.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// For continuous problems, 0.7 and 1.43 are good values.
// XXX I have to find the best values for bit and disc.
// Information from: M. Clerc, L’optimisation par essaim particulaire: versions
// paramétriques et adaptatives, Hermes Science Publications, Lavoisier, Paris, 2005.
```
</details>


**144.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:103`

**Issue:** Use rounding off for now XXX it isn't so effective, but for now is easier

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
//
// For discrete:
// Use rounding off for now XXX it isn't so effective, but for now is easier
// to implement. I'll use MVPSO, JPSO or IMVPSO later.
// In the transformation i'll use min value of 0 and 1 for max in the continuous
```
</details>


**145.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:133`

**Issue:** XXX If i have time, i'll put some variation here to get a better global search.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// with Artificial Intelligence IEEE-ICTAI, vol. 1, Arras, France, 27–29 October 2010,
// 2010, pp. 87–93.
// XXX If i have time, i'll put some variation here to get a better global search.
double bit_min_value, bit_max_value, // [0,1] <- XXX these two aren't used yet.
disc_min_value, disc_max_value, // [0,1] in rounding off
```
</details>


**146.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:253`

**Issue:** XXX Explanation

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX Explanation
bool new_bit_value(const double& vel){
return (randGen().randdouble() < // Sigmoid
```
</details>


**147.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:281`

**Issue:** XXX Explanation

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX Explanation
disc_t new_disc_value(double& cvalue,
const double& vel, const unsigned max_dvalue){
```
</details>


**148.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:303`

**Issue:** XXX Explanation

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX Explanation
contin_t new_cont_value(const contin_t& value, const double& vel){
// Wind dispersion enter here.
```
</details>


**149.** `components/learning/moses/moses/moses/optimization/particle-swarm.h:340`

**Issue:** XXX This could help PSO if we maintain the best particle.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// In fact, all of the current code uses this entry point, no one
// bothers to supply an initial instance.
// XXX This could help PSO if we maintain the best particle.
void operator()(deme_t& deme,
const iscorer_base& iscorer,
```
</details>


**150.** `components/learning/moses/moses/moses/optimization/hill-climbing.h:80`

**Issue:** scan is that much better than a random sampling.  XXX Or is it?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// nearest neighbor scan for such cases will eat up a huge amount of
// RAM in the instance_set, and its not currently obvious that a full
// scan is that much better than a random sampling.  XXX Or is it?
//
// XXX This parameter should probably be automatically adjusted with
```
</details>


**151.** `components/learning/moses/moses/moses/optimization/hill-climbing.h:82`

**Issue:** XXX This parameter should probably be automatically adjusted with

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// scan is that much better than a random sampling.  XXX Or is it?
//
// XXX This parameter should probably be automatically adjusted with
// free RAM availability!?  Or something like that !?
//
```
</details>


**152.** `components/learning/moses/moses/moses/optimization/hill-climbing.h:97`

**Issue:** XXX I don't understand what the below is saying.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// improved.
//
// XXX I don't understand what the below is saying.
// One should probably try first to tweak pop_size_ratio to
// control the allocation of resources. However in some cases (for
```
</details>


**153.** `components/learning/moses/moses/moses/optimization/hill-climbing.h:103`

**Issue:** XXX pop_size_ratio disabled in hill-climbing, since its definition

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// there is only one deme to explore and tweaking that parameter
// can make a difference (breadth vs depth)
// XXX pop_size_ratio disabled in hill-climbing, since its definition
// was insane/non-sensical.  I can't figure out how it was supposed
// to work.
```
</details>


**154.** `components/learning/moses/moses/moses/optimization/hill-climbing.h:110`

**Issue:** XXX TODO make sure this value is appropriately updated.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Range of scores for which to keep instances.  This *should* be
// set to the value given by metapopulation::useful_score_range().
// XXX TODO make sure this value is appropriately updated.
//
// The range of scores is used to keep the size of the deme in check.
```
</details>


**155.** `components/learning/moses/moses/moses/optimization/hill-climbing.h:281`

**Issue:** XXX In fact, all of the current code uses this entry point, no one

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Like above but assumes that init_inst is null (backward compatibility)
// XXX In fact, all of the current code uses this entry point, no one
// bothers to supply an initial instance.
void operator()(deme_t& deme,
```
</details>


**156.** `components/learning/moses/moses/moses/optimization/hill-climbing.cc:732`

**Issue:** /the vector.  But this ixsn't know ... XXX experiment with this!?

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// regions of low scores, in one go.  It is possible that a faster
/// algorithm would be to sort first, and then delete the tail-end of
/// the vector.  But this ixsn't know ... XXX experiment with this!?
/// ... err, but right now, trimming takes a small fraction of a second,
/// so there is no rush to fis this.
```
</details>


**157.** `components/learning/moses/moses/moses/optimization/optimization.cc:79`

**Issue:** XXX Why n^1.05 ??? This is going to have a significant effect

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// N = p.popsize_ratio * n^1.05
// XXX Why n^1.05 ??? This is going to have a significant effect
// (as compared to n^1.00) only when n is many thousands or bigger...
unsigned optim_parameters::pop_size(const field_set& fs) const
```
</details>


**158.** `components/learning/moses/moses/moses/optimization/particle-swarm.cc:122`

**Issue:** XXX What score do i use?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
current_number_of_evals += swarm_size;

// XXX What score do i use?
// I'll use best_score for now.
bool has_improved = false;
```
</details>


**159.** `components/learning/moses/moses/moses/metapopulation/ensemble.h:55`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* from the metapopulation, which has no particular admission criteria.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**160.** `components/learning/moses/moses/moses/metapopulation/metapopulation.h:90`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* for inference, the metapopulation for breeding.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**161.** `components/learning/moses/moses/moses/metapopulation/merging.cc:261`

**Issue:** XXX FIXME: we should use a pointer set for scored_combo_tree_set

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
candidates.size());

// XXX FIXME: we should use a pointer set for scored_combo_tree_set
// This would avoid some pointless copying here and a few other
// places.  This is easier said than done, because the stupid
```
</details>


**162.** `components/learning/moses/moses/moses/metapopulation/merging.cc:554`

**Issue:** XXX FIXME looks to me like it++ can often be collaed twice within this loop!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME looks to me like it++ can often be collaed twice within this loop!
prev_it = it++;
}
```
</details>


**163.** `components/learning/moses/moses/moses/metapopulation/metapopulation.cc:222`

**Issue:** XXX FIXME should probably not recompute every time ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _best_cscore;

// XXX FIXME should probably not recompute every time ...
// need to figure who is calling this method, and what they are expecting.
return _cscorer.get_cscore(_ensemble.get_ensemble());
```
</details>


**164.** `components/learning/moses/moses/moses/metapopulation/ensemble.cc:235`

**Issue:** if we are here, its the in-exact experts code.  XXX currently broken ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

} else {
// if we are here, its the in-exact experts code.  XXX currently broken ...
// Any score worse than half is terrible. Half gives a weight of zero.
// More than half gives negative weights, which wreaks things.
```
</details>


**165.** `components/learning/moses/moses/moses/metapopulation/ensemble.cc:259`

**Issue:** XXX the logic below is probably wrong.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
size_t bslen = _bscorer.size();

// XXX the logic below is probably wrong.
OC_ASSERT(false, "this doesn't work right now.");
// Now, look to see where this scorer was wrong, and bump the
```
</details>


**166.** `components/learning/moses/moses/moses/scoring/scoring_base.h:124`

**Issue:** XXX TODO should be a std::valarray not a vector.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// A vector of per-bscore weights, used to tote up the behavioral
/// score into a single number.
// XXX TODO should be a std::valarray not a vector.
virtual void update_weights(const std::vector<double>&);

```
</details>


**167.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:213`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
_full_bscore(true)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 2;

```
</details>


**168.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:394`

**Issue:** umm XXX I think the normalization here should be the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// because the precision is normalized as well.  So e.g.
// max precision for boolean problems is 1.0.  However...
// umm XXX I think the normalization here should be the
// best-possible activation, not the usize, right?
//
```
</details>


**169.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:417`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, min_precision, max_precision, hardness)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 2;
}
```
</details>


**170.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:486`

**Issue:** XXX TODO -- should not return the penalties as part of the bscore,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, min_recall, max_recall, hardness)
{
// XXX TODO -- should not return the penalties as part of the bscore,
// since this messes up boosting.
_size = ct.size() + 2;
```
</details>


**171.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:586`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, min_diff, max_diff, hardness)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 2;
}
```
</details>


**172.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:663`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, 0.0, 1.0, 1.0e-20)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 1;
}
```
</details>


**173.** `components/learning/moses/moses/moses/moses/mpi_moses.h:51`

**Issue:** this just right now. XXX TODO: do this, someday.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// have less dispatcher code to maintain.  But this will require some
// fair amount of work and testing, and there's no pressing need to do
// this just right now. XXX TODO: do this, someday.
//
class moses_mpi_comm
```
</details>


**174.** `components/learning/moses/moses/moses/moses/local_moses.cc:56`

**Issue:** XXX When would one never expand?  Wouldn't that be a bug?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Attempt to create a non-empty representation, by looping
// over exemplars until we find one that expands.
// XXX When would one never expand?  Wouldn't that be a bug?
while (1) {
scored_combo_tree_ptr_set_cit exemplar = mp.select_exemplar();
```
</details>


**175.** `components/learning/moses/moses/moses/moses/partial.cc:264`

**Issue:** XXX is this correct? I think we need to ask the cscorer for the total ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
behavioral_score bs = _bscore->operator()(cand);

// XXX is this correct? I think we need to ask the cscorer for the total ...
score_t total_score = 0.0;
for (const score_t& sc : bs)
```
</details>


**176.** `components/learning/moses/moses/moses/moses/partial.cc:304`

**Issue:** XXX Ineffective predicates may be due to enums that have been

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
effective(predicate, good_count, fail_count);

// XXX Ineffective predicates may be due to enums that have been
// completely accounted for ... not sure what to do about that...
if ((0 < good_count) || (0 < fail_count))
```
</details>


**177.** `components/learning/moses/moses/moses/moses/neighborhood_sampling.h:295`

**Issue:** @todo: handle term algebras XXX

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// Handle term knobs with early return optimization
if (starting_index >= fs.begin_term_raw_idx()) {
// @todo: handle term algebras XXX
out = vary_n_knobs(fs, tmp_inst, dist,
starting_index + fs.end_term_raw_idx(),
```
</details>


**178.** `components/learning/moses/moses/moses/moses/mpi_moses.cc:201`

**Issue:** XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
///
/// This sends a pretty big glob.
// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.
void moses_mpi_comm::send_deme(const metapopulation& mp, int n_evals)
{
```
</details>


**179.** `components/learning/moses/moses/moses/moses/mpi_moses.cc:307`

**Issue:** XXX replace this with appropriate message back to root!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
right expansion
count */)) {
// XXX replace this with appropriate message back to root!
OC_ASSERT(false, "Exemplar failed to expand!\n");
}
```
</details>


**180.** `components/learning/moses/moses/moses/moses/types.h:189`

**Issue:** XXX wouldn't it be better to store ints here ??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// comes from demeID "0".
//
// XXX wouldn't it be better to store ints here ??
struct demeID_t : public std::string
{
```
</details>


**181.** `components/learning/moses/moses/moses/deme/deme_expander.cc:356`

**Issue:** * XXX TODO I honestly just don't see the utility of this multi-deme

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* set, and thus, a different deme.
*
* XXX TODO I honestly just don't see the utility of this multi-deme
* creation mechanism.  Feature selection is a very crude mechanism,
* and the representation is just randomly peppered with knobs. By
```
</details>


**182.** `components/learning/moses/moses/moses/deme/deme_expander.cc:502`

**Issue:** XXX FIXME this is a bug .. the user may have specified that

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// reset scorer to use all variables (important so that
// behavioral score is consistent across generations
// XXX FIXME this is a bug .. the user may have specified that
// certain incdexes should be ignored, and this just wipes
// those out...
```
</details>


**183.** `components/learning/moses/moses/moses/eda/local_structure.h:262`

**Issue:** XXX ??? Huh? More details, please...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Now that we have created all of the dtrees, construct a
// feasible order that respects the initial dependencies
// XXX ??? Huh? More details, please...
randomized_topological_sort(_initial_deps, _ordering.begin());
}
```
</details>


**184.** `components/learning/moses/moses/moses/eda/local_structure.h:271`

**Issue:** themselves shouldn't change (XXX why would we want to do this ??)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//
// instance_set is not const so that we can reorder it - the instances
// themselves shouldn't change (XXX why would we want to do this ??)
//
// For univariate(), the dtree for each field will be empty.
```
</details>


**185.** `components/learning/moses/moses/moses/representation/representation.cc:51`

**Issue:** XXX TODO: One might think that varying the stepsize, i.e. shrinking

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// distribution of the contin variables.
//
// XXX TODO: One might think that varying the stepsize, i.e. shrinking
// it, as the optimizers tune into a specific value, would be a good
// thing (so that the optimizer could tune to a more precise value).
```
</details>


**186.** `components/learning/moses/moses/moses/representation/representation.cc:236`

**Issue:** XXX TODO need to add support for "term algebra" knobs

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void representation::transform(const instance& inst)
{
// XXX TODO need to add support for "term algebra" knobs

contin_map_it ckb = contin.begin();
```
</details>


**187.** `components/learning/moses/moses/moses/representation/representation.cc:391`

**Issue:** XXX This is dead code, no one uses it, and looking at the below, it

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

#ifdef EXEMPLAR_INST_IS_UNDEAD
// XXX This is dead code, no one uses it, and looking at the below, it
// looks inconsistent to me. I'm going to leave it here for a while, but
// it should be removed by 2013 or 2014 if not sooner...
```
</details>


**188.** `components/learning/moses/moses/moses/representation/representation.cc:395`

**Issue:** XXX why are we clearing this, instead of setting it back to the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// it should be removed by 2013 or 2014 if not sooner...

// XXX why are we clearing this, instead of setting it back to the
// _exemplar_inst ??? XXX is this broken??
//
```
</details>


**189.** `components/learning/moses/moses/moses/representation/representation.cc:396`

**Issue:** _exemplar_inst ??? XXX is this broken??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// XXX why are we clearing this, instead of setting it back to the
// _exemplar_inst ??? XXX is this broken??
//
// XXX Note that the clear_exemplar() methods on the knobs are probably
```
</details>


**190.** `components/learning/moses/moses/moses/representation/representation.cc:398`

**Issue:** XXX Note that the clear_exemplar() methods on the knobs are probably

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// _exemplar_inst ??? XXX is this broken??
//
// XXX Note that the clear_exemplar() methods on the knobs are probably
//  not needed either!?
void representation::clear_exemplar()
```
</details>


**191.** `components/learning/moses/moses/moses/representation/representation.cc:409`

**Issue:** XXX that, and contin seems to be handled inconsistently with disc...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// What is this doing ? seems to be clearing things out, why do we need this?
// XXX that, and contin seems to be handled inconsistently with disc...
// I mean, shouldn't we be setting the exemplar_inst fields so that
// they match the exmplar?  Do we even need the exemplar_inst for anything?
```
</details>


**192.** `components/learning/moses/moses/moses/representation/field_set.h:213`

**Issue:** XXX should be enum ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX should be enum ...
static const disc_t Stop;  // 0
static const disc_t Left;  // 1
```
</details>


**193.** `components/learning/moses/moses/moses/representation/instance_scorer.h:89`

**Issue:** XXX FIXME, calling score_tree above does not throw the exceptionthis should be done

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _cscorer.get_cscore(tr);
} catch (...) {
// XXX FIXME, calling score_tree above does not throw the exception; this should be done
// differntly, maybe call bscorer directly, then ascorer...
// ??? Huh? why couldn't we evaluate a tree anyway?  why would we want an exception here?
```
</details>


**194.** `components/learning/moses/moses/moses/representation/build_knobs.cc:115`

**Issue:** Petbrain  XXX does this call code that actually works??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}
else if (output_type == id::action_result_type) {
// Petbrain  XXX does this call code that actually works??
action_canonize(_exemplar.begin());
build_action(_exemplar.begin());
```
</details>


**195.** `components/learning/moses/moses/moses/representation/build_knobs.cc:121`

**Issue:** ANN  XXX This is calling unfinished, broken code, below.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}
else if (output_type == id::ann_type) {
// ANN  XXX This is calling unfinished, broken code, below.
ann_canonize(_exemplar.begin());
build_contin(_exemplar.begin());
```
</details>


**196.** `components/learning/moses/moses/moses/representation/build_knobs.cc:211`

**Issue:** * XXX TODO: see comments on disc_probe() below.  This method is a real

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
* the exemplar is split into two at each recursion.
*
* XXX TODO: see comments on disc_probe() below.  This method is a real
* CPU time-waster; it is not clear that the computational cost is ever
* repaid as a performance advantage later on, during the instance search.
```
</details>


**197.** `components/learning/moses/moses/moses/representation/build_knobs.cc:690`

**Issue:** XXX TODO: Is this really optimal?  The below adds an entire copy

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO: Is this really optimal?  The below adds an entire copy
// of the tree at it, which clearly increases the overall complexity.
// But is this really a wise thig to do? It seems gratuitous, and it's
```
</details>


**198.** `components/learning/moses/moses/moses/representation/build_knobs.cc:917`

**Issue:** This may not be desirable...!?  XXX some experimental validation

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
#ifdef LATER
// The canonization used here raises the degree of polynomials.
// This may not be desirable...!?  XXX some experimental validation
// that this speeds convergence is needed.
for (sib_it sib = it.begin(); sib != it.end(); ++sib) {
```
</details>


**199.** `components/learning/moses/moses/moses/representation/build_knobs.cc:1283`

**Issue:** XXX TODO this below is clearly unfinished, broken, etc.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO this below is clearly unfinished, broken, etc.
// and can't possibly work ...
void build_knobs::ann_canonize(pre_it it)
```
</details>


**200.** `components/learning/moses/moses/moses/representation/build_knobs.cc:1342`

**Issue:** //FIXME: now just attaches to the first output

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

//now attach the subtree to the hidden nodes
//FIXME: now just attaches to the first output
sib_it first_hidden = it.begin();

```
</details>


**201.** `components/learning/moses/moses/moses/representation/knobs.h:225`

**Issue:** XXX This uses reduct::logical_reduction rulesit is not clear if those

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// tree underneath it.
//
// XXX This uses reduct::logical_reduction rules; it is not clear if those
// rules tolerate predicates.
//
```
</details>


**202.** `components/learning/moses/moses/moses/representation/knobs.h:228`

**Issue:** XXX what is the difference between "present" and "absent" ??? A

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// rules tolerate predicates.
//
// XXX what is the difference between "present" and "absent" ??? A
// knob that is "absent" from a logical "or" is the same as "present
// and false".  while one that is absent from a logical "and" is the
```
</details>


**203.** `components/learning/moses/moses/moses/representation/knobs.h:237`

**Issue:** XXX Also -- I think I want to rename this to "logical unary knob",

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// justification.
//
// XXX Also -- I think I want to rename this to "logical unary knob",
// or something like that, as it is a unary logical function ... err...
// well, I guess all combo opers are unary, due to Currying.
```
</details>


**204.** `components/learning/moses/moses/moses/main/problem-params.h:46`

**Issue:** XXX FIXME TODO The structure below should be split into multiple

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
namespace opencog { namespace moses {

// XXX FIXME TODO The structure below should be split into multiple
// parts, with each sub-part responsible for picking out the argv's
// that it cares about. Unfortunately, this requires getting rid of
```
</details>


**205.** `components/learning/moses/moses/moses/main/table-problems.cc:138`

**Issue:** XXX FIXME -- the multiple tables should be merged into one.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
logger().info("Number of rows in tables = %d", num_rows);

// XXX FIXME -- the multiple tables should be merged into one.
ctable = _ctables.front();
table = _tables.front();
```
</details>


**206.** `components/learning/moses/moses/moses/main/table-problems.cc:150`

**Issue:** XXX FIXME .. check that they all have the same signature.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Check that all input data files have the same arity
// XXX FIXME .. check that they all have the same signature.
if (_tables.size() > 1) {
for (size_t i = 1; i < _tables.size(); ++i) {
```
</details>


**207.** `components/learning/moses/moses/moses/main/table-problems.cc:487`

**Issue:** XXX Eh ??? for precision/recall scorers,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// For enum targets, like boolean targets, the score
// can never exceed zero (perfect score).
// XXX Eh ??? for precision/recall scorers,
// the score range is 0.0 to 1.0 so this is wrong...
if (0.0 < pms.moses_params.max_score) {
```
</details>


**208.** `components/learning/moses/moses/moses/main/problem-params.cc:166`

**Issue:** XXX TODO: make this print correctly, instead of using brackets.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Declare the supported options.
// XXX TODO: make this print correctly, instead of using brackets.
desc.add_options()

```
</details>


**209.** `components/learning/moses/moses/feature-selection/algo/simple.h:150`

**Issue:** XXX or use partial_sort, as mentioned above...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
} else {
// stair-step distribution: keep the top num_desired only.
//  XXX or use partial_sort, as mentioned above...
for (auto pr = sorted_flist.begin(); pr != sorted_flist.end(); pr++) {
final.insert(*pr->second.begin());
```
</details>


**210.** `components/learning/moses/moses/comboreduct/type_checker/type_tree.cc:599`

**Issue:** XXX TODO the code below was modified to allow arg lists of

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// and a4 inherit from T2.  T3 is the output type.

// XXX TODO the code below was modified to allow arg lists of
// mixed type, e.g. so that the cond primitive could be
// supported (as the current definition of cond alternates
```
</details>


**211.** `components/learning/moses/moses/comboreduct/interpreter/interpreter.cc:336`

**Issue:** XXX TODO: contin_if should go away.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO: contin_if should go away.
case id::contin_if :
case id::cond : {
```
</details>


**212.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:313`

**Issue:** @todo FIXME there should be a general way to distinguish between

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const vertex& v = *it;

// @todo FIXME there should be a general way to distinguish between
// "f" (the function f, being passed to fold) vs "f" (the function f
// being called with no arguments). If you don't handle that you get
```
</details>


**213.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:530`

**Issue:** XXX TODO: contin_if should go away.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO: contin_if should go away.
case id::contin_if :
case id::cond : {
```
</details>


**214.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:573`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
try {
vertex retv(eval_throws_vertex(bmap, it));
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**215.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:579`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
catch (EvalException& e) {
vertex retv = e.get_vertex();
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**216.** `components/learning/moses/moses/comboreduct/combo/descriptions.cc:85`

**Issue:** XXX Should probably be "union", yeah?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//
// The 'value' is marked unknown, as it can be of any type.
// XXX Should probably be "union", yeah?
{ id::cond,               "->(arg_list(boolean unknown) unknown unknown)" },

```
</details>


**217.** `components/learning/moses/moses/comboreduct/combo/descriptions.cc:91`

**Issue:** Tests for equality. XXX Someday, we should also allow equality

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
{ id::contin_if,            "->(boolean contin contin contin)" },

// Tests for equality. XXX Someday, we should also allow equality
// testing for booleans, contin as well, but not today.
// Fixing this will require work in typechecker, and use of union
```
</details>


**218.** `components/learning/moses/moses/comboreduct/combo/descriptions.cc:94`

**Issue:** type. XXX FIXME...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// testing for booleans, contin as well, but not today.
// Fixing this will require work in typechecker, and use of union
// type. XXX FIXME...
{ id::equ,                  "->(enum enum boolean)" },
};
```
</details>


**219.** `components/learning/moses/moses/comboreduct/combo/vertex.h:94`

**Issue:** be generalized soon.  XXX do this...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Generic functions
// Currently take enum as arg or return enum, but should
// be generalized soon.  XXX do this...
cond,
equ,
```
</details>


**220.** `components/learning/moses/moses/comboreduct/combo/vertex.h:101`

**Issue:** XXX This should be obsoleted by cond, at some point.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
apply,

// XXX This should be obsoleted by cond, at some point.
// Maybe action_boolean_if too, I guess?
contin_if,
```
</details>


**221.** `components/learning/moses/moses/comboreduct/combo/vertex.h:418`

**Issue:** inequality XXX Huh? but its not in namespace boost !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// don't know why this is needed *in namespace boost*, but it is, for
// e.g. calling a generic stl function that compares vertices for
// inequality XXX Huh? but its not in namespace boost !?
inline bool operator!=(const vertex& v1, const vertex& v2)
{
```
</details>


**222.** `components/learning/moses/moses/comboreduct/combo/iostream_combo.cc:440`

**Issue:** XXX ?? Ahem, won't calling out<<(*m) just lead to infinite

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return out << m->getContent();

// XXX ?? Ahem, won't calling out<<(*m) just lead to infinite
// recursion ??
if (const ann_type* z = get<ann_type>(&v))
```
</details>


**223.** `components/learning/moses/moses/comboreduct/table/table.h:633`

**Issue:** XXX TODO WARNING ERROR: builtin hardcoded shit!!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
for (unsigned i = 0; i < seq.size(); ++i) {
if (it != filter.cend() && (typename F::value_type)i == *it) {
// XXX TODO WARNING ERROR: builtin hardcoded shit!!!
res.push_back(seq.get_at<builtin>(i));
++it;
```
</details>


**224.** `components/learning/moses/moses/comboreduct/table/table.h:637`

**Issue:** XXX TODO WARNING ERROR: builtin hardcoded shit!!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
++it;
} else {
// XXX TODO WARNING ERROR: builtin hardcoded shit!!!
res.push_back(id::null_vertex);
}
```
</details>


**225.** `components/learning/moses/moses/comboreduct/table/table.h:1068`

**Issue:** * XXX TODO -- this also should probably support the weight column,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* to add enum support, cut-n-paste from CTable code below.
*
* XXX TODO -- this also should probably support the weight column,
* since not all rows are important, and the ones that are not
* important should not contribute to the MI.
```
</details>


**226.** `components/learning/moses/moses/comboreduct/table/table.h:1273`

**Issue:** XXX TODO, it would be easier if KLD took a sorted list

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO, it would be easier if KLD took a sorted list
// as the argument.
std::vector<contin_t> p, q;
```
</details>


**227.** `components/learning/moses/moses/comboreduct/table/table.h:1282`

**Issue:** XXX review this, is this really correct?  At any rate,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// KLD is negative; we want the IC to be postive.
// XXX review this, is this really correct?  At any rate,
// feature selection utterly fails with negative IC.
// Also a problem, this is returning values greater than 1.0;
```
</details>


**228.** `components/learning/moses/moses/comboreduct/table/table.cc:403`

**Issue:** XXX TODO replace this by the util p_norm function.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO replace this by the util p_norm function.
contin_t OTable::abs_distance(const OTable& ot) const
{
```
</details>


**229.** `components/learning/moses/moses/comboreduct/table/table.cc:428`

**Issue:** XXX TODO replace this by the util p_norm function.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO replace this by the util p_norm function.
contin_t OTable::sum_squared_error(const OTable& ot) const
{
```
</details>


**230.** `components/learning/moses/moses/comboreduct/table/table.cc:661`

**Issue:** XXX this cannot possibly be correct, the total count is in general

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
row_it != end() and idx_it != idxs.end();) {
auto& outputs = row_it->second;
// XXX this cannot possibly be correct, the total count is in general
// a fraction, not an integer; it is merely the sum of the weights
// of the rows. It is NOT equal to the toal number of rows!
```
</details>


**231.** `components/learning/moses/moses/comboreduct/table/table.cc:842`

**Issue:** XXX TODO replace this by the util p_norm function.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// -------------------------------------------------------

// XXX TODO replace this by the util p_norm function.
complete_truth_table::size_type
complete_truth_table::hamming_distance(const complete_truth_table& other) const
```
</details>


**232.** `components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:100`

**Issue:** XXX TODO: I don't understand why this is not damaging contin_if  !??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// which are pairs. If we remove the condition, we must also remove
// the consequent.
// XXX TODO: I don't understand why this is not damaging contin_if  !??
// But .. umm, maybe build_knobs is not creating any kinds of contin_if's
// that can be damaged... well, no matter, because thes if's will be
```
</details>


**233.** `components/learning/moses/moses/comboreduct/main/eval-table.cc:147`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// HERE WE ARE ASSUMING THAT THE INPUT FILE HAS A HEADER!!!
// XXX FIXME
vector<string> header = get_header(pa.input_table_file);

```
</details>


### Language-Learning Component (9 items)


**1.** `language-learning/tests/test_grammar_learner.py:65`

**Issue:** 'template_path': 'poc-turtle',  FIXME: changed in June 2018 Grammar Tester

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 'test_corpus': module_path + '/data/POC-Turtle/poc-turtle-corpus.txt',
# 'reference_path': module_path + '/data/POC-Turtle/poc-turtle-parses-expected.txt',
# 'template_path': 'poc-turtle',  # FIXME: changed in June 2018 Grammar Tester
pass

```
</details>


**2.** `language-learning/tests/test_grammar_learner.py:309`

**Issue:** FIXME: check with further test_grammar updates and delete.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}
# Sometimes pqa_meter(with test_grammar updated 2018-10-19) returns pa,recall = 0,0
# FIXME: check with further test_grammar updates and delete.
x = 0.
n = 0
```
</details>


**3.** `language-learning/src/grammar_learner/preprocessing.py:141`

**Issue:** else:  FIXME: raise error / assert ?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
list2file(re['corpus_stats'], corpus_stats_file)
re.update({'corpus_stats_file': corpus_stats_file})
# else:  # FIXME: raise error / assert ?
#    return {'error': 'input_files'}, re

```
</details>


**4.** `language-learning/src/grammar_learner/category_learner.py:170`

**Issue:** 81231 cleanup after upstream merge and conflicts resolution (FIXME: 2nd check)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 81012 cdf2cats
# 81102 sparse wordspace agglomerative clustering
# 81231 cleanup after upstream merge and conflicts resolution (FIXME: 2nd check)
# 90221 tmpath defined in learn, tweaks removed here
# 90410 empty filtered parses dataset issue
```
</details>


**5.** `language-learning/src/grammar_learner/write_files.py:226`

**Issue:** 90128 restore Link Grammar 5.4.4 'UNKNOWN-WORD: XXX+;' option

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 90110 remove filename
# 90119 remove Link Grammar 5.4.4 options (v.0.6)
# 90128 restore Link Grammar 5.4.4 'UNKNOWN-WORD: XXX+;' option
# 190428 WSD ⇒ learner: optional, configurable
```
</details>


**6.** `language-learning/src/grammar_learner/widgets.py:142`

**Issue:** 80627 display_tree  FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 80521 html_table, plot_2d copied from utl.turtle.py
# 80521 save file
# 80627 display_tree  FIXME:DEL?
# 80817 corpus_histograms ⇒ ? FIXME:DEL?
# 81231 cleanup
```
</details>


**7.** `language-learning/src/grammar_learner/widgets.py:143`

**Issue:** 80817 corpus_histograms ⇒ ? FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 80521 save file
# 80627 display_tree  FIXME:DEL?
# 80817 corpus_histograms ⇒ ? FIXME:DEL?
# 81231 cleanup
```
</details>


**8.** `language-learning/src/grammar_learner/generalization.py:417`

**Issue:** 81217 FIXME? generalize_categories [F] with new reorder (Turtle tests)

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
# 80802 fix compatibility with dj_counts & max_disjuncts, delete ...05.py?
# 81121 generalise_rules
# 81217 FIXME? generalize_categories [F] with new reorder (Turtle tests)
# 81220 refactor, test
# 81231 cleanup
```
</details>


**9.** `language-learning/src/grammar_learner/skl_clustering.py:208`

**Issue:** FIXME: try...except

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 190118 cleanup: remove debug printing
# 190425 fix n_clusters > n_words case
# FIXME: try...except
```
</details>


### Moses Component (113 items)


**1.** `moses/examples/example-progs/trap-bit.cc:26`

**Issue:** XXX under construction XXX

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
using boost::lexical_cast;

// XXX under construction XXX

// XXX this example is broken, and will remain so until "multivariate"
```
</details>


**2.** `moses/examples/example-progs/trap-bit.cc:28`

**Issue:** XXX this example is broken, and will remain so until "multivariate"

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// XXX under construction XXX

// XXX this example is broken, and will remain so until "multivariate"
// is ported over/re-implemented. Basically, there is no structure
// learning at this time.
```
</details>


**3.** `moses/examples/example-progs/trap-bit.cc:31`

**Issue:** XXX some of the documentation below may be misleading.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// is ported over/re-implemented. Basically, there is no structure
// learning at this time.
// XXX some of the documentation below may be misleading.
//
// Demonstration program for the "bit-trap" optimization problem.
```
</details>


**4.** `moses/examples/example-progs/trap-bit.cc:42`

**Issue:** XXX which is why we need to put structure leanring back in the code XXX

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// the optimal solution cannot be found without structure learning; the
// MOSES univariate() learner is a no-op, and so cannot solve this problem.
// XXX which is why we need to put structure leanring back in the code XXX
//
// The correlation between variables is accomplished by using a
```
</details>


**5.** `moses/examples/example-progs/nmax.cc:38`

**Issue:** XXX setting n=2 currently fails due to a bug, see

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// This is a generalization of the "onemax" problem, which is nmax with n=2.
//
// XXX setting n=2 currently fails due to a bug, see
// https://bugs.launchpad.net/moses/+bug/908230
//
```
</details>


**6.** `moses/examples/example-progs/ontomax.cc:36`

**Issue:** XXX Someday, fix all of this!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// instead of a 2-ary tree for contin.
//
// XXX Someday, fix all of this!

void recbuild(term_tree& tr, term_tree::iterator it,
```
</details>


**7.** `moses/examples/example-progs/continmax.cc:46`

**Issue:** XXX Currently, this doesn't really work well, or maybe at all, in

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// is how the problem becomes easy when applying calculus).
//
// XXX Currently, this doesn't really work well, or maybe at all, in
// part because the contin implementation in the field set is incomplete
// or broken or maybe both; there is a confusion between depth and arity
```
</details>


**8.** `moses/moses/moses/optimization/star-anneal.cc:42`

**Issue:** XXX TODO the annealing temperature control code should be ported over

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/////////////////////////

// XXX TODO the annealing temperature control code should be ported over
// to the hill-climbing code, thus rendering the below obsolete.  The
// hill-climbing code is much more sophisticated in every way: correct
```
</details>


**9.** `moses/moses/moses/optimization/optimization.h:82`

**Issue:** XXX Why n^1.05 ??? This is going to have a significant effect

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// N = p.popsize_ratio * n^1.05
// XXX Why n^1.05 ??? This is going to have a significant effect
// (as compared to n^1.00) only when n is many thousands or bigger...
unsigned pop_size(const field_set& fs) const;
```
</details>


**10.** `moses/moses/moses/optimization/optimization.h:116`

**Issue:** problem size in info-theoretic bits.  (XXX Why 1.05 ???)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Populations are sized at N = popsize_ratio*n^1.05 where n is
// problem size in info-theoretic bits.  (XXX Why 1.05 ???)
double pop_size_ratio;

```
</details>


**11.** `moses/moses/moses/optimization/particle-swarm.h:45`

**Issue:** XXX PSO parameters hardcoded just for now.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Particle Swarm parameters
// XXX PSO parameters hardcoded just for now.
struct ps_parameters
{
```
</details>


**12.** `moses/moses/moses/optimization/particle-swarm.h:82`

**Issue:** XXX I have to find the best values for bit and disc.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// For continuous problems, 0.7 and 1.43 are good values.
// XXX I have to find the best values for bit and disc.
// Information from: M. Clerc, L’optimisation par essaim particulaire: versions
// paramétriques et adaptatives, Hermes Science Publications, Lavoisier, Paris, 2005.
```
</details>


**13.** `moses/moses/moses/optimization/particle-swarm.h:103`

**Issue:** Use rounding off for now XXX it isn't so effective, but for now is easier

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
//
// For discrete:
// Use rounding off for now XXX it isn't so effective, but for now is easier
// to implement. I'll use MVPSO, JPSO or IMVPSO later.
// In the transformation i'll use min value of 0 and 1 for max in the continuous
```
</details>


**14.** `moses/moses/moses/optimization/particle-swarm.h:133`

**Issue:** XXX If i have time, i'll put some variation here to get a better global search.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// with Artificial Intelligence IEEE-ICTAI, vol. 1, Arras, France, 27–29 October 2010,
// 2010, pp. 87–93.
// XXX If i have time, i'll put some variation here to get a better global search.
double bit_min_value, bit_max_value, // [0,1] <- XXX these two aren't used yet.
disc_min_value, disc_max_value, // [0,1] in rounding off
```
</details>


**15.** `moses/moses/moses/optimization/particle-swarm.h:253`

**Issue:** XXX Explanation

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX Explanation
bool new_bit_value(const double& vel){
return (randGen().randdouble() < // Sigmoid
```
</details>


**16.** `moses/moses/moses/optimization/particle-swarm.h:281`

**Issue:** XXX Explanation

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX Explanation
disc_t new_disc_value(double& cvalue,
const double& vel, const unsigned max_dvalue){
```
</details>


**17.** `moses/moses/moses/optimization/particle-swarm.h:303`

**Issue:** XXX Explanation

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX Explanation
contin_t new_cont_value(const contin_t& value, const double& vel){
// Wind dispersion enter here.
```
</details>


**18.** `moses/moses/moses/optimization/particle-swarm.h:340`

**Issue:** XXX This could help PSO if we maintain the best particle.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// In fact, all of the current code uses this entry point, no one
// bothers to supply an initial instance.
// XXX This could help PSO if we maintain the best particle.
void operator()(deme_t& deme,
const iscorer_base& iscorer,
```
</details>


**19.** `moses/moses/moses/optimization/hill-climbing.h:80`

**Issue:** scan is that much better than a random sampling.  XXX Or is it?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// nearest neighbor scan for such cases will eat up a huge amount of
// RAM in the instance_set, and its not currently obvious that a full
// scan is that much better than a random sampling.  XXX Or is it?
//
// XXX This parameter should probably be automatically adjusted with
```
</details>


**20.** `moses/moses/moses/optimization/hill-climbing.h:82`

**Issue:** XXX This parameter should probably be automatically adjusted with

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// scan is that much better than a random sampling.  XXX Or is it?
//
// XXX This parameter should probably be automatically adjusted with
// free RAM availability!?  Or something like that !?
//
```
</details>


**21.** `moses/moses/moses/optimization/hill-climbing.h:97`

**Issue:** XXX I don't understand what the below is saying.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// improved.
//
// XXX I don't understand what the below is saying.
// One should probably try first to tweak pop_size_ratio to
// control the allocation of resources. However in some cases (for
```
</details>


**22.** `moses/moses/moses/optimization/hill-climbing.h:103`

**Issue:** XXX pop_size_ratio disabled in hill-climbing, since its definition

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// there is only one deme to explore and tweaking that parameter
// can make a difference (breadth vs depth)
// XXX pop_size_ratio disabled in hill-climbing, since its definition
// was insane/non-sensical.  I can't figure out how it was supposed
// to work.
```
</details>


**23.** `moses/moses/moses/optimization/hill-climbing.h:110`

**Issue:** XXX TODO make sure this value is appropriately updated.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Range of scores for which to keep instances.  This *should* be
// set to the value given by metapopulation::useful_score_range().
// XXX TODO make sure this value is appropriately updated.
//
// The range of scores is used to keep the size of the deme in check.
```
</details>


**24.** `moses/moses/moses/optimization/hill-climbing.h:281`

**Issue:** XXX In fact, all of the current code uses this entry point, no one

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Like above but assumes that init_inst is null (backward compatibility)
// XXX In fact, all of the current code uses this entry point, no one
// bothers to supply an initial instance.
void operator()(deme_t& deme,
```
</details>


**25.** `moses/moses/moses/optimization/hill-climbing.cc:732`

**Issue:** /the vector.  But this ixsn't know ... XXX experiment with this!?

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// regions of low scores, in one go.  It is possible that a faster
/// algorithm would be to sort first, and then delete the tail-end of
/// the vector.  But this ixsn't know ... XXX experiment with this!?
/// ... err, but right now, trimming takes a small fraction of a second,
/// so there is no rush to fis this.
```
</details>


**26.** `moses/moses/moses/optimization/optimization.cc:79`

**Issue:** XXX Why n^1.05 ??? This is going to have a significant effect

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// N = p.popsize_ratio * n^1.05
// XXX Why n^1.05 ??? This is going to have a significant effect
// (as compared to n^1.00) only when n is many thousands or bigger...
unsigned optim_parameters::pop_size(const field_set& fs) const
```
</details>


**27.** `moses/moses/moses/optimization/particle-swarm.cc:122`

**Issue:** XXX What score do i use?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
current_number_of_evals += swarm_size;

// XXX What score do i use?
// I'll use best_score for now.
bool has_improved = false;
```
</details>


**28.** `moses/moses/moses/metapopulation/ensemble.h:55`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* from the metapopulation, which has no particular admission criteria.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**29.** `moses/moses/moses/metapopulation/metapopulation.h:90`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* for inference, the metapopulation for breeding.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**30.** `moses/moses/moses/metapopulation/merging.cc:261`

**Issue:** XXX FIXME: we should use a pointer set for scored_combo_tree_set

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
candidates.size());

// XXX FIXME: we should use a pointer set for scored_combo_tree_set
// This would avoid some pointless copying here and a few other
// places.  This is easier said than done, because the stupid
```
</details>


**31.** `moses/moses/moses/metapopulation/merging.cc:554`

**Issue:** XXX FIXME looks to me like it++ can often be collaed twice within this loop!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME looks to me like it++ can often be collaed twice within this loop!
prev_it = it++;
}
```
</details>


**32.** `moses/moses/moses/metapopulation/metapopulation.cc:222`

**Issue:** XXX FIXME should probably not recompute every time ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _best_cscore;

// XXX FIXME should probably not recompute every time ...
// need to figure who is calling this method, and what they are expecting.
return _cscorer.get_cscore(_ensemble.get_ensemble());
```
</details>


**33.** `moses/moses/moses/metapopulation/ensemble.cc:235`

**Issue:** if we are here, its the in-exact experts code.  XXX currently broken ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

} else {
// if we are here, its the in-exact experts code.  XXX currently broken ...
// Any score worse than half is terrible. Half gives a weight of zero.
// More than half gives negative weights, which wreaks things.
```
</details>


**34.** `moses/moses/moses/metapopulation/ensemble.cc:259`

**Issue:** XXX the logic below is probably wrong.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
size_t bslen = _bscorer.size();

// XXX the logic below is probably wrong.
OC_ASSERT(false, "this doesn't work right now.");
// Now, look to see where this scorer was wrong, and bump the
```
</details>


**35.** `moses/moses/moses/scoring/scoring_base.h:124`

**Issue:** XXX TODO should be a std::valarray not a vector.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// A vector of per-bscore weights, used to tote up the behavioral
/// score into a single number.
// XXX TODO should be a std::valarray not a vector.
virtual void update_weights(const std::vector<double>&);

```
</details>


**36.** `moses/moses/moses/scoring/discriminating_bscore.cc:213`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
_full_bscore(true)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 2;

```
</details>


**37.** `moses/moses/moses/scoring/discriminating_bscore.cc:394`

**Issue:** umm XXX I think the normalization here should be the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// because the precision is normalized as well.  So e.g.
// max precision for boolean problems is 1.0.  However...
// umm XXX I think the normalization here should be the
// best-possible activation, not the usize, right?
//
```
</details>


**38.** `moses/moses/moses/scoring/discriminating_bscore.cc:417`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, min_precision, max_precision, hardness)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 2;
}
```
</details>


**39.** `moses/moses/moses/scoring/discriminating_bscore.cc:486`

**Issue:** XXX TODO -- should not return the penalties as part of the bscore,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, min_recall, max_recall, hardness)
{
// XXX TODO -- should not return the penalties as part of the bscore,
// since this messes up boosting.
_size = ct.size() + 2;
```
</details>


**40.** `moses/moses/moses/scoring/discriminating_bscore.cc:586`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, min_diff, max_diff, hardness)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 2;
}
```
</details>


**41.** `moses/moses/moses/scoring/discriminating_bscore.cc:620`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t bep_bscore::get_variable(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / (cnt * _true_total);
double best_possible_recall = 1.0 / _true_total;
```
</details>


**42.** `moses/moses/moses/scoring/discriminating_bscore.cc:629`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t bep_bscore::get_fixed(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / (cnt);
double best_possible_recall = (0.0 < pos) ? 1.0 : 0.0;
```
</details>


**43.** `moses/moses/moses/scoring/discriminating_bscore.cc:648`

**Issue:** XXX Currently, this scorer does not return a true behavioral score

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
: discriminating_bscore(ct, 0.0, 1.0, 1.0e-20)
{
// XXX Currently, this scorer does not return a true behavioral score
_size = 1;
}
```
</details>


**44.** `moses/moses/moses/scoring/discriminating_bscore.cc:681`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t f_one_bscore::get_fixed(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
return 1.0;
}
```
</details>


**45.** `moses/moses/moses/scoring/discriminating_bscore.cc:688`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t f_one_bscore::get_variable(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / cnt;
double best_possible_recall = 1.0;
```
</details>


**46.** `moses/moses/moses/moses/mpi_moses.h:51`

**Issue:** this just right now. XXX TODO: do this, someday.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// have less dispatcher code to maintain.  But this will require some
// fair amount of work and testing, and there's no pressing need to do
// this just right now. XXX TODO: do this, someday.
//
class moses_mpi_comm
```
</details>


**47.** `moses/moses/moses/moses/local_moses.cc:56`

**Issue:** XXX When would one never expand?  Wouldn't that be a bug?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Attempt to create a non-empty representation, by looping
// over exemplars until we find one that expands.
// XXX When would one never expand?  Wouldn't that be a bug?
while (1) {
scored_combo_tree_ptr_set_cit exemplar = mp.select_exemplar();
```
</details>


**48.** `moses/moses/moses/moses/partial.cc:264`

**Issue:** XXX is this correct? I think we need to ask the cscorer for the total ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
behavioral_score bs = _bscore->operator()(cand);

// XXX is this correct? I think we need to ask the cscorer for the total ...
score_t total_score = 0.0;
for (const score_t& sc : bs)
```
</details>


**49.** `moses/moses/moses/moses/partial.cc:304`

**Issue:** XXX Ineffective predicates may be due to enums that have been

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
effective(predicate, good_count, fail_count);

// XXX Ineffective predicates may be due to enums that have been
// completely accounted for ... not sure what to do about that...
if ((0 < good_count) || (0 < fail_count))
```
</details>


**50.** `moses/moses/moses/moses/neighborhood_sampling.h:284`

**Issue:** @todo: handle term algebras XXX

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
(starting_index < fs.end_term_raw_idx()))
{
// @todo: handle term algebras XXX
out = vary_n_knobs(fs, tmp_inst, dist,
starting_index + fs.end_term_raw_idx(),
```
</details>


**51.** `moses/moses/moses/moses/neighborhood_sampling.h:327`

**Issue:** XXX TODO, unroll the last tail call, just like the single-bit

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
{
// Recursive call, moved for one position
// XXX TODO, unroll the last tail call, just like the single-bit
// knob case, below.
out = vary_n_knobs(fs, tmp_inst, dist, starting_index + 1, out, end);
```
</details>


**52.** `moses/moses/moses/moses/moses_main.h:102`

**Issue:** XXX TODO this should be fixed, someday...

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// a printer at all, or use a null_printer.  Unfortunately,
// the current code structure makes this hard to implement.
// XXX TODO this should be fixed, someday...
if (is_mpi && metapop.size() == 0)
return;
```
</details>


**53.** `moses/moses/moses/moses/distributed_moses.cc:194`

**Issue:** Arghhh FIXME. fuser might not be installed, or it may not be in

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
bool is_being_written(const string& file_name, int pid)
{
// Arghhh FIXME. fuser might not be installed, or it may not be in
// the default search path.  RedHat/CentOS puts it into /sbin/fuser
// which is not in the default searchpath.
```
</details>


**54.** `moses/moses/moses/moses/mpi_moses.cc:201`

**Issue:** XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
///
/// This sends a pretty big glob.
// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.
void moses_mpi_comm::send_deme(const metapopulation& mp, int n_evals)
{
```
</details>


**55.** `moses/moses/moses/moses/mpi_moses.cc:307`

**Issue:** XXX replace this with appropriate message back to root!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
right expansion
count */)) {
// XXX replace this with appropriate message back to root!
OC_ASSERT(false, "Exemplar failed to expand!\n");
}
```
</details>


**56.** `moses/moses/moses/moses/mpi_moses.cc:311`

**Issue:** XXX TODO should probably fetch max_time from somewhere...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO should probably fetch max_time from somewhere...
time_t max_time = INT_MAX;
dex.optimize_demes(max_evals, max_time);
```
</details>


**57.** `moses/moses/moses/moses/mpi_moses.cc:608`

**Issue:** XXX TODO instead of overwritting the demeID it should be

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
stats.n_expansions ++;

// XXX TODO instead of overwritting the demeID it should be
// correctly defined by the worker and send back to the
// dispatcher. That way we can have the breadth_first
```
</details>


**58.** `moses/moses/moses/moses/types.h:189`

**Issue:** XXX wouldn't it be better to store ints here ??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// comes from demeID "0".
//
// XXX wouldn't it be better to store ints here ??
struct demeID_t : public std::string
{
```
</details>


**59.** `moses/moses/moses/deme/deme_expander.cc:356`

**Issue:** * XXX TODO I honestly just don't see the utility of this multi-deme

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* set, and thus, a different deme.
*
* XXX TODO I honestly just don't see the utility of this multi-deme
* creation mechanism.  Feature selection is a very crude mechanism,
* and the representation is just randomly peppered with knobs. By
```
</details>


**60.** `moses/moses/moses/deme/deme_expander.cc:502`

**Issue:** XXX FIXME this is a bug .. the user may have specified that

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// reset scorer to use all variables (important so that
// behavioral score is consistent across generations
// XXX FIXME this is a bug .. the user may have specified that
// certain incdexes should be ignored, and this just wipes
// those out...
```
</details>


**61.** `moses/moses/moses/eda/local_structure.h:262`

**Issue:** XXX ??? Huh? More details, please...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Now that we have created all of the dtrees, construct a
// feasible order that respects the initial dependencies
// XXX ??? Huh? More details, please...
randomized_topological_sort(_initial_deps, _ordering.begin());
}
```
</details>


**62.** `moses/moses/moses/eda/local_structure.h:271`

**Issue:** themselves shouldn't change (XXX why would we want to do this ??)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//
// instance_set is not const so that we can reorder it - the instances
// themselves shouldn't change (XXX why would we want to do this ??)
//
// For univariate(), the dtree for each field will be empty.
```
</details>


**63.** `moses/moses/moses/representation/representation.cc:51`

**Issue:** XXX TODO: One might think that varying the stepsize, i.e. shrinking

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// distribution of the contin variables.
//
// XXX TODO: One might think that varying the stepsize, i.e. shrinking
// it, as the optimizers tune into a specific value, would be a good
// thing (so that the optimizer could tune to a more precise value).
```
</details>


**64.** `moses/moses/moses/representation/representation.cc:236`

**Issue:** XXX TODO need to add support for "term algebra" knobs

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void representation::transform(const instance& inst)
{
// XXX TODO need to add support for "term algebra" knobs

contin_map_it ckb = contin.begin();
```
</details>


**65.** `moses/moses/moses/representation/representation.cc:391`

**Issue:** XXX This is dead code, no one uses it, and looking at the below, it

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

#ifdef EXEMPLAR_INST_IS_UNDEAD
// XXX This is dead code, no one uses it, and looking at the below, it
// looks inconsistent to me. I'm going to leave it here for a while, but
// it should be removed by 2013 or 2014 if not sooner...
```
</details>


**66.** `moses/moses/moses/representation/representation.cc:395`

**Issue:** XXX why are we clearing this, instead of setting it back to the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// it should be removed by 2013 or 2014 if not sooner...

// XXX why are we clearing this, instead of setting it back to the
// _exemplar_inst ??? XXX is this broken??
//
```
</details>


**67.** `moses/moses/moses/representation/representation.cc:396`

**Issue:** _exemplar_inst ??? XXX is this broken??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// XXX why are we clearing this, instead of setting it back to the
// _exemplar_inst ??? XXX is this broken??
//
// XXX Note that the clear_exemplar() methods on the knobs are probably
```
</details>


**68.** `moses/moses/moses/representation/representation.cc:398`

**Issue:** XXX Note that the clear_exemplar() methods on the knobs are probably

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// _exemplar_inst ??? XXX is this broken??
//
// XXX Note that the clear_exemplar() methods on the knobs are probably
//  not needed either!?
void representation::clear_exemplar()
```
</details>


**69.** `moses/moses/moses/representation/representation.cc:409`

**Issue:** XXX that, and contin seems to be handled inconsistently with disc...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// What is this doing ? seems to be clearing things out, why do we need this?
// XXX that, and contin seems to be handled inconsistently with disc...
// I mean, shouldn't we be setting the exemplar_inst fields so that
// they match the exmplar?  Do we even need the exemplar_inst for anything?
```
</details>


**70.** `moses/moses/moses/representation/field_set.h:213`

**Issue:** XXX should be enum ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX should be enum ...
static const disc_t Stop;  // 0
static const disc_t Left;  // 1
```
</details>


**71.** `moses/moses/moses/representation/instance_scorer.h:89`

**Issue:** XXX FIXME, calling score_tree above does not throw the exceptionthis should be done

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _cscorer.get_cscore(tr);
} catch (...) {
// XXX FIXME, calling score_tree above does not throw the exception; this should be done
// differntly, maybe call bscorer directly, then ascorer...
// ??? Huh? why couldn't we evaluate a tree anyway?  why would we want an exception here?
```
</details>


**72.** `moses/moses/moses/representation/build_knobs.cc:115`

**Issue:** Petbrain  XXX does this call code that actually works??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}
else if (output_type == id::action_result_type) {
// Petbrain  XXX does this call code that actually works??
action_canonize(_exemplar.begin());
build_action(_exemplar.begin());
```
</details>


**73.** `moses/moses/moses/representation/build_knobs.cc:121`

**Issue:** ANN  XXX This is calling unfinished, broken code, below.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}
else if (output_type == id::ann_type) {
// ANN  XXX This is calling unfinished, broken code, below.
ann_canonize(_exemplar.begin());
build_contin(_exemplar.begin());
```
</details>


**74.** `moses/moses/moses/representation/build_knobs.cc:211`

**Issue:** * XXX TODO: see comments on disc_probe() below.  This method is a real

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
* the exemplar is split into two at each recursion.
*
* XXX TODO: see comments on disc_probe() below.  This method is a real
* CPU time-waster; it is not clear that the computational cost is ever
* repaid as a performance advantage later on, during the instance search.
```
</details>


**75.** `moses/moses/moses/representation/build_knobs.cc:690`

**Issue:** XXX TODO: Is this really optimal?  The below adds an entire copy

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO: Is this really optimal?  The below adds an entire copy
// of the tree at it, which clearly increases the overall complexity.
// But is this really a wise thig to do? It seems gratuitous, and it's
```
</details>


**76.** `moses/moses/moses/representation/build_knobs.cc:917`

**Issue:** This may not be desirable...!?  XXX some experimental validation

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
#ifdef LATER
// The canonization used here raises the degree of polynomials.
// This may not be desirable...!?  XXX some experimental validation
// that this speeds convergence is needed.
for (sib_it sib = it.begin(); sib != it.end(); ++sib) {
```
</details>


**77.** `moses/moses/moses/representation/build_knobs.cc:1283`

**Issue:** XXX TODO this below is clearly unfinished, broken, etc.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO this below is clearly unfinished, broken, etc.
// and can't possibly work ...
void build_knobs::ann_canonize(pre_it it)
```
</details>


**78.** `moses/moses/moses/representation/build_knobs.cc:1342`

**Issue:** //FIXME: now just attaches to the first output

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

//now attach the subtree to the hidden nodes
//FIXME: now just attaches to the first output
sib_it first_hidden = it.begin();

```
</details>


**79.** `moses/moses/moses/representation/knobs.h:200`

**Issue:** XXX Huh?? what does this do?? Why does shifting matter,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
int _current;   // The current knob setting.

// XXX Huh?? what does this do?? Why does shifting matter,
// if the only thing done is to count the number of bits set ??
// WTF ??  I think the shift is n the  wrong direction, yeah?
```
</details>


**80.** `moses/moses/moses/representation/knobs.h:204`

**Issue:** the shift is definitely in the wrong direction!! FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// WTF ??  I think the shift is n the  wrong direction, yeah?
// If the goal is to skip over index values that are disallowed, then
// the shift is definitely in the wrong direction!! FIXME.
int map_idx(int idx) const
{
```
</details>


**81.** `moses/moses/moses/representation/knobs.h:218`

**Issue:** XXX This uses reduct::logical_reduction rulesit is not clear if those

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// tree underneath it.
//
// XXX This uses reduct::logical_reduction rules; it is not clear if those
// rules tolerate predicates.
//
```
</details>


**82.** `moses/moses/moses/representation/knobs.h:221`

**Issue:** XXX what is the difference between "present" and "absent" ??? A

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// rules tolerate predicates.
//
// XXX what is the difference between "present" and "absent" ??? A
// knob that is "absent" from a logical "or" is the same as "present
// and false".  while one that is absent from a logical "and" is the
```
</details>


**83.** `moses/moses/moses/representation/knobs.h:230`

**Issue:** XXX Also -- I think I want to rename this to "logical unary knob",

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// justification.
//
// XXX Also -- I think I want to rename this to "logical unary knob",
// or something like that, as it is a unary logical function ... err...
// well, I guess all combo opers are unary, due to Currying.
```
</details>


**84.** `moses/moses/moses/main/problem-params.h:46`

**Issue:** XXX FIXME TODO The structure below should be split into multiple

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
namespace opencog { namespace moses {

// XXX FIXME TODO The structure below should be split into multiple
// parts, with each sub-part responsible for picking out the argv's
// that it cares about. Unfortunately, this requires getting rid of
```
</details>


**85.** `moses/moses/moses/main/table-problems.cc:138`

**Issue:** XXX FIXME -- the multiple tables should be merged into one.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
logger().info("Number of rows in tables = %d", num_rows);

// XXX FIXME -- the multiple tables should be merged into one.
ctable = _ctables.front();
table = _tables.front();
```
</details>


**86.** `moses/moses/moses/main/table-problems.cc:150`

**Issue:** XXX FIXME .. check that they all have the same signature.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Check that all input data files have the same arity
// XXX FIXME .. check that they all have the same signature.
if (_tables.size() > 1) {
for (size_t i = 1; i < _tables.size(); ++i) {
```
</details>


**87.** `moses/moses/moses/main/table-problems.cc:487`

**Issue:** XXX Eh ??? for precision/recall scorers,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// For enum targets, like boolean targets, the score
// can never exceed zero (perfect score).
// XXX Eh ??? for precision/recall scorers,
// the score range is 0.0 to 1.0 so this is wrong...
if (0.0 < pms.moses_params.max_score) {
```
</details>


**88.** `moses/moses/moses/main/problem-params.cc:166`

**Issue:** XXX TODO: make this print correctly, instead of using brackets.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Declare the supported options.
// XXX TODO: make this print correctly, instead of using brackets.
desc.add_options()

```
</details>


**89.** `moses/moses/feature-selection/algo/simple.h:150`

**Issue:** XXX or use partial_sort, as mentioned above...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
} else {
// stair-step distribution: keep the top num_desired only.
//  XXX or use partial_sort, as mentioned above...
for (auto pr = sorted_flist.begin(); pr != sorted_flist.end(); pr++) {
final.insert(*pr->second.begin());
```
</details>


**90.** `moses/moses/comboreduct/type_checker/type_tree.cc:614`

**Issue:** XXX TODO the code below was modified to allow arg lists of

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// and a4 inherit from T2.  T3 is the output type.

// XXX TODO the code below was modified to allow arg lists of
// mixed type, e.g. so that the cond primitive could be
// supported (as the current definition of cond alternates
```
</details>


**91.** `moses/moses/comboreduct/interpreter/interpreter.cc:336`

**Issue:** XXX TODO: contin_if should go away.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO: contin_if should go away.
case id::contin_if :
case id::cond : {
```
</details>


**92.** `moses/moses/comboreduct/interpreter/eval.cc:313`

**Issue:** @todo FIXME there should be a general way to distinguish between

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const vertex& v = *it;

// @todo FIXME there should be a general way to distinguish between
// "f" (the function f, being passed to fold) vs "f" (the function f
// being called with no arguments). If you don't handle that you get
```
</details>


**93.** `moses/moses/comboreduct/interpreter/eval.cc:563`

**Issue:** XXX TODO: contin_if should go away.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO: contin_if should go away.
case id::contin_if :
case id::cond : {
```
</details>


**94.** `moses/moses/comboreduct/interpreter/eval.cc:606`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
try {
vertex retv(eval_throws_vertex(bmap, it));
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**95.** `moses/moses/comboreduct/interpreter/eval.cc:612`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
catch (EvalException& e) {
vertex retv = e.get_vertex();
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**96.** `moses/moses/comboreduct/combo/descriptions.cc:85`

**Issue:** XXX Should probably be "union", yeah?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//
// The 'value' is marked unknown, as it can be of any type.
// XXX Should probably be "union", yeah?
{ id::cond,               "->(arg_list(boolean unknown) unknown unknown)" },

```
</details>


**97.** `moses/moses/comboreduct/combo/descriptions.cc:91`

**Issue:** Tests for equality. XXX Someday, we should also allow equality

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
{ id::contin_if,            "->(boolean contin contin contin)" },

// Tests for equality. XXX Someday, we should also allow equality
// testing for booleans, contin as well, but not today.
// Fixing this will require work in typechecker, and use of union
```
</details>


**98.** `moses/moses/comboreduct/combo/descriptions.cc:94`

**Issue:** type. XXX FIXME...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// testing for booleans, contin as well, but not today.
// Fixing this will require work in typechecker, and use of union
// type. XXX FIXME...
{ id::equ,                  "->(enum enum boolean)" },
};
```
</details>


**99.** `moses/moses/comboreduct/combo/vertex.h:94`

**Issue:** be generalized soon.  XXX do this...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Generic functions
// Currently take enum as arg or return enum, but should
// be generalized soon.  XXX do this...
cond,
equ,
```
</details>


**100.** `moses/moses/comboreduct/combo/vertex.h:101`

**Issue:** XXX This should be obsoleted by cond, at some point.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
apply,

// XXX This should be obsoleted by cond, at some point.
// Maybe action_boolean_if too, I guess?
contin_if,
```
</details>


**101.** `moses/moses/comboreduct/combo/vertex.h:418`

**Issue:** inequality XXX Huh? but its not in namespace boost !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// don't know why this is needed *in namespace boost*, but it is, for
// e.g. calling a generic stl function that compares vertices for
// inequality XXX Huh? but its not in namespace boost !?
inline bool operator!=(const vertex& v1, const vertex& v2)
{
```
</details>


**102.** `moses/moses/comboreduct/combo/iostream_combo.cc:440`

**Issue:** XXX ?? Ahem, won't calling out<<(*m) just lead to infinite

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return out << m->getContent();

// XXX ?? Ahem, won't calling out<<(*m) just lead to infinite
// recursion ??
if (const ann_type* z = get<ann_type>(&v))
```
</details>


**103.** `moses/moses/comboreduct/table/table.h:634`

**Issue:** XXX TODO WARNING ERROR: builtin hardcoded shit!!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
for (unsigned i = 0; i < seq.size(); ++i) {
if (it != filter.cend() && (typename F::value_type)i == *it) {
// XXX TODO WARNING ERROR: builtin hardcoded shit!!!
res.push_back(seq.get_at<builtin>(i));
++it;
```
</details>


**104.** `moses/moses/comboreduct/table/table.h:638`

**Issue:** XXX TODO WARNING ERROR: builtin hardcoded shit!!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
++it;
} else {
// XXX TODO WARNING ERROR: builtin hardcoded shit!!!
res.push_back(id::null_vertex);
}
```
</details>


**105.** `moses/moses/comboreduct/table/table.h:1069`

**Issue:** * XXX TODO -- this also should probably support the weight column,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* to add enum support, cut-n-paste from CTable code below.
*
* XXX TODO -- this also should probably support the weight column,
* since not all rows are important, and the ones that are not
* important should not contribute to the MI.
```
</details>


**106.** `moses/moses/comboreduct/table/table.h:1274`

**Issue:** XXX TODO, it would be easier if KLD took a sorted list

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO, it would be easier if KLD took a sorted list
// as the argument.
std::vector<contin_t> p, q;
```
</details>


**107.** `moses/moses/comboreduct/table/table.h:1283`

**Issue:** XXX review this, is this really correct?  At any rate,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// KLD is negative; we want the IC to be postive.
// XXX review this, is this really correct?  At any rate,
// feature selection utterly fails with negative IC.
// Also a problem, this is returning values greater than 1.0;
```
</details>


**108.** `moses/moses/comboreduct/table/table.cc:403`

**Issue:** XXX TODO replace this by the util p_norm function.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO replace this by the util p_norm function.
contin_t OTable::abs_distance(const OTable& ot) const
{
```
</details>


**109.** `moses/moses/comboreduct/table/table.cc:428`

**Issue:** XXX TODO replace this by the util p_norm function.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX TODO replace this by the util p_norm function.
contin_t OTable::sum_squared_error(const OTable& ot) const
{
```
</details>


**110.** `moses/moses/comboreduct/table/table.cc:661`

**Issue:** XXX this cannot possibly be correct, the total count is in general

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
row_it != end() and idx_it != idxs.end();) {
auto& outputs = row_it->second;
// XXX this cannot possibly be correct, the total count is in general
// a fraction, not an integer; it is merely the sum of the weights
// of the rows. It is NOT equal to the toal number of rows!
```
</details>


**111.** `moses/moses/comboreduct/table/table.cc:842`

**Issue:** XXX TODO replace this by the util p_norm function.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// -------------------------------------------------------

// XXX TODO replace this by the util p_norm function.
complete_truth_table::size_type
complete_truth_table::hamming_distance(const complete_truth_table& other) const
```
</details>


**112.** `moses/moses/comboreduct/reduct/logical_rules.cc:100`

**Issue:** XXX TODO: I don't understand why this is not damaging contin_if  !??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// which are pairs. If we remove the condition, we must also remove
// the consequent.
// XXX TODO: I don't understand why this is not damaging contin_if  !??
// But .. umm, maybe build_knobs is not creating any kinds of contin_if's
// that can be damaged... well, no matter, because thes if's will be
```
</details>


**113.** `moses/moses/comboreduct/main/eval-table.cc:147`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// HERE WE ARE ASSUMING THAT THE INPUT FILE HAS A HEADER!!!
// XXX FIXME
vector<string> header = get_header(pa.input_table_file);

```
</details>


### Unify Component (1 items)


**1.** `unify/opencog/unify/atoms/UnifierLink.cc:141`

**Issue:** XXX FIXME, Maybe. This seems to handle all of the cases I've

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// This is my best guess.

// XXX FIXME, Maybe. This seems to handle all of the cases I've
// looked at so far. However, the unifier has all sorts of fancy
// reduction code, and I don't understand what it is or why it
```
</details>



## ✅ Easy Priority (39 items)

### Analyze_Fixme_Instances.Py Component (2 items)


**1.** `analyze_fixme_instances.py:103`

**Issue:** Skip meta-comments about FIXME processing

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
return False

# Skip meta-comments about FIXME processing
if ('fixme instances' in line_lower) or \
('fixme text' in line_lower) or \
```
</details>


**2.** `analyze_fixme_instances.py:131`

**Issue:** Look for actual FIXME patterns in comments

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
return False

# Look for actual FIXME patterns in comments
return (('fixme' in line_lower) or
('xxx' in line_lower and (line_lower.strip().startswith('#') or line_lower.strip().startswith('//'))) or  # XXX comments
```
</details>


### Atomspace Component (8 items)


**1.** `atomspace/opencog/scm/opencog.scm:122`

**Issue:** FIXME: Both of the above-described problems might no longer exist.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
; grabbing the existing atomspace, if there already is one.
;
; FIXME: Both of the above-described problems might no longer exist.
; I'm not sure. The below is simple and painless, I'm leaving it for
; now.
```
</details>


**2.** `atomspace/opencog/query/NextSearchMixin.cc:267`

**Issue:** this simpler code is good enough. XXX FIXME someday?

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// incoming set. Ideally, we should look only at the incoming
// set that matches the type of the parent term. But for now,
// this simpler code is good enough. XXX FIXME someday?
std::multimap<std::size_t, Handle> thick_vars;

```
</details>


**3.** `atomspace/opencog/query/Satisfier.cc:41`

**Issue:** XXX Temp hack alert. When Continuations finally terminate, they

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
_result = true;

// XXX Temp hack alert. When Continuations finally terminate, they
// supply us with empty groundings. This probably needs to be fixed
// someday. For now, for the simple examples, its good enough.
```
</details>


**4.** `atomspace/opencog/atomspace/AtomTable.cc:623`

**Issue:** XXX Also, a minor bug, not sure if it matters: if parent is set

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// frames.
//
// XXX Also, a minor bug, not sure if it matters: if parent is set
// to true, then any UniqueLinks appearing here and in the parent
// will be duplicated repeatedly in the result. Might be nice to
```
</details>


**5.** `atomspace/opencog/scm/opencog/base/atom-docs.scm:41`

**Issue:** XXX FIXME replace below by real docs.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

; ---------------------------------------------------------------
; XXX FIXME replace below by real docs.
(set-procedure-property! BindLink 'documentation
(procedure-property QueryLink 'documentation))
```
</details>


**6.** `atomspace/opencog/atoms/flow/CollectionOfLink.cc:69`

**Issue:** FIXME: _outoging[0] could be executable, in which case

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
_have_typespec = true;

// FIXME: _outoging[0] could be executable, in which case
// is should be executed, first. But I'm lazy. Also:
// instead of being a simple type, the output could be
```
</details>


**7.** `atomspace/opencog/atoms/truthvalue/SimpleTruthValue.h:77`

**Issue:** XXX FIXME Are all of these really needed?

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
const MergeCtrl& mc=MergeCtrl()) const;

// XXX FIXME Are all of these really needed?
// Can we get rid of some of them?
static SimpleTruthValuePtr createSTV(strength_t mean, confidence_t conf)
```
</details>


**8.** `atomspace/opencog/atoms/reduct/BoolOpLink.cc:44`

**Issue:** XXX TODO we can relax this, and accept simple truth values, too.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
if (1 == sz and BOOL_NOT_LINK != get_type()) return vp;

// XXX TODO we can relax this, and accept simple truth values, too.
if (not nameserver().isA(vp->get_type(), BOOL_VALUE))
throw InvalidParamException(TRACE_INFO, "Expecting a BoolBalue");
```
</details>


### Components Component (15 items)


**1.** `components/language/learn/scm/attic/cluster/cset-class.scm:148`

**Issue:** XXX FIXME this might be pointless and useless?

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
; The goal is to trim the list of sections to something smaller.
;
; XXX FIXME this might be pointless and useless?
(define (get-all-sections-in-classes WCL)

```
</details>


**2.** `components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:134`

**Issue:** XXX TODO (1) this could be converted into a simple GetLink

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
TruthQuerySpeechAct, etc...
"
; XXX TODO (1) this could be converted into a simple GetLink
; and probably should be. (2) There should be a syntax for GetLink
; that is lest verbose, and closer in style to what is written
```
</details>


**3.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:505`

**Issue:** XXX FIXME (and in MonoSpace, too). According to the BackingStore

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

/// Backend callback
// XXX FIXME (and in MonoSpace, too). According to the BackingStore
// docs, it says if the Value is absent, i.e. not in storage, that
// means it has been deleted, and that it should be deleted in the
```
</details>


**4.** `components/learning/moses/examples/example-progs/continmax.cc:67`

**Issue:** XXX todo -- finish documentation to make it look more like the

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
//    distinction between continuous variables what will be drawn.
//
// XXX todo -- finish documentation to make it look more like the
// onemax/nmax example programs.

```
</details>


**5.** `components/learning/moses/moses/moses/metapopulation/merging.cc:404`

**Issue:** XXX TODO fix the cap so its more sensitive to the size of

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// value of _params.cap_coef=50 seems to work well.
//
// XXX TODO fix the cap so its more sensitive to the size of
// each exemplar, right!? So if the exemplars are huges, then the
// population size has to be smaller.  ... On the other hand, if
```
</details>


**6.** `components/learning/moses/moses/moses/scoring/precision_bscore.cc:111`

**Issue:** /set to false.  TBD XXX document that someday.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
/// The above only descirbes the "precision_full_bscore" (default)
/// scoring; something else is done when precision_full_bscore is
/// set to false.  TBD XXX document that someday.


```
</details>


**7.** `components/learning/moses/moses/moses/scoring/select_bscore.cc:314`

**Issue:** XXX This is not quite right, for weighted rows.  A row with a small

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
}

// XXX This is not quite right, for weighted rows.  A row with a small
// weight could result in a much smaller min-improv.
// (But I think boosting should not affect min-improv, right?)
```
</details>


**8.** `components/learning/moses/moses/moses/moses/partial.cc:96`

**Issue:** XXX TODO: we need to get the actual number of gens run, back

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
_moses_params.max_evals -= _num_evals;

// XXX TODO: we need to get the actual number of gens run, back
// from moses, and subtract it here.  But there's no easy way
// to get this number ...
```
</details>


**9.** `components/learning/moses/moses/moses/moses/mpi_moses.cc:482`

**Issue:** XXX should print stats less often...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
});

// XXX should print stats less often...
// Print stats in a way that makes them easy to graph.
// (columns of tab-seprated numbers)
```
</details>


**10.** `components/learning/moses/moses/moses/moses/mpi_moses.cc:641`

**Issue:** XXX this is kind-of buggy, since the following data is not

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// Print stats in a way that makes them easy to graph.
// (columns of tab-seprated numbers)
// XXX this is kind-of buggy, since the following data is not
// updated and collected atomically... other threads may be
// merging and updating as this print happens. Yuck. Oh well.
```
</details>


**11.** `components/learning/moses/moses/moses/deme/feature_selector.cc:117`

**Issue:** /XXX TODO Explain what this function does. Why does it create a second

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
}

/// XXX TODO Explain what this function does. Why does it create a second
/// table that is different than _ctable? How are these two different
/// tables used?  Why do we need two different tables?  WTF.  This is
```
</details>


**12.** `components/learning/moses/moses/moses/eda/local_structure.h:145`

**Issue:** XXX TODO document what this does...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
};*/

// XXX TODO document what this does...
struct local_structure_probs_learning
{
```
</details>


**13.** `components/learning/moses/moses/moses/eda/local_structure.h:279`

**Issue:** XXX TODO this is unclear, explain what is being accumulated where.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// iterate over the dtrees, and accumulate statistics.
//
// XXX TODO this is unclear, explain what is being accumulated where.
template<typename It>
void local_structure_probs_learning::operator()(const field_set& fs,
```
</details>


**14.** `components/learning/moses/moses/moses/representation/build_knobs.cc:581`

**Issue:** XXX TODO clarify actual breakeven on range of problems...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// measurements of relatively simple exemplars; its maybe even
// too low.  For large exemplars, it might be too big !?
// XXX TODO clarify actual breakeven on range of problems...
#define BREAKEVEN 30000
size_t np = perms.size();
```
</details>


**15.** `components/learning/moses/moses/moses/representation/knobs.h:283`

**Issue:** XXX what does the above comment mean ???

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

// Note - children aren't canonized when parents are called.
// XXX what does the above comment mean ???
struct action_subtree_knob : public discrete_knob<MAX_PERM_ACTIONS>
{
```
</details>


### Language-Learning Component (1 items)


**1.** `language-learning/tests/test_grammar_learner.py:211`

**Issue:** 81019 changes:    FIXME: DEL comments

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
}
re = learn_grammar(**kwargs)
# 81019 changes:    # FIXME: DEL comments
# a, q, qa = pqa_meter(re['grammar_file'], outpath, cp, rp, **kwargs)
# print('parse-ability, parse-quality:', a, q)
```
</details>


### Moses Component (11 items)


**1.** `moses/moses/moses/metapopulation/merging.cc:404`

**Issue:** XXX TODO fix the cap so its more sensitive to the size of

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// value of _params.cap_coef=50 seems to work well.
//
// XXX TODO fix the cap so its more sensitive to the size of
// each exemplar, right!? So if the exemplars are huges, then the
// population size has to be smaller.  ... On the other hand, if
```
</details>


**2.** `moses/moses/moses/scoring/precision_bscore.cc:111`

**Issue:** /set to false.  TBD XXX document that someday.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
/// The above only descirbes the "precision_full_bscore" (default)
/// scoring; something else is done when precision_full_bscore is
/// set to false.  TBD XXX document that someday.


```
</details>


**3.** `moses/moses/moses/scoring/select_bscore.cc:314`

**Issue:** XXX This is not quite right, for weighted rows.  A row with a small

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
}

// XXX This is not quite right, for weighted rows.  A row with a small
// weight could result in a much smaller min-improv.
// (But I think boosting should not affect min-improv, right?)
```
</details>


**4.** `moses/moses/moses/moses/partial.cc:96`

**Issue:** XXX TODO: we need to get the actual number of gens run, back

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
_moses_params.max_evals -= _num_evals;

// XXX TODO: we need to get the actual number of gens run, back
// from moses, and subtract it here.  But there's no easy way
// to get this number ...
```
</details>


**5.** `moses/moses/moses/moses/mpi_moses.cc:479`

**Issue:** XXX should print stats less often...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
});

// XXX should print stats less often...
// Print stats in a way that makes them easy to graph.
// (columns of tab-seprated numbers)
```
</details>


**6.** `moses/moses/moses/moses/mpi_moses.cc:639`

**Issue:** XXX this is kind-of buggy, since the following data is not

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// Print stats in a way that makes them easy to graph.
// (columns of tab-seprated numbers)
// XXX this is kind-of buggy, since the following data is not
// updated and collected atomically... other threads may be
// merging and updating as this print happens. Yuck. Oh well.
```
</details>


**7.** `moses/moses/moses/deme/feature_selector.cc:117`

**Issue:** /XXX TODO Explain what this function does. Why does it create a second

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
}

/// XXX TODO Explain what this function does. Why does it create a second
/// table that is different than _ctable? How are these two different
/// tables used?  Why do we need two different tables?  WTF.  This is
```
</details>


**8.** `moses/moses/moses/eda/local_structure.h:145`

**Issue:** XXX TODO document what this does...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
};*/

// XXX TODO document what this does...
struct local_structure_probs_learning
{
```
</details>


**9.** `moses/moses/moses/eda/local_structure.h:279`

**Issue:** XXX TODO this is unclear, explain what is being accumulated where.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// iterate over the dtrees, and accumulate statistics.
//
// XXX TODO this is unclear, explain what is being accumulated where.
template<typename It>
void local_structure_probs_learning::operator()(const field_set& fs,
```
</details>


**10.** `moses/moses/moses/representation/build_knobs.cc:581`

**Issue:** XXX TODO clarify actual breakeven on range of problems...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// measurements of relatively simple exemplars; its maybe even
// too low.  For large exemplars, it might be too big !?
// XXX TODO clarify actual breakeven on range of problems...
#define BREAKEVEN 30000
size_t np = perms.size();
```
</details>


**11.** `moses/moses/moses/representation/knobs.h:276`

**Issue:** XXX what does the above comment mean ???

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

// Note - children aren't canonized when parents are called.
// XXX what does the above comment mean ???
struct action_subtree_knob : public discrete_knob<MAX_PERM_ACTIONS>
{
```
</details>


### Ure Component (2 items)


**1.** `ure/opencog/ure/Rule.cc:535`

**Issue:** as any variable in the source. XXX This is only a stochastic

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

// To guarantee that the rule variable does not have the same name
// as any variable in the source. XXX This is only a stochastic
// guarantee, there is a small chance that the new random name
// will still collide.
```
</details>


**2.** `ure/opencog/ure/Rule.cc:572`

**Issue:** as any variable in the target. XXX This is only a stochastic

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

// To guarantee that the rule variable does not have the same name
// as any variable in the target. XXX This is only a stochastic
// guarantee, there is a small chance that the new random name
// will still collide.
```
</details>



---

## Implementation Guidance

### VERY_HARD Items - Specialized Expertise Required

**Characteristics:**
- Distributed systems algorithms
- Research-level computational problems  
- Exponential complexity algorithms
- Byzantine fault tolerance
- Consensus mechanisms

**Recommended Approach:**
- Consult academic literature and research papers
- Consider collaboration with research institutions
- Prototype and test extensively before production
- May require months of dedicated research and development

### HARD Items - Senior Developer Focus

**Characteristics:**  
- Thread safety and concurrency issues
- Performance-critical algorithms
- Complex recursive implementations
- Memory management optimizations

**Recommended Approach:**
- Assign to senior developers with domain expertise
- Implement comprehensive unit and integration tests
- Use static analysis tools (ThreadSanitizer, Valgrind)
- Code review by multiple senior developers

### MEDIUM Items - Standard Development Work

**Characteristics:**
- Feature implementations
- Algorithm completions  
- Stub replacements
- API enhancements

**Recommended Approach:**
- Can be distributed among team members
- Follow standard development workflows
- Implement with appropriate test coverage
- Regular code review process

### EASY Items - Entry-level Tasks

**Characteristics:**
- Documentation updates
- Comment improvements
- Simple code cleanups
- Minor feature additions

**Recommended Approach:**
- Suitable for new contributors
- Good for onboarding and learning codebase
- Can be completed in parallel with other work

---

## Component Analysis

| Component | VERY_HARD | HARD | MEDIUM | EASY | Total |
|-----------|-----------|------|--------|------|-------|
| analyze_fixme_instances.py | 0 | 0 | 0 | 2 | 2 |
| atomspace | 9 | 10 | 97 | 8 | 124 |
| atomspace-restful | 0 | 0 | 4 | 0 | 4 |
| atomspace-rocks | 0 | 0 | 2 | 0 | 2 |
| atomspace-storage | 2 | 1 | 14 | 0 | 17 |
| cogserver | 0 | 1 | 3 | 0 | 4 |
| cogutil | 0 | 0 | 3 | 0 | 3 |
| components | 18 | 27 | 233 | 15 | 293 |
| language-learning | 0 | 0 | 9 | 1 | 10 |
| moses | 3 | 16 | 113 | 11 | 143 |
| unify | 0 | 0 | 1 | 0 | 1 |
| ure | 0 | 0 | 0 | 2 | 2 |

---

## Next Steps

1. **Immediate Actions** (Next 2 weeks)
   - Begin with EASY items to build momentum
   - Identify domain experts for VERY_HARD items
   - Set up development environment and testing infrastructure

2. **Short Term** (1-3 months)
   - Complete all EASY items
   - Begin systematic work on MEDIUM items
   - Research and plan approach for HARD items

3. **Medium Term** (3-12 months)  
   - Complete MEDIUM and HARD items
   - Begin prototyping solutions for VERY_HARD items
   - Establish partnerships for specialized expertise

4. **Long Term** (12+ months)
   - Complete all FIXME items
   - Establish maintenance practices to prevent accumulation
   - Create contributor guidelines for high-quality code

---

*This catalog was generated automatically by analyzing 605 FIXME instances across 288 files in the OpenCog Unified repository.*
