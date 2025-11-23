
# Analysis of Actionable TODO Items in opencog-unified

**Date:** 2025-11-23
**Author:** Manus AI

## 1. Executive Summary

This report provides a detailed analysis of the **98 actionable TODO and FIXME items** identified in the `opencog-unified` repository. These items represent tasks that are suitable for immediate implementation, ranging from simple bug fixes to more involved feature additions. 

The analysis categorizes each item by its parent **component**, **implementation complexity**, **estimated time**, and **type of work**. The majority of items are concentrated in the **Moses (33)** and **NLP (23)** components. Most tasks are of **medium complexity (85 items)** and are classified as **bug fixes (56 items)**, with a typical implementation time of **2-4 hours**.

Based on this analysis, a prioritized roadmap is proposed, starting with **8 quick wins** that can be completed in under 2 hours each. This report includes detailed breakdowns and visualizations to guide development efforts.

## 2. Overall Analysis and Distribution

The 98 actionable items are distributed across several dimensions, providing a clear overview of where development effort is most needed.

![Overall Analysis](actionable_todos_analysis.png)

*Figure 1: Distribution of actionable items by component, complexity, work type, and estimated time.*

### Key Observations:

- **Component Focus:** The **Moses** and **NLP** components contain over half of the actionable items, indicating these are key areas for improvement.
- **Complexity:** The vast majority of tasks are of **medium complexity**, suggesting that a significant amount of straightforward, valuable work is available.
- **Work Type:** **Bug fixes** are the most common task, highlighting an opportunity to improve the stability and correctness of the codebase.
- **Time Estimate:** A large number of tasks are estimated to take **2-4 hours**, making them ideal for focused development sprints.

## 3. Priority Recommendations and Roadmap

To effectively tackle these items, the following prioritized roadmap is recommended. This approach focuses on delivering value quickly by starting with low-effort, high-impact tasks.

![Priority Breakdown](priority_breakdown.png)

*Figure 2: Breakdown of priority categories by component and effort distribution.*

### Priority 1: Quick Wins (8 items, ≤2 hours each)
These are simple fixes that can be completed quickly to build momentum.

| Component | Quick Wins |
|-----------|-------------|
| moses     | 3           |
| atomspace | 2           |
| nlp       | 1           |
| ure       | 1           |
| other     | 1           |

### Priority 2: Bug Fixes (56 items)
This is the largest category of work and is crucial for improving code quality. After the quick wins, development should focus on the bug fixes within the **Moses** and **NLP** components.

### Priority 3: Feature Additions & Enhancements (17 items)
These items add new functionality to the system. The most significant feature additions are in the **Moses** component.

### Priority 4: Refactoring & Technical Debt (7 items)
These tasks improve the long-term health of the codebase. They are primarily located in the **NLP** and **Chatbot** components.

### Priority 5: Complex Items (5 items, 1-7 days each)
These tasks require significant effort and expertise. They should be planned carefully and may require dedicated development time.

## 4. Detailed Component Breakdown

Below is a summary of the actionable items for the top 3 components.

### Moses Component (33 items)

| Complexity | Count | Time Estimate | Count |
|------------|-------|---------------|-------|
| Simple     | 3     | 30min-1hr     | 1     |
| Medium     | 28    | 1-2hrs        | 2     |
| Complex    | 2     | 2-4hrs        | 24    |
|            |       | 4-8hrs        | 4     |
|            |       | 3-7days       | 2     |

**Top Priorities:**
1.  **Add support for "term algebra" knobs:** (2 items, 1-2hrs each) - Simple feature addition.
2.  **Extend comment handling in table I/O:** (1 item, 30min-1hr) - Simple bug fix.
3.  **Merge multiple tables in `table-problems.cc`:** (2 items, 2-4hrs each) - Medium complexity bug fix.

### NLP Component (23 items)

| Complexity | Count | Time Estimate | Count |
|------------|-------|---------------|-------|
| Simple     | 1     | 30min-1hr     | 1     |
| Medium     | 22    | 2-4hrs        | 21    |
|            |       | 8-16hrs       | 1     |

**Top Priorities:**
1.  **Fix tagging of "finished" documents:** (1 item, 30min-1hr) - Simple bug fix.
2.  **Fix being called too often in `WordSenseProcessor.cc`:** (1 item, 2-4hrs) - Medium complexity bug fix.
3.  **Replace cheesy implementation in `WordRelQuery.cc`:** (1 item, 8-16hrs) - Medium complexity refactoring.

### Chatbot Component (13 items)

| Complexity | Count | Time Estimate | Count |
|------------|-------|---------------|-------|
| Medium     | 13    | 2-4hrs        | 11    |
|            |       | 8-16hrs       | 2     |

**Top Priorities:**
1.  **Replace emotional state modeling with OpenPsi:** (1 item, 8-16hrs) - Medium complexity refactoring.
2.  **Fix `Show random expression` implementation:** (1 item, 2-4hrs) - Medium complexity bug fix.
3.  **Complete face study saccade implementation:** (1 item, 2-4hrs) - Medium complexity enhancement.

## 5. Full Actionable TODO List

The following table provides the complete list of 98 actionable items, categorized and sorted by component and complexity.

| Component | File | Line | Complexity | Time Estimate | Work Type | Description |
|---|---|---|---|---|---|---|
| atomspace | `atomspace/opencog/atoms/execution/ExecutionOutputLink.cc` | 155 | medium | 2-4hrs | Bug Fix | // XXX should be be unwrapping SetLinks here?... |
| atomspace | `atomspace/opencog/guile/modules/TypeUtilsSCM.cc` | 58 | medium | 2-4hrs | Technical Debt | // XXX HACK ALERT This needs to be static, in order for pyth... |
| atomspace | `atomspace/opencog/query/InitiateSearchMixin.cc` | 126 | medium | 2-4hrs | Bug Fix | // XXX FIXME; we should be using ptm->isVariable() instead !... |
| atomspace | `atomspace/opencog/query/PatternMatchEngine.cc` | 1116 | medium | 2-4hrs | Incomplete Implementation | // XXX The current implementation is a brute-force search, a... |
| atomspace | `atomspace/opencog/query/PatternMatchEngine.cc` | 2066 | medium | 2-4hrs | Bug Fix | // XXX TODO FIXME. The ptm needs to be decomposed into conne... |
| atomspace | `atomspace/opencog/query/PatternMatchEngine.cc` | 1159 | simple | 1-2hrs | Bug Fix | // XXX TODO The logic here should be updated to resemble tha... |
| atomspace-restful | `atomspace-restful/tests/python/restapi/test_restapi.py` | 456 | simple | 1-2hrs | Feature Addition | # TODO: The Python module "graphviz" needs to be added to oc... |
| atomspace-restful | `components/core/atomspace-restful/tests/python/restapi/test_restapi.py` | 456 | simple | 1-2hrs | Feature Addition | # TODO: The Python module "graphviz" needs to be added to oc... |
| atomspace-storage | `atomspace-storage/opencog/persist/sexcom/Commands.cc` | 164 | complex | 3-7days | Refactoring | // XXX this should be nuked, and replaced by appropriate kin... |
| atomspace-storage | `atomspace-storage/opencog/persist/proxy/CachingProxy.cc` | 48 | simple | 1-2hrs | Feature Addition | // XXX TODO Add support for expiration times, limited AtomSp... |
| chatbot | `components/integration/opencog/opencog/eva/behavior/behavior.scm` | 147 | medium | 8-16hrs | Refactoring | ;; XXX Needs to be replaced by OpenPsi emotional state model... |
| chatbot | `components/integration/opencog/opencog/eva/behavior/behavior.scm` | 179 | medium | 2-4hrs | Bug Fix | ; XXX FIXME this should be a part of "Show random expression... |
| chatbot | `components/integration/opencog/opencog/eva/behavior/behavior.scm` | 378 | medium | 2-4hrs | Enhancement | ; XXX incomplete!  need the face study saccade stuff...... |
| chatbot | `components/integration/opencog/opencog/eva/behavior/face-priority.scm` | 74 | medium | 2-4hrs | Bug Fix | ; FIXME: There should never be an empty set. The value shoul... |
| chatbot | `components/integration/opencog/opencog/eva/chatbot-eva/imperative-rules.scm` | 45 | medium | 2-4hrs | Enhancement | ; XXX needs to be public, so that cog-execute! can find this... |
| chatbot | `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm` | 258 | medium | 2-4hrs | Incomplete Implementation | ; XXX FIXME -- Implement-me, actually -- need to do the abov... |
| chatbot | `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm` | 358 | medium | 2-4hrs | Bug Fix | ; XXX a bunch of verb synonyms -- handled manually. These sh... |
| chatbot | `components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm` | 404 | medium | 8-16hrs | Refactoring | ; XXX FIXME -- this list contains lots of synonyms; needs to... |
| chatbot | `components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm` | 156 | medium | 2-4hrs | Bug Fix | ; XXX this should be moved to cog-utils. Also needs to be fi... |
| chatbot | `components/integration/opencog/opencog/eva/model/self-model.scm` | 374 | medium | 2-4hrs | Bug Fix | ;; XXX FIXME -- the psi subsystem should be performing this... |
| chatbot | `components/integration/opencog/opencog/eva/model/time-map.scm` | 9 | medium | 2-4hrs | Bug Fix | ; XXX FIXME -- some of the below should be handled as psi-ru... |
| chatbot | `components/integration/opencog/opencog/eva/model/time-map.scm` | 161 | medium | 2-4hrs | Bug Fix | ;; XXX FIXME -- this kind of crazy angle computation should... |
| chatbot | `components/integration/opencog/opencog/eva/model/time-map.scm` | 196 | medium | 2-4hrs | Bug Fix | ;; XXX FIXME -- this kind of tulity needs to be in the space... |
| cogserver | `cogserver/opencog/cogserver/server/CogServer.cc` | 125 | complex | 3-7days | Technical Debt | // XXX FIXME. terrible terrible hack. What we should be... |
| integration | `components/integration/opencog/opencog/ghost/terms.scm` | 238 | medium | 2-4hrs | Bug Fix | ; TODO: Should be handled in OpenCog internally?... |
| integration | `components/integration/opencog/opencog/ghost/terms.scm` | 255 | medium | 2-4hrs | Bug Fix | ; TODO: Should be handled in OpenCog internally?... |
| integration | `components/integration/opencog/opencog/ghost/translator.scm` | 189 | medium | 2-4hrs | Bug Fix | ; TODO: The specificity of ordered vs unordered should be... |
| integration | `components/integration/opencog/opencog/openpsi/dynamics/updater.scm` | 69 | medium | 2-4hrs | Incomplete Implementation | ; Todo: implement these tables in the atomspace... |
| moses | `components/learning/moses/moses/comboreduct/table/table_io.h` | 137 | complex | 3-7days | Incomplete Implementation | // TODO: reimplement loadITable with the same model of loadT... |
| moses | `moses/moses/comboreduct/table/table_io.h` | 137 | complex | 3-7days | Incomplete Implementation | // TODO: reimplement loadITable with the same model of loadT... |
| moses | `components/learning/moses/moses/comboreduct/table/table_io.cc` | 1256 | medium | 4-8hrs | Feature Addition | // TODO: implement timestamp support... |
| moses | `components/learning/moses/moses/moses/main/table-problems.cc` | 138 | medium | 2-4hrs | Bug Fix | // XXX FIXME -- the multiple tables should be merged into on... |
| moses | `components/learning/moses/moses/moses/moses/mpi_moses.cc` | 566 | medium | 2-4hrs | Bug Fix | print_stats_header(NULL, false /* XXX stats for diversity, s... |
| moses | `components/learning/moses/moses/moses/optimization/star-anneal.cc` | 42 | medium | 2-4hrs | Bug Fix | // XXX TODO the annealing temperature control code should be... |
| moses | `components/learning/moses/moses/moses/scoring/scoring_base.cc` | 108 | medium | 2-4hrs | Bug Fix | // XXX FIXME complexity_t should be a double not an int ...... |
| moses | `moses/moses/comboreduct/table/table_io.cc` | 1241 | medium | 4-8hrs | Feature Addition | // TODO: implement timestamp support... |
| moses | `moses/moses/moses/main/table-problems.cc` | 138 | medium | 2-4hrs | Bug Fix | // XXX FIXME -- the multiple tables should be merged into on... |
| moses | `moses/moses/moses/moses/mpi_moses.cc` | 570 | medium | 2-4hrs | Bug Fix | print_stats_header(NULL, false /* XXX stats for diversity, s... |
| moses | `moses/moses/moses/moses/mpi_moses.cc` | 616 | medium | 2-4hrs | Bug Fix | // XXX TODO instead of overwritting the demeID it should be... |
| moses | `moses/moses/moses/optimization/star-anneal.cc` | 42 | medium | 2-4hrs | Bug Fix | // XXX TODO the annealing temperature control code should be... |
| moses | `moses/moses/moses/scoring/scoring_base.cc` | 142 | medium | 2-4hrs | Bug Fix | // XXX FIXME complexity_t should be a double not an int ...... |
| moses | `components/learning/moses/moses/comboreduct/combo/vertex.h` | 101 | medium | 2-4hrs | Bug Fix | // XXX This should be obsoleted by cond, at some point.... |
| moses | `components/learning/moses/moses/comboreduct/table/table.h` | 98 | medium | 2-4hrs | Incomplete Implementation | // XXX FIXME TODO: change the implementation, per the above... |
| moses | `components/learning/moses/moses/comboreduct/table/table.h` | 1089 | medium | 4-8hrs | Feature Addition | // XXX TODO to implement enum support, cut-n-paste from CTab... |
| moses | `components/learning/moses/moses/moses/main/problem-params.h` | 46 | medium | 2-4hrs | Bug Fix | // XXX FIXME TODO The structure below should be split into m... |
| moses | `components/learning/moses/moses/moses/moses/types.h` | 210 | medium | 2-4hrs | Bug Fix | // TODO this should be a std::valarray not std::vector but I... |
| moses | `components/learning/moses/moses/moses/representation/field_set.h` | 213 | medium | 2-4hrs | Bug Fix | // XXX should be enum ...... |
| moses | `components/learning/moses/moses/moses/representation/instance_scorer.h` | 89 | medium | 2-4hrs | Bug Fix | // XXX FIXME, calling score_tree above does not throw the ex... |
| moses | `components/learning/moses/moses/moses/scoring/scoring_base.h` | 124 | medium | 2-4hrs | Bug Fix | // XXX TODO should be a std::valarray not a vector.... |
| moses | `moses/moses/comboreduct/combo/vertex.h` | 101 | medium | 2-4hrs | Bug Fix | // XXX This should be obsoleted by cond, at some point.... |
| moses | `moses/moses/comboreduct/table/table.h` | 99 | medium | 2-4hrs | Incomplete Implementation | // XXX FIXME TODO: change the implementation, per the above... |
| moses | `moses/moses/comboreduct/table/table.h` | 1126 | medium | 4-8hrs | Feature Addition | // XXX TODO to implement enum support, cut-n-paste from CTab... |
| moses | `moses/moses/moses/main/problem-params.h` | 46 | medium | 2-4hrs | Bug Fix | // XXX FIXME TODO The structure below should be split into m... |
| moses | `moses/moses/moses/moses/moses_main.h` | 102 | medium | 2-4hrs | Bug Fix | // XXX TODO this should be fixed, someday...... |
| moses | `moses/moses/moses/moses/types.h` | 210 | medium | 2-4hrs | Bug Fix | // TODO this should be a std::valarray not std::vector but I... |
| moses | `moses/moses/moses/representation/field_set.h` | 213 | medium | 2-4hrs | Bug Fix | // XXX should be enum ...... |
| moses | `moses/moses/moses/representation/instance_scorer.h` | 89 | medium | 2-4hrs | Bug Fix | // XXX FIXME, calling score_tree above does not throw the ex... |
| moses | `moses/moses/moses/scoring/scoring_base.h` | 124 | medium | 2-4hrs | Bug Fix | // XXX TODO should be a std::valarray not a vector.... |
| moses | `components/learning/moses/moses/moses/representation/representation.cc` | 236 | simple | 1-2hrs | Feature Addition | // XXX TODO need to add support for "term algebra" knobs... |
| moses | `moses/moses/comboreduct/table/table_io.cc` | 99 | simple | 30min-1hr | Bug Fix | // TODO: This routine should be extended so that comments th... |
| moses | `moses/moses/moses/representation/representation.cc` | 238 | simple | 1-2hrs | Feature Addition | // XXX TODO need to add support for "term algebra" knobs... |
| nlp | `components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc` | 361 | medium | 8-16hrs | Refactoring | // XXX this needs to be replaced in the end, for now its jus... |
| nlp | `components/integration/opencog/opencog/nlp/wsd/WordSenseProcessor.cc` | 120 | medium | 2-4hrs | Bug Fix | // XXX we are being called too often. this needs to be fixed... |
| nlp | `components/language/lg-atomese/opencog/nlp/lg-parse/LGParseLink.cc` | 229 | medium | 2-4hrs | Bug Fix | // XXX FIXME. This should be part of the LgDictNode but sinc... |
| nlp | `components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm` | 230 | medium | 2-4hrs | Enhancement | ;; XXX someday, this needs to be an or-list of WH- words.... |
| nlp | `components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm` | 617 | medium | 2-4hrs | Bug Fix | ; XXX this is wrong, it should be PrepositionalRelationshipN... |
| nlp | `components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm` | 14 | medium | 2-4hrs | Bug Fix | (let* (; TODO: Should be bias according to the score... |
| nlp | `components/integration/opencog/opencog/nlp/microplanning/anaphora.scm` | 23 | medium | 2-4hrs | Enhancement | ; TODO also insert anaphora for missing subjects/objects... |
| nlp | `components/integration/opencog/opencog/nlp/microplanning/main.scm` | 83 | medium | 2-4hrs | Bug Fix | ; XXX FIXME utterance-type should be an atom, not a string!... |
| nlp | `components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm` | 140 | medium | 2-4hrs | Bug Fix | ; XXX FIXME should be changed to just use sha-256 -- that wo... |
| nlp | `components/integration/opencog/opencog/nlp/relex2logic/relex2logic.scm` | 39 | medium | 2-4hrs | Bug Fix | ; XXX maybe this should be part of the ure module??... |
| nlp | `components/integration/opencog/opencog/nlp/scm/type-definitions.scm` | 13 | medium | 2-4hrs | Enhancement | ; XXX This list is *probably* incomplete, and needs to be re... |
| nlp | `components/integration/opencog/opencog/nlp/scm/type-definitions.scm` | 128 | medium | 2-4hrs | Enhancement | ; XXX this list is highly incomplete... |
| nlp | `components/language/learn/attic/repair/word-merge.scm` | 197 | medium | 2-4hrs | Bug Fix | ; XXX technically, this is wrong, we should be renaming thes... |
| nlp | `components/language/learn/scm/attic/summary.scm` | 5 | medium | 2-4hrs | Enhancement | ; XXX This is stale and semi-abandoned and needs to be moder... |
| nlp | `components/language/learn/scm/attic/cluster/agglo-loops.scm` | 381 | medium | 2-4hrs | Bug Fix | ; XXX FIXME: The DONE-LIST should be scrubbed for short junk... |
| nlp | `components/language/learn/scm/attic/cluster/cset-merge.scm` | 90 | medium | 2-4hrs | Enhancement | ; XXX Incomplete, in development.... |
| nlp | `components/language/learn/scm/attic/cluster/gram-pairwise.scm` | 592 | medium | 2-4hrs | Incomplete Implementation | ; XXX TODO once make-merge-majority is done, this can be rei... |
| nlp | `components/language/learn/scm/gram-class/gram-class-api.scm` | 133 | medium | 2-4hrs | Bug Fix | ; XXX FIXME the semantics of this thing is ugly, and should... |
| nlp | `components/language/learn/scm/gram-class/gram-majority.scm` | 205 | medium | 2-4hrs | Bug Fix | ; XXX TODO this should be either... |
| nlp | `components/language/learn/scm/gram-class/shape-project.scm` | 307 | medium | 2-4hrs | Bug Fix | ; XXX TODO -- generic deletion should be moved to a method... |
| nlp | `components/language/learn/scm/gram-class/shape-vec.scm` | 109 | medium | 2-4hrs | Bug Fix | ; TODO: with appropriate cleanup, this probably should be mo... |
| nlp | `components/language/learn/scm/gram-class/singletons.scm` | 61 | medium | 2-4hrs | Incomplete Implementation | ; TODO: Implement delete functionality... |
| nlp | `components/integration/opencog/opencog/nlp/wsd-post/collect-stats/stats-collection.scm` | 359 | simple | 30min-1hr | Incomplete Implementation | ; XXX The method used here, of tagging documents with "finis... |
| other | `analyze_issue_examples.py` | 13 | complex | 3-7days | Bug Fix | "./atomspace/examples/atomspace/queue.scm:; XXX FIXME, this... |
| other | `analyze_issue_examples.py` | 42 | medium | 2-4hrs | Bug Fix | "./atomspace/opencog/query/InitiateSearchMixin.cc:		// XXX F... |
| other | `analyze_issue_examples.py` | 106 | medium | 2-4hrs | Incomplete Implementation | markdown = """# FIXME Instances from Issue #74 - Sorted by I... |
| other | `implement_functional_fixes.py` | 42 | medium | 2-4hrs | Incomplete Implementation | new_impl = ' ' * indent + f'# TODO: Implement {func_name} fu... |
| other | `implement_scheme_stubs.py` | 89 | medium | 2-4hrs | Incomplete Implementation | impl = indent_str + '; TODO: Implement full question handlin... |
| other | `implement_scheme_stubs.py` | 96 | medium | 2-4hrs | Incomplete Implementation | impl = indent_str + '; TODO: Implement full SVO/SVIO relatio... |
| other | `implement_scheme_stubs.py` | 103 | medium | 2-4hrs | Incomplete Implementation | impl = indent_str + '; TODO: Implement prepositional object... |
| other | `implement_scheme_stubs.py` | 110 | medium | 2-4hrs | Incomplete Implementation | impl = indent_str + '; TODO: Implement ' + func_name + ' fun... |
| other | `setup_fixme_environment.py` | 83 | medium | 2-4hrs | Incomplete Implementation | echo "   // TODO: Implement error handling for edge case X"... |
| ure | `ure/opencog/ure/BetaDistribution.cc` | 33 | medium | 8-16hrs | Refactoring | // TODO should be replaced by tv->get_mode() once implemente... |
| ure | `ure/opencog/ure/backwardchainer/BIT.h` | 72 | medium | 2-4hrs | Bug Fix | // TODO: Maybe this should be moved to BackwardChainer... |
| ure | `ure/opencog/ure/backwardchainer/TraceRecorder.h` | 94 | medium | 2-4hrs | Bug Fix | // TODO: the TV on the evaluation link should be more carefu... |
| ure | `ure/opencog/ure/forwardchainer/SourceRuleSet.h` | 102 | medium | 2-4hrs | Incomplete Implementation | // TODO: implement tournament selection as well, as a cheape... |
| ure | `ure/tests/ure/backwardchainer/scm/green-balls-targets.scm` | 105 | medium | 2-4hrs | Bug Fix | ;; TODO: the type of G should be further specified, such the... |
