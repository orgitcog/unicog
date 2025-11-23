# Entelechy Marker Resolution - Actionable Items
Generated: 2025-11-22 18:48:50

## 🚨 Critical Priority (Address First)

### 1. FIXME in `analyze_fixme_instances.py:138`
**Content:** ('xxx' in line_lower and (line_lower.strip().startswith('#') or line_lower.strip().startswith('//'))) or  # XXX comments
**Category:** documentation
**Estimated Effort:** 1-2 hours
**Suggestion:** Add comprehensive documentation explaining the code

### 2. FIXME in `analyze_issue_examples.py:13`
**Content:** "./atomspace/examples/atomspace/queue.scm:; XXX FIXME, this example is not yet complete and does not yet work...",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 3. FIXME in `analyze_issue_examples.py:14`
**Content:** "./atomspace/opencog/atomspace/Transient.cc:/// XXX FIXME. Performance has not been recently measured; there",
**Category:** performance
**Estimated Effort:** 3-7 days
**Suggestion:** Profile code and implement optimizations

### 4. FIXME in `analyze_issue_examples.py:16`
**Content:** "./atomspace/opencog/atomspace/AtomSpace.cc:	// Fixme maybe later someday, if/when this is needed.",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 5. FIXME in `analyze_issue_examples.py:17`
**Content:** "./atomspace/opencog/atomspace/AtomSpace.cc:// XXX FIXME -- The recursive design of the depth() routine below makes",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 6. FIXME in `analyze_issue_examples.py:18`
**Content:** "./atomspace/opencog/atomspace/AtomSpace.h:    // XXX FIXME Users should call StorageNode::add_nocheck() instead.",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 7. FIXME in `analyze_issue_examples.py:19`
**Content:** "./atomspace/opencog/cython/PythonEval.cc:    // XXX FIXME this does a lot of wasteful string copying.",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 8. FIXME in `analyze_issue_examples.py:21`
**Content:** "./atomspace/opencog/haskell/AtomSpace_CWrapper.h:     * XXX FIXME no one should be using Handle's to work with atoms,",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 9. FIXME in `analyze_issue_examples.py:22`
**Content:** "./atomspace/opencog/haskell/PatternMatcher_CWrapper.h: * XXX FIXME: atoms must never be accessed by UUID except by the",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 10. FIXME in `analyze_issue_examples.py:23`
**Content:** "./atomspace/opencog/ocaml/CamlWrap.cc:	// XXX FIXME",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 11. FIXME in `analyze_issue_examples.py:25`
**Content:** "./atomspace/opencog/guile/modules/ExecSCM.cc:// XXX FIXME: can we fix cython to not do this, already?",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 12. FIXME in `analyze_issue_examples.py:26`
**Content:** "./atomspace/opencog/guile/SchemeSmobAtom.cc:// XXX FIXME. Work around the despicable, horrible guile UTF8 handling.",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 13. FIXME in `analyze_issue_examples.py:27`
**Content:** "./atomspace/opencog/guile/SchemeSmobValue.cc: * XXX FIXME Clearly, a factory for values is called for.",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 14. FIXME in `analyze_issue_examples.py:28`
**Content:** "./atomspace/opencog/guile/SchemeEval.cc:	// XXX FIXME This lock is not needed, because in guile-2.2,",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 15. FIXME in `analyze_issue_examples.py:30`
**Content:** "./atomspace/opencog/atoms/pattern/PatternUtils.cc:		// XXX FIXME Are the below needed?",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 16. FIXME in `analyze_issue_examples.py:32`
**Content:** "./atomspace/opencog/atoms/pattern/PatternLink.cc:		// XXX FIXME, more correct would be to loop over",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 17. FIXME in `analyze_issue_examples.py:33`
**Content:** "./atomspace/opencog/atoms/truthvalue/FormulaTruthValue.cc:// XXX FIXME This update is not thread-safe.",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 18. FIXME in `analyze_issue_examples.py:35`
**Content:** "./atomspace/opencog/atoms/core/RandomChoice.cc:// XXX FIXME - fix this so it can also choose a single value",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 19. FIXME in `analyze_issue_examples.py:36`
**Content:** "./atomspace/opencog/atoms/core/Variables.cc:	// XXX FIXME URE calls us with broken handle!!",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

### 20. FIXME in `analyze_issue_examples.py:38`
**Content:** "./atomspace/opencog/atoms/value/FormulaStream.cc:// XXX FIXME The update here is not thread-safe...",
**Category:** bug_fix
**Estimated Effort:** 1-3 days
**Suggestion:** Identify root cause and implement fix with tests

## ✅ Easy Wins (Quick Resolutions)

1. `components/integration/opencog/opencog/openpsi/rule.scm:128` - TODO: Add utilities for declaring custom urge formula.
2. `components/integration/opencog/opencog/openpsi/rule.scm:197` - TODO: Uncomment after testing with ghost
3. `components/integration/opencog/opencog/openpsi/rule.scm:223` - TODO: Uncomment after testing with ghost
4. `components/integration/opencog/opencog/openpsi/main.scm:35` - TODO: Adding  a component to a category makes no sense
5. `components/integration/opencog/opencog/ghost/translator.scm:189` - TODO: The specificity of ordered vs unordered should be
6. `components/integration/opencog/opencog/ghost/translator.scm:547` - TODO: Handle variables as well
7. `components/integration/opencog/opencog/ghost/test.scm:18` - TODO: Remove once experimentation is over
8. `components/integration/opencog/opencog/ghost/cs-parse.scm:90` - TODO: Add tester function for this
9. `components/integration/opencog/opencog/ghost/cs-parse.scm:710` - TODO: This has a restart_matching effect. See chatscript documentation
10. `components/integration/opencog/opencog/ghost/matcher.scm:176` - TODO: Return the actual action instead of a rule
11. `components/integration/opencog/opencog/ghost/stimulation.scm:32` - TODO: Find some better representation for that
12. `components/integration/opencog/opencog/ghost/terms.scm:238` - TODO: Should be handled in OpenCog internally?
13. `components/integration/opencog/opencog/ghost/terms.scm:255` - TODO: Should be handled in OpenCog internally?
14. `components/integration/opencog/opencog/ghost/terms.scm:291` - TODO: Check to make sure the function has been defined
15. `components/integration/opencog/opencog/ghost/terms.scm:304` - TODO: Check to make sure the function has been defined
16. `components/integration/opencog/opencog/eva/model/self-model.scm:143` - TODO: Remove this when the time-server is ready.
17. `components/integration/opencog/opencog/eva/behavior/psi-behavior.scm:20` - TODO: make generic for orchestration.
18. `components/integration/opencog/opencog/eva/behavior/psi-behavior.scm:82` - TODO: test the behabior when talking.
19. `components/integration/opencog/opencog/eva/behavior/face-priority.scm:124` - TODO: Move to config file
20. `components/integration/opencog/opencog/eva/behavior/behavior.scm:19` - TODO: 
