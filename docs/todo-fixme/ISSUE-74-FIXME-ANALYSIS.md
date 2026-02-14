# FIXME Instances from Issue #74 - Sorted by Implementation Difficulty

This document analyzes the specific FIXME instances mentioned in issue #74, categorized by implementation difficulty.

## Summary

- **Very Hard**: 1 items (3.1%)
- **Hard**: 4 items (12.5%)
- **Medium**: 25 items (78.1%)
- **Easy**: 2 items (6.2%)

**Total analyzed**: 32 instances

---

## 🚨 Critical Priority - Research/Distributed Systems (1 items)

These require specialized expertise in distributed systems, algorithms research, or complex mathematical implementations.

**1.** `atomspace/opencog/sheaf/attic/linear-parser.scm`
**Issue:** XXX FIXME WARNING DANGER: As written, this runs in exponential time

## ⚡ High Priority - Performance/Threading (4 items)

These involve thread safety, performance optimization, or complex algorithmic work requiring senior developer expertise.

**1.** `atomspace/opencog/atomspace/Transient.cc`
**Issue:** /// XXX FIXME. Performance has not been recently measured; there

**2.** `atomspace/opencog/atomspace/AtomSpace.cc`
**Issue:** // XXX FIXME -- The recursive design of the depth() routine below makes

**3.** `atomspace/opencog/atoms/truthvalue/FormulaTruthValue.cc`
**Issue:** // XXX FIXME This update is not thread-safe.

**4.** `atomspace/opencog/atoms/value/FormulaStream.cc`
**Issue:** // XXX FIXME The update here is not thread-safe...

## 📋 Medium Priority - Feature Implementation (25 items)

Standard implementation work that can be handled by experienced developers with domain knowledge.

**1.** `atomspace/examples/atomspace/queue.scm`
**Issue:** ; XXX FIXME, this example is not yet complete and does not yet work...

**2.** `atomspace/opencog/atomspace/AtomTable.cc`
**Issue:** // atom in the parent. What??? XXX NOT TRUE FIXME

**3.** `atomspace/opencog/atomspace/AtomSpace.h`
**Issue:** // XXX FIXME Users should call StorageNode::add_nocheck() instead.

**4.** `atomspace/opencog/cython/PythonEval.cc`
**Issue:** // XXX FIXME this does a lot of wasteful string copying.

**5.** `atomspace/opencog/haskell/AtomSpace_CWrapper.h`
**Issue:** * XXX FIXME no one should be using Handle's to work with atoms,

**6.** `atomspace/opencog/haskell/PatternMatcher_CWrapper.h`
**Issue:** * XXX FIXME: atoms must never be accessed by UUID except by the

**7.** `atomspace/opencog/ocaml/CamlWrap.cc`
**Issue:** // XXX FIXME

**8.** `atomspace/opencog/guile/SchemeSmobAS.cc`
**Issue:** * until a better permission system is invented. XXX FIXME.

**9.** `atomspace/opencog/guile/modules/ExecSCM.cc`
**Issue:** // XXX FIXME: can we fix cython to not do this, already?

**10.** `atomspace/opencog/guile/SchemeSmobAtom.cc`
**Issue:** // XXX FIXME. Work around the despicable, horrible guile UTF8 handling.

**11.** `atomspace/opencog/guile/SchemeSmobValue.cc`
**Issue:** * XXX FIXME Clearly, a factory for values is called for.

**12.** `atomspace/opencog/guile/SchemeEval.cc`
**Issue:** // XXX FIXME This lock is not needed, because in guile-2.2,

**13.** `atomspace/opencog/atoms/pattern/PatternUtils.cc`
**Issue:** // XXX FIXME Are the below needed?

**14.** `atomspace/opencog/atoms/pattern/BindLink.cc`
**Issue:** // Shoot. XXX FIXME. Most of the unit tests require that the atom

**15.** `atomspace/opencog/atoms/pattern/PatternLink.cc`
**Issue:** // XXX FIXME, more correct would be to loop over

**16.** `atomspace/opencog/atoms/core/TypeChoice.cc`
**Issue:** // For now, just avoid throwing an exception. XXX FIXME.

**17.** `atomspace/opencog/atoms/core/RandomChoice.cc`
**Issue:** // XXX FIXME - fix this so it can also choose a single value

**18.** `atomspace/opencog/atoms/core/Variables.cc`
**Issue:** // XXX FIXME URE calls us with broken handle!!

**19.** `atomspace/opencog/atoms/core/TypeUtils.cc`
**Issue:** "Not implemented! TODO XXX FIXME");

**20.** `atomspace/opencog/atoms/execution/Instantiator.cc`
**Issue:** /// cleanly separated from each other. (XXX FIXME, these need to be

**21.** `atomspace/opencog/atoms/join/JoinLink.cc`
**Issue:** /// i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.

**22.** `atomspace/opencog/atoms/flow/FilterLink.cc`
**Issue:** "Globbing for Values not implemented! FIXME!");

**23.** `atomspace/opencog/query/InitiateSearchMixin.cc`
**Issue:** // XXX FIXME; we should be using ptm->isVariable() instead !?

**24.** `atomspace/opencog/query/RewriteMixin.cc`
**Issue:** // See issue #950 and pull req #962. XXX FIXME later.

**25.** `atomspace/opencog/query/PatternMatchEngine.cc`
**Issue:** /// XXX FIXME: this is currently a weak stop-gap measure to handle

## ✅ Easy Wins - Documentation/Simple Fixes (2 items)

Quick fixes suitable for new contributors or warm-up tasks.

**1.** `atomspace/opencog/atomspace/AtomSpace.cc`
**Issue:** // Fixme maybe later someday, if/when this is needed.

**2.** `atomspace/opencog/cython/PyIncludeWrapper.h`
**Issue:** // 0.15.1 and maybe other versions)  FIXME someday...

---

## Implementation Recommendations

### Immediate Actions (Week 1-2)
1. **Start with EASY items** - Build momentum and familiarize team with codebase
2. **Research VERY_HARD items** - Begin literature review and expert consultation
3. **Plan HARD items** - Assign to senior developers with relevant expertise

### Short-term Goals (Month 1-3)  
1. **Complete all EASY items** - Should take 1-4 weeks total
2. **Begin MEDIUM items** - Systematic implementation of standard features
3. **Prototype HARD solutions** - Design and test approaches for complex items

### Long-term Strategy (Month 3-12)
1. **Complete MEDIUM and HARD items** - Main development effort
2. **Implement VERY_HARD solutions** - Based on research and prototyping
3. **Establish practices** - Prevent future accumulation of technical debt

---

*This analysis focuses on the specific FIXME instances mentioned in issue #74. For a complete catalog of all 307 FIXME instances in the repository, see `FIXME-SORTED-CATALOG.md`.*
