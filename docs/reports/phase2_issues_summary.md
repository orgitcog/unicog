# Phase 2 High-Priority Issues Summary

## Sprint 1: Exception Handling (5 issues)

### 1. TypeUtils.cc:421 - Return UNDEFINED instead of throwing
**File:** `atomspace/opencog/atoms/core/TypeUtils.cc`
**Function:** `filter_vardecl()`
**Issue:** Returns `Handle::UNDEFINED` for unrecognized variable declarations instead of throwing an exception
**Impact:** Silent failures make debugging difficult

### 2. RewriteMixin.cc:118 - Try-catch workaround (issue #950)
**File:** `atomspace/opencog/query/RewriteMixin.cc`
**Issue:** Using try-catch as a workaround instead of proper fix
**Related:** GitHub issue #950, PR #962
**Impact:** Masks underlying problems, makes code harder to maintain

### 3. SchemeEval.cc:1057 - Catch-and-rethrow breaks tests
**File:** `components/language/learn/attic/run-ull-2019/SchemeEval.cc`
**Issue:** Catch-and-rethrow pattern breaks unit tests
**Impact:** Poor exception propagation, test failures

### 4. instance_scorer.h:89 - Exception not thrown (components)
**File:** `components/learning/moses/moses/moses/representation/instance_scorer.h`
**Issue:** `score_tree()` doesn't properly throw exceptions
**Impact:** Silent failures in scoring

### 5. instance_scorer.h:89 - Exception not thrown (moses)
**File:** `moses/moses/moses/representation/instance_scorer.h`
**Issue:** Duplicate of #4 (same file in different location)
**Impact:** Same as #4

---

## Sprint 2: Thread Safety (2 issues)

### 6. CogServer.cc:125 - Spinning loop instead of semaphore
**File:** `cogserver/opencog/cogserver/server/CogServer.cc`
**Issue:** Uses inefficient spinning loop instead of proper thread synchronization
**Impact:** Wastes CPU cycles, poor performance

### 7. SchemeEval.cc:93 - Unnecessary lock
**File:** `components/language/learn/attic/run-ull-2019/SchemeEval.cc`
**Issue:** Lock not needed in guile-2.2 (per-thread output ports)
**Impact:** Unnecessary synchronization overhead

---

## Sprint 2: Performance (3 issues)

### 8. AtomSpace.cc:283 - Recursive depth() bottleneck
**File:** `atomspace/opencog/atomspace/AtomSpace.cc`
**Issue:** Recursive design makes depth() a performance bottleneck
**Impact:** Slow with deep AtomSpace hierarchies

### 9. Transient.cc:43 - Performance not measured
**File:** `atomspace/opencog/atomspace/Transient.cc`
**Issue:** Performance hasn't been measured after redesigns
**Impact:** Unknown if optimization is still beneficial

### 10. WriteThruProxy.cc:114 - Inefficient key storage
**File:** `cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc`
**Issue:** Inefficient key storage pattern
**Impact:** Performance degradation in proxy operations

---

## Implementation Priority

**Week 1:** Exception handling issues (#1-5)
**Week 2:** Thread safety (#6-7) and Performance (#8-10)
**Week 3:** Testing, documentation, and integration

