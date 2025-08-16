# OpenCog Complete Dependency Analysis & Build Optimization
## Generated from Issue #85 Dependency Diagram

### Executive Summary

- **Total Components Analyzed**: 38
- **Available Components**: 26
- **External Dependencies**: 40
- **Critical Path Length**: 7
- **Parallelization Levels**: 7
- **Dependency Cycles**: None detected

### Critical Path Analysis

**Complete Critical Path** (7 components):
```
cogutil → atomspace → atomspace_storage → cogserver → ure → pln → opencog
```

**Available Critical Path** (2 components):
```
cogutil → opencog
```

### Parallelization Analysis

**Maximum Parallel Jobs**: 4 components simultaneously

**Level 1**: 3 parallel builds
- Components: atomspace, moses, language_learning

**Level 2**: 4 parallel builds
- Components: atomspace_storage, unify, spacetime, lg_atomese

**Level 3**: 2 parallel builds
- Components: cogserver, atomspace_rocks

**Level 4**: 4 parallel builds
- Components: atomspace_restful, ure, attention, learn

**Level 5**: 3 parallel builds
- Components: pln, miner, asmoses

### Optimized Build Phases

#### Phase 1 - Foundation
Components (1): cogutil

#### Phase 2 - Core Systems
Components (5): atomspace, atomspace_storage, cogserver, atomspace_rocks, atomspace_restful

#### Phase 3 - Logic & Reasoning
Components (4): unify, ure, pln, miner

#### Phase 4 - Advanced Systems
Components (4): moses, spacetime, attention, asmoses

#### Phase 5 - Integration
Components (4): language_learning, lg_atomese, learn, opencog

### External Dependencies Summary

**Most Common External Dependencies**:

- **cxxtest**: required by 15 components
- **boost**: required by 14 components
- **doxygen**: required by 12 components
- **valgrind**: required by 10 components
- **mpi**: required by 2 components
- **stack**: required by 2 components
- **attentionbank**: required by 2 components
- **uuid**: required by 1 components
- **linkgrammar**: required by 1 components
- **rocksdb**: required by 1 components

**Per-Component External Dependencies**:

- **asmoses**: cxxtest, valgrind, boost, mpi, doxygen
- **atomspace**: valgrind, boost, pgsql, doxygen, stack, folly, cxxtest, ocaml, unixodbc
- **atomspace_restful**: boost, doxygen, tbb, pkgconfig, cxxtest, jsoncpp, attentionbank, zmq
- **atomspace_rocks**: doxygen, valgrind, cxxtest, rocksdb
- **atomspace_storage**: doxygen, cxxtest, boost, guile
- **attention**: doxygen, valgrind, cxxtest, boost
- **cogserver**: cxxtest, valgrind, openssl, boost, doxygen
- **cogutil**: parallelstl, stlport, boost, doxygen, pthreads, bfd, cxxtest, iberty, gnubacktrace
- **lg_atomese**: doxygen, cxxtest, uuid, linkgrammar
- **miner**: cxxtest, valgrind, boost
- **moses**: cxxtest, valgrind, boost, mpi, doxygen
- **opencog**: valgrind, boost, doxygen, stack, ghc, cxxtest, lgatomese, attentionbank
- **pln**: boost
- **spacetime**: doxygen, cxxtest, octomap, boost
- **unify**: cxxtest, valgrind, boost
- **ure**: cxxtest, valgrind, boost

### Build Optimization Recommendations

- Critical path: cogutil -> atomspace -> atomspace_storage -> cogserver -> ure -> pln -> opencog
- Focus optimization efforts on critical path components
- Maximum parallelization: 20 components simultaneously
- Highest complexity components:
-   - cogutil: score 45
-   - atomspace: score 44
-   - opencog: score 22
-   - cogserver: score 18
-   - ure: score 17
- Components with many external deps: cogutil, atomspace, atomspace_storage, cogserver, atomspace_rocks, atomspace_restful, attention, spacetime, moses, asmoses, lg_atomese, vision, TinyCog, opencog

### Additional Recommendations

- **Containerization**: Use Docker for reproducible builds with all dependencies
- **CI/CD Pipeline**: Implement parallel builds based on dependency levels
- **Build Caching**: Cache builds at component level for faster iteration
- **Resource Allocation**: Allocate more build resources to critical path components
- **Dependency Management**: Monitor external dependencies for security updates

### Generated Artifacts

The analysis has generated the following optimization files:

1. **dependency_analysis_report.json** - Complete dependency analysis
2. **build_optimization_report.json** - Available components optimization
3. **OPTIMAL_BUILD_SEQUENCE.md** - Step-by-step build instructions
4. **CMakeLists_available.txt** - Optimized CMake configuration
5. **CMakeLists_optimized.txt** - Complete optimized CMake configuration
6. **dependency_graph.png** - Visual dependency graph

### Integration with Existing Build System

The optimization integrates with the existing OpenCog unified build system:

- Uses existing `validate-integration.py` for validation
- Extends `integrate-components.sh` functionality
- Compatible with current CMakeLists.txt structure
- Preserves existing dependency relationships
