# OpenCog Unified - Optimal Build Sequence

**!!! ADD BUILD DEPENDENCY SEQUENCE: cogutil > atomspace > atomspace-storage > atomspace-rocks > cogserver > ...

**Generated for 26 available components**

## Summary
- **Critical Path**: cogutil → opencog
- **Maximum Parallelization**: 4 components
- **Build Phases**: 5

## Available Components

- agentic_kernels_catalog
- asmoses
- atomspace
- atomspace_restful
- atomspace_rocks
- atomspace_storage
- attention
- cmake
- cognitive_patterns
- cognitive_visualization
- cogserver
- cogutil
- distributed_cognition
- ggml_tensor_kernel
- language_learning
- learn
- lg_atomese
- miner
- moses
- neural_symbolic_integration
- opencog
- pln
- spacetime
- tutorial_automation
- unify
- ure

## Build Phases

### Phase 1 - Foundation
Components: cogutil

### Phase 2 - Core Systems
Components: atomspace, atomspace_storage, cogserver, atomspace_rocks, atomspace_restful

### Phase 3 - Logic & Reasoning
Components: unify, ure, pln, miner

### Phase 4 - Advanced Systems
Components: moses, spacetime, attention, asmoses

### Phase 5 - Integration
Components: language_learning, lg_atomese, learn, opencog

## Parallelization Opportunities

**Level 1**: 3 parallel builds possible
- atomspace, moses, language_learning

**Level 2**: 4 parallel builds possible
- atomspace_storage, unify, spacetime, lg_atomese

**Level 3**: 2 parallel builds possible
- cogserver, atomspace_rocks

**Level 4**: 4 parallel builds possible
- atomspace_restful, ure, attention, learn

**Level 5**: 3 parallel builds possible
- pln, miner, asmoses

## External Dependencies

**moses**: cxxtest, valgrind, boost, mpi, doxygen
**miner**: cxxtest, valgrind, boost
**lg_atomese**: doxygen, cxxtest, uuid, linkgrammar
**atomspace_rocks**: doxygen, valgrind, cxxtest, rocksdb
**cogutil**: parallelstl, stlport, boost, doxygen, pthreads, bfd, cxxtest, iberty, gnubacktrace
**ure**: cxxtest, valgrind, boost
**asmoses**: cxxtest, valgrind, boost, mpi, doxygen
**attention**: doxygen, valgrind, cxxtest, boost
**cogserver**: cxxtest, valgrind, openssl, boost, doxygen
**atomspace**: valgrind, boost, pgsql, doxygen, stack, folly, cxxtest, ocaml, unixodbc
**opencog**: valgrind, boost, doxygen, stack, ghc, cxxtest, lgatomese, attentionbank
**atomspace_restful**: boost, doxygen, tbb, pkgconfig, cxxtest, jsoncpp, attentionbank, zmq
**pln**: boost
**unify**: cxxtest, valgrind, boost
**atomspace_storage**: doxygen, cxxtest, boost, guile
**spacetime**: doxygen, cxxtest, octomap, boost

## Build Instructions

# OpenCog Unified Build Instructions
# Optimized sequence based on dependency analysis

## Prerequisites
# Install system dependencies first:
sudo apt-get update
sudo apt-get install -y cmake build-essential libboost-all-dev
sudo apt-get install -y python3-dev guile-2.2-dev librocksdb-dev

## Build Sequence

### Level 0 - 1 components (can build in parallel)
cd cogutil
mkdir -p build && cd build
cmake ..
make -j$(nproc)
cd ../..

### Level 1 - 3 components (can build in parallel)
# Parallel build possible:
# Component: atomspace (directory: atomspace)
# Component: moses (directory: moses)
# Component: language_learning (directory: language-learning)
# Build all in parallel:
parallel -j3 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: atomspace moses language-learning

### Level 2 - 4 components (can build in parallel)
# Parallel build possible:
# Component: atomspace_storage (directory: atomspace-storage)
# Component: unify (directory: unify)
# Component: spacetime (directory: spacetime)
# Component: lg_atomese (directory: lg-atomese)
# Build all in parallel:
parallel -j4 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: atomspace-storage unify spacetime lg-atomese

### Level 3 - 2 components (can build in parallel)
# Parallel build possible:
# Component: cogserver (directory: cogserver)
# Component: atomspace_rocks (directory: atomspace-rocks)
# Build all in parallel:
parallel -j2 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: cogserver atomspace-rocks

### Level 4 - 4 components (can build in parallel)
# Parallel build possible:
# Component: atomspace_restful (directory: atomspace-restful)
# Component: ure (directory: ure)
# Component: attention (directory: attention)
# Component: learn (directory: learn)
# Build all in parallel:
parallel -j4 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: atomspace-restful ure attention learn

### Level 5 - 3 components (can build in parallel)
# Parallel build possible:
# Component: pln (directory: pln)
# Component: miner (directory: miner)
# Component: asmoses (directory: asmoses)
# Build all in parallel:
parallel -j3 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: pln miner asmoses

### Level 6 - 1 components (can build in parallel)
cd opencog
mkdir -p build && cd build
cmake ..
make -j$(nproc)
cd ../..
