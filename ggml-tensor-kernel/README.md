# Unified GGML Tensor Kernel

## Overview

This module implements a unified GGML tensor kernel that integrates all OpenCog cognitive modules (cogutil, atomspace, cogserver, etc.) as distributed tensor operations. The architecture enables neural-symbolic integration, attention allocation, and distributed execution through GGML tensor operations.

## Architecture

The unified tensor kernel consists of several key components:

### 1. TensorKernel (Main Orchestrator)
- **File**: `include/opencog/tensor/TensorKernel.h`, `src/TensorKernel.cc`
- **Purpose**: Main unified tensor processing engine
- **Key Features**:
  - Registers cognitive modules with tensor shapes
  - Manages GGML context and tensor operations
  - Coordinates between all subsystems
  - Provides unified API for tensor operations

### 2. TensorRegistry (Shape & Operation Management)
- **File**: `include/opencog/tensor/TensorRegistry.h`, `src/TensorRegistry.cc`
- **Purpose**: Registry for managing tensor shapes and operations
- **Key Features**:
  - Registers tensor shapes for cognitive modules
  - Manages tensor operations catalog
  - Supports dynamic composition of shapes
  - Export/import functionality for tensor catalogs

### 3. AtomSpaceTensorMapper (Neural-Symbolic Bridge)
- **File**: `include/opencog/tensor/AtomSpaceTensorMapper.h`, `src/AtomSpaceTensorMapper.cc`
- **Purpose**: Maps AtomSpace hypergraph structures to GGML tensors
- **Key Features**:
  - Bidirectional conversion between atoms and tensors
  - Hypergraph pattern encoding
  - Neural-symbolic transformation rules
  - Batch operations for efficient processing

### 4. AttentionAllocator (ECAN Integration)
- **File**: `include/opencog/tensor/AttentionAllocator.h`, `src/AttentionAllocator.cc`
- **Purpose**: Implements ECAN as adaptive tensor attention masks
- **Key Features**:
  - Economic attention flow simulation
  - Attention context management
  - Membrane boundary routing (P-System inspired)
  - Adaptive attention based on feedback

### 5. GrammarRegistry (Agentic Functions)
- **File**: `include/opencog/tensor/GrammarRegistry.h`, `src/GrammarRegistry.cc`
- **Purpose**: Dynamic grammar registry for agentic functions
- **Key Features**:
  - Grammar function registration and execution
  - Agentic kernel management
  - Compositional grammar creation
  - Integration with standard OpenCog modules

### 6. DistributedExecutor (Distributed Processing)
- **File**: `include/opencog/tensor/DistributedExecutor.h`, `src/DistributedExecutor.cc`
- **Purpose**: Distributed execution and sharding for GGML operations
- **Key Features**:
  - Tensor sharding across nodes
  - Load balancing and fault tolerance
  - Parallel execution coordination
  - Network communication abstraction

### 7. NeuralSymbolicBridge (Integration Layer)
- **File**: `include/opencog/tensor/NeuralSymbolicBridge.h`, `src/NeuralSymbolicBridge.cc`
- **Purpose**: Neural-symbolic integration using Scheme macros and tensor operations
- **Key Features**:
  - Transformation rule management
  - Scheme macro system integration
  - Bidirectional reasoning capabilities
  - Emergent pattern detection

## Standard Tensor Shapes

The system defines standard tensor shapes for core OpenCog modules:

```cpp
// Core utilities tensor field: [64, 32, 16]
COGUTIL_SHAPE

// AtomSpace hypergraph tensor field: [1024, 512, 256]  
ATOMSPACE_SHAPE

// Distributed cognitive server tensor field: [128, 64, 32]
COGSERVER_SHAPE

// RelEx NLP tensor field: [256, 128, 64]
RELEX_SHAPE

// PLN reasoning tensor field: [512, 256, 128]
PLN_SHAPE

// ECAN attention tensor field: [256, 128, 64]
ECAN_SHAPE

// MOSES evolution tensor field: [512, 256, 128]
MOSES_SHAPE

// GHOST chat engine tensor field: [256, 128, 64]
GHOST_SHAPE

// Loving AI tensor field: [128, 64, 32]
LOVING_AI_SHAPE

// Game AI tensor field: [256, 128, 64]
GAME_AI_SHAPE
```

## Scheme Bindings

The module provides comprehensive Scheme bindings through `scheme/tensor-kernel.scm`:

### Core Functions
- `(initialize-tensor-kernel)` - Initialize with standard modules
- `(tensor-map-atoms atoms)` - Map atoms to tensor representation
- `(tensor-allocate-attention atoms context)` - Allocate attention using tensors
- `(tensor-execute-grammar function-name . args)` - Execute grammar functions

### Neural-Symbolic Integration
- `(symbolic-to-neural atom)` - Convert atom to tensor
- `(neural-to-symbolic tensor-handle)` - Convert tensor to atom

### Attention Allocation
- `(ecan-allocate-attention atoms)` - ECAN-style attention allocation
- `(focus-attention atoms focus-type)` - Focus attention with specific type

### Pattern Matching
- `(neural-pattern-match pattern target)` - Pattern match with neural guidance
- `(detect-emergent-patterns atoms)` - Detect emergent patterns

### Cognitive Flows
- `(cognitive-flow-execute flow-name atoms)` - Execute predefined cognitive flows
- `(compose-cognitive-grammars . grammar-names)` - Compose multiple grammars

## Usage Example

```scheme
;; Initialize the tensor kernel
(define kernel (initialize-tensor-kernel))

;; Create some atoms
(define concept-a (ConceptNode "A"))
(define concept-b (ConceptNode "B"))
(define atoms (list concept-a concept-b))

;; Map atoms to tensor representation
(define tensor-repr (tensor-map-atoms atoms))

;; Allocate attention using ECAN
(define attention-mask (ecan-allocate-attention atoms))

;; Execute a cognitive flow
(define result (cognitive-flow-execute 'reasoning-chain atoms))

;; Check kernel status
(print-tensor-kernel-info)
```

## Build Instructions

The tensor kernel integrates with the main OpenCog unified build system:

```bash
cd /path/to/opencog-unified
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4
```

The tensor kernel will be built automatically as part of the unified system.

## Dependencies

- **GGML**: Tensor computation library (automatically fetched)
- **OpenCog CogUtil**: Core utilities
- **OpenCog AtomSpace**: Knowledge representation
- **Guile**: Scheme interpreter for bindings
- **OpenMP**: Parallel processing support

## Architecture Principles

1. **Minimal Changes**: The tensor kernel extends existing OpenCog modules without modifying their core functionality
2. **Tensor-First Design**: All cognitive operations are expressed as tensor computations
3. **Neural-Symbolic Integration**: Seamless bidirectional conversion between symbolic and neural representations
4. **Distributed by Design**: Built-in support for distributed execution and sharding
5. **Attention-Aware**: ECAN attention allocation integrated at the tensor level
6. **Grammar-Driven**: Dynamic grammar registry enables compositional cognitive operations
7. **Emergent Patterns**: Support for detecting and utilizing emergent cognitive patterns

## Future Enhancements

- Integration with GPU backends for GGML
- Advanced neural-symbolic transformation learning
- Real-time distributed cognitive processing
- Enhanced pattern recognition and emergence detection
- Quantum-inspired tensor operations
- Advanced fault tolerance and recovery mechanisms

## Status

This is the initial implementation providing the foundational architecture for unified tensor-based cognitive processing. The current version establishes the framework and basic functionality, with full implementation of all components planned for subsequent releases.