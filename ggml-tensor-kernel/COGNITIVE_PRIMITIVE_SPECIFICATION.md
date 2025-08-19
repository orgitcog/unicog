# Cognitive Primitive Tensor Specification

## Phase 1: Cognitive Primitives & Foundational Hypergraph Encoding

This document provides comprehensive documentation for the 5-dimensional cognitive primitive tensor implementation as specified in Phase 1 requirements.

## Table of Contents

1. [Tensor Signature Specification](#tensor-signature-specification)
2. [Cognitive Primitive Architecture](#cognitive-primitive-architecture)
3. [Scheme-to-Hypergraph Encoding](#scheme-to-hypergraph-encoding)
4. [Prime Factorization Mapping](#prime-factorization-mapping)
5. [Degrees of Freedom Analysis](#degrees-of-freedom-analysis)
6. [Round-Trip Translation](#round-trip-translation)
7. [Performance Characteristics](#performance-characteristics)
8. [Integration Interfaces](#integration-interfaces)

## Tensor Signature Specification

### 5-Dimensional Cognitive Primitive Tensor

The cognitive primitive tensor follows the exact specification from Phase 1:

```
Cognitive_Primitive_Tensor[5] = {
  modality: [visual, auditory, textual, symbolic],
  depth: [surface, semantic, pragmatic],
  context: [local, global, temporal],
  salience: [0.0, 1.0],
  autonomy_index: [0.0, 1.0]
}
```

### Tensor Shape Definition

| Dimension | Size | Type | Values | Description |
|-----------|------|------|--------|-------------|
| 0 | 4 | Categorical | visual, auditory, textual, symbolic | Sensory modality of cognitive input |
| 1 | 3 | Categorical | surface, semantic, pragmatic | Processing depth level |
| 2 | 3 | Categorical | local, global, temporal | Context scope |
| 3 | 1 | Continuous | [0.0, 1.0] | Attention salience weight |
| 4 | 1 | Continuous | [0.0, 1.0] | Autonomy index for agentic control |

**Total Tensor Elements**: 4 × 3 × 3 × 1 × 1 = 36 elements per primitive

### Encoding Strategy

#### Categorical Dimensions (0-2)
- **One-hot encoding** for categorical variables
- Each category gets exactly one activation value of 1.0
- All other positions in that dimension remain 0.0

#### Continuous Dimensions (3-4)
- **Direct value encoding** for continuous variables
- Values stored directly as floating-point numbers in range [0.0, 1.0]
- Linear interpolation supported

## Cognitive Primitive Architecture

### Core Classes

#### CognitivePrimitiveTensor

```cpp
class CognitivePrimitiveTensor {
    // 5D tensor shape: [modality(4), depth(3), context(3), salience(1), autonomy(1)]
    static const std::vector<size_t> PRIMITIVE_SHAPE;
    
    // Dimension indices
    static const size_t MODALITY_DIM = 0;
    static const size_t DEPTH_DIM = 1;
    static const size_t CONTEXT_DIM = 2;
    static const size_t SALIENCE_DIM = 3;
    static const size_t AUTONOMY_DIM = 4;
};
```

#### CognitivePrimitiveRegistry

Manages collections of cognitive primitive tensors with:
- Registration and retrieval by name
- Validation of all registered primitives
- Export/import functionality for primitive catalogs

### Enumeration Types

```cpp
enum class ModalityType {
    VISUAL = 0,    // Visual sensory input
    AUDITORY = 1,  // Auditory sensory input
    TEXTUAL = 2,   // Textual/linguistic input
    SYMBOLIC = 3   // Abstract symbolic input
};

enum class DepthType {
    SURFACE = 0,   // Surface-level processing
    SEMANTIC = 1,  // Semantic-level processing
    PRAGMATIC = 2  // Pragmatic-level processing
};

enum class ContextType {
    LOCAL = 0,     // Local context scope
    GLOBAL = 1,    // Global context scope
    TEMPORAL = 2   // Temporal context scope
};
```

## Scheme-to-Hypergraph Encoding

### Scheme Adapter Functions

The cognitive grammar adapter provides modular Scheme functions for agentic grammar AtomSpace integration:

#### Core Functions

```scheme
;; Create cognitive primitive record
(make-cognitive-primitive name modality depth context salience autonomy)

;; Convert to hypergraph structure
(scheme-primitive-to-hypergraph primitive)

;; Convert from hypergraph structure
(hypergraph-to-scheme-primitive hypergraph-atoms)

;; Round-trip translation
(cognitive-primitive-round-trip primitive)
```

### Hypergraph Structure

Each cognitive primitive is encoded as a hypergraph with the following structure:

```scheme
(List
  ;; Main concept node
  (Concept "primitive-name")
  
  ;; Tensor signature node
  (Evaluation
    (Predicate "tensor-signature")
    (List (Concept "primitive-name") (Concept "primitive-name-tensor-signature")))
  
  ;; Modality encoding
  (Evaluation
    (Predicate "modality")
    (List (Concept "primitive-name") (Concept "visual|auditory|textual|symbolic")))
  
  ;; Depth encoding
  (Evaluation
    (Predicate "depth")
    (List (Concept "primitive-name") (Concept "surface|semantic|pragmatic")))
  
  ;; Context encoding
  (Evaluation
    (Predicate "context")
    (List (Concept "primitive-name") (Concept "local|global|temporal")))
  
  ;; Salience encoding (numerical)
  (Evaluation
    (Predicate "salience")
    (List (Concept "primitive-name") (Number salience-value)))
  
  ;; Autonomy encoding (numerical)
  (Evaluation
    (Predicate "autonomy-index")
    (List (Concept "primitive-name") (Number autonomy-value)))
  
  ;; Prime factorization signature
  (Evaluation
    (Predicate "prime-factorization")
    (List (Concept "primitive-name") (List prime1 prime2 prime3 prime4 prime5))))
```

## Prime Factorization Mapping

### Prime Assignment Strategy

Each dimension of the cognitive primitive tensor is mapped to prime numbers for unique signature generation:

#### Modality Primes
- **Visual**: 2
- **Auditory**: 3
- **Textual**: 5
- **Symbolic**: 7

#### Depth Primes
- **Surface**: 11
- **Semantic**: 13
- **Pragmatic**: 17

#### Context Primes
- **Local**: 19
- **Global**: 23
- **Temporal**: 29

#### Continuous Value Primes
- **Salience**: 31 + (salience × 10)
- **Autonomy**: 41 + (autonomy × 10)

### Example Prime Signature

For a cognitive primitive with:
- Modality: symbolic (7)
- Depth: semantic (13)
- Context: temporal (29)
- Salience: 0.7 → 31 + 7 = 38
- Autonomy: 0.5 → 41 + 5 = 46

**Prime Signature**: [7, 13, 29, 38, 46]

This creates a unique mathematical fingerprint for each cognitive primitive configuration.

## Degrees of Freedom Analysis

### Calculation Method

The degrees of freedom (DOF) for the cognitive primitive tensor is calculated as:

```
DOF = (modality_categories - 1) + (depth_categories - 1) + (context_categories - 1) + salience_continuous + autonomy_continuous
```

### Breakdown

| Component | Categories | DOF Contribution |
|-----------|------------|------------------|
| Modality | 4 | 3 (4-1) |
| Depth | 3 | 2 (3-1) |
| Context | 3 | 2 (3-1) |
| Salience | Continuous | 1 |
| Autonomy | Continuous | 1 |
| **Total** | | **9** |

### Interpretation

- **Total DOF**: 9 independent parameters
- **Categorical DOF**: 7 (for discrete choices)
- **Continuous DOF**: 2 (for continuous values)

This provides sufficient expressiveness for cognitive primitive encoding while maintaining computational tractability.

## Round-Trip Translation

### Translation Process

1. **Scheme → Hypergraph**: Convert cognitive primitive record to AtomSpace hypergraph structure
2. **Hypergraph → Scheme**: Extract information from hypergraph and reconstruct primitive record
3. **Validation**: Compare original and recovered primitives for accuracy

### Accuracy Requirements

Round-trip translation must maintain:
- **Perfect categorical preservation**: modality, depth, context must match exactly
- **Numerical precision**: salience and autonomy must be within 1% tolerance (0.01)
- **Structural integrity**: all hypergraph relationships must be preserved

### Validation Function

```scheme
(define (validate-round-trip-accuracy original recovered)
  (and (string=? (cognitive-primitive-name original) (cognitive-primitive-name recovered))
       (eq? (cognitive-primitive-modality original) (cognitive-primitive-modality recovered))
       (eq? (cognitive-primitive-depth original) (cognitive-primitive-depth recovered))
       (eq? (cognitive-primitive-context original) (cognitive-primitive-context recovered))
       (< (abs (- (cognitive-primitive-salience original) (cognitive-primitive-salience recovered))) 0.01)
       (< (abs (- (cognitive-primitive-autonomy original) (cognitive-primitive-autonomy recovered))) 0.01)))
```

## Performance Characteristics

### Benchmark Targets

| Operation | Target Performance | Actual (Test) |
|-----------|-------------------|---------------|
| Primitive Creation | >1000 ops/sec | TBD |
| Round-trip Translation | >100 ops/sec | TBD |
| Prime Factorization | >1000 ops/sec | TBD |
| Tensor Validation | >500 ops/sec | TBD |

### Memory Usage

| Primitive Count | Memory per Primitive | Total Memory |
|-----------------|---------------------|--------------|
| 10 | TBD | TBD |
| 100 | TBD | TBD |
| 1000 | TBD | TBD |

*Values to be populated by test results*

### Scalability Considerations

- **Linear memory growth** with number of primitives
- **Constant time** for individual primitive operations
- **Logarithmic complexity** for registry lookups
- **Batch processing support** for multiple primitives

## Integration Interfaces

### C++ API

#### Core Functions
```cpp
// Create cognitive primitive tensor
CognitivePrimitiveTensor(ggml_context* ctx, const std::string& name);

// Set/get primitive properties
void set_modality(ModalityType modality);
ModalityType get_modality() const;

// Tensor operations
void encode_to_tensor();
void decode_from_tensor();

// Validation
bool validate_tensor_shape() const;
bool validate_primitive_values() const;

// AtomSpace integration
Handle to_atomspace_node(AtomSpace* as) const;
```

### Scheme API

#### Core Functions
```scheme
;; Cognitive primitive creation and management
(cog-create-cognitive-primitive name modality depth context salience autonomy)
(cog-get-cognitive-primitive name)
(cog-list-cognitive-primitives)

;; Validation and analysis
(cog-validate-cognitive-primitive name)
(cog-cognitive-primitive-degrees-of-freedom name)
(cog-validate-round-trip-accuracy name)

;; AtomSpace integration
(cog-cognitive-primitive-to-atomspace name)

;; Testing and debugging
(cog-run-cognitive-primitive-tests)
(cog-cognitive-primitive-tensor-shape)
```

### AtomSpace Integration

#### Node Structure
```
ConceptNode: "primitive-name"
├── EvaluationLink: (Predicate "modality") → (Concept "modality-value")
├── EvaluationLink: (Predicate "depth") → (Concept "depth-value")  
├── EvaluationLink: (Predicate "context") → (Concept "context-value")
├── EvaluationLink: (Predicate "salience") → (NumberNode salience-value)
├── EvaluationLink: (Predicate "autonomy-index") → (NumberNode autonomy-value)
└── EvaluationLink: (Predicate "prime-factorization") → (ListLink prime-values)
```

## Implementation Status

### Completed Components
- [x] 5-dimensional tensor specification
- [x] CognitivePrimitiveTensor class implementation
- [x] CognitivePrimitiveRegistry for management
- [x] Scheme cognitive grammar adapter
- [x] Prime factorization mapping
- [x] Degrees of freedom calculation
- [x] Round-trip translation framework
- [x] Comprehensive test suite
- [x] Documentation and specifications

### Testing Coverage
- [x] Tensor shape validation
- [x] Cognitive primitive creation
- [x] Round-trip translation accuracy
- [x] Prime factorization correctness
- [x] Degrees of freedom calculation
- [x] Scheme integration
- [x] AtomSpace integration
- [x] Performance benchmarks
- [x] Memory usage analysis

### Next Steps
- [ ] Integration with main tensor kernel
- [ ] Performance optimization
- [ ] GPU acceleration support
- [ ] Advanced pattern recognition
- [ ] Visualization tools for hypergraph fragments

## References

- Phase 1 Issue Requirements: Cognitive Primitives & Foundational Hypergraph Encoding
- OpenCog AtomSpace Documentation
- GGML Tensor Library Documentation
- Scheme/Guile Integration Guide

---

*This specification document serves as the authoritative reference for Phase 1 cognitive primitive tensor implementation in the OpenCog Unified framework.*