# OpenCog Unified Tensor Dimensions

This document defines the tensor field dimensions for all cognitive layers in the OpenCog unified architecture, including the build dependency chain and integration points for autogenetic inference.

## Build Dependency Chain

```
cogutil (Foundation)
    │
    ├── atomspace (Core)
    │       │
    │       ├── atomspace-storage (Core Storage)
    │       │       │
    │       │       └── atomspace-rocks (Core Persistence)
    │       │               │
    │       │               └── cogserver (Network)
    │       │                       │
    │       │                       └── cogzero (Autogenetic)
    │       │                               │
    │       │                               └── atenspace (Tensor Bridge)
    │       │                                       │
    │       │                                       └── hypermind (Distributed)
    │       │                                               │
    │       │                                               └── entelechy (Actualization)
    │       │
    │       ├── ure (Logic)
    │       │       │
    │       │       ├── pln (Probabilistic Logic)
    │       │       │
    │       │       └── miner (Pattern Mining)
    │       │
    │       ├── unify (Unification)
    │       │
    │       └── attention (Attention Allocation)
    │
    └── moses (Program Evolution)
```

## Tensor Dimension Tables

### Foundation Layer

| Component | Tensor Shape | DOF | Role |
|-----------|-------------|-----|------|
| cogutil | [512, 128, 8] | 524,288 | Foundation utilities |
| moses | [512, 128, 8] | 524,288 | Program evolution |

**Total Foundation DOF:** 1,048,576

### Core Layer

| Component | Tensor Shape | DOF | Role |
|-----------|-------------|-----|------|
| atomspace | [1024, 256, 16, 4] | 16,777,216 | Hypergraph substrate |
| atomspace-storage | [896, 224, 14, 4] | 11,239,424 | Storage abstraction |
| atomspace-rocks | [768, 192, 12] | 1,769,472 | RocksDB persistence |

**Total Core DOF:** 29,786,112

### Logic Layer

| Component | Tensor Shape | DOF | Role |
|-----------|-------------|-----|------|
| ure | [768, 192, 12] | 1,769,472 | Unified Rule Engine |
| unify | [640, 160, 10] | 1,024,000 | Unification engine |
| pln | [896, 224, 14, 7] | 19,668,992 | Probabilistic Logic Networks |
| miner | [768, 192, 12, 6] | 10,616,832 | Pattern mining |

**Total Logic DOF:** 33,079,296

### Cognitive Layer

| Component | Tensor Shape | DOF | Role |
|-----------|-------------|-----|------|
| attention | [512, 128, 8, 2] | 1,048,576 | Attention allocation |
| spacetime | [896, 224, 14] | 2,809,856 | Temporal reasoning |
| cogserver | [640, 160, 8, 2] | 1,638,400 | Network substrate |
| learn | [1024, 256, 16, 8] | 33,554,432 | Learning system |
| language-learning | [768, 192, 12, 6] | 10,616,832 | NLP learning |
| asmoses | [640, 160, 10, 5] | 5,120,000 | AS-MOSES |
| sensory | [512, 128, 8, 4] | 2,097,152 | Sensory interface |

**Total Cognitive DOF:** 56,885,248

### Integration Layer

| Component | Tensor Shape | DOF | Role |
|-----------|-------------|-----|------|
| opencog | [2048, 512, 32, 16, 8] | 4,294,967,296 | Full integration |

**Total Integration DOF:** 4,294,967,296

### Autogenetic Layer (NEW)

| Component | Tensor Shape | DOF | Role |
|-----------|-------------|-----|------|
| cogzero | [1536, 384, 24, 12, 6] | 1,019,215,872 | Agent-zero neural substrate |
| atenspace | [1280, 320, 20, 10, 5] | 409,600,000 | ATen tensor-atomspace bridge |
| hypermind | [1792, 448, 28, 14, 7] | 1,107,296,256 | Distributed neural reactor |
| entelechy | [2560, 640, 40, 20, 10, 5] | 6,553,600,000 | Entelechy actualization engine |
| cognitive-das | [1024, 256, 16, 8, 4] | 134,217,728 | Distributed AtomSpace |
| cognitive-gnn | [896, 224, 14, 7] | 19,668,992 | Graph neural networks |

**Total Autogenetic DOF:** 9,243,598,848

## Grand Total

| Layer | DOF |
|-------|-----|
| Foundation | 1,048,576 |
| Core | 29,786,112 |
| Logic | 33,079,296 |
| Cognitive | 56,885,248 |
| Integration | 4,294,967,296 |
| Autogenetic | 9,243,598,848 |
| **TOTAL** | **13,659,365,376** |

## OEIS A000081 Nested Shell Structure

The architecture follows the OEIS A000081 sequence (number of rooted trees with n nodes):

| Nesting Level | Terms | Components |
|---------------|-------|------------|
| N=1 | 1 | cogutil |
| N=2 | 3 | atomspace → atomspace-storage → atomspace-rocks |
| N=3 | 4 | ure, unify, pln, miner |
| N=4 | 9 | attention, spacetime, cogserver, learn, language-learning, asmoses, moses, opencog, sensory |
| N=5 | 6 | cogzero, atenspace, hypermind, entelechy, cognitive-das, cognitive-gnn |

## Autogenetic Inference Integration Points

### CogZero (Agent-Zero Neural Substrate)

| Interface | Tensor Shape | Role |
|-----------|-------------|------|
| agent-core | [1536, 384, 24, 12, 6] | Core agent processing |
| communication | [768, 192, 12, 6] | Multi-agent coordination |
| action-executor | [512, 128, 8, 4] | Action planning |
| memory-interface | [1024, 256, 16, 8] | Memory access patterns |

### ATenSpace (Tensor-AtomSpace Bridge)

| Interface | Tensor Shape | Role |
|-----------|-------------|------|
| tensor-atoms | [1280, 320, 20, 10, 5] | Tensor embeddings for atoms |
| similarity | [640, 160, 10, 5] | Semantic similarity |
| ecan | [512, 128, 8, 4] | Economic attention |
| pln-bridge | [896, 224, 14, 7] | Tensor-accelerated PLN |

### HyperMind (Distributed Neural Reactor)

| Interface | Tensor Shape | Role |
|-----------|-------------|------|
| neural-reactor | [1792, 448, 28, 14, 7] | Core neural computation |
| session-manager | [896, 224, 14, 7] | Session orchestration |
| command-system | [512, 128, 8, 4] | Async command execution |
| distributed | [2048, 512, 32, 16] | Distributed computation |

## 12-Step Cognitive Loop

The autogenetic inference loop runs 3 concurrent streams, phased 120° apart:

| Step | Stream 1 | Stream 2 | Stream 3 |
|------|----------|----------|----------|
| 1 | perception | - | - |
| 2 | - | action | - |
| 3 | - | - | simulation |
| 4 | integration | - | - |
| 5 | - | perception | - |
| 6 | - | - | action |
| 7 | simulation | - | - |
| 8 | - | integration | - |
| 9 | - | - | perception |
| 10 | action | - | - |
| 11 | - | simulation | - |
| 12 | - | - | integration |

## Agent-Arena-Relation (AAR) Self-Image

| Component | Representation | Tensor Shape |
|-----------|---------------|-------------|
| Agent | Dynamic tensor transformations | [1536, 384, 24, 12, 6] |
| Arena | Base manifold state space | [2048, 512, 32, 16, 8] |
| Relation | Recurrent attentional feedback | [1280, 320, 20, 10, 5] |

## Entelechy Metrics

Entelechy = actualization of potential (actual DOF / potential DOF)

| Level | Threshold | Description |
|-------|-----------|-------------|
| latent | 0% | No active components |
| emerging | 10% | Foundation layer active |
| developing | 25% | Core layer active |
| maturing | 50% | Logic layer active |
| actualizing | 75% | Cognitive layer active |
| realized | 90% | Integration layer active |
| transcendent | 100% | Full autogenetic actualization |

---

*Generated by OpenCog Unified Autognostic Ontogenesis System*
