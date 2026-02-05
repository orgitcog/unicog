# OpenCog Unified Cognitive Repository

## Cognitive Vision
Unified integration of OpenCog core components as a single monorepo, designed for ease of deployment, automation, and interactive neural-symbolic exploration. All components are directly included (no submodules) for seamless development and integration.

## Repository Structure
```
opencog-unified/
├── cogutil/                    # Core utilities
├── atomspace/                  # Knowledge representation core  
├── cogserver/                  # Distributed cognitive server
├── unify/                      # Pattern unification system
├── ure/                        # Unified Rule Engine
├── language-learning/          # Language learning components
├── moses/                      # Meta-Optimizing Semantic Evolutionary Search
├── atomspace-restful/          # RESTful API for AtomSpace
├── atomspace-rocks/            # RocksDB storage backend
├── atomspace-storage/          # Storage backends
├── cognitive-patterns/         # Pattern recognition components
├── cognitive-visualization/    # Cognitive visualization tools
├── neural-symbolic-integration/ # Neural-symbolic bridge
├── ggml-tensor-kernel/         # GGML tensor processing
├── agentic-kernels-catalog/    # Agentic AI kernels
├── distributed-cognition/      # Distributed processing
├── knowledge-base/             # Knowledge management
├── tutorial-automation/        # Interactive tutorials
├── scripts/                    # Automation & validation scripts
│   ├── analysis/              # Analysis and metrics scripts
│   ├── bootstrap/             # Integration and setup scripts
│   ├── build/                 # Build optimization scripts
│   ├── demo/                  # Demonstration scripts
│   ├── entelechy/             # Entelechy framework scripts
│   ├── implementation/        # Implementation automation
│   └── testing/               # Test execution scripts
├── tests/                      # Integration tests
├── docs/                       # Project documentation
│   ├── archive/               # Historical documentation
│   ├── cognitive/             # Cognitive architecture docs
│   ├── entelechy/             # Entelechy framework docs
│   ├── guides/                # Implementation guides
│   ├── phases/                # Phase-specific documentation
│   ├── reports/               # Progress reports and summaries
│   └── todo-fixme/            # TODO/FIXME tracking
├── data/                       # Generated data and results
│   ├── analysis-results/      # Analysis outputs
│   ├── cognitive-state/       # Cognitive state snapshots
│   ├── entelechy/             # Entelechy tracking data
│   ├── test-results/          # Test outputs and logs
│   └── todo-fixme/            # TODO/FIXME tracking data
└── documentation/              # Core documentation framework
```

## Monorepo Structure

This repository has been converted from a submodule-based structure to a unified monorepo. All components are now directly included:

- **No submodules**: All dependencies are directly integrated
- **Unified build system**: Single CMake configuration for all components  
- **Simplified development**: Clone once, build everything
- **Integrated testing**: Cross-component testing and validation

### Previously External Components (now integrated)
- `unify/` - Pattern unification system (from opencog/unify)
- `ure/` - Unified Rule Engine (from opencog/ure) 
- `language-learning/` - Language learning components (from opencog/language-learning)

## Next Steps
- Set up containerized builds (Docker)
- Configure Continuous Integration (CI/CD)
- Develop interactive chatbot tutorial
- Prototype cognitive visualization GUI

