# CogZero - Agent-Zero C++ Implementation for OpenCog

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL%203.0-blue.svg)](LICENSE)
[![C++17](https://img.shields.io/badge/C++-17-blue.svg)](https://en.cppreference.com/w/cpp/17)
[![OpenCog](https://img.shields.io/badge/OpenCog-Integration-green.svg)](https://opencog.org)

**CogZero** is a high-performance C++ implementation of Agent-Zero, specially optimized for integration with the OpenCog cognitive architecture. It serves as an orchestration workbench system that provides a modular catalog of powerful cognitive tools, skills, abilities, and knowledge enhancements.

## 🎯 Project Vision

CogZero implements the **AGENT-ZERO-GENESIS** initiative, creating a C++ variant of Agent-Zero that provides optimal grip and maximum effectiveness within OpenCog's cognitive architecture. The system creates a well-orchestrated and modular catalog of powerful tools, skills, abilities and knowledge enhancements as a coherent and integrated whole.

## 🏗️ Architecture

CogZero is organized into 9 specialized modules, each optimized for specific cognitive functions:

### Core Modules

| Module | Purpose | Key Dependencies |
|--------|---------|------------------|
| **[agentzero-core](agentzero-core/)** | Main orchestration engine & cognitive loop | cogutil, atomspace, cogserver |
| **[agentzero-perception](agentzero-perception/)** | Multi-modal sensory processing | sensory, vision, perception |
| **[agentzero-planning](agentzero-planning/)** | Hierarchical planning & goal management | spacetime, cogserver |
| **[agentzero-learning](agentzero-learning/)** | Continuous learning & adaptation | moses, asmoses, learn |
| **[agentzero-memory](agentzero-memory/)** | Memory & context management | atomspace-rocks, attention |
| **[agentzero-communication](agentzero-communication/)** | NLP & multi-agent communication | lg-atomese, opencog |
| **[agentzero-tools](agentzero-tools/)** | External tool integration | external-tools, ros-behavior-scripting |
| **[agentzero-distributed](agentzero-distributed/)** | Distributed computing support | Network protocols, clustering |
| **[agentzero-python-bridge](agentzero-python-bridge/)** | Python interoperability | Python C API, pybind11 |

### Module Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                     agentzero-core                          │
│              (Orchestration & Cognitive Loop)               │
└─────────────────────────────────────────────────────────────┘
                            │
        ┌───────────────────┼───────────────────┐
        │                   │                   │
┌───────▼────────┐  ┌──────▼──────┐  ┌────────▼────────┐
│  perception    │  │   planning   │  │   learning      │
└───────┬────────┘  └──────┬───────┘  └────────┬────────┘
        │                  │                    │
        └──────────────────┼────────────────────┘
                           │
        ┌──────────────────┼──────────────────┐
        │                  │                  │
┌───────▼────────┐  ┌─────▼──────┐  ┌───────▼────────┐
│  memory        │  │ communication│  │    tools       │
└────────────────┘  └──────────────┘  └────────────────┘
        │                  │                  │
        └──────────────────┼──────────────────┘
                           │
        ┌──────────────────┴──────────────────┐
        │                                     │
┌───────▼────────┐              ┌────────────▼────────┐
│  distributed   │              │  python-bridge      │
└────────────────┘              └─────────────────────┘
```

## 🚀 Quick Start

### Prerequisites

**Required:**
- CMake 3.16+
- C++17 compatible compiler (GCC 7+, Clang 5+, MSVC 2017+)
- Boost 1.46+
- OpenCog core components:
  - [cogutil](https://github.com/opencog/cogutil)
  - [atomspace](https://github.com/opencog/atomspace)
  - [cogserver](https://github.com/opencog/cogserver)

**Optional (for specific modules):**
- PLN (Probabilistic Logic Networks)
- URE (Unified Rule Engine)
- MOSES (Meta-Optimizing Semantic Evolutionary Search)
- Pattern Miner
- Link Grammar

### Installation

```bash
# Clone the repository
git clone https://github.com/o9nn/cogzero.git
cd cogzero

# Create build directory
mkdir build && cd build

# Configure with CMake
cmake ..

# Build the system
make -j$(nproc)

# Run tests
make test

# Install (optional)
sudo make install
```

### Build Options

```bash
# Debug build with symbols
cmake -DCMAKE_BUILD_TYPE=Debug ..

# Release build with optimizations
cmake -DCMAKE_BUILD_TYPE=Release ..

# Without examples
cmake -DBUILD_EXAMPLES=OFF ..

# Without tests
cmake -DBUILD_TESTING=OFF ..

# With documentation (requires Doxygen)
cmake -DBUILD_DOCS=ON ..

# Custom install prefix
cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..
```

## 📚 Documentation

**📖 [Complete Documentation Index](docs/INDEX.md)** - Start here for all documentation

### Quick Links

**Getting Started:**
- [Quick Start Guide](docs/QUICK_START.md) - Get up and running in 30 minutes
- [Architecture Overview](docs/ARCHITECTURE.md) - System design and components
- [Build System Guide](BUILD_SYSTEM.md) - Detailed build instructions

**Developer Resources:**
- [API Reference](docs/API_REFERENCE.md) - Complete API documentation
- [Developer Guide](docs/DEVELOPER_GUIDE.md) - Development workflow and standards
- [Integration Guide](docs/INTEGRATION_GUIDE.md) - OpenCog integration patterns
- [Code Standards](docs/CODE_STANDARDS.md) - Coding conventions

**Testing & Quality:**
- [Testing Guide](docs/TESTING_GUIDE.md) - Unit, integration, and performance testing
- [Benchmarking Guide](docs/BENCHMARKING_GUIDE.md) - Performance measurement
- [CI/CD Documentation](AGENT_ZERO_CI_README.md) - Continuous integration setup

**Operations:**
- [Deployment Guide](docs/DEPLOYMENT_GUIDE.md) - Production deployment
- [Troubleshooting Guide](docs/TROUBLESHOOTING.md) - Common issues and solutions

**Project Resources:**
- [AGENT-ZERO-GENESIS.md](AGENT-ZERO-GENESIS.md) - Complete project roadmap
- [Module READMEs](.) - Each module has detailed documentation in its subdirectory

## 🔄 Integration with OpenCog

### AtomSpace Integration
- All agent state represented as Atoms
- Knowledge structures use standard AtomSpace types
- Temporal information via TimeNodes and AtTimeLinks
- Goal hierarchies as structured Atom trees

### CogServer Integration
- Agent runs as CogServer module
- Real-time state monitoring via CogServer commands
- Network interface for distributed operation
- Debug and introspection capabilities

### Reasoning Integration
- PLN rules for inference and reasoning
- URE integration for flexible reasoning patterns
- Custom Agent-Zero reasoning rules
- Uncertainty handling through TruthValues

### Learning Integration
- MOSES integration for policy optimization
- Pattern mining for knowledge discovery
- Online learning through AtomSpace updates
- Experience replay using stored trajectories

## 🧪 Testing

### Test Structure

```
tests/
├── unit-tests/         # Unit tests for individual classes
├── integration-tests/  # Integration tests with OpenCog
├── performance-tests/  # Performance benchmarks
├── regression-tests/   # Regression test suite
├── framework/          # Testing framework utilities
└── mocks/             # Mock objects for testing
```

### Running Tests

```bash
# All tests
make test

# Specific test suite
ctest -R unit_tests

# With verbose output
ctest -V

# Performance benchmarks
ctest -R performance

# Integration tests
ctest -R integration
```

## 📊 Performance Targets

- **Response Time**: < 100ms for routine decisions
- **Memory Efficiency**: Linear scaling with knowledge base size
- **Learning Rate**: Demonstrable improvement within 1000 interactions
- **Integration Overhead**: < 10% vs. standalone Agent-Zero
- **Scalability**: Support for 10M+ Atoms in knowledge base

## 🤝 Contributing

We welcome contributions! Please see our contributing guidelines:

### Getting Started

1. **Read the Roadmap**: Understand the project vision in [AGENT-ZERO-GENESIS.md](AGENT-ZERO-GENESIS.md)
2. **Find a Task**: Browse existing issues or check the roadmap
3. **Setup Environment**: Install OpenCog dependencies and build system
4. **Submit PR**: Follow standard GitHub workflow with tests and documentation

### Code Standards

- **C++17 Standard**: Modern C++ features encouraged
- **OpenCog Patterns**: Follow established OpenCog architectural patterns
- **Documentation**: Document all public interfaces
- **Testing**: Comprehensive unit and integration tests
- **Performance**: Consider memory and CPU optimization

See [CODE_STANDARDS.md](docs/CODE_STANDARDS.md) for detailed guidelines.

### Development Phases

| Phase | Focus | Status |
|-------|-------|--------|
| **Phase 1** | Foundation Layer | 🚧 In Progress |
| **Phase 2** | Perception & Action | 📅 Planned |
| **Phase 3** | Knowledge & Reasoning | 📅 Planned |
| **Phase 4** | Planning & Goals | 📅 Planned |
| **Phase 5** | Learning & Adaptation | 📅 Planned |
| **Phase 6** | Communication & NLP | 📅 Planned |
| **Phase 7** | Memory & Context | 📅 Planned |
| **Phase 8** | Tool Integration | 📅 Planned |
| **Phase 9** | Integration & Testing | 📅 Planned |
| **Phase 10** | Advanced Features | 📅 Planned |

## 🔮 Roadmap

### Short Term (3 months)
- [x] Foundation architecture and build system
- [ ] Core orchestration engine (Phase 1)
- [ ] Basic perception-action loop (Phase 2)
- [ ] Initial knowledge representation (Phase 3)

### Medium Term (6 months)
- [ ] Advanced reasoning and planning (Phase 4-5)
- [ ] Natural language interaction (Phase 6)
- [ ] Memory and context management (Phase 7)

### Long Term (12 months)
- [ ] Complete cognitive architecture (Phase 8-9)
- [ ] Multi-agent coordination (Phase 10)
- [ ] Python interoperability bridge
- [ ] Distributed computing integration

See [AGENT-ZERO-GENESIS.md](AGENT-ZERO-GENESIS.md) for the complete roadmap.

## 📦 Project Structure

```
cogzero/
├── CMakeLists.txt              # Main build configuration
├── README.md                   # This file
├── LICENSE                     # AGPL-3.0 license
├── AGENT-ZERO-GENESIS.md      # Complete project roadmap
├── BUILD_SYSTEM.md            # Build system documentation
├── AGENT_ZERO_CI_README.md    # CI/CD documentation
├── cmake/                      # CMake configuration files
├── docs/                       # Comprehensive documentation
├── agentzero-core/            # Core orchestration module
├── agentzero-perception/      # Perception module
├── agentzero-planning/        # Planning module
├── agentzero-learning/        # Learning module
├── agentzero-memory/          # Memory module
├── agentzero-communication/   # Communication module
├── agentzero-tools/           # Tools integration module
├── agentzero-distributed/     # Distributed computing module
├── agentzero-python-bridge/   # Python bridge module
├── tests/                     # Test suites
├── profiling/                 # Profiling tools
├── examples/                  # Example applications
└── .github/                   # GitHub workflows and configs
```

## 🆘 Troubleshooting

For comprehensive troubleshooting information, see the [Troubleshooting Guide](docs/TROUBLESHOOTING.md).

### Quick Fixes

**Build fails with "cogutil not found"**
```bash
# Install OpenCog dependencies first
# See: https://github.com/opencog/cogutil
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
```

**CMake configuration errors**
```bash
# Ensure CMake 3.16+ is installed
cmake --version

# Check C++17 compiler support
g++ --version  # GCC 7+ required
```

**Runtime errors with AtomSpace**
- Check AtomSpace initialization
- Verify proper Handle usage and memory management
- Ensure thread-safe operations where needed

See [Troubleshooting Guide](docs/TROUBLESHOOTING.md) for detailed solutions.

## 📄 License

This project is licensed under the **AGPL-3.0-or-later** license. See [LICENSE](LICENSE) for details.

## 🙏 Acknowledgments

- **OpenCog Foundation** - For the cognitive architecture framework
- **Agent-Zero Project** - For the original agent architecture
- **Contributors** - All developers who have contributed to this project

## 📞 Contact & Support

- **GitHub Issues**: [Report bugs and request features](https://github.com/o9nn/cogzero/issues)
- **Documentation**: Check [docs/](docs/) directory for comprehensive guides
- **OpenCog Community**: Join the [OpenCog discussion forums](https://wiki.opencog.org/)

## 🔗 Related Projects

- [OpenCog](https://github.com/opencog) - Cognitive architecture framework
- [AtomSpace](https://github.com/opencog/atomspace) - Hypergraph knowledge representation
- [CogServer](https://github.com/opencog/cogserver) - Network server for AtomSpace
- [pycog0](https://github.com/o9nn/pycog0) - Parent repository with full OpenCog ecosystem

---

**CogZero** is part of the AGENT-ZERO-GENESIS initiative to create a C++ variant of Agent-Zero optimized for OpenCog integration as an orchestration workbench system.

*Built with ❤️ for the OpenCog community*
