# Navigation Guide - OpenCog Unified Repository

## 🚀 Quick Start

New to the repository? Start here:
1. **[README.md](README.md)** - Repository overview and vision
2. **[QUICK-START.md](QUICK-START.md)** - Essential commands and workflows
3. **[CONTRIBUTING.md](CONTRIBUTING.md)** - How to contribute
4. **[DEVELOPMENT-ROADMAP.md](DEVELOPMENT-ROADMAP.md)** - 20-week integration plan

## 📂 Repository Organization

### Root Level (Essential Files Only)
```
unicog/
├── README.md                    # Repository overview
├── QUICK-START.md               # Quick start guide
├── CONTRIBUTING.md              # Contribution guidelines
├── DEVELOPMENT-ROADMAP.md       # Development roadmap
├── NAVIGATION.md                # This file
├── component-config.json        # Component metadata
├── CMakeLists.txt               # Build configuration
├── validate-integration.py      # Integration validator
├── integrate-components.sh      # Symlink to scripts/bootstrap/
└── requirements.txt             # Python dependencies
```

### Documentation (`docs/`)
Comprehensive documentation organized by category.

- **[docs/INDEX.md](docs/INDEX.md)** - Complete documentation index
- **reports/** - Progress reports and implementation summaries
- **phases/** - Phase-specific documentation (5 integration phases)
- **cognitive/** - Cognitive architecture documentation
- **guides/** - Implementation guides and how-tos
- **todo-fixme/** - TODO/FIXME tracking and resolution
- **entelechy/** - Entelechy framework documentation
- **archive/** - Historical documentation and copilot memories
- **build/** - Build status and workflow documentation

[Browse all documentation →](docs/INDEX.md)

### Scripts (`scripts/`)
Automation scripts organized by functional area.

- **[scripts/README.md](scripts/README.md)** - Complete scripts guide
- **analysis/** - Code analysis and metrics (22 scripts)
- **bootstrap/** - Integration and setup (7 scripts)
- **build/** - Build optimization (6 scripts)
- **demo/** - Demonstrations (4 scripts)
- **entelechy/** - Entelechy framework tools (7 scripts)
- **fixme/** - FIXME resolution tools (Makefile)
- **implementation/** - Implementation automation (26 scripts)
- **testing/** - Test execution (27 scripts)

[Browse all scripts →](scripts/README.md)

### Data (`data/`)
Generated data, analysis results, and tracking files.

- **[data/README.md](data/README.md)** - Data management guide
- **analysis-results/** - Analysis outputs and metrics
- **cognitive-state/** - Cognitive system state snapshots
- **entelechy/** - Entelechy tracking data
- **test-results/** - Test outputs and logs
- **todo-fixme/** - TODO/FIXME tracking data

**Note**: Most files in `data/` are generated artifacts excluded from git.

[Learn more about data management →](data/README.md)

### Component Directories
OpenCog cognitive architecture components:

**Foundation Layer:**
- `cogutil/` - Core utilities (build first)
- `atomspace/` - Knowledge representation
- `cogserver/` - Distributed server

**Storage & API:**
- `atomspace-rocks/` - RocksDB backend
- `atomspace-restful/` - REST API
- `atomspace-storage/` - Storage abstractions

**Logic & Reasoning:**
- `unify/` - Pattern matching
- `ure/` - Unified Rule Engine
- `pln/` - Probabilistic Logic Networks

**Cognitive Systems:**
- `attention/` - ECAN attention allocation
- `spacetime/` - Spatial-temporal reasoning

**Learning & Optimization:**
- `moses/` - Evolutionary optimization
- `asmoses/` - AtomSpace MOSES integration
- `miner/` - Pattern mining

**Language Processing:**
- `lg-atomese/` - Link Grammar
- `learn/` - Unsupervised learning
- `language-learning/` - Language acquisition

**Integration:**
- `opencog/` - Final integration component

## 🎯 Common Tasks

### First Time Setup
```bash
# 1. Install dependencies (see QUICK-START.md)
sudo apt-get install cmake build-essential libboost-all-dev ...

# 2. Integrate components
./integrate-components.sh all

# 3. Build system
mkdir build && cd build
cmake ..
make -j$(nproc)

# 4. Validate
cd ..
./validate-integration.py
```

### Running Tests
```bash
# Phase tests (from repo root)
./scripts/testing/test-phase-ii-comprehensive.sh
./scripts/testing/test-phase-iii-validation.sh

# Component tests
python3 scripts/testing/test_atomspace_rocks_integration.py

# Comprehensive test
./scripts/testing/comprehensive-integration-test.sh
```

### Analyzing the Codebase
```bash
# Placeholder analysis
python3 scripts/analysis/analyze_placeholders.py

# Dependency analysis
python3 scripts/analysis/dependency_analyzer.py

# Test coverage
python3 scripts/analysis/analyze_test_coverage.py
```

### Working with TODO/FIXME Markers
```bash
# View current status
make -f scripts/fixme/Makefile.fixme fixme-status

# Find easy wins
make -f scripts/fixme/Makefile.fixme fixme-easy-wins

# Generate report
make -f scripts/fixme/Makefile.fixme fixme-report
```

### Building Specific Phases
```bash
# Integrate and build phase
./integrate-components.sh 1 build  # Phase 1
./integrate-components.sh 2 build  # Phase 2
# etc.
```

## 📚 Documentation Navigation

### By Role

**New Contributors:**
1. [README.md](README.md) - Start here
2. [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guide
3. [docs/guides/FOUNDATION_LAYER_GUIDE.md](docs/guides/FOUNDATION_LAYER_GUIDE.md)

**Active Developers:**
1. [QUICK-START.md](QUICK-START.md) - Quick reference
2. [docs/phases/](docs/phases/) - Phase documentation
3. [docs/guides/](docs/guides/) - Implementation guides
4. [scripts/README.md](scripts/README.md) - Script reference

**Cognitive Architecture Researchers:**
1. [docs/cognitive/](docs/cognitive/) - Architecture documentation
2. [docs/entelechy/](docs/entelechy/) - Entelechy framework
3. [docs/phases/](docs/phases/) - Integration details

### By Topic

**Integration:**
- [DEVELOPMENT-ROADMAP.md](DEVELOPMENT-ROADMAP.md) - 20-week plan
- [docs/phases/](docs/phases/) - Phase documentation
- [scripts/bootstrap/](scripts/bootstrap/) - Integration scripts

**Testing:**
- [docs/testing/TESTING_GUIDE.md](docs/testing/TESTING_GUIDE.md)
- [scripts/testing/](scripts/testing/) - Test scripts

**Build System:**
- [docs/guides/BUILD_OPTIMIZATION_GUIDE.md](docs/guides/BUILD_OPTIMIZATION_GUIDE.md)
- [scripts/build/](scripts/build/) - Build scripts

**Progress Tracking:**
- [docs/reports/](docs/reports/) - Progress reports
- [docs/todo-fixme/](docs/todo-fixme/) - TODO/FIXME tracking

## 🔧 Utility Commands

### Finding Files
```bash
# Find documentation
find docs/ -name "*.md" | grep -i "keyword"

# Find scripts
find scripts/ -name "*.py" | grep "keyword"

# Find test results
find data/test-results/ -name "*.log"
```

### Grepping Content
```bash
# Search documentation
grep -r "search term" docs/

# Search scripts
grep -r "function_name" scripts/

# Search components
grep -r "ClassName" cogutil/ atomspace/
```

## 📊 Repository Statistics

After organization:
- **Root files**: 8 essential files (down from 300+)
- **Documentation**: 158 files in organized structure
- **Scripts**: 132 files in categorical directories
- **Data**: 81 files (mostly gitignored artifacts)
- **Components**: 14+ integrated OpenCog components

## 🗺️ Directory Map

```
unicog/
├── docs/                       # 📚 All documentation
│   ├── INDEX.md               # Complete documentation index
│   ├── reports/               # Progress and summaries (33)
│   ├── phases/                # Phase docs (11)
│   ├── cognitive/             # Architecture docs (15)
│   ├── guides/                # How-to guides (12)
│   ├── todo-fixme/            # Marker tracking (45)
│   ├── entelechy/             # Entelechy framework (15)
│   ├── archive/               # Historical (19)
│   └── build/                 # Build docs (3)
├── scripts/                    # ⚙️ All automation
│   ├── README.md              # Complete scripts guide
│   ├── analysis/              # Analysis tools (22)
│   ├── bootstrap/             # Integration (7)
│   ├── build/                 # Build tools (6)
│   ├── demo/                  # Demos (4)
│   ├── entelechy/             # Entelechy tools (7)
│   ├── fixme/                 # FIXME tools (1)
│   ├── implementation/        # Automation (26)
│   └── testing/               # Tests (27)
├── data/                       # 📊 Generated data
│   ├── README.md              # Data management guide
│   ├── analysis-results/      # Analysis outputs (23)
│   ├── cognitive-state/       # State snapshots (3)
│   ├── entelechy/             # Entelechy data (7)
│   ├── test-results/          # Test outputs (16)
│   └── todo-fixme/            # Tracking data (31)
├── cogutil/                    # 🧱 Foundation: Core utilities
├── atomspace/                  # 🧱 Foundation: Knowledge graph
├── cogserver/                  # 🧱 Core: Distributed server
├── unify/                      # 🔷 Logic: Pattern matching
├── ure/                        # 🔷 Logic: Rule engine
├── pln/                        # 🔷 Logic: Probabilistic reasoning
├── attention/                  # 🧠 Cognitive: ECAN
├── spacetime/                  # 🧠 Cognitive: Temporal reasoning
├── moses/                      # 🎓 Learning: Evolutionary
├── asmoses/                    # 🎓 Learning: AS-MOSES
├── miner/                      # 🎓 Learning: Pattern mining
├── lg-atomese/                 # 📝 Language: Link Grammar
├── learn/                      # 📝 Language: Learning
├── language-learning/          # 📝 Language: Acquisition
└── opencog/                    # 🔗 Integration component
```

## 💡 Tips

- **Use tab completion** with organized directory names
- **Bookmark** `docs/INDEX.md` and `scripts/README.md`
- **Check READMEs** in each directory for detailed guidance
- **Run from root** - Most scripts expect to be run from repository root
- **Follow symlinks** - Critical scripts are symlinked to root for convenience

## 🔗 External Resources

- [OpenCog Wiki](https://wiki.opencog.org/)
- [OpenCog GitHub Organization](https://github.com/opencog/)
- [AtomSpace Documentation](https://wiki.opencog.org/w/AtomSpace)

---

**Last Updated**: 2026-02-05 - Repository organization completed
**Navigation Complexity**: Optimized for cognitive grip and relevance realization ✨
