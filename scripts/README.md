# OpenCog Unified Scripts Directory

This directory contains all automation scripts for the OpenCog Unified repository, organized by functional area.

## Directory Structure

```
scripts/
├── analysis/           # Analysis and metrics scripts
├── bootstrap/          # Integration and setup scripts  
├── build/              # Build optimization scripts
├── demo/               # Demonstration scripts
├── entelechy/          # Entelechy framework scripts
├── fixme/              # FIXME resolution tools
├── implementation/     # Implementation automation
└── testing/            # Test execution scripts
```

## Quick Navigation

### 🔍 Analysis Scripts (`analysis/`)
Tools for analyzing code quality, dependencies, and tracking metrics.
- [Analysis Scripts README](analysis/README.md)
- Placeholder analysis, dependency tracking, code quality metrics

### 🚀 Bootstrap Scripts (`bootstrap/`)
Integration and setup scripts for initializing the development environment.
- [Bootstrap Scripts README](bootstrap/README.md)
- **`integrate-components.sh`** - Main component integration script (symlinked to root)
- Repository initialization and component integration

### 🔨 Build Scripts (`build/`)
Build optimization and validation tools.
- [Build Scripts README](build/README.md)
- Build optimization, CMake improvements, validation

### 🎯 Demo Scripts (`demo/`)
Demonstration and visualization scripts.
- [Demo Scripts README](demo/README.md)
- Live demos, roadmap tracking

### ✨ Entelechy Scripts (`entelechy/`)
Entelechy framework tools for marker management and system coherence.
- [Entelechy Scripts README](entelechy/README.md)
- Marker analysis, resolution, introspection

### 🛠️ FIXME Tools (`fixme/`)
FIXME resolution workflow tools and Makefile targets.
- `Makefile.fixme` - Makefile targets for FIXME resolution

### ⚙️ Implementation Scripts (`implementation/`)
Automation for implementing fixes and generating documentation.
- [Implementation Scripts README](implementation/README.md)
- Placeholder implementation, fix automation, documentation generation

### 🧪 Testing Scripts (`testing/`)
Comprehensive test suites for all integration phases.
- [Testing Scripts README](testing/README.md)
- Phase-specific tests, component validation, integration testing

## Most Frequently Used Scripts

### Component Integration
```bash
# From repository root (symlinked for convenience)
./integrate-components.sh all              # Integrate all components
./integrate-components.sh 1                # Integrate Phase 1
```

### Validation
```bash
# From repository root
./validate-integration.py                  # Full validation
./validate-integration.py --phase 1        # Phase-specific
```

### Testing
```bash
# Phase tests
./scripts/testing/test-phase-ii-comprehensive.sh
./scripts/testing/test-phase-iii-validation.sh
./scripts/testing/test-phase-iv-comprehensive.sh

# Component tests
./scripts/testing/test_atomspace_rocks_integration.py
```

### Analysis
```bash
# Code quality analysis
python3 scripts/analysis/analyze_placeholders.py
python3 scripts/analysis/analyze_cpp_fixmes.py
python3 scripts/analysis/dependency_analyzer.py
```

### Build Optimization
```bash
./scripts/build/build.sh                   # Optimized build
python3 scripts/build/build_optimizer.py   # Analyze build
```

## Script Conventions

### Naming
- `analyze_*.py` - Analysis scripts
- `implement_*.py` - Implementation automation
- `generate_*.py` - Report/documentation generation
- `test-*.sh` - Test execution scripts
- `*-demo.py` - Demonstration scripts

### Execution
- **Shell scripts**: Must be made executable (`chmod +x`)
- **Python scripts**: Run with `python3 script.py`
- **Working directory**: Always run from repository root unless documented otherwise

### Output Locations
- Analysis results → `data/analysis-results/`
- Test results → `data/test-results/`
- Reports → `docs/reports/`
- Generated docs → Appropriate `docs/` subdirectory

## Prerequisites

### Python Scripts
```bash
pip3 install -r requirements.txt
```

### Shell Scripts
- bash >= 4.0
- Standard Unix utilities (grep, sed, awk, etc.)

### Component Scripts
Some scripts require specific components to be built:
- Test scripts require component binaries in `build/`
- Analysis scripts work on source code only

## Development Workflow

### Daily Development
1. Run analysis to understand current state
2. Make code changes
3. Run relevant test suite
4. Generate progress report

### Weekly Maintenance
1. Run comprehensive analysis
2. Update TODO/FIXME tracking
3. Review and apply quick wins
4. Sync with upstream fixes

### Release Preparation
1. Run all phase tests
2. Generate comprehensive reports
3. Update documentation
4. Validate integration

## Integration with Main Workflow

These scripts integrate with the main development workflow:

1. **Bootstrap** → `scripts/bootstrap/integrate-components.sh`
2. **Build** → `scripts/build/build.sh`
3. **Test** → `scripts/testing/test-phase-*.sh`
4. **Validate** → `./validate-integration.py`
5. **Analyze** → `scripts/analysis/*.py`
6. **Report** → `scripts/implementation/generate_progress_report.py`

## Documentation References

- [QUICK-START.md](../QUICK-START.md) - Quick reference for common workflows
- [DEVELOPMENT-ROADMAP.md](../DEVELOPMENT-ROADMAP.md) - 20-week integration plan
- [docs/guides/](../docs/guides/) - Implementation guides
- [docs/entelechy/](../docs/entelechy/) - Entelechy framework docs

## Maintenance

When adding new scripts:
1. Place in appropriate category directory
2. Update category README
3. Follow naming conventions
4. Document prerequisites and usage
5. Update this index if creating new categories

---

**OpenCog Unified** - Comprehensive cognitive architecture for AGI development

