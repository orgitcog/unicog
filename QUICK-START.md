# OpenCog Unified Quick Start Guide

## Overview
This guide provides quick instructions for working with the OpenCog Unified component integration roadmap.

## File Structure
```
opencog-unified/
├── DEVELOPMENT-ROADMAP.md      # Main development roadmap
├── component-config.json       # Component definitions and metadata
├── integrate-components.sh     # Symlink to scripts/bootstrap/integrate-components.sh
├── validate-integration.py     # Integration validation tool
├── scripts/                    # Automation & validation scripts
│   ├── analysis/              # Analysis and metrics scripts
│   ├── bootstrap/             # Integration and setup scripts
│   ├── build/                 # Build optimization scripts
│   ├── demo/                  # Demonstration scripts
│   ├── entelechy/             # Entelechy framework scripts
│   ├── implementation/        # Implementation automation
│   └── testing/               # Test execution scripts
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
├── components/                 # Integrated components directory
│   ├── core/                  # Core layer components
│   ├── logic/                 # Logic layer components  
│   ├── cognitive/             # Cognitive systems
│   ├── advanced/              # Advanced systems
│   ├── learning/              # Learning systems
│   ├── language/              # Language processing
│   └── integration/           # Final integration
├── tests/                      # Testing framework
│   ├── integration/           # Integration tests
│   ├── performance/           # Performance tests
│   └── end_to_end/            # End-to-end tests
└── documentation/              # Core documentation framework
```

## Quick Commands

### 1. Start Component Integration

#### Integrate All Components
```bash
./integrate-components.sh all
```

#### Integrate Specific Phase
```bash
./integrate-components.sh 1    # Phase 1: Core Extensions
./integrate-components.sh 2    # Phase 2: Logic Systems  
./integrate-components.sh 3    # Phase 3: Cognitive Systems
./integrate-components.sh 4    # Phase 4: Advanced & Learning
./integrate-components.sh 5    # Phase 5: Language & Integration
```

#### Integrate and Build
```bash
./integrate-components.sh all build
```

### 2. Validate Integration

#### Full Validation
```bash
./validate-integration.py
```

#### Validate Specific Phase
```bash
./validate-integration.py --phase 1
```

#### Validation with Custom Output
```bash
./validate-integration.py --output my_validation.json
```

#### Skip Build Testing (faster)
```bash
./validate-integration.py --no-build
```

### 3. Manual Build Process

#### Standard Build
```bash
mkdir build
cd build
cmake ..
make -j$(nproc)
```

#### Build with Dependencies
```bash
# Install dependencies first (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install -y \
    cmake build-essential \
    libboost-all-dev \
    python3-dev \
    guile-2.2-dev \
    librocksdb-dev
    
# Then build
mkdir build && cd build
cmake ..
make -j$(nproc)
```

## Integration Phases

### Phase 1: Core Extensions (Weeks 1-4)
**Components**: atomspace-rocks, atomspace-restful, moses
```bash
./integrate-components.sh 1
./validate-integration.py --phase 1
```

### Phase 2: Logic Systems (Weeks 5-8)  
**Components**: unify, ure, language-learning
```bash
./integrate-components.sh 2
./validate-integration.py --phase 2
```

### Phase 3: Cognitive Systems (Weeks 9-12)
**Components**: attention, spacetime
```bash
./integrate-components.sh 3
./validate-integration.py --phase 3
```

### Phase 4: Advanced & Learning (Weeks 13-16)
**Components**: pln, miner, asmoses
```bash
./integrate-components.sh 4
./validate-integration.py --phase 4
```

### Phase 5: Language & Integration (Weeks 17-20)
**Components**: lg-atomese, learn, opencog
```bash
./integrate-components.sh 5
./validate-integration.py --phase 5
```

## Component Status Tracking

### Check Component Configuration
```bash
cat component-config.json | jq '.opencog_unified_components'
```

### Check Integration Status
```bash
# View last validation results
cat integration_validation.json | jq '.summary'
```

### List Components by Phase
```bash
cat component-config.json | jq '.integration_phases'
```

## Troubleshooting

### Common Issues

#### Component Issues
```bash
# Check what's missing
./validate-integration.py --phase 1 | grep "missing"

# Install common dependencies (Ubuntu)
sudo apt-get install -y libboost-all-dev python3-dev guile-2.2-dev
```

#### Build Failures
```bash
# Clean rebuild
rm -rf build
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
make -j1 VERBOSE=1  # Single threaded with verbose output
```

#### Monorepo Integration Issues
```bash
# Re-integrate a component if needed
rm -rf components/category/component
./integrate-components.sh 1  # or specific phase

# Verify components are properly integrated
ls -la components/*/  # Should show folders without .git directories
```

#### Integration Script Issues
```bash
# Make scripts executable
chmod +x integrate-components.sh
chmod +x validate-integration.py

# Run with debug output
bash -x integrate-components.sh 1
```

### Get Help
```bash
./integrate-components.sh --help
./validate-integration.py --help
```

## Testing Strategy

### Run Integration Tests
```bash
# Run all integration tests
cd tests/integration
python3 -m pytest

# Run specific component test
python3 test_atomspace.py
```

### Run Performance Tests
```bash
cd tests/performance
python3 benchmark_suite.py
```

### Run End-to-End Tests
```bash
cd tests/end_to_end
python3 test_complete_workflow.py
```

## Development Workflow

### 1. Before Starting
```bash
# Check current status
git status
./validate-integration.py

# Review roadmap
less DEVELOPMENT-ROADMAP.md
```

### 2. Integrate Components
```bash
# Start with Phase 1
./integrate-components.sh 1

# Validate after each phase
./validate-integration.py --phase 1
```

### 3. Test and Validate
```bash
# Run tests
cd build && make test

# Validate integration
./validate-integration.py
```

### 4. Document Progress
```bash
# Update component status
# Edit component-config.json to mark components as "integrated"

# Commit progress
git add .
git commit -m "Integrated Phase N components"
```

## Configuration Files

### component-config.json
- Component definitions and dependencies
- Integration phase assignments
- Status tracking
- Build requirements

### DEVELOPMENT-ROADMAP.md
- Detailed 20-week roadmap
- Phase-by-phase tasks
- Success criteria and deliverables
- Risk management and resources

## Quick Reference

### Component Dependencies
```
Foundation: cogutil
Core: atomspace (→ atomspace-rocks, atomspace-restful), cogserver
Logic: unify → ure
Cognitive: attention, spacetime  
Advanced: pln, miner
Learning: moses, asmoses
Language: lg-atomese, learn, language-learning
Integration: opencog (final)
```

### Phase Summary
- **Phase 1**: Core storage and basic learning
- **Phase 2**: Logic and rule systems
- **Phase 3**: Cognitive attention and reasoning
- **Phase 4**: Advanced probabilistic and learning systems
- **Phase 5**: Language processing and final integration

### Key Scripts
- `integrate-components.sh`: Automated component integration
- `validate-integration.py`: Integration validation and testing
- `component-config.json`: Configuration and status tracking
- `DEVELOPMENT-ROADMAP.md`: Complete development plan

## Support

For issues or questions:
1. Check validation output: `./validate-integration.py`
2. Review integration logs in script output
3. Check component-specific documentation in each integrated component folder
4. Refer to the detailed DEVELOPMENT-ROADMAP.md for context