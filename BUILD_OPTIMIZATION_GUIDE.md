# OpenCog Build Optimization Guide

This guide explains how to use the comprehensive dependency analysis and build optimization system implemented for OpenCog Unified.

**!!! ADD DEPENDENCY BUILD SEQUENCE: cogutil > atomspace > atomspace-storage > atomspace-rocks > cogserver > ...

## Quick Start

### 1. Run Dependency Analysis
```bash
# Generate complete dependency analysis
python3 dependency_analyzer.py --report

# Generate optimized CMake files
python3 dependency_analyzer.py --cmake

# Create dependency visualization
python3 dependency_analyzer.py --visualize
```

### 2. Build Optimization for Available Components
```bash
# Display optimization summary
python3 build_optimizer.py --summary

# Save all optimization files
python3 build_optimizer.py --save-all

# Validate build sequence
python3 build_optimizer.py --validate
```

### 3. Generate Comprehensive Summary
```bash
# Create final summary report
python3 optimal_build_summary.py
```

## System Overview

The build optimization system analyzes the complete OpenCog dependency graph from issue #85 and provides:

### Analysis Capabilities
- **37 Components Analyzed** - Complete component dependency mapping
- **40 External Dependencies** - System-level dependency tracking  
- **6-Level Dependency Graph** - Hierarchical build organization
- **Critical Path Analysis** - Identifies build bottlenecks
- **Parallelization Opportunities** - Maximum concurrent build optimization

### Key Findings

#### Critical Path (Complete System)
```
cogutil → atomspace → cogserver → ure → pln → opencog
```

#### Available Critical Path (Current Repository)
```
cogutil → opencog
```

#### Maximum Parallelization
- **5 components** can build simultaneously at peak levels
- **21 total components** available for parallel builds across levels

## Build Sequences

### Optimized Build Phases

1. **Phase 1 - Foundation** (1 component)
   - `cogutil` - Core utilities foundation

2. **Phase 2 - Core Systems** (4 components)  
   - `atomspace` - Knowledge representation core
   - `cogserver` - Distributed server infrastructure
   - `atomspace_rocks` - RocksDB storage backend
   - `atomspace_restful` - REST API interface

3. **Phase 3 - Logic & Reasoning** (4 components)
   - `unify` - Pattern matching engine
   - `ure` - Unified Rule Engine  
   - `pln` - Probabilistic Logic Networks
   - `miner` - Pattern mining system

4. **Phase 4 - Advanced Systems** (4 components)
   - `moses` - Evolutionary optimization
   - `spacetime` - Spatial-temporal reasoning
   - `attention` - ECAN attention allocation
   - `asmoses` - AtomSpace MOSES integration

5. **Phase 5 - Integration** (4 components)
   - `language_learning` - Unsupervised language learning
   - `lg_atomese` - Link Grammar integration
   - `learn` - Learning module
   - `opencog` - Final integration layer

### Parallel Build Opportunities

#### Level 1: 3 Parallel Builds
```bash
# Can build simultaneously after cogutil
make -j3 atomspace moses language_learning
```

#### Level 2: 5 Parallel Builds  
```bash
# Can build simultaneously after Level 1
make -j5 cogserver atomspace_rocks unify spacetime lg_atomese
```

#### Level 3: 4 Parallel Builds
```bash
# Can build simultaneously after Level 2  
make -j4 atomspace_restful attention ure learn
```

#### Level 4: 3 Parallel Builds
```bash
# Can build simultaneously after Level 3
make -j3 pln miner asmoses
```

## Integration with Existing System

### CMake Integration
The system generates optimized CMakeLists.txt files:

- **CMakeLists_available.txt** - Optimized for currently available components
- **CMakeLists_optimized.txt** - Complete optimized configuration

```bash
# Use optimized build
cp CMakeLists_available.txt CMakeLists.txt
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Validation Integration
The system integrates with existing validation:

```bash
# Run integrated validation
python3 build_optimizer.py --validate

# Use existing validation tools
./validate-integration.py
```

### Component Integration
Works with existing integration scripts:

```bash
# Use with existing component integration
./integrate-components.sh all

# Validate with optimized sequence
python3 build_optimizer.py --summary
```

## External Dependencies

### System Dependencies by Priority

**Critical Path Dependencies:**
- `boost` (required by 12 components)
- `cmake` (build system)
- `cxxtest` (testing framework)
- `doxygen` (documentation)

**Storage Dependencies:**
- `rocksdb` (atomspace-rocks)
- `pgsql` (PostgreSQL storage)

**Specialized Dependencies:**
- `mpi` (moses, asmoses - parallel processing)
- `guile` (scheme integration)
- `opencv` (vision components)

### Installation Commands

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install -y cmake build-essential libboost-all-dev
sudo apt-get install -y python3-dev guile-2.2-dev librocksdb-dev
sudo apt-get install -y libpq-dev cxxtest doxygen
```

**Advanced Dependencies:**
```bash
# For MPI support (moses/asmoses)
sudo apt-get install -y libopenmpi-dev

# For vision components  
sudo apt-get install -y libopencv-dev

# For specialized components
sudo apt-get install -y libzmq3-dev libjsoncpp-dev
```

## Performance Optimization

### Build Time Optimization

1. **Parallel Jobs Allocation:**
   ```bash
   # Use dependency-aware parallel builds
   make -j$(python3 -c "
   from build_optimizer import BuildOptimizer
   opt = BuildOptimizer()
   result = opt.generate_optimal_sequence()
   max_parallel = max(len(comps) for comps in result['parallel_groups'].values())
   print(max_parallel)
   ")
   ```

2. **Resource Allocation:**
   - Allocate more CPU cores to critical path components
   - Use SSDs for build directories
   - Increase RAM for parallel builds

3. **Build Caching:**
   ```bash
   # Enable ccache for C++ builds
   export CC="ccache gcc"
   export CXX="ccache g++"
   ```

### CI/CD Integration

**GitHub Actions Example:**
```yaml
name: Optimized OpenCog Build

on: [push, pull_request]

jobs:
  dependency-analysis:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run Dependency Analysis
        run: |
          python3 dependency_analyzer.py --report
          python3 build_optimizer.py --validate
      
  parallel-build:
    needs: dependency-analysis
    strategy:
      matrix:
        level: [1, 2, 3, 4]
    runs-on: ubuntu-latest
    steps:
      - name: Build Level ${{ matrix.level }}
        run: |
          python3 build_optimizer.py --save-all
          # Build components for this level
```

## Troubleshooting

### Common Issues

1. **Missing Dependencies:**
   ```bash
   python3 build_optimizer.py --validate
   # Check external_dependencies in output
   ```

2. **Build Order Issues:**
   ```bash
   # Use optimized CMake file
   cp CMakeLists_available.txt CMakeLists.txt
   rm -rf build && mkdir build && cd build && cmake ..
   ```

3. **Component Not Found:**
   ```bash
   # Check available components
   python3 build_optimizer.py --summary
   ```

### Debug Mode

```bash
# Generate detailed analysis
python3 dependency_analyzer.py --report --visualize
python3 optimal_build_summary.py

# Check generated files
ls -la *.json *.md *.txt *.png
```

## Generated Files Reference

| File | Purpose |
|------|---------|
| `dependency_analysis_report.json` | Complete dependency analysis data |
| `build_optimization_report.json` | Available components optimization |
| `OPTIMAL_BUILD_SEQUENCE.md` | Human-readable build instructions |
| `CMakeLists_available.txt` | Optimized CMake for available components |
| `CMakeLists_optimized.txt` | Complete optimized CMake |
| `dependency_graph.png` | Visual dependency graph |
| `DEPENDENCY_ANALYSIS_SUMMARY.md` | Executive summary |

## Advanced Usage

### Custom Component Analysis

```python
from dependency_analyzer import DependencyAnalyzer

analyzer = DependencyAnalyzer()
report = analyzer.generate_report()

# Custom analysis
critical_path = report['build_sequence']['critical_path']
parallel_groups = report['build_sequence']['parallel_groups']

print(f"Critical components: {critical_path}")
```

### Integration with Other Build Systems

```bash
# Generate Makefile
python3 -c "
from build_optimizer import BuildOptimizer
opt = BuildOptimizer()
result = opt.generate_optimal_sequence()
# Convert to Makefile format
"

# Generate Docker builds
python3 -c "
# Generate Dockerfile with optimized build sequence
"
```

This optimization system provides a complete solution for building OpenCog components with maximum efficiency and minimal build time.
