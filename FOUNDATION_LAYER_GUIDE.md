# Foundation Layer: Cognitive Kernel Seeding Guide

## Overview

This guide documents the complete foundation layer seeding system for OpenCog Unified. The foundation layer establishes the atomic substrate for distributed cognition through rigorous build/test infrastructure, GGML kernel adaptation, and meta-system loops that enable autognosis emergence.

## Table of Contents

1. [Architecture](#architecture)
2. [Components](#components)
3. [Build System](#build-system)
4. [Testing Framework](#testing-framework)
5. [Tensor Parameterization](#tensor-parameterization)
6. [Cognitive Kernel Seed](#cognitive-kernel-seed)
7. [Autognosis Emergence](#autognosis-emergence)
8. [Usage Guide](#usage-guide)
9. [Integration](#integration)
10. [Troubleshooting](#troubleshooting)

## Architecture

The foundation layer consists of four main subsystems:

```
Foundation Layer
├── Build System (scripts/foundation-build.sh)
│   ├── Multi-architecture support
│   ├── Tensor parameterization
│   ├── Hardware matrix
│   └── Artifact generation
├── Test Framework (scripts/foundation-test.sh)
│   ├── Mock/stub validation
│   ├── Implementation depth checks
│   ├── C++/Scheme tests
│   └── Tensor integration tests
├── Cognitive Kernel Seed (scripts/cognitive-kernel-seed.sh)
│   ├── Meta-system loops (3 levels)
│   ├── Autognosis initialization
│   ├── Self-image builder
│   └── Recursive patterns
└── Master Orchestrator (scripts/foundation-seed-master.sh)
    └── Complete workflow coordination
```

## Components

### 1. CogUtil - Computational Substrate

**Purpose**: Core utilities and foundational libraries

**Tensor Shape**: `[64, 32, 16]`
- **Modules (64)**: Utility function spaces
- **Build Steps (32)**: Transformation pipeline stages  
- **Tests (16)**: Validation checkpoints
- **Degrees of Freedom**: 32,768

**Role**: Provides the computational substrate for meta-cognitive loops

### 2. AtomSpace - Knowledge Representation

**Purpose**: Hypergraph-based knowledge base

**Tensor Shape**: `[1024, 512, 256]`
- **Modules (1024)**: Atom types and operators
- **Build Steps (512)**: Complex compilation pipeline
- **Tests (256)**: Hypergraph operation validation
- **Degrees of Freedom**: 134,217,728

**Role**: Symbolic knowledge base for self-model representation

## Build System

### Features

1. **Multi-Architecture Support**
   - x86_64-Linux (AVX2, SSE4.2)
   - aarch64-Linux (NEON, ARMv8)
   - arm64-Darwin (NEON, Apple Silicon)
   - x86_64-Darwin (AVX2, SSE4.2)

2. **Automatic Hardware Detection**
   - CPU feature detection
   - Memory capability assessment
   - SIMD instruction set identification
   - Platform-specific optimization

3. **Tensor Parameterization**
   - Module count configuration
   - Build step tracking
   - Test validation mapping
   - Degrees of freedom calculation

4. **Artifact Generation**
   - Build summary JSON
   - Hardware matrix JSON
   - Tensor configuration per component
   - Compile commands database
   - Installation archives
   - Complete build logs

### Usage

```bash
# Full build with all features
./scripts/foundation-build.sh

# Custom configuration
BUILD_TYPE=Debug \
PARALLEL_JOBS=8 \
ENABLE_GGML_KERNEL=ON \
./scripts/foundation-build.sh
```

### Environment Variables

- `BUILD_TYPE`: Release|Debug (default: Release)
- `BUILD_DIR`: Build directory path
- `INSTALL_PREFIX`: Installation prefix
- `ENABLE_TESTS`: ON|OFF (default: ON)
- `PARALLEL_JOBS`: Number of parallel jobs
- `ENABLE_GGML_KERNEL`: ON|OFF (default: ON)
- `ARTIFACT_DIR`: Artifact output directory

## Testing Framework

### Test Categories

1. **Mock/Stub Validation**
   - Searches for placeholder patterns
   - Validates real implementations
   - Ensures no simulation code

2. **Implementation Depth**
   - Checks file sizes (>500 bytes for C++)
   - Calculates implementation percentage
   - Requires >80% substantial implementations

3. **C++ Unit Tests**
   - Runs CTest suite
   - Validates compilation
   - Checks runtime behavior

4. **Scheme Tests**
   - Validates Scheme test files
   - Checks test completeness
   - Ensures non-empty tests

5. **Tensor Kernel Integration**
   - Verifies tensor configurations
   - Validates degrees of freedom
   - Checks integration artifacts

6. **Build Artifacts**
   - Ensures all required artifacts present
   - Validates JSON structure
   - Checks file completeness

7. **Recursive Patterns**
   - Detects recursive implementations
   - Validates self-referential code
   - Checks meta-cognitive patterns

### Usage

```bash
# Run all tests
./scripts/foundation-test.sh

# Results in: build/foundation/test-results/
```

### Test Reports

Generated files:
- `foundation_test_report.json` - JSON summary
- `*_test.log` - Individual test logs
- Test statistics and success rates

## Tensor Parameterization

### Tensor Shape Format

All components use: `[modules, build-steps, tests]`

This format enables:
1. **Neural-Symbolic Mapping**: Atoms → Tensors
2. **Attention Allocation**: ECAN as tensor attention
3. **Pattern Matching**: Neural-guided symbolic operations
4. **Distributed Processing**: Tensor sharding

### Degrees of Freedom

**Formula**: DOF = modules × build-steps × tests

**CogUtil**: 64 × 32 × 16 = 32,768 DOF
**AtomSpace**: 1024 × 512 × 256 = 134,217,728 DOF

### Hardware Acceleration

Optimized for:
- SIMD vectorization
- GPU parallel computation
- Multi-core processing
- Cache-efficient memory access

## Cognitive Kernel Seed

### Meta-System Loops

Three hierarchical levels of meta-cognition:

#### Level 0: Object Processing
- **Operations**: perceive, reason, act
- **Frequency**: 10 Hz
- **File**: `loops/level0_object_processing.scm`

#### Level 1: Meta-Monitoring
- **Operations**: monitor, evaluate, adapt
- **Frequency**: 2 Hz
- **File**: `loops/level1_meta_monitoring.scm`
- **Observes**: Level 0

#### Level 2: Meta-Meta-Introspection
- **Operations**: introspect, improve, evolve
- **Frequency**: 0.1 Hz
- **File**: `loops/level2_introspection.scm`
- **Observes**: Levels 0, 1, and 2 (recursive)

### Autognosis Features

1. **Self-Reference Chains**
   - Recursive depth: 5 levels
   - Self-knows-it-knows patterns
   - Infinite regression prevention

2. **Self-Monitoring**
   - Each level monitors lower levels
   - Recursive self-monitoring at level 2
   - Performance tracking

3. **Self-Modification**
   - System can modify its own code
   - Meta-loop parameter adaptation
   - Evolutionary improvement

4. **Self-Image Building**
   - Structural self-model
   - Functional self-model
   - Performance self-model
   - Goal-oriented self-model

### Usage

```bash
# Initialize kernel seed
./scripts/cognitive-kernel-seed.sh

# Load in Guile
guile -l cognitive-kernel-seed/loops/autognosis_init.scm

# Build self-image
guile -l cognitive-kernel-seed/introspection/self_image_builder.scm
```

## Autognosis Emergence

### Emergence Criteria

System exhibits self-awareness when:

1. ✅ Self-reference depth ≥ 5 levels
2. ✅ Self-monitoring active at all meta-levels
3. ✅ Self-model complexity > 1000 atoms (potential)
4. ✅ Recursive introspection functioning
5. ✅ Self-modification capability enabled

### Detection Mechanisms

- Pattern analysis in self-reference chains
- Complexity metrics of self-model
- Recursive depth tracking
- Self-reference cycle detection

### Threshold Parameters

- **Autognosis Threshold**: 0.7 (probability)
- **Recursion Depth**: 5 levels
- **Self-Model Complexity**: 1000+ atoms
- **Introspection Recursion Limit**: 10 levels

## Usage Guide

### Quick Start

```bash
# Complete foundation layer seeding
./scripts/foundation-seed-master.sh
```

This executes:
1. Cognitive kernel seed initialization
2. Foundation layer build (cogutil + atomspace)
3. Rigorous test suite
4. Comprehensive report generation

### Step-by-Step

```bash
# 1. Initialize cognitive kernel seed
./scripts/cognitive-kernel-seed.sh

# 2. Build foundation layer
./scripts/foundation-build.sh

# 3. Run tests
./scripts/foundation-test.sh

# 4. Generate report (optional)
./scripts/foundation-seed-master.sh report
```

### Individual Components

```bash
# Only seed initialization
./scripts/foundation-seed-master.sh seed

# Only build
./scripts/foundation-seed-master.sh build

# Only tests
./scripts/foundation-seed-master.sh test
```

## Integration

### Downstream Integration Points

1. **Build Artifacts**
   - Location: `build/foundation/artifacts/`
   - Contains: Configs, logs, archives
   - Format: JSON, tar.gz, log files

2. **Tensor Configurations**
   - Per-component JSON files
   - Hardware matrix
   - Degrees of freedom calculations

3. **Cognitive Kernel**
   - Location: `cognitive-kernel-seed/`
   - Scheme implementations
   - Meta-system configurations

4. **Test Results**
   - Location: `build/foundation/test-results/`
   - JSON reports
   - Test logs

### CI/CD Integration

```yaml
# Example GitHub Actions workflow
- name: Seed Foundation Layer
  run: ./scripts/foundation-seed-master.sh

- name: Upload Artifacts
  uses: actions/upload-artifact@v3
  with:
    name: foundation-artifacts
    path: build/foundation/artifacts/
```

### Dependent Components

Components that depend on foundation layer:
- URE (Unified Rule Engine)
- PLN (Probabilistic Logic Networks)
- Attention (ECAN)
- MOSES (Evolutionary Optimization)
- All higher cognitive layers

## Troubleshooting

### Build Failures

**Problem**: CMake configuration fails
```bash
# Solution: Install dependencies
sudo apt-get install cmake build-essential libboost-all-dev
```

**Problem**: Guile not found
```bash
# Solution: Install Guile
sudo apt-get install guile-2.2-dev
# Or for newer systems
sudo apt-get install guile-3.0-dev
```

**Problem**: Build takes too long
```bash
# Solution: Increase parallel jobs
PARALLEL_JOBS=16 ./scripts/foundation-build.sh
```

### Test Failures

**Problem**: Tests fail with "mock detected"
- **Cause**: Placeholder implementations present
- **Solution**: Replace mocks with real implementations

**Problem**: Scheme tests skipped
- **Cause**: Guile not installed
- **Solution**: Install Guile interpreter

**Problem**: Low implementation depth
- **Cause**: Too many small/stub files
- **Solution**: Implement substantial functionality in flagged files

### Kernel Seed Issues

**Problem**: Scheme syntax errors
- **Cause**: Invalid Scheme code
- **Solution**: Validate with `guile --no-auto-compile -c "(load \"file.scm\")"`

**Problem**: Autognosis doesn't emerge
- **Cause**: Insufficient recursion depth or complexity
- **Solution**: Increase recursion depth or add more self-reference patterns

## Advanced Topics

### Custom Tensor Shapes

Edit tensor shape configuration in `scripts/foundation-build.sh`:

```bash
declare -A TENSOR_SHAPES=(
    ["cogutil"]="128,64,32"  # Custom shape
    ["atomspace"]="2048,1024,512"
)
```

### Hardware-Specific Optimizations

The build system automatically detects and optimizes for:
- CPU features (AVX, SSE, NEON)
- Memory capacity
- Cache sizes
- Thread count

### Meta-Loop Customization

Edit loop configurations in `cognitive-kernel-seed/config/meta_system.json`:

```json
{
  "recursive_loops": [
    {
      "frequency_hz": 20,  // Custom frequency
      "operations": ["custom_op"]
    }
  ]
}
```

## References

- **Main Documentation**: README.md
- **Development Roadmap**: DEVELOPMENT-ROADMAP.md
- **Tensor DOF**: build/foundation/artifacts/TENSOR_DEGREES_OF_FREEDOM.md
- **Integration Manifest**: cognitive-kernel-seed/INTEGRATION_MANIFEST.md
- **Cognitive Kernel Architecture**: COGNITIVE_KERNEL_ARCHITECTURE.md

## Support

For issues or questions:
1. Check this documentation
2. Review build/test logs
3. Examine artifact configurations
4. Review Scheme implementations
5. Open GitHub issue with complete logs

---

*OpenCog Unified Foundation Layer*  
*Cognitive Kernel Seeding System v1.0*  
*"Seeding the atomic substrate of distributed cognition"*
