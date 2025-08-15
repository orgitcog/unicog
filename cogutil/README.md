OpenCog Utilities (CogUtil)
============================

[![Build Status](https://github.com/OzCog/opencog-unified/actions/workflows/main.yml/badge.svg)](https://github.com/OzCog/opencog-unified/actions)

The OpenCog utilities (`cogutil`) is a foundational collection of C++ utilities
used for typical programming tasks across all OpenCog projects within the
**OpenCog Unified** monorepo.

## Core Components

These utilities include:
* **Thread-safe collections**: queues, stacks and sets
* **Async processing**: asynchronous method caller
* **Resource management**: thread-safe resource pool
* **Debugging tools**: thread-safe backtrace printing with symbol demangling
* **Performance**: high-performance signal-slot implementation
* **AI utilities**: random tournament selection for evolutionary algorithms
* **Portability**: OS abstraction layers for cross-platform support

## Integration Context

`cogutil` serves as the foundational utility library for the entire OpenCog Unified ecosystem:
- **Core dependency**: Required by atomspace, cogserver, and all other components
- **Build order**: Built first in the unified build system
- **Shared utilities**: Provides common functionality to avoid code duplication

The main project site is at http://opencog.org

## Build Prerequisites

### Required Dependencies
To build cogutil within the OpenCog Unified repository, the following packages are required:

**Note**: In the unified repository, cogutil is built automatically as part of the overall build system. These dependencies should be installed at the repository root level.

**Essential Libraries:**

###### boost (≥ 1.60)
> C++ utilities package - **REQUIRED**
> ```bash
> sudo apt-get install libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-thread-dev
> ```
> http://www.boost.org/

###### cmake (≥ 3.12)  
> Build management tool - **REQUIRED**
> ```bash
> sudo apt-get install cmake
> ```
> http://www.cmake.org/

### Optional Dependencies (Recommended)

The following packages are **strongly recommended** for OpenCog development as they enable enhanced debugging and documentation capabilities:

###### cxxtest
> Unit test framework - **Recommended for development**
> ```bash
> sudo apt-get install cxxtest
> ```
> https://cxxtest.com/

###### binutils-dev (BFD library)
> The GNU binutils "Binary File Descriptor" library - **Essential for debugging**
> ```bash
> sudo apt-get install binutils-dev  
> ```
> Enables pretty-printed stack traces with symbol resolution
> http://gnu.org/s/binutils

###### libiberty-dev
> GNU GCC compiler tools libiberty component - **Essential for debugging**
> ```bash
> sudo apt-get install libiberty-dev
> ```
> Required for C++ symbol demangling in stack traces  
> http://gcc.gnu.org

###### doxygen
> Documentation generator - **Recommended for documentation**
> ```bash
> sudo apt-get install doxygen
> ```
> Generates comprehensive API documentation
> http://www.stack.nl/~dimitri/doxygen/

### Quick Install (Ubuntu/Debian)
```bash
# Essential dependencies
sudo apt-get install libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-thread-dev cmake

# Recommended for development  
sudo apt-get install cxxtest binutils-dev libiberty-dev doxygen
```

## Building in OpenCog Unified

### Unified Repository Build
In the OpenCog Unified monorepo, `cogutil` is built automatically as part of the overall build process:

```bash
# From the repository root
cd /path/to/opencog-unified/
mkdir build  
cd build
cmake ..
make -j$(nproc)
```

`cogutil` will be built first as it's a core dependency for other components.

### Building CogUtil Standalone
If you need to build only the cogutil component:

```bash
cd cogutil/
mkdir build
cd build  
cmake ..
make -j$(nproc)
```

**Build Output**: Libraries are built into `build/opencog/util/` mirroring the source structure.


## Testing

### Unit Tests
To build and run the unit tests (requires cxxtest):

```bash
# From cogutil build directory
cd cogutil/build/
make check
```

**Note**: Unit tests require the `cxxtest` package to be installed.

### Integration Testing  
CogUtil is also tested as part of the unified repository's comprehensive test suite:

```bash
# From repository root build directory  
cd build/
ctest
```


## Installation

### Unified Repository Installation
When building the complete OpenCog Unified repository, installation is handled at the repository level:

```bash
# From repository root build directory
cd build/
sudo make install
```

### Standalone Installation
For standalone cogutil installation:

```bash
# From cogutil build directory
cd cogutil/build/
sudo make install  
```

**Important**: Installation is required for proper linking with dependent OpenCog components.

## Development Notes

### Debugging Support
For optimal debugging experience, ensure both `binutils-dev` and `libiberty-dev` are installed. This enables:
- **Symbol resolution**: Function names in stack traces instead of hex addresses
- **Demangled C++ names**: Human-readable C++ function signatures  
- **Enhanced error reporting**: Detailed backtrace information

### Thread Safety
CogUtil provides thread-safe implementations of common data structures. Key components:
- `concurrent_queue<T>`: Lock-free queue implementation
- `concurrent_stack<T>`: Thread-safe stack  
- `concurrent_set<T>`: Thread-safe set operations
- `ResourcePool<T>`: Thread-safe object pooling

### Performance Considerations  
- **Signal-slot**: High-performance observer pattern implementation
- **Random selection**: Optimized tournament selection for evolutionary algorithms
- **Memory management**: Efficient resource pooling to reduce allocation overhead

## Troubleshooting

### Common Build Issues

**Missing Boost libraries:**
```
Could NOT find Boost (missing: Boost_INCLUDE_DIR)
```
**Solution**: Install boost development packages:
```bash
sudo apt-get install libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-thread-dev
```

**Missing stack trace support:**  
```
BFD not found
Libiberty-dev missing: No pretty stack-trace printing.
```
**Solution**: Install debugging support libraries:
```bash
sudo apt-get install binutils-dev libiberty-dev
```

**CxxTest not found:**
```
CxxTest missing: needed for unit tests.
```
**Solution**: Install cxxtest package:
```bash
sudo apt-get install cxxtest
```

### CMake Policy Warnings
You may see warnings about CMake policy CMP0167. These are harmless and can be ignored, or suppressed with:
```bash  
cmake -Wno-dev ..
```

## Contributing

When contributing to cogutil:
- **Run tests**: Always run `make check` before submitting changes
- **Address warnings**: Fix any compiler warnings in your code
- **Update docs**: Update this README if adding new utilities or dependencies
- **Thread safety**: Ensure new utilities are thread-safe where applicable

## Related Components

In the OpenCog Unified repository:
- **atomspace/**: Knowledge representation (depends on cogutil)
- **cogserver/**: Cognitive server (depends on atomspace → cogutil)  
- **ure/**: Unified Rule Engine (depends on atomspace → cogutil)
- **pln/**: Probabilistic Logic Networks (depends on ure → atomspace → cogutil)
