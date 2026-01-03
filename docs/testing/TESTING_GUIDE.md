# OpenCog Unified Testing Guide

## Overview

This document provides comprehensive guidelines for writing and running tests across all OpenCog Unified components. Consistent testing practices are essential for maintaining code quality and enabling confident refactoring.

## Testing Frameworks

### C++ Testing with CxxTest

OpenCog uses CxxTest for C++ unit testing. CxxTest is a lightweight, header-only testing framework that integrates well with CMake.

**Template Location:** `tests/templates/CxxTestTemplate.h`

#### Creating a New C++ Test Suite

1. Copy the template to your component's `tests/` directory:
   ```bash
   cp tests/templates/CxxTestTemplate.h your_component/tests/YourComponentUTest.h
   ```

2. Update the CMakeLists.txt in your component's tests directory:
   ```cmake
   LINK_LIBRARIES(
       your_component
       ${ATOMSPACE_LIBRARIES}
       ${COGUTIL_LIBRARY}
   )

   ADD_CXXTEST(YourComponentUTest)
   ```

3. Implement your tests following the template structure.

#### Running C++ Tests

```bash
# Build and run all tests
cd build
make test

# Run specific test
./tests/your_component/YourComponentUTest

# Run with verbose output
ctest -V

# Run tests matching a pattern
ctest -R "YourComponent"
```

### Python Testing with pytest

Python bindings are tested using pytest, which provides powerful fixtures and parametrized testing.

**Template Location:** `tests/templates/pytest_template.py`

#### Creating a New Python Test Suite

1. Copy the template to your component's tests directory:
   ```bash
   cp tests/templates/pytest_template.py your_component/tests/test_your_component.py
   ```

2. Install pytest if not already installed:
   ```bash
   pip install pytest pytest-cov
   ```

#### Running Python Tests

```bash
# Run all Python tests
pytest

# Run tests for a specific component
pytest your_component/tests/

# Run with verbose output
pytest -v

# Run with coverage
pytest --cov=opencog --cov-report=html

# Run slow tests (marked with @pytest.mark.slow)
pytest -m slow

# Run excluding slow tests
pytest -m "not slow"
```

### Scheme Testing

Scheme tests are run directly with Guile and use a custom assertion framework.

**Template Location:** `tests/templates/scheme_test_template.scm`

#### Running Scheme Tests

```bash
# Run a single Scheme test file
guile -l tests/scm/your_test.scm

# Run with AtomSpace preloaded
guile -l opencog.scm -l tests/scm/your_test.scm
```

## Test Organization

### Directory Structure

```
your_component/
├── opencog/
│   └── your_component/
│       └── *.cc, *.h
└── tests/
    ├── CMakeLists.txt
    ├── YourComponentUTest.h
    ├── python/
    │   └── test_your_component.py
    └── scm/
        └── your_component_test.scm
```

### Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| C++ Test Class | `ComponentNameUTest` | `AtomSpaceUTest` |
| C++ Test File | `ComponentNameUTest.h` | `AtomSpaceUTest.h` |
| Python Test File | `test_component_name.py` | `test_atomspace.py` |
| Python Test Class | `TestComponentName` | `TestAtomSpace` |
| Python Test Function | `test_functionality` | `test_add_node` |
| Scheme Test File | `component-name-test.scm` | `atomspace-test.scm` |

## Test Categories

### Unit Tests

- Test individual functions and classes in isolation
- Mock external dependencies
- Fast execution (< 1 second per test)
- High coverage of edge cases

### Integration Tests

- Test interaction between multiple components
- May use real AtomSpace instances
- Moderate execution time
- Test real-world workflows

### Performance Tests

Mark with `@pytest.mark.slow` in Python or put in separate test file:

```python
@pytest.mark.slow
def test_performance():
    # Performance-sensitive test
    pass
```

## Coverage Requirements

### Target Coverage by Component Priority

| Priority | Components | Target Coverage |
|----------|------------|-----------------|
| Critical | atomspace, cogutil | 70% |
| High | cogserver, moses, ure | 60% |
| Medium | unify, persistence | 50% |
| Standard | Other components | 40% |

### Generating Coverage Reports

```bash
# C++ coverage with gcov/lcov
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCOVERAGE=ON
make
make test
lcov --capture --directory . --output-file coverage.info
genhtml coverage.info --output-directory coverage_report

# Python coverage
pytest --cov=opencog --cov-report=html
```

## Best Practices

### Test Independence

- Each test should be independent and not rely on other tests
- Use fixtures for setup and teardown
- Don't share mutable state between tests

### Meaningful Assertions

```python
# Good: Specific assertion message
assert result == expected, f"Expected {expected}, got {result}"

# Bad: No message
assert result == expected
```

### Test Edge Cases

Always test:
- Empty inputs
- Null/None values
- Boundary conditions
- Invalid inputs (should raise appropriate exceptions)

### AtomSpace Testing Patterns

```python
def test_atomspace_operations():
    """Standard AtomSpace test pattern."""
    # Create fresh AtomSpace for each test
    atomspace = AtomSpace()

    # Create atoms
    node = atomspace.add_node(types.ConceptNode, "test")

    # Perform operations
    result = some_operation(node)

    # Assert results
    assert result is not None

    # Cleanup is automatic when atomspace goes out of scope
```

### Mocking External Dependencies

```python
from unittest.mock import Mock, patch

def test_with_mock():
    """Test with mocked dependency."""
    with patch('opencog.persistence.storage') as mock_storage:
        mock_storage.load.return_value = [mock_atom]
        result = component.load_atoms()
        assert len(result) == 1
        mock_storage.load.assert_called_once()
```

## Continuous Integration

Tests are automatically run in CI/CD pipeline on:
- Every pull request
- Every merge to main branch
- Nightly builds for extended tests

### CI Test Configuration

The CI runs tests with the following matrix:
- Ubuntu 22.04 and 24.04
- GCC and Clang compilers
- Python 3.10 and 3.11

## Troubleshooting

### Common Issues

1. **Missing dependencies**: Ensure all component libraries are built
2. **AtomSpace not found**: Check PYTHONPATH includes build directory
3. **Guile load errors**: Ensure GUILE_LOAD_PATH is set correctly

### Debug Mode

```bash
# Run test with debug output
OPENCOG_LOG_LEVEL=DEBUG ./tests/YourTest

# Python debug mode
pytest -v --tb=long
```

## Resources

- [CxxTest Documentation](https://cxxtest.com/)
- [pytest Documentation](https://docs.pytest.org/)
- [OpenCog Wiki - Testing](https://wiki.opencog.org/w/Testing)
