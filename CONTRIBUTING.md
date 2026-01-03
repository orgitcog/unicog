# Contributing to OpenCog Unified

Thank you for your interest in contributing to OpenCog Unified! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Pre-commit Hooks](#pre-commit-hooks)
- [Coding Standards](#coding-standards)
- [Commit Guidelines](#commit-guidelines)
- [Pull Request Process](#pull-request-process)
- [Cognitive Architecture Guidelines](#cognitive-architecture-guidelines)

## Code of Conduct

This project adheres to a code of conduct. By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## Getting Started

### Prerequisites

- Python 3.11+
- Git 2.30+
- CMake 3.16+
- GCC 11+ or Clang 14+
- Guile 3.0+

### Cloning the Repository

```bash
git clone https://github.com/OzCog/opencog-unified.git
cd opencog-unified
```

## Development Setup

### 1. Install Pre-commit Hooks

Pre-commit hooks are **required** for all contributors. They ensure code quality and consistency before commits.

```bash
# Install pre-commit
pip install pre-commit

# Install the hooks
pre-commit install

# (Optional) Install commit-msg hooks
pre-commit install --hook-type commit-msg
```

### 2. Run Hooks Manually

```bash
# Run on all files
pre-commit run --all-files

# Run on staged files only
pre-commit run

# Run a specific hook
pre-commit run <hook-id>
```

### 3. Update Hooks

```bash
# Update to latest versions
pre-commit autoupdate
```

## Pre-commit Hooks

The following hooks are configured:

| Hook | Description |
|------|-------------|
| `trailing-whitespace` | Removes trailing whitespace |
| `end-of-file-fixer` | Ensures files end with newline |
| `check-yaml` | Validates YAML syntax |
| `check-json` | Validates JSON syntax |
| `check-merge-conflict` | Prevents merge conflict markers |
| `detect-private-key` | Prevents committing private keys |
| `yamllint` | Lints YAML files |
| `shellcheck` | Lints shell scripts |
| `ruff` | Lints and formats Python code |
| `actionlint` | Validates GitHub Actions |
| `markdownlint` | Lints Markdown files |
| `cmakelint` | Lints CMake files |
| `detect-secrets` | Scans for secrets |
| `no-new-fixmes` | Prevents new FIXME comments |
| `validate-tensor-dims` | Validates tensor dimension format |
| `no-hardcoded-paths` | Checks for hardcoded runner paths |

### Bypassing Hooks (Emergency Only)

In rare cases where you need to bypass hooks:

```bash
# Skip all hooks
git commit --no-verify -m "message"

# Skip specific hooks
SKIP=hook-id git commit -m "message"
```

**Note:** Bypassing hooks should be rare and justified. CI will still run all checks.

## Coding Standards

### Python

- Follow PEP 8 with 120 character line limit
- Use type hints for function signatures
- Use docstrings for public functions
- Ruff handles formatting automatically

```python
def process_atoms(atoms: list[Atom], threshold: float = 0.5) -> list[Atom]:
    """Process atoms based on attention value threshold.
    
    Args:
        atoms: List of atoms to process
        threshold: Minimum attention value (default: 0.5)
    
    Returns:
        Filtered list of atoms above threshold
    """
    return [a for a in atoms if a.attention_value >= threshold]
```

### C++

- Use 4-space indentation
- Follow OpenCog C++ style guide
- Use smart pointers over raw pointers
- Document public APIs with Doxygen

```cpp
/**
 * @brief Process atoms based on attention value
 * @param atoms Vector of atom handles
 * @param threshold Minimum attention value
 * @return Filtered vector of atoms
 */
HandleSeq processAtoms(const HandleSeq& atoms, double threshold = 0.5);
```

### Scheme

- Use 2-space indentation
- Document functions with docstrings
- Follow OpenCog Atomese conventions

```scheme
;; Process atoms based on attention value
;; @param atoms List of atoms
;; @param threshold Minimum attention value
;; @return Filtered list of atoms
(define (process-atoms atoms threshold)
  (filter (lambda (a) (>= (cog-av-sti a) threshold)) atoms))
```

### Shell Scripts

- Use `set -eu` at the start
- Quote variables: `"$var"`
- Use `[[ ]]` for conditionals
- Document with comments

```bash
#!/bin/bash
set -eu

# Process cognitive analysis
# Usage: ./script.sh <input_dir>
main() {
    local input_dir="$1"
    # ...
}
```

### YAML (Workflows)

- Use 2-space indentation
- Use `$GITHUB_WORKSPACE` not hardcoded paths
- Use `set -eu` not `set -euo pipefail` (sh compatibility)
- Quote strings with special characters

## Commit Guidelines

### Commit Message Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types

| Type | Description |
|------|-------------|
| `feat` | New feature |
| `fix` | Bug fix |
| `docs` | Documentation |
| `style` | Formatting |
| `refactor` | Code restructuring |
| `perf` | Performance improvement |
| `test` | Adding tests |
| `chore` | Maintenance |
| `ci` | CI/CD changes |

### Examples

```
feat(atomspace): Add tensor dimension tracking

Implement tensor field tracking for cognitive layers with
OEIS A000081 nested shell structure.

- Add tensor shape annotations to all 16 layers
- Calculate degrees of freedom for each component
- Update autognostic ontogenesis script

Closes #123
```

```
fix(workflow): Clone prerequisites in isolated jobs

GitHub Actions jobs run in isolated containers, so cached
build artifacts are not available across jobs.

- Clone cogutil, atomspace in each job
- Build prerequisites before target component
```

## Pull Request Process

1. **Create a branch** from `main`:
   ```bash
   git checkout -b feat/my-feature
   ```

2. **Make changes** and commit with meaningful messages

3. **Run pre-commit** on all files:
   ```bash
   pre-commit run --all-files
   ```

4. **Push** your branch:
   ```bash
   git push origin feat/my-feature
   ```

5. **Create PR** with:
   - Clear title following commit format
   - Description of changes
   - Link to related issues
   - Screenshots if UI changes

6. **Address review feedback** promptly

7. **Squash and merge** when approved

## Cognitive Architecture Guidelines

### Tensor Dimensions

When adding or modifying cognitive components, follow the tensor dimension format:

```yaml
# Layer | Component | Tensor Shape | DOF
# Foundation | cogutil | [512, 128, 8] | 524,288
# Core | atomspace | [1024, 256, 16, 4] | 16,777,216
```

### Cognitive Layers

Valid cognitive layers (in order):
1. `foundation` - Base utilities
2. `core` - AtomSpace and storage
3. `logic` - Reasoning engines
4. `cognitive` - Attention and context
5. `advanced` - PLN and mining
6. `integration` - Full OpenCog
7. `autogenetic` - Self-improvement

### OEIS A000081 Structure

The cognitive architecture follows OEIS A000081 nested shell structure:
- N=1: 1 term (Foundation)
- N=2: 2 terms (Core)
- N=3: 4 terms (Logic)
- N=4: 9 terms (Cognitive)

### 12-Step Cognitive Loop

When implementing cognitive processes, follow the 12-step loop:
1. Perception (Stream 1)
2. Action (Stream 2)
3. Simulation (Stream 3)
4. Integration (Stream 1)
... (repeating pattern)

## Questions?

- Open an issue for bugs or feature requests
- Join the OpenCog community discussions
- Check existing documentation in `/docs`

Thank you for contributing to OpenCog Unified!
