# Entelechy Repair Scripts

This directory contains automated tools for systematic resolution of code markers (TODO, FIXME, STUB, etc.) as part of the Entelechy Framework.

## Scripts Overview

### 1. `sync_upstream_fixes.py`

Syncs with upstream OpenCog repositories to learn from community fixes and best practices.

**Usage:**
```bash
# Basic sync (top 5 repos, last 90 days)
python3 sync_upstream_fixes.py

# Custom sync
python3 sync_upstream_fixes.py --days 30 --max-repos 3 --show

# Export report
python3 sync_upstream_fixes.py --export upstream_report.json
```

**Features:**
- Clones/updates OpenCog repo caches in `.upstream_cache/`
- Searches git history for marker-related commits
- Identifies fixes applicable to our codebase
- Generates actionable recommendations

**Output:** `upstream_sync_report.json`

### 2. `intelligent_marker_repair.py`

AI-powered analysis and automated repair suggestions for code markers.

**Usage:**
```bash
# Analyze markers and generate repair plan
python3 intelligent_marker_repair.py --analyze

# Apply safe repairs (dry run)
python3 intelligent_marker_repair.py --apply --dry-run

# Apply safe repairs for real
python3 intelligent_marker_repair.py --apply --no-dry-run
```

**Features:**
- Smart categorization (remove, document, implement, test, defer)
- Confidence scoring (0.0-1.0)
- Safe automated fixes for high-confidence cases
- Quick wins identification

**Output:** `repair_plan.json`

### 3. `repair_workflow_orchestrator.py`

Orchestrates the complete repair workflow end-to-end.

**Usage:**
```bash
# Run full workflow (analysis only)
python3 repair_workflow_orchestrator.py

# Include safe repair application
python3 repair_workflow_orchestrator.py --apply-safe

# Apply repairs for real
python3 repair_workflow_orchestrator.py --apply-safe --no-dry-run
```

**Features:**
- Coordinates all repair tools
- Generates master report
- Tracks progress metrics
- Creates actionable summaries

**Output:** `entelechy_repair_master_report.json`

## Quick Start

**Step 1:** Run complete analysis workflow
```bash
cd /path/to/opencog-unified
python3 scripts/repair_workflow_orchestrator.py
```

**Step 2:** Review results
```bash
# View master summary
cat entelechy_repair_master_report.json | python3 -m json.tool | less

# Check quick wins
python3 scripts/intelligent_marker_repair.py --analyze
```

**Step 3:** Apply safe repairs (optional)
```bash
# Dry run first
python3 scripts/repair_workflow_orchestrator.py --apply-safe

# If satisfied, apply
python3 scripts/repair_workflow_orchestrator.py --apply-safe --no-dry-run
```

## Integration

### With Existing Tools

These scripts integrate with existing entelechy tools:
- `entelechy_marker_analyzer.py` - Core marker analysis
- `entelechy_marker_resolver.py` - Resolution tracking
- `entelechy_marker_prioritizer.py` - Priority scoring

### With Git

All scripts are git-aware and support:
- Tracking changes over time
- Commit integration
- Branch-based workflows

### With CI/CD

Can be integrated into GitHub Actions or other CI systems for:
- Weekly marker tracking
- Automated safe repairs
- Progress reporting

See `ENTELECHY_REPAIR_WORKFLOW_GUIDE.md` for CI/CD examples.

## Configuration

Scripts use sensible defaults but can be customized:

```python
# In sync_upstream_fixes.py
OPENCOG_REPOS = ['atomspace', 'cogutil', ...]  # Repos to sync
MARKER_PATTERNS = [r'\bTODO\b', ...]           # Marker patterns

# In intelligent_marker_repair.py
SAFE_REMOVAL_PATTERNS = [...]  # Patterns for safe removal
DOC_MARKERS = [...]            # Documentation markers
```

## Output Files

| File | Description |
|------|-------------|
| `upstream_sync_report.json` | Upstream fixes found |
| `repair_plan.json` | Repair recommendations |
| `entelechy_repair_master_report.json` | Master summary |
| `.upstream_cache/` | Cloned repo caches |

## Troubleshooting

**"Script not found" error:**
```bash
# Make scripts executable
chmod +x scripts/*.py

# Or run with python3
python3 scripts/script_name.py
```

**"Import error" for entelechy modules:**
```bash
# Ensure you're in repo root
cd /path/to/opencog-unified

# Run from repo root
python3 scripts/sync_upstream_fixes.py
```

**"Network error" during upstream sync:**
```bash
# Check connectivity
ping github.com

# Try fewer repos
python3 scripts/sync_upstream_fixes.py --max-repos 1

# Clear cache if corrupted
rm -rf .upstream_cache/
```

## Best Practices

1. **Run full workflow regularly** (weekly recommended)
2. **Review before applying** - Always check dry-run results
3. **Commit incrementally** - Small, focused commits
4. **Track progress** - Use git to track marker reduction
5. **Sync upstream** - Learn from community regularly

## Documentation

- **Complete Guide:** `ENTELECHY_REPAIR_WORKFLOW_GUIDE.md`
- **Framework Overview:** `.github/agents/entelechy.md`
- **Resolution System:** `ENTELECHY_MARKER_RESOLUTION.md`

## Support

For issues or questions:
1. Check `ENTELECHY_REPAIR_WORKFLOW_GUIDE.md`
2. Run with `--help` flag
3. Review generated reports
4. Create issue with `entelechy-repair` label

---

## Foundation Layer Scripts

### Overview

Scripts for seeding and managing the OpenCog Unified foundation layer (cogutil and atomspace) with GGML kernel adaptation and autognosis emergence.

### Core Foundation Scripts

#### `foundation-seed-master.sh` - Master Orchestrator
Complete workflow coordination for foundation layer seeding.

**Usage:**
```bash
./scripts/foundation-seed-master.sh [all|seed|build|test|report|help]
```

**What it does:**
1. Checks prerequisites (cmake, g++, make)
2. Initializes cognitive kernel seed with meta-system loops
3. Builds cogutil and atomspace with tensor parameterization
4. Runs rigorous tests (no mocks/stubs validation)
5. Generates comprehensive report

#### `foundation-build.sh` - Build System
Rigorous build system with multi-architecture support and tensor parameterization.

**Features:**
- Multi-arch support (x86_64, ARM64, Apple Silicon)
- Automatic hardware capability detection
- Tensor shapes: CogUtil [64,32,16], AtomSpace [1024,512,256]
- GGML kernel adaptation
- Comprehensive artifact generation

**Usage:**
```bash
./scripts/foundation-build.sh
```

**Environment Variables:**
- `BUILD_TYPE` - Release|Debug (default: Release)
- `PARALLEL_JOBS` - Number of parallel jobs (default: nproc)
- `ENABLE_GGML_KERNEL` - ON|OFF (default: ON)

#### `foundation-test.sh` - Test Framework
Rigorous testing with mock/stub validation.

**Test Categories:**
- Mock/stub detection
- Implementation depth verification (>80% substantial)
- C++ unit tests via CTest
- Scheme tests
- Tensor kernel integration
- Build artifact verification
- Recursive pattern detection

**Usage:**
```bash
./scripts/foundation-test.sh
```

#### `cognitive-kernel-seed.sh` - Kernel Initialization
Initializes cognitive kernel seed with meta-system loops and autognosis.

**Features:**
- 3-level meta-system loops (Object, Meta, Meta-Meta)
- Autognosis through recursive self-reference (depth 5)
- Self-image building (4 aspects: structural, functional, performance, goal)
- Full Scheme implementation (no mocks/stubs)

**Usage:**
```bash
./scripts/cognitive-kernel-seed.sh
```

**Outputs:**
- `cognitive-kernel-seed/` - Complete kernel structure
  - `config/` - JSON configurations
  - `loops/` - Meta-system loop Scheme implementations
  - `introspection/` - Self-image builder
  - `INTEGRATION_MANIFEST.md` - Integration guide

### Quick Start - Foundation Layer

```bash
# Complete foundation layer seeding
./scripts/foundation-seed-master.sh

# Or step-by-step:
./scripts/cognitive-kernel-seed.sh    # 1. Seed kernel
./scripts/foundation-build.sh         # 2. Build
./scripts/foundation-test.sh          # 3. Test
```

### Output Artifacts

**Build Artifacts** (`build/foundation/artifacts/`):
- `build_summary.json` - Build metadata
- `hardware_matrix.json` - Architecture capabilities
- `*_tensor_config.json` - Tensor configurations
- `*_compile_commands.json` - Compilation databases
- `foundation_install.tar.gz` - Installation archive
- `TENSOR_DEGREES_OF_FREEDOM.md` - Tensor documentation

**Test Results** (`build/foundation/test-results/`):
- `foundation_test_report.json` - Test summary
- Individual test logs per component

**Cognitive Kernel** (`cognitive-kernel-seed/`):
- Meta-system loop implementations (Scheme)
- Autognosis initialization scripts
- Self-image building process
- Configuration files (JSON)

### Tensor Parameterization

**Format:** `[modules, build-steps, tests]`

- **CogUtil**: [64, 32, 16] = 32,768 DOF
- **AtomSpace**: [1024, 512, 256] = 134,217,728 DOF

Enables neural-symbolic integration via GGML kernel.

### Meta-System Loops

**Level 0 (Object)**: perceive, reason, act - 10 Hz  
**Level 1 (Meta)**: monitor, evaluate, adapt - 2 Hz  
**Level 2 (Meta-Meta)**: introspect, improve, evolve - 0.1 Hz  

Each level recursively observes itself and lower levels, enabling autognosis emergence.

### Prerequisites

**Required:**
- cmake (>= 3.10)
- g++ (C++17 support)
- make or ninja

**Optional:**
- guile (for Scheme execution)
- ccache (for faster builds)
- jq (for JSON validation)

**Install on Ubuntu:**
```bash
sudo apt-get install -y cmake build-essential libboost-all-dev \
  guile-2.2-dev ninja-build ccache jq
```

### Documentation

- **Complete Guide**: `FOUNDATION_LAYER_GUIDE.md`
- **Integration Manifest**: `cognitive-kernel-seed/INTEGRATION_MANIFEST.md`
- **Cognitive Kernel Architecture**: `COGNITIVE_KERNEL_ARCHITECTURE.md`

---

**Part of the Entelechy Framework** - Systematic actualization through continuous improvement 🧠✨
