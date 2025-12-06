# GitHub Actions Workflow Issues Analysis

## Executive Summary

Analysis of 16 GitHub Actions workflows in the opencog-unified repository has identified **critical missing script dependencies** that will cause workflow failures. This document categorizes issues by severity and provides implementation priorities.

## Critical Issues (Blocking Workflow Execution)

### 1. Missing Script Files

The following workflows reference scripts that do not exist in the repository:

#### bootstrap.yml
- ❌ `tests/comprehensive-test-runner.sh` - **EXISTS** ✅
- ❌ `scripts/extract-attention-metrics.sh` - **EXISTS** ✅
- ❌ `scripts/test-neural-symbolic-integration.sh` - **EXISTS** ✅
- ❌ `scripts/simple-tensor-analysis.sh` - **EXISTS** ✅
- ❌ `scripts/encode-hypergraph-patterns.sh` - **EXISTS** ✅
- ❌ `scripts/analyze-meta-completeness.sh` - **EXISTS** ✅
- ❌ `scripts/generate-recursive-feedback.sh` - **EXISTS** ✅

**Status**: All scripts exist in root scripts/ directory. No issues.

#### cognitive-membrane-sync.yml
- ❌ `scripts/cognitive-membrane/extract-cognitive-patterns.py` - **MISSING**
- ❌ `scripts/cognitive-membrane/generate-membrane-report.py` - **MISSING**

**Impact**: Workflow will fail at cognitive pattern extraction step.

#### cognitive-orchestration.yml
- ❌ `scripts/cognitive-orchestration/extract-architecture.py` - **MISSING**
- ❌ `scripts/cognitive-orchestration/generate-cognitive-issues.py` - **MISSING**
- ❌ `scripts/cognitive-orchestration/generate-orchestration-report.py` - **MISSING**

**Impact**: Workflow will fail at architecture extraction step.

#### cognitive-synergy-engine.yml
- ❌ `scripts/cognitive-synergy/analyze-synergy.py` - **MISSING**
- ❌ `scripts/cognitive-synergy/generate-synergy-report.py` - **MISSING**

**Impact**: Workflow will fail at synergy analysis step.

#### entelechy-assessment.yml
- ❌ `scripts/entelechy/extract-entelechy-metrics.py` - **MISSING**
- ❌ `scripts/entelechy/generate-entelechy-report.py` - **MISSING**

**Impact**: Workflow will fail at entelechy extraction step.

#### fixme-tracking.yml
- ❌ `scripts/track-fixme.sh` - **MISSING**

**Impact**: Workflow will fail at FIXME tracking step.

#### gen-cognitive-issues.yml
- ❌ `scripts/generate-cognitive-issues.sh` - **MISSING**

**Impact**: Workflow will fail at issue generation step.

#### ontogenesis-orchestration.yml
- ❌ `scripts/ontogenesis/parse-architecture.py` - **MISSING**
- ❌ `scripts/ontogenesis/generate-tensor-analysis.py` - **MISSING**
- ❌ `scripts/ontogenesis/analyze-dependencies.py` - **MISSING**
- ❌ `scripts/ontogenesis/generate-orchestration-issues.py` - **MISSING**

**Impact**: Workflow will fail at architecture parsing step.

### 2. Missing Script Subdirectories

The following subdirectories need to be created:
- `scripts/cognitive-membrane/`
- `scripts/cognitive-orchestration/`
- `scripts/cognitive-synergy/`
- `scripts/entelechy/`
- `scripts/ontogenesis/`

## High Priority Issues (Best Practices)

### 1. Missing Error Handling in Shell Scripts

Many workflows contain multi-line shell scripts without `set -e` or `set -euo pipefail`, which means errors in the middle of scripts may be silently ignored. This affects:

- **occ-build.yml**: 33 instances
- **ocwin-build.yml**: 32 instances
- **cognitive-orchestration.yml**: Multiple instances
- **ontogenesis-orchestration.yml**: Multiple instances
- **st.yml**: 11 instances
- **todo-catalog-update.yml**: 5 instances

### 2. Missing Permissions Declarations

The following workflows lack top-level permissions declarations:
- apt-packaging.yml
- chocolatey-packaging.yml
- cognitive-membrane-sync.yml
- cognitive-orchestration.yml
- cognitive-synergy-engine.yml
- debian-packaging.yml
- electron-packaging.yml
- entelechy-assessment.yml
- fixme-tracking.yml
- gen-cognitive-issues.yml
- occ-build.yml
- ocwin-build.yml
- ontogenesis-orchestration.yml
- st.yml
- todo-catalog-update.yml

## Medium Priority Issues (Warnings)

### 1. Outdated Action Versions

- **todo-catalog-update.yml**: Uses `actions/setup-python@v6` (consider updating to latest stable)

## Implementation Priority

### Phase 1: Critical Script Creation (Immediate)
1. Create missing script subdirectories
2. Implement missing Python scripts for cognitive workflows
3. Implement missing shell scripts for tracking workflows

### Phase 2: Error Handling Enhancement (High Priority)
1. Add `set -euo pipefail` to all multi-line shell scripts
2. Add appropriate error handling and logging

### Phase 3: Security & Best Practices (Medium Priority)
1. Add explicit permissions declarations to all workflows
2. Update action versions to latest stable releases
3. Add timeout configurations where appropriate

## Affected Workflows Summary

| Workflow | Critical Issues | High Priority Issues | Status |
|----------|----------------|---------------------|---------|
| bootstrap.yml | 0 | Multiple | ✅ Scripts exist |
| cognitive-membrane-sync.yml | 2 scripts | Multiple | ❌ Needs implementation |
| cognitive-orchestration.yml | 3 scripts | Multiple | ❌ Needs implementation |
| cognitive-synergy-engine.yml | 2 scripts | Multiple | ❌ Needs implementation |
| entelechy-assessment.yml | 2 scripts | Multiple | ❌ Needs implementation |
| fixme-tracking.yml | 1 script | Multiple | ❌ Needs implementation |
| gen-cognitive-issues.yml | 1 script | Multiple | ❌ Needs implementation |
| ontogenesis-orchestration.yml | 4 scripts | Multiple | ❌ Needs implementation |
| occ-build.yml | 0 | 33 warnings | ⚠️ Needs error handling |
| ocwin-build.yml | 0 | 32 warnings | ⚠️ Needs error handling |
| st.yml | 0 | 11 warnings | ⚠️ Needs error handling |
| todo-catalog-update.yml | 0 | 5 warnings | ⚠️ Needs error handling |
| apt-packaging.yml | 0 | Permissions | ⚠️ Minor issues |
| chocolatey-packaging.yml | 0 | Permissions | ⚠️ Minor issues |
| debian-packaging.yml | 0 | Permissions | ⚠️ Minor issues |
| electron-packaging.yml | 0 | Permissions | ⚠️ Minor issues |

## Next Steps

1. ✅ Create missing script directories
2. ✅ Implement missing Python scripts with proper functionality
3. ✅ Implement missing shell scripts
4. ✅ Add error handling to existing scripts
5. ✅ Add permissions declarations to workflows
6. ✅ Test workflow execution
7. ✅ Commit and push changes
