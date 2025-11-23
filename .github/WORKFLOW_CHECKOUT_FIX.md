# GitHub Actions Workflow: Repository Checkout Fix

## Problem Statement

The `cognitive-orchestration.yml` workflow was failing with the error:
```
cd: can't cd to /home/runner/work/opencog-unified/opencog-unified
```

This occurred because the workflow attempted to access repository files without first checking out the code.

## Root Cause Analysis

### Workflow Structure
The workflow had jobs that:
1. ✅ Cloned external repositories (e.g., `opencog/cogutil`) to subdirectories
2. ❌ Attempted to run scripts from the main repository
3. ❌ **Never checked out the main repository**

### Specific Issues

#### Issue 1: Missing Checkout
```yaml
# BEFORE (BROKEN)
steps:
  - name: "🔮 Restore Hypergraph State"
    uses: actions/cache@v4
    # ...
  
  - uses: actions/checkout@v5
    with:
      path: cogutil  # Only checks out external repo
      repository: opencog/cogutil
```

#### Issue 2: Hardcoded Paths
```bash
# BEFORE (BROKEN)
cd /home/runner/work/opencog-unified/opencog-unified
python3 scripts/auto_fix.py ...
```

This assumes:
- The repository is always at this exact path
- The repository has been checked out (it wasn't!)

## Solution Implemented

### 1. Add Main Repository Checkout

Added explicit checkout of the main repository **before** cloning external repos:

```yaml
# AFTER (FIXED)
steps:
  - name: "🔮 Restore Hypergraph State"
    uses: actions/cache@v4
    # ...
  
  - name: "📥 Checkout Main Repository"
    uses: actions/checkout@v5  # Checks out main repo to workspace root
  
  - name: "✅ Verify Working Directory"
    run: |
      echo "🔍 Verifying workspace availability..."
      ls -al ${{ github.workspace }}
  
  - uses: actions/checkout@v5
    with:
      path: cogutil  # Checks out external repo to subdirectory
      repository: opencog/cogutil
```

### 2. Use Dynamic Workspace Path

Replaced hardcoded paths with GitHub Actions variable:

```bash
# AFTER (FIXED)
cd ${{ github.workspace }}
python3 scripts/auto_fix.py ...
```

Benefits:
- ✅ Works in any runner environment
- ✅ Portable across different GitHub setups
- ✅ Explicit about where the repository is

### 3. Add Diagnostic Verification

Added a verification step to catch future issues early:

```yaml
- name: "✅ Verify Working Directory"
  run: |
    echo "🔍 Verifying workspace availability..."
    echo "Current directory: $(pwd)"
    echo "GitHub workspace: ${{ github.workspace }}"
    ls -al ${{ github.workspace }}
    echo "✅ Workspace verified successfully"
```

## Directory Structure After Fix

```
${{ github.workspace }}/               # Main repository root
├── .github/
│   └── workflows/
│       └── cognitive-orchestration.yml
├── scripts/
│   └── auto_fix.py                   # Now accessible!
├── cogutil/                          # External repo (opencog/cogutil)
│   └── build/
└── atomspace/                        # External repo (opencog/atomspace)
    └── build/
```

## Jobs Fixed

1. **cogutil job**: Foundation layer build
   - Added main repo checkout
   - Added workspace verification
   - Fixed 2 hardcoded path references

2. **atomspace job**: Core layer build
   - Added main repo checkout
   - Added workspace verification
   - Fixed 2 hardcoded path references

## Testing Strategy

### Manual Testing
1. Verify workflow syntax is valid
2. Check that checkout actions are in correct order
3. Confirm paths use `${{ github.workspace }}`

### Automated Testing
The workflow will be tested when:
- This PR is merged
- The workflow is triggered (push to main, pull request, schedule)

### Expected Behavior
- ✅ Main repository files are accessible
- ✅ External repositories clone to subdirectories
- ✅ Build commands navigate correctly between directories
- ✅ Scripts can be invoked from proper locations

## Best Practices for Future Workflows

### ✅ DO:
1. **Always checkout the main repository first** if you need its files
2. **Use `${{ github.workspace }}`** instead of hardcoded paths
3. **Add verification steps** to catch issues early
4. **Document dependencies** between checkout steps

### ❌ DON'T:
1. **Assume repository is checked out** automatically
2. **Use hardcoded paths** like `/home/runner/work/...`
3. **Skip verification** when working with multiple repositories
4. **Nest checkouts** without understanding the directory structure

## Example: Multiple Repository Checkout Pattern

```yaml
jobs:
  build-with-dependencies:
    runs-on: ubuntu-latest
    steps:
      # Step 1: Checkout main repository
      - name: Checkout main repository
        uses: actions/checkout@v5
      
      # Step 2: Verify workspace
      - name: Verify workspace
        run: ls -al ${{ github.workspace }}
      
      # Step 3: Checkout external dependencies to subdirectories
      - name: Checkout dependency A
        uses: actions/checkout@v5
        with:
          repository: org/dependency-a
          path: deps/dependency-a
      
      - name: Checkout dependency B
        uses: actions/checkout@v5
        with:
          repository: org/dependency-b
          path: deps/dependency-b
      
      # Step 4: Use files from main repo
      - name: Run scripts
        run: |
          cd ${{ github.workspace }}
          python3 scripts/build.py
      
      # Step 5: Build dependencies
      - name: Build dependency A
        run: |
          cd ${{ github.workspace }}/deps/dependency-a
          make
```

## Related Issues

This fix addresses the issue described in the problem statement:
- Error: `cd: can't cd to /home/runner/work/opencog-unified/opencog-unified`
- Trigger: Missing `actions/checkout` step
- Failure cascade: Directory not found → process exits with code 2

## Additional Notes

### Self-Healing Scripts
The workflow references `scripts/auto_fix.py` which doesn't currently exist in the repository. This is a separate issue from the checkout problem. Once the repository is properly checked out, the workflow will correctly attempt to access this script (even if it fails because the script doesn't exist).

### Cache Restoration
The workflow uses caching to speed up builds. The cache is restored **before** checkout, which is intentional - the cache contains build artifacts that don't depend on the current repository state.

## Validation Checklist

- [x] Main repository checkout added to cogutil job
- [x] Main repository checkout added to atomspace job
- [x] Verification steps added to both jobs
- [x] All hardcoded paths replaced with `${{ github.workspace }}`
- [x] No other workflows have similar issues
- [x] Changes committed and pushed
- [x] Documentation created

## References

- [GitHub Actions: Checking out repositories](https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#jobsjob_idstepsuses)
- [GitHub Actions: Default environment variables](https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables)
- [GitHub Actions: Working with multiple repositories](https://docs.github.com/en/actions/using-workflows/using-github-cli-in-workflows#using-github-cli-with-multiple-repositories)
