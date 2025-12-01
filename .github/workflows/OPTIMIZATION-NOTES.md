# Windows Build Workflow - Future Optimizations

## Code Review Findings

The following optimizations were identified during code review:

### 1. Chocolatey Package Caching
**Location:** ocwin-build.yml, lines 46-47  
**Issue:** Chocolatey packages are downloaded and installed repeatedly across jobs  
**Optimization:** Cache the Chocolatey installation directory

**Potential Implementation:**
```yaml
- name: Cache Chocolatey Packages
  uses: actions/cache@v4
  with:
    path: C:\ProgramData\chocolatey\lib
    key: choco-packages-${{ hashFiles('.github/workflows/ocwin-build.yml') }}
    restore-keys: |
      choco-packages-
```

### 2. Component Rebuild Duplication
**Location:** ocwin-build.yml, lines 126-132, 191-197 (and similar throughout)  
**Issue:** CogUtil and AtomSpace are rebuilt in every dependent job  
**Optimization:** Use artifacts from previous jobs or extract into reusable action

**Potential Solutions:**

**Option A - Reusable Action:**
```yaml
# .github/actions/install-cogutil/action.yml
name: Install CogUtil
runs:
  using: composite
  steps:
    - name: Build and Install CogUtil
      run: |
        mkdir cogutil\build -Force
        cd cogutil\build
        cmake .. -G "${{ env.CMAKE_GENERATOR }}" ...
        cmake --build . --config ${{ env.BUILD_TYPE }}
        cmake --install . --config ${{ env.BUILD_TYPE }}
      shell: powershell
```

**Option B - Artifact Sharing:**
```yaml
# In build-cogutil job
- name: Package Installation
  run: cmake --install . --config Release --prefix install-dir
- uses: actions/upload-artifact@v4
  with:
    name: cogutil-install
    path: install-dir/

# In dependent jobs
- uses: actions/download-artifact@v4
  with:
    name: cogutil-install
    path: C:/Program Files/
```

**Option C - Matrix Strategy:**
```yaml
strategy:
  matrix:
    component:
      - name: cogutil
        depends: []
      - name: atomspace
        depends: [cogutil]
      - name: cogserver
        depends: [cogutil, atomspace]
```

## Impact Assessment

### Current Performance
- Each job rebuilds dependencies: ~5-10 minutes per component
- Total unnecessary rebuild time: ~30-50 minutes per workflow run
- Storage: Multiple copies of same artifacts

### Optimized Performance (Estimated)
- Artifact download/cache restore: ~1-2 minutes
- Storage savings: ~60-70%
- Total time savings: ~25-40 minutes per run

## Implementation Priority

1. **High Priority:** Component rebuild duplication (biggest time savings)
2. **Medium Priority:** Chocolatey package caching (moderate savings, complexity)
3. **Low Priority:** Further parallelization (diminishing returns)

## Tradeoffs

### Reusable Actions
**Pros:**
- DRY principle
- Easier maintenance
- Consistent behavior

**Cons:**
- Additional complexity
- Harder to debug
- Learning curve

### Artifact Sharing
**Pros:**
- Simple to implement
- Works with existing structure
- Good for incremental builds

**Cons:**
- Artifact upload/download overhead
- Storage costs
- Version consistency challenges

### Matrix Strategy
**Pros:**
- Clean YAML
- Automatic parallelization
- Scalable

**Cons:**
- Major refactoring required
- Complex dependency management
- Limited flexibility

## Recommendation

**Phase 1 (Immediate):**
- Document these optimizations (this file)
- Keep current simple approach for experimental phase

**Phase 2 (After Testing):**
- Implement artifact sharing for CogUtil/AtomSpace
- Add Chocolatey package caching

**Phase 3 (Production Ready):**
- Consider reusable actions if patterns emerge
- Evaluate matrix strategy for full component set

## Testing These Optimizations

```bash
# Test artifact sharing
gh workflow run ocwin-build.yml --ref feature/artifact-optimization

# Compare timing
gh run list --workflow=ocwin-build.yml --limit=5

# Check artifact sizes
gh run view <run-id> --log | grep "upload-artifact"
```

## References

- [GitHub Actions: Caching dependencies](https://docs.github.com/en/actions/using-workflows/caching-dependencies-to-speed-up-workflows)
- [GitHub Actions: Reusing workflows](https://docs.github.com/en/actions/using-workflows/reusing-workflows)
- [GitHub Actions: Artifacts](https://docs.github.com/en/actions/using-workflows/storing-workflow-data-as-artifacts)

## Status

- [x] Identified optimizations
- [x] Documented tradeoffs
- [ ] Implemented artifact caching
- [ ] Implemented component sharing
- [ ] Performance benchmarking

Current approach prioritizes clarity and debuggability over optimization, which is appropriate for experimental workflows.
