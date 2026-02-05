# Placeholder Resolution Guide

## Overview

This document provides a systematic approach to resolving placeholder markers (TODO, FIXME, STUB, etc.) in the OpenCog Unified codebase.

## Current Status

**Last Updated**: 2025-12-26

**Total Markers**: 400 (filtered, excluding std::placeholders)

### Distribution by Category

| Category | Count | Priority | Approach |
|----------|-------|----------|----------|
| Complex Implementation | 202 | Low | Requires architectural decisions |
| Trivial Implementation | 63 | High | Can be resolved quickly |
| Design Decision | 57 | Medium | Requires team discussion |
| Thread Safety | 30 | High | Critical for production |
| Optimization | 19 | Medium | Requires benchmarking |
| Testing | 12 | High | Improves reliability |
| Error Handling | 11 | High | Improves robustness |
| Documentation | 6 | High | Quick wins |

### Distribution by Component

| Component | Count | Notes |
|-----------|-------|-------|
| moses | 146 | Evolutionary optimization - many are research notes |
| atomspace | 109 | Core component - prioritize critical path |
| ure | 59 | Rule engine - many are future enhancements |
| unify | 8 | Pattern matching - well-maintained |
| opencog | 8 | Integration layer |
| cogserver | 2 | Server component - stable |
| atomspace-rocks | 2 | Storage backend - stable |

## Resolution Strategy

### Phase 1: Documentation and Comments (Weeks 1-2)

**Goal**: Convert informal TODOs to proper documentation

**Actions**:
- Remove "stub" language from functioning code
- Add proper API documentation where TODOs indicate gaps
- Convert design questions to documented trade-offs
- Update outdated FIXME comments

**Example**:
```cpp
// Before:
// TODO: Document this function

// After:
/**
 * @brief Performs pattern matching on the given atom.
 * @param pattern The pattern to match against
 * @param callback Function to call for each match
 * @return Number of matches found
 */
```

### Phase 2: Trivial Implementations (Weeks 3-4)

**Goal**: Implement simple stub functions and add basic error handling

**Criteria for "Trivial"**:
- Function body < 10 lines
- No complex algorithms required
- Clear expected behavior from context
- No dependency on unimplemented features

**Examples**:
- Adding null checks
- Throwing NotImplementedException with proper message
- Implementing simple getters/setters
- Adding basic validation logic

### Phase 3: Error Handling and Testing (Weeks 5-6)

**Goal**: Add robust error handling and test coverage

**Actions**:
- Add error checks where TODOs indicate need
- Implement basic unit tests for untested code
- Add input validation
- Improve error messages

### Phase 4: Thread Safety (Weeks 7-10)

**Goal**: Address concurrency issues

**Actions**:
- Audit thread safety TODOs/FIXMEs
- Add appropriate locking mechanisms
- Use atomic operations where appropriate
- Add thread safety tests

### Phase 5: Optimization (Weeks 11-14)

**Goal**: Address performance TODOs

**Approach**:
- Benchmark current performance
- Implement optimizations only where proven beneficial
- Document performance characteristics
- Add performance regression tests

### Phase 6: Complex Implementations (Ongoing)

**Goal**: Address architectural TODOs

**Approach**:
- Each complex TODO requires:
  - Design document
  - Team review
  - Implementation plan
  - Test strategy
- Prioritize based on user impact
- May require breaking changes

## Resolution Process

### For Each Marker:

1. **Analyze**:
   - Read surrounding code
   - Understand the intent
   - Check if still relevant
   - Assess complexity

2. **Categorize**:
   - Documentation only
   - Simple implementation
   - Complex implementation
   - Obsolete (can be removed)

3. **Action**:
   - **Documentation**: Add proper docs, remove TODO
   - **Simple**: Implement, test, remove marker
   - **Complex**: Create issue, add detailed TODO with issue number
   - **Obsolete**: Remove marker, document why

4. **Validate**:
   - Run relevant tests
   - Check for regressions
   - Update documentation
   - Commit with descriptive message

## Guidelines

### What to Fix Immediately

✅ Obsolete TODOs referencing completed work
✅ Missing documentation markers
✅ Simple error handling additions
✅ Trivial stub implementations
✅ Outdated comments that are now incorrect

### What to Document and Defer

⏸️ Complex architectural changes
⏸️ Performance optimizations requiring benchmarking
⏸️ Features requiring design decisions
⏸️ Changes requiring external dependencies
⏸️ Breaking changes to public APIs

### What to Remove

❌ TODOs for completed features
❌ FIXMEs for resolved bugs
❌ Obsolete workarounds
❌ Deprecated features no longer supported
❌ Duplicate markers (same issue noted multiple times)

## Metrics and Tracking

### Success Metrics

- **Reduction Rate**: Markers resolved per week
- **Quality**: Zero regressions from resolutions
- **Coverage**: Test coverage improvement
- **Documentation**: API documentation completeness

### Tracking

Use `entelechy_marker_resolver.py` for progress tracking:

```bash
# Generate current status
./entelechy_marker_resolver.py --report

# Export actionable items
./entelechy_marker_resolver.py --export

# Update resolution status
./entelechy_marker_resolver.py --resolve MARKER_ID
```

## Examples

### Example 1: Documentation

**Before**:
```cpp
// TODO: explain this
Handle process(Handle h) { ... }
```

**After**:
```cpp
/**
 * Process an atom and return its normalized form.
 * 
 * @param h The atom to process
 * @return Normalized atom, or original if already normalized
 */
Handle process(Handle h) { ... }
```

### Example 2: Trivial Implementation

**Before**:
```cpp
bool validate() {
    // TODO: add validation
    return true;
}
```

**After**:
```cpp
bool validate() {
    if (!initialized_) {
        logger().error("Validator not initialized");
        return false;
    }
    if (data_.empty()) {
        logger().warn("No data to validate");
        return false;
    }
    return true;
}
```

### Example 3: Complex → Documented

**Before**:
```cpp
// TODO: implement distributed consensus algorithm
void synchronize() { }
```

**After**:
```cpp
/**
 * Synchronize state across distributed nodes.
 * 
 * TODO(#1234): Implement full Byzantine fault-tolerant consensus.
 * Current implementation uses simple leader election.
 * 
 * See docs/distributed-consensus.md for design discussion.
 */
void synchronize() {
    // Current simple implementation
    elect_leader();
    replicate_to_followers();
}
```

## Anti-Patterns

### Don't Do This

❌ Remove TODOs without addressing them
❌ Replace TODO with another TODO
❌ Implement complex features without design review
❌ Add new TODOs while resolving old ones
❌ Break existing functionality to resolve markers

### Do This Instead

✅ Remove obsolete markers completely
✅ Convert TODOs to tracked issues
✅ Document complex decisions
✅ Test all changes
✅ Maintain backward compatibility

## Tools

### Analysis

```bash
# Categorize markers
python3 /tmp/categorize_markers.py

# Find easy wins
./entelechy_marker_analyzer.py --quick-wins

# Generate resolution roadmap
./entelechy_marker_prioritizer.py
```

### Resolution

```bash
# Track progress
./entelechy_marker_resolver.py --report

# Validate fixes
./validate-integration.py

# Run tests
cd build && ctest
```

## Communication

### Commit Messages

Format: `resolve(component): Brief description of resolution`

Examples:
- `resolve(atomspace): Add API documentation for PatternMatcher`
- `resolve(moses): Implement validation in precision_bscore`
- `resolve(ure): Remove obsolete TODO for completed feature`

### Pull Requests

Title: `[Placeholder Resolution] Component - Brief summary`

Include:
- Category of resolutions (docs, trivial, etc.)
- Number of markers resolved
- Testing performed
- Any breaking changes

## References

- [ENTELECHY_MARKER_RESOLUTION.md](ENTELECHY_MARKER_RESOLUTION.md) - Original analysis
- [IMPLEMENTATION-ROADMAP.md](IMPLEMENTATION-ROADMAP.md) - Detailed implementation plan
- [entelechy_marker_analyzer.py](entelechy_marker_analyzer.py) - Analysis tool
- [entelechy_marker_resolver.py](entelechy_marker_resolver.py) - Resolution tracking

## Conclusion

Placeholder resolution is an ongoing process. The goal is not to eliminate all markers, but to:

1. **Document** complex decisions properly
2. **Implement** straightforward improvements
3. **Track** remaining work systematically
4. **Prioritize** based on user impact
5. **Maintain** code quality throughout

Progress over perfection. Systematic reduction over complete elimination.
