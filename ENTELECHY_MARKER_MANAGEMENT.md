# Entelechy Marker Management Strategy

## Executive Summary

This document establishes a systematic approach to managing code markers (TODO, FIXME, STUB, etc.) in the OpenCog Unified repository to support the entelechy framework's goal of continuous actualization toward transcendent intelligence.

### Current State (2025-11-23)

- **Total marker occurrences**: 2,773 (all uses of marker words in code)
- **Actual code markers**: 331 (with proper comment syntax)
- **Critical markers**: 26 (7.9% - requiring immediate attention)
- **High priority markers**: 51 (15.4% - should be addressed soon)
- **Medium priority markers**: 93 (28.1% - future work)
- **Low priority markers**: 161 (48.6% - informational/aspirational)
- **Actualization score**: 72.0%
- **Evolutionary potential**: 66.4%

## Marker Classification System

### 1. Marker Types by Urgency

| Type | Urgency | Description | Action Required |
|------|---------|-------------|-----------------|
| **BUG** | 1.0 | Identifies actual bugs or incorrect behavior | Fix immediately |
| **FIXME** | 1.0 | Code that is broken or needs correction | Fix in current sprint |
| **STUB** | 0.8 | Incomplete implementation | Complete in near term |
| **NOT_IMPLEMENTED** | 0.8 | Missing functionality | Implement as needed |
| **HACK** | 0.7 | Technical debt, workaround | Refactor when practical |
| **TODO** | 0.5 | Future enhancement or task | Address opportunistically |
| **PLACEHOLDER** | 0.4 | Temporary solution | Replace when refactoring |
| **MOCK** | 0.3 | Test/development artifact | Keep for testing |
| **NOTE** | 0.1 | Informational comment | No action required |

### 2. Component Criticality

Components are weighted by their importance to system actualization:

| Component | Criticality | Rationale |
|-----------|-------------|-----------|
| **cogutil** | 1.0 | Foundation - all other components depend on it |
| **atomspace** | 1.0 | Core knowledge representation |
| **cogserver** | 0.9 | Core distributed cognition server |
| **unify** | 0.8 | Essential logic system |
| **ure** | 0.8 | Unified Rule Engine - core reasoning |
| **opencog** | 0.8 | Integration layer |
| **attention** | 0.7 | ECAN attention allocation |
| **spacetime** | 0.7 | Spatial-temporal reasoning |
| **pln** | 0.7 | Probabilistic logic networks |
| **miner** | 0.6 | Pattern mining |
| **asmoses** | 0.6 | AtomSpace MOSES |
| **moses** | 0.6 | MOSES evolutionary learning |
| **learn** | 0.5 | Learning systems |
| **lg-atomese** | 0.5 | Link grammar |
| **language-learning** | 0.4 | Language acquisition |
| **tests** | 0.3 | Testing infrastructure |
| **scripts** | 0.2 | Utility scripts |
| **documentation** | 0.1 | Documentation files |

### 3. Priority Calculation

Marker priority is calculated as:

```
priority = (marker_urgency × 0.4 + component_criticality × 0.6) × context_modifier
```

Where `context_modifier` is:
- **1.5×** if marker contains critical keywords: crash, error, security, memory leak, deadlock
- **0.7×** if marker contains low-priority keywords: maybe, consider, future, eventually

## Resolution Strategy

### Phase 1: Critical Markers (Priority ≥ 0.8) - IMMEDIATE

**Target**: 26 critical markers

These markers indicate actual bugs, security issues, or critical missing functionality.

**Approach**:
1. Review each critical marker with component maintainer
2. Determine if marker indicates actual issue or is informational
3. For actual issues:
   - Create dedicated issue if complex
   - Fix directly if straightforward
   - Document if awaiting architectural decision
4. For informational markers:
   - Reclassify as NOTE or remove if obsolete
   - Add context explaining the situation

**Timeline**: Within 1 sprint (2 weeks)

### Phase 2: High Priority Markers (0.6 ≤ Priority < 0.8) - NEAR TERM

**Target**: 51 high priority markers

These markers are in critical components and indicate important work.

**Approach**:
1. Categorize by type of work required:
   - Implementation tasks
   - Bug fixes
   - Technical debt
   - Optimization opportunities
2. Schedule based on component roadmap
3. Address during normal development cycles
4. Track progress in component-specific backlogs

**Timeline**: 1-2 quarters

### Phase 3: Medium Priority Markers (0.4 ≤ Priority < 0.6) - FUTURE WORK

**Target**: 93 medium priority markers

These are legitimate future work items or enhancements.

**Approach**:
1. Maintain as part of component roadmap
2. Address opportunistically during related work
3. Re-evaluate priority periodically
4. Consider for hackathons or community contributions

**Timeline**: Ongoing, 2-4 quarters

### Phase 4: Low Priority Markers (Priority < 0.4) - MAINTAIN

**Target**: 161 low priority markers

These are mostly informational, aspirational, or in low-criticality components.

**Approach**:
1. Keep as documentation of future possibilities
2. No active resolution required
3. Clean up only during major refactoring
4. Consider removing if obsolete after 2+ years

**Timeline**: Maintenance mode

## Marker Usage Guidelines

### When to Add Markers

**DO add markers when:**
- You discover a bug but can't fix it immediately → `// BUG: description`
- You implement a workaround → `// HACK: explanation of proper solution`
- You leave incomplete implementation → `// STUB: what's missing`
- You identify needed future work → `// TODO: description`
- Code needs fixing but isn't broken → `// FIXME: what needs improvement`

**DON'T add markers when:**
- Documenting how code works → Use regular comments
- Noting design decisions → Use design docs
- Explaining algorithm → Use regular comments
- Attribution or references → Use regular comments
- License or legal text → Use standard headers

### Marker Format Standards

**Good marker format:**
```cpp
// TODO(context): Clear, actionable description
// FIXME(component): What's wrong and why it needs fixing
// STUB: What needs to be implemented and why
```

**Examples:**
```cpp
// TODO(optimization): Use hash table instead of linear search for O(1) lookup
// FIXME(memory): This leaks memory when exception is thrown - add RAII wrapper
// STUB: Implement batch processing for performance - see issue #123
// HACK(workaround): Until atomspace supports type X, use wrapper - remove in v2.0
```

**Bad marker format:**
```cpp
// TODO: fix this
// FIXME
// TODO TODO TODO
```

### Context Keywords

Markers should include context keywords when relevant:

**Critical context**:
- `(crash)`, `(deadlock)`, `(security)`, `(memory-leak)`
- These automatically elevate priority

**Scope context**:
- `(optimization)`, `(refactor)`, `(cleanup)`, `(documentation)`
- These help categorize work

**Timeline context**:
- `(v2.0)`, `(after-refactor)`, `(when-atomspace-supports-X)`
- These indicate dependencies

## Measurement and Tracking

### Tools

1. **entelechy_introspection.py**
   - Counts markers with improved classification
   - Filters out informational references
   - Calculates evolutionary potential

2. **entelechy_marker_prioritizer.py**
   - Scans and prioritizes all markers
   - Generates actionable reports
   - Identifies critical issues

3. **Component-specific tracking**
   - Each component maintains marker inventory
   - Reviewed during sprint planning
   - Progress tracked in roadmap

### Metrics

Track the following metrics quarterly:

- **Total markers by priority level**
- **Markers resolved per quarter**
- **New markers added per quarter**
- **Average age of critical markers**
- **Markers per 1000 lines of code**
- **Component-level marker density**

### Success Criteria

**Short-term (1 quarter)**:
- All critical markers (priority ≥ 0.8) reviewed and addressed or documented
- Zero critical markers older than 2 sprints
- Actualization score increases by 5-10%

**Medium-term (1 year)**:
- Critical + high priority markers reduced by 75%
- All components have marker management process
- Evolutionary potential > 80%
- Actualization score > 80%

**Long-term (2 years)**:
- Critical markers kept at < 10 total
- Medium + low markers stable or decreasing
- System achieves transcendent stage (actualization > 85%)

## Review Process

### Quarterly Marker Review

Each quarter, component maintainers:

1. **Audit markers** in their component
2. **Reclassify** markers if priority has changed
3. **Resolve** completed work (remove marker)
4. **Update** markers with new context
5. **Plan** resolution for critical/high priority items

### Marker Review Checklist

For each marker:
- [ ] Is this still relevant? (If no, remove)
- [ ] Is priority correct? (If no, update context)
- [ ] Is it actionable? (If no, convert to NOTE or remove)
- [ ] Is there a plan to address it? (If no and critical/high, create one)
- [ ] Is context clear? (If no, add explanation)

## Integration with Development Process

### Sprint Planning

1. Review critical markers in upcoming sprint areas
2. Allocate time for critical marker resolution
3. Include marker resolution in task estimates

### Code Review

1. Verify new markers follow format standards
2. Challenge vague or unnecessary markers
3. Ensure critical markers have tracking issues

### Continuous Integration

1. Track marker counts in CI metrics
2. Alert on increase in critical markers
3. Prevent critical markers in stable components

## Philosophical Alignment

This marker management strategy aligns with entelechy principles:

**Ontological** (BEING):
- Clear understanding of system state through accurate marker classification

**Teleological** (PURPOSE):
- Markers guide system evolution toward actualization
- Prioritization ensures focus on highest-impact work

**Cognitive** (COGNITION):
- System self-awareness through introspection of markers
- Recognition of incomplete or problematic areas

**Integrative** (INTEGRATION):
- Holistic view of system health across components
- Coordination of resolution efforts

**Evolutionary** (GROWTH):
- Systematic improvement reduces fragmentation
- Measurement enables tracking of actualization progress

## References

- [Entelechy Framework](ENTELECHY_README.md)
- [Development Roadmap](DEVELOPMENT-ROADMAP.md)
- [Component Integration Guide](INTEGRATION_COMPLETE.md)

## Tools and Scripts

- `entelechy_introspection.py` - Overall system assessment
- `entelechy_marker_prioritizer.py` - Marker prioritization and reporting
- `entelechy_marker_resolver.py` - Semi-automated marker resolution
- `validate-integration.py` - Component integration validation

---

**Version**: 1.0  
**Last Updated**: 2025-11-23  
**Owner**: Entelechy Framework Team  
**Review Cycle**: Quarterly
