# FIXME Resolution Implementation Guide

## Overview

This guide implements the "Next Steps" from FIXME-SORTED-CATALOG.md, providing a systematic approach to resolving all 605 FIXME instances across the OpenCog Unified repository.

## Current Status

- **Total FIXME items**: 605
- **Resolved**: 4 (0.7%)
- **Easy wins remaining**: 35
- **Phase 1 progress**: 10.3% complete

## Implementation Strategy

### Phase 1: Immediate Actions (Next 2 weeks) - IN PROGRESS

**Goal**: Complete all EASY items to build momentum
**Target**: 39 items → **4 completed**, 35 remaining

#### Completed ✅
1. `atomspace/opencog/scm/opencog/base/atom-docs.scm:41` - Added proper BindLink documentation
2. `atomspace/opencog/scm/opencog.scm:122` - Updated outdated FIXME to NOTE
3. `atomspace/opencog/atoms/truthvalue/SimpleTruthValue.h:77` - Clarified factory method documentation
4. `atomspace/opencog/atoms/reduct/BoolOpLink.cc:44` - Improved type handling comment

#### Priority Actions for Next Week
1. **Documentation fixes** (12 items)
   - Complete remaining comment clarifications
   - Fix outdated documentation references
   - Add missing parameter descriptions

2. **Simple code cleanups** (8 items)
   - Remove obsolete TODOs
   - Update deprecated warning messages
   - Fix typos in comments

3. **Minor feature documentation** (15 items)
   - Document unfinished features
   - Add usage examples
   - Clarify API intentions

### Phase 2: Short Term (1-3 months)

**Goal**: Complete all EASY items and begin MEDIUM items
**Target**: 479 MEDIUM items

#### Strategy
- Assign MEDIUM items by expertise area
- Focus on algorithmic improvements
- Complete feature implementations
- Address stub replacements

### Phase 3: Medium Term (3-12 months)

**Goal**: Complete MEDIUM and HARD items
**Target**: 534 items (MEDIUM + HARD)

#### Focus Areas
- Performance optimizations (55 HARD items)
- Thread safety issues
- Complex algorithm completions
- Memory management improvements

### Phase 4: Long Term (12+ months)

**Goal**: Complete all FIXME items including VERY_HARD
**Target**: All 605 items

#### Specialized Expertise Required
- Distributed systems (32 VERY_HARD items)
- Research-level work
- Architectural changes
- Byzantine fault tolerance

## Tools and Automation

### Available Scripts

1. **fixme_resolution_tracker.py** - Track progress and generate reports
```bash
python3 fixme_resolution_tracker.py --report
python3 fixme_resolution_tracker.py --easy-wins
```

2. **fixme_easy_wins.py** - Automated easy fixes
```bash
python3 fixme_easy_wins.py --dry-run  # Preview changes
python3 fixme_easy_wins.py            # Apply fixes
```

3. **analyze_fixme_instances.py** - Original analysis tool
```bash
python3 analyze_fixme_instances.py
```

### Development Workflow Integration

1. **Before making changes**: Check current FIXME status
2. **During development**: Fix related FIXMEs encountered
3. **After completion**: Update tracking system
4. **Weekly reviews**: Progress assessment and planning

## Contributor Guidelines

### Preventing New FIXMEs

1. **Use specific TODO comments** instead of FIXME
   ```cpp
   // TODO: Implement error handling for edge case X
   // instead of: FIXME: This doesn't work properly
   ```

2. **Include context and next steps**
   ```cpp
   // TODO: Add caching to improve performance (see issue #123)
   // Estimated effort: 2-3 days
   ```

3. **Link to issues or documentation**
   ```cpp
   // TODO: Replace with new API when available (RFC #456)
   ```

4. **Set priorities and deadlines**
   ```cpp
   // TODO(v2.0): Refactor to use new pattern matching engine
   ```

### Code Review Checklist

- [ ] No new FIXME comments added
- [ ] Related FIXMEs addressed or documented
- [ ] TODO comments are specific and actionable
- [ ] Documentation updated for API changes

## Component-Specific Guidelines

### High-Priority Components

1. **Atomspace** (124 items)
   - Core functionality - prioritize safety and performance
   - Many EASY documentation fixes available

2. **Components** (293 items)
   - Large integration codebase
   - Mix of all difficulty levels
   - Good candidate for distributed effort

3. **Moses** (143 items)
   - Evolutionary algorithms
   - Performance-critical code
   - Requires domain expertise

### Expert Assignment Recommendations

- **Distributed Systems**: VERY_HARD items requiring Byzantine fault tolerance
- **Performance Engineering**: HARD threading and optimization items
- **Documentation Team**: EASY comment and doc improvements
- **Algorithm Specialists**: MEDIUM feature completions

## Progress Monitoring

### Weekly Metrics
- Items resolved by category
- Time to resolution tracking
- Component completion rates
- Developer assignment efficiency

### Monthly Reviews
- Phase progress assessment
- Resource allocation adjustments
- Expert consultation planning
- Community engagement metrics

### Quarterly Planning
- Phase transition preparation
- Expertise gap identification
- Tool and process improvements
- Research collaboration setup

## Success Criteria

### Phase 1 Success (2 weeks)
- [ ] All 39 EASY items resolved
- [ ] Development workflow established
- [ ] Contributor guidelines adopted
- [ ] Weekly review process active

### Phase 2 Success (3 months)
- [ ] 25% of MEDIUM items resolved
- [ ] Expert assignment system working
- [ ] HARD item planning complete
- [ ] Community engagement strong

### Overall Success (12+ months)
- [ ] All 605 FIXME items resolved
- [ ] Zero new FIXMEs introduced
- [ ] Maintenance practices established
- [ ] Code quality metrics improved

## Getting Started

### For New Contributors
1. Run `python3 fixme_resolution_tracker.py --easy-wins`
2. Pick an EASY item in a component you're familiar with
3. Read the contributor guidelines above
4. Make your fix and update the tracking system

### For Maintainers
1. Review weekly progress reports
2. Assign MEDIUM/HARD items to appropriate experts
3. Plan VERY_HARD item research collaborations
4. Monitor new FIXME introduction

### For Component Experts
1. Focus on items in your expertise area
2. Provide guidance for complex items
3. Review fixes in your component
4. Help estimate effort for remaining items

---

**Next Update**: Weekly progress reports every Friday
**Contact**: Use GitHub issues for questions or suggestions
**Status Dashboard**: See FIXME_RESOLUTION_PROGRESS_REPORT.md for latest metrics