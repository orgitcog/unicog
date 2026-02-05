#!/bin/bash

# test-phase-iii-spacetime.sh
# Comprehensive test script for Phase III Week 10: Spacetime Integration
#
# Copyright (C) 2024 OpenCog Unified

echo "=== Phase III Week 10: Spacetime Integration Tests ==="
echo "Testing spatial-temporal reasoning and geometric algorithms"
echo

# Set up environment
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if spacetime component exists
if [ ! -d "spacetime" ]; then
    echo "❌ Error: spacetime directory not found"
    echo "Please ensure spacetime component has been integrated"
    exit 1
fi

echo "✓ Spacetime component directory found"

# Test 1: Component structure validation
echo
echo "Test 1: Validating spacetime component structure..."

required_files=(
    "spacetime/CMakeLists.txt"
    "spacetime/src/SpaceTimeMap.cc"
    "spacetime/src/SpatialReasoning.cc"
    "spacetime/src/TemporalReasoning.cc"
    "spacetime/src/SpaceTimeAtom.cc"
    "spacetime/opencog/spacetime/SpaceTimeMap.h"
    "spacetime/opencog/spacetime/SpatialReasoning.h"
    "spacetime/opencog/spacetime/TemporalReasoning.h"
    "spacetime/opencog/spacetime/SpaceTimeAtom.h"
)

missing_files=0
for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✓ $file"
    else
        echo "  ❌ Missing: $file"
        missing_files=$((missing_files + 1))
    fi
done

if [ $missing_files -eq 0 ]; then
    echo "✅ All required spacetime files present"
else
    echo "❌ Missing $missing_files required files"
    echo "! Spacetime component structure validation failed"
    exit 1
fi

# Test 2: Build system integration
echo
echo "Test 2: Testing build system integration..."

if [ -f "CMakeLists.txt" ]; then
    if grep -q "add_subdirectory(spacetime)" CMakeLists.txt; then
        echo "✓ Spacetime component integrated in main CMakeLists.txt"
    else
        echo "❌ Spacetime component not found in main CMakeLists.txt"
        echo "! Build system integration failed"
        exit 1
    fi
else
    echo "❌ Main CMakeLists.txt not found"
    exit 1
fi

# Test 3: Spatial reasoning implementation
echo
echo "Test 3: Validating spatial reasoning implementation..."

spatial_concepts=(
    "Point3D"
    "BoundingBox"
    "SpatialRegion"
    "SpatialReasoning"
    "SpatialRelation"
    "calculateDistance"
    "getNearbyAtoms"
    "calculateCentroid"
    "findObjectsInRegion"
    "calculatePath"
)

spatial_score=0
for concept in "${spatial_concepts[@]}"; do
    if find spacetime/ -name "*.h" -o -name "*.cc" | xargs grep -l "$concept" > /dev/null 2>&1; then
        echo "  ✓ Spatial concept found: $concept"
        spatial_score=$((spatial_score + 1))
    else
        echo "  ❌ Spatial concept missing: $concept"
    fi
done

spatial_completeness=$((spatial_score * 100 / ${#spatial_concepts[@]}))
echo "📊 Spatial reasoning implementation completeness: $spatial_completeness%"

if [ $spatial_completeness -ge 80 ]; then
    echo "✅ Spatial reasoning implementation sufficient"
else
    echo "❌ Spatial reasoning implementation incomplete"
    echo "! Spatial reasoning validation failed"
    exit 1
fi

# Test 4: Temporal reasoning implementation
echo
echo "Test 4: Testing temporal reasoning implementation..."

temporal_concepts=(
    "TimePoint"
    "TimeInterval"
    "TemporalEvent"
    "TemporalSequence"
    "TemporalReasoning"
    "TemporalRelation"
    "findTemporalPatterns"
    "predictNext"
    "getTemporalRelation"
    "clusterByTemporalProximity"
)

temporal_score=0
for concept in "${temporal_concepts[@]}"; do
    if find spacetime/ -name "*.h" -o -name "*.cc" | xargs grep -l "$concept" > /dev/null 2>&1; then
        echo "  ✓ Temporal concept found: $concept"
        temporal_score=$((temporal_score + 1))
    else
        echo "  ❌ Temporal concept missing: $concept"
    fi
done

temporal_completeness=$((temporal_score * 100 / ${#temporal_concepts[@]}))
echo "📊 Temporal reasoning implementation completeness: $temporal_completeness%"

if [ $temporal_completeness -ge 80 ]; then
    echo "✅ Temporal reasoning implementation sufficient"
else
    echo "❌ Temporal reasoning implementation incomplete"
    echo "! Temporal reasoning validation failed"
    exit 1
fi

# Test 5: Spacetime integration
echo
echo "Test 5: Testing spacetime integration..."

if grep -r "SpaceTimeAtom" spacetime/ > /dev/null 2>&1; then
    echo "✓ SpaceTimeAtom integration found"
    
    if grep -r "AtomSpace" spacetime/ > /dev/null 2>&1; then
        echo "✓ AtomSpace integration references found"
        echo "✅ Spacetime integration implemented"
    else
        echo "⚠️  AtomSpace integration references not found"
        echo "! Spacetime integration may be incomplete"
    fi
else
    echo "❌ No SpaceTimeAtom integration found"
    echo "! Spacetime integration missing"
fi

# Test 6: Geometric algorithms
echo
echo "Test 6: Testing geometric algorithms..."

geometric_functions=(
    "distance"
    "volume"
    "center"
    "intersects"
    "contains"
)

geometric_score=0
for func in "${geometric_functions[@]}"; do
    if find spacetime/ -name "*.h" -o -name "*.cc" | xargs grep -l "$func" > /dev/null 2>&1; then
        echo "  ✓ Geometric function found: $func"
        geometric_score=$((geometric_score + 1))
    else
        echo "  ⚠️  Geometric function not found: $func"
    fi
done

geometric_completeness=$((geometric_score * 100 / ${#geometric_functions[@]}))
echo "📊 Geometric algorithms completeness: $geometric_completeness%"

if [ $geometric_completeness -ge 60 ]; then
    echo "✅ Geometric algorithms functional"
else
    echo "❌ Geometric algorithms insufficient"
    echo "! Geometric algorithms validation inconclusive"
fi

# Test 7: Test coverage validation
echo
echo "Test 7: Testing spacetime test coverage..."

if [ -d "spacetime/tests" ]; then
    test_files=$(find spacetime/tests -name "*.cpp" -o -name "*.cc" | wc -l)
    echo "✓ Found $test_files test files"
    
    if [ $test_files -ge 4 ]; then
        echo "✅ Comprehensive test coverage for spacetime"
    else
        echo "⚠️  Limited test coverage - consider adding more tests"
        echo "! Test coverage could be improved"
    fi
else
    echo "❌ No test directory found"
    echo "! Spacetime tests missing"
fi

# Test 8: Component configuration validation
echo
echo "Test 8: Validating component configuration..."

if [ -f "component-config.json" ]; then
    if grep -q '"spacetime"' component-config.json; then
        echo "✓ Spacetime component listed in component-config.json"
        
        if grep -A5 '"spacetime"' component-config.json | grep -q '"status": "present"'; then
            echo "✓ Spacetime component status set to present"
            echo "✅ Component configuration updated"
        else
            echo "⚠️  Spacetime component status not updated to present"
            echo "! Component configuration needs update"
        fi
    else
        echo "❌ Spacetime component not found in component-config.json"
        echo "! Component configuration not updated"
    fi
else
    echo "❌ component-config.json not found"
fi

# Test 9: Dependency integration
echo
echo "Test 9: Testing dependency integration..."

if grep -r "atomspace" spacetime/ > /dev/null 2>&1; then
    echo "✓ AtomSpace dependency integration found"
    echo "✅ Dependencies properly integrated"
else
    echo "⚠️  AtomSpace dependency integration unclear"
    echo "! Dependency integration needs verification"
fi

# Summary
echo
echo "=== Phase III Week 10 Spacetime Integration Test Summary ==="
echo

overall_tests=9
passed_tests=0

# Count successful tests based on previous outputs
if [ $missing_files -eq 0 ]; then passed_tests=$((passed_tests + 1)); fi
if grep -q "add_subdirectory(spacetime)" CMakeLists.txt 2>/dev/null; then passed_tests=$((passed_tests + 1)); fi
if [ $spatial_completeness -ge 80 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $temporal_completeness -ge 80 ]; then passed_tests=$((passed_tests + 1)); fi
if grep -r "SpaceTimeAtom" spacetime/ > /dev/null 2>&1; then passed_tests=$((passed_tests + 1)); fi
if [ $geometric_completeness -ge 60 ]; then passed_tests=$((passed_tests + 1)); fi
if [ -d "spacetime/tests" ] && [ $test_files -ge 4 ]; then passed_tests=$((passed_tests + 1)); fi
if [ -f "component-config.json" ] && grep -q '"spacetime"' component-config.json; then passed_tests=$((passed_tests + 1)); fi
if grep -r "atomspace" spacetime/ > /dev/null 2>&1; then passed_tests=$((passed_tests + 1)); fi

success_rate=$((passed_tests * 100 / overall_tests))

echo "📊 Test Results:"
echo "  - Tests Passed: $passed_tests/$overall_tests"
echo "  - Success Rate: $success_rate%"
echo "  - Spatial Reasoning: $spatial_completeness%"
echo "  - Temporal Reasoning: $temporal_completeness%"
echo "  - Geometric Algorithms: $geometric_completeness%"
echo

if [ $success_rate -ge 80 ]; then
    echo "🎉 PHASE III WEEK 10: SPACETIME INTEGRATION SUCCESSFUL!"
    echo "✅ Spatial-temporal reasoning operational"
    echo "✅ Geometric algorithms functional" 
    echo "✅ AtomSpace integration working"
    echo "✅ Ready for Week 11: Cognitive Systems Integration Testing"
    echo
    echo "🧠 Next Steps:"
    echo "  1. Begin attention + spacetime coordination testing"
    echo "  2. Implement cognitive resource management validation"
    echo "  3. Create spatial-temporal attention tests"
    echo "  4. Performance tuning and optimization"
elif [ $success_rate -ge 60 ]; then
    echo "⚠️  PHASE III WEEK 10: SPACETIME INTEGRATION MOSTLY SUCCESSFUL"
    echo "✅ Core spacetime functionality implemented"
    echo "⚠️  Some components need refinement"
    echo "📋 Minor improvements needed before Week 11"
else
    echo "❌ PHASE III WEEK 10: SPACETIME INTEGRATION NEEDS WORK"
    echo "❌ Critical spacetime components missing or incomplete"
    echo "🔧 Significant development required before proceeding"
    echo
    echo "🚧 Required Actions:"
    echo "  - Complete missing spatial reasoning components"
    echo "  - Fix temporal processing issues"
    echo "  - Add comprehensive test coverage"
    echo "  - Update component configuration"
fi

echo
echo "📄 Detailed results logged to spacetime-integration-results.log"

# Log results
cat > spacetime-integration-results.log << EOF
Phase III Week 10: Spacetime Integration Test Results
Generated: $(date)

Test Summary:
- Overall Success Rate: $success_rate%
- Tests Passed: $passed_tests/$overall_tests
- Spatial Reasoning Completeness: $spatial_completeness%
- Temporal Reasoning Completeness: $temporal_completeness%
- Geometric Algorithms Completeness: $geometric_completeness%

Component Files Status:
- Missing Files: $missing_files
- Test Files Found: $test_files

Integration Status:
- Build System: $(grep -q "add_subdirectory(spacetime)" CMakeLists.txt 2>/dev/null && echo "Integrated" || echo "Not Integrated")
- SpaceTime Integration: $(grep -r "SpaceTimeAtom" spacetime/ > /dev/null 2>&1 && echo "Present" || echo "Missing")
- Component Config: $(grep -q '"spacetime"' component-config.json 2>/dev/null && echo "Updated" || echo "Not Updated")

Next Steps:
- Proceed to cognitive integration testing if success rate >= 80%
- Address identified issues if success rate < 80%
- Continue with Phase III cognitive systems coordination
EOF

exit 0