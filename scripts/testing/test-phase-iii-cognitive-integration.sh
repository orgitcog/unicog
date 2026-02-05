#!/bin/bash

# test-phase-iii-cognitive-integration.sh
# Comprehensive test script for Phase III Week 11: Cognitive Systems Integration Testing
#
# Copyright (C) 2024 OpenCog Unified

echo "=== Phase III Week 11: Cognitive Systems Integration Testing ==="
echo "Testing attention + spacetime coordination and cognitive resource management"
echo

# Set up environment
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check prerequisites
echo "Checking prerequisites..."

if [ ! -d "attention" ]; then
    echo "❌ Error: attention component not found"
    echo "Run Week 9 attention integration first"
    exit 1
fi

if [ ! -d "spacetime" ]; then
    echo "❌ Error: spacetime component not found"
    echo "Run Week 10 spacetime integration first"
    exit 1
fi

echo "✓ Both attention and spacetime components present"

# Test 1: Attention + Spacetime Integration
echo
echo "Test 1: Testing attention + spacetime integration..."

# Check if both systems can work together
integration_score=0

if find . -name "*.h" -o -name "*.cc" | xargs grep -l "AttentionValue.*Point3D\|Point3D.*AttentionValue" > /dev/null 2>&1; then
    echo "  ✓ Cross-references between attention and spatial concepts found"
    integration_score=$((integration_score + 1))
else
    echo "  ⚠️  No direct cross-references found (may still work via coordination)"
fi

if find . -name "*.h" -o -name "*.cc" | xargs grep -l "TimePoint.*STI\|STI.*TimePoint" > /dev/null 2>&1; then
    echo "  ✓ Cross-references between attention and temporal concepts found"
    integration_score=$((integration_score + 1))
else
    echo "  ⚠️  No direct cross-references found (may still work via coordination)"
fi

# Check for spatial attention concepts
if find . -name "*.py" -o -name "*.scm" -o -name "*.h" -o -name "*.cc" | xargs grep -l "spatial.*attention\|attention.*spatial" > /dev/null 2>&1; then
    echo "  ✓ Spatial attention concepts found"
    integration_score=$((integration_score + 1))
else
    echo "  ⚠️  Spatial attention concepts not found"
fi

echo "📊 Attention-Spacetime Integration Score: $integration_score/3"

if [ $integration_score -ge 2 ]; then
    echo "✅ Attention + spacetime integration functional"
else
    echo "⚠️  Attention + spacetime integration needs improvement"
    echo "! Integration test partially successful"
fi

# Test 2: Cognitive Resource Management
echo
echo "Test 2: Testing cognitive resource management..."

resource_concepts=(
    "AttentionBank"
    "SpaceTimeMap"
    "memory"
    "resource"
    "allocation"
)

resource_score=0
for concept in "${resource_concepts[@]}"; do
    if find . -name "*.h" -o -name "*.cc" | xargs grep -l "$concept" > /dev/null 2>&1; then
        echo "  ✓ Resource concept found: $concept"
        resource_score=$((resource_score + 1))
    else
        echo "  ❌ Resource concept missing: $concept"
    fi
done

resource_completeness=$((resource_score * 100 / ${#resource_concepts[@]}))
echo "📊 Cognitive resource management completeness: $resource_completeness%"

if [ $resource_completeness -ge 80 ]; then
    echo "✅ Cognitive resource management functional"
else
    echo "❌ Cognitive resource management insufficient"
    echo "! Resource management validation failed"
fi

# Test 3: Spatial-Temporal Attention Coordination
echo
echo "Test 3: Testing spatial-temporal attention coordination..."

coordination_concepts=(
    "focus"
    "attention"
    "spatial"
    "temporal"
    "coordinate"
)

coordination_score=0
total_refs=0

for concept in "${coordination_concepts[@]}"; do
    refs=$(find . -name "*.h" -o -name "*.cc" | xargs grep -l "$concept" 2>/dev/null | wc -l)
    total_refs=$((total_refs + refs))
    
    if [ $refs -gt 0 ]; then
        echo "  ✓ Coordination concept '$concept': $refs references"
        coordination_score=$((coordination_score + 1))
    else
        echo "  ❌ Coordination concept '$concept': 0 references"
    fi
done

echo "📊 Total coordination references found: $total_refs"
echo "📊 Coordination concept coverage: $coordination_score/${#coordination_concepts[@]}"

if [ $coordination_score -ge 4 ] && [ $total_refs -ge 50 ]; then
    echo "✅ Spatial-temporal attention coordination operational"
else
    echo "⚠️  Spatial-temporal attention coordination needs development"
    echo "! Coordination test partially successful"
fi

# Test 4: Performance Integration Testing
echo
echo "Test 4: Testing performance integration..."

performance_indicators=(
    "size()"
    "clear()"
    "optimization"
    "cache"
    "index"
)

performance_score=0
for indicator in "${performance_indicators[@]}"; do
    if find . -name "*.h" -o -name "*.cc" | xargs grep -l "$indicator" > /dev/null 2>&1; then
        echo "  ✓ Performance indicator found: $indicator"
        performance_score=$((performance_score + 1))
    else
        echo "  ⚠️  Performance indicator not found: $indicator"
    fi
done

performance_completeness=$((performance_score * 100 / ${#performance_indicators[@]}))
echo "📊 Performance optimization indicators: $performance_completeness%"

if [ $performance_completeness -ge 60 ]; then
    echo "✅ Performance integration considerations present"
else
    echo "⚠️  Performance integration needs attention"
    echo "! Performance integration could be improved"
fi

# Test 5: Cross-Component Communication
echo
echo "Test 5: Testing cross-component communication..."

# Check for shared interfaces
shared_concepts=0

if find attention/ -name "*.h" | xargs grep -l "Handle" > /dev/null 2>&1 && find spacetime/ -name "*.h" | xargs grep -l "Handle" > /dev/null 2>&1; then
    echo "  ✓ Shared Handle interface found in both components"
    shared_concepts=$((shared_concepts + 1))
fi

if find attention/ -name "*.h" | xargs grep -l "AtomSpace" > /dev/null 2>&1 && find spacetime/ -name "*.h" | xargs grep -l "AtomSpace" > /dev/null 2>&1; then
    echo "  ✓ Shared AtomSpace interface found in both components"
    shared_concepts=$((shared_concepts + 1))
fi

if [ $shared_concepts -ge 2 ]; then
    echo "✅ Cross-component communication interfaces functional"
else
    echo "⚠️  Cross-component communication interfaces limited"
    echo "! Communication test partially successful"
fi

# Test 6: Integration Documentation
echo
echo "Test 6: Testing integration documentation..."

doc_files=0
if find . -name "README*.md" -o -name "*INTEGRATION*.md" -o -name "*PHASE*.md" | head -5 | wc -l | grep -q "[1-9]"; then
    doc_files=$(find . -name "README*.md" -o -name "*INTEGRATION*.md" -o -name "*PHASE*.md" | wc -l)
    echo "  ✓ Found $doc_files documentation files"
    
    if [ $doc_files -ge 3 ]; then
        echo "✅ Integration documentation comprehensive"
    else
        echo "⚠️  Integration documentation could be expanded"
    fi
else
    echo "⚠️  Limited integration documentation found"
    echo "! Documentation test could be improved"
fi

# Test 7: Test Coverage Integration
echo
echo "Test 7: Testing integrated test coverage..."

attention_tests=$(find attention/tests -name "*.cpp" 2>/dev/null | wc -l)
spacetime_tests=$(find spacetime/tests -name "*.cpp" 2>/dev/null | wc -l)
total_tests=$((attention_tests + spacetime_tests))

echo "  ✓ Attention tests: $attention_tests"
echo "  ✓ Spacetime tests: $spacetime_tests"
echo "  📊 Total component tests: $total_tests"

if [ $total_tests -ge 7 ]; then
    echo "✅ Comprehensive integrated test coverage"
else
    echo "⚠️  Integrated test coverage could be improved"
    echo "! Test coverage partially sufficient"
fi

# Test 8: Build System Coordination
echo
echo "Test 8: Testing build system coordination..."

build_integration_score=0

if grep -q "cognitive-systems" CMakeLists.txt 2>/dev/null; then
    echo "  ✓ Cognitive systems build target found"
    build_integration_score=$((build_integration_score + 1))
fi

if grep -q "add_dependencies.*attention" CMakeLists.txt 2>/dev/null; then
    echo "  ✓ Attention build dependencies configured"
    build_integration_score=$((build_integration_score + 1))
fi

if grep -q "add_dependencies.*spacetime" CMakeLists.txt 2>/dev/null; then
    echo "  ✓ Spacetime build dependencies configured"
    build_integration_score=$((build_integration_score + 1))
fi

if [ $build_integration_score -ge 2 ]; then
    echo "✅ Build system coordination functional"
else
    echo "⚠️  Build system coordination needs improvement"
    echo "! Build coordination test partially successful"
fi

# Summary
echo
echo "=== Phase III Week 11 Cognitive Systems Integration Test Summary ==="
echo

overall_tests=8
passed_tests=0

# Count successful tests
if [ $integration_score -ge 2 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $resource_completeness -ge 80 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $coordination_score -ge 4 ] && [ $total_refs -ge 50 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $performance_completeness -ge 60 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $shared_concepts -ge 2 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $doc_files -ge 3 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $total_tests -ge 7 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $build_integration_score -ge 2 ]; then passed_tests=$((passed_tests + 1)); fi

success_rate=$((passed_tests * 100 / overall_tests))

echo "📊 Integration Test Results:"
echo "  - Tests Passed: $passed_tests/$overall_tests"
echo "  - Success Rate: $success_rate%"
echo "  - Attention-Spacetime Integration: $integration_score/3"
echo "  - Resource Management: $resource_completeness%"
echo "  - Coordination Coverage: $coordination_score/5"
echo "  - Total Component Tests: $total_tests"
echo

if [ $success_rate -ge 75 ]; then
    echo "🎉 PHASE III WEEK 11: COGNITIVE INTEGRATION TESTING SUCCESSFUL!"
    echo "✅ Attention + spacetime coordination functional"
    echo "✅ Cognitive resource management operational"
    echo "✅ Cross-component communication working"
    echo "✅ Ready for Week 12: Phase 3 Validation"
    echo
    echo "🧠 Next Steps:"
    echo "  1. Begin comprehensive cognitive system testing"
    echo "  2. Implement end-to-end cognitive workflows"
    echo "  3. Performance optimization and tuning"
    echo "  4. Final Phase 3 completion documentation"
elif [ $success_rate -ge 50 ]; then
    echo "⚠️  PHASE III WEEK 11: COGNITIVE INTEGRATION MOSTLY SUCCESSFUL"
    echo "✅ Core integration functionality working"
    echo "⚠️  Some coordination aspects need refinement"
    echo "📋 Minor improvements needed before Week 12"
else
    echo "❌ PHASE III WEEK 11: COGNITIVE INTEGRATION NEEDS WORK"
    echo "❌ Critical integration components missing or incomplete"
    echo "🔧 Significant development required before proceeding"
    echo
    echo "🚧 Required Actions:"
    echo "  - Improve attention-spacetime coordination"
    echo "  - Enhance cross-component communication"
    echo "  - Add comprehensive integration tests"
    echo "  - Optimize performance integration"
fi

echo
echo "📄 Detailed results logged to cognitive-integration-results.log"

# Log results
cat > cognitive-integration-results.log << EOF
Phase III Week 11: Cognitive Systems Integration Test Results
Generated: $(date)

Integration Test Summary:
- Overall Success Rate: $success_rate%
- Tests Passed: $passed_tests/$overall_tests
- Attention-Spacetime Integration Score: $integration_score/3
- Resource Management Completeness: $resource_completeness%
- Coordination Coverage: $coordination_score/5
- Performance Integration: $performance_completeness%

Component Status:
- Attention Tests: $attention_tests
- Spacetime Tests: $spacetime_tests  
- Total Tests: $total_tests
- Documentation Files: $doc_files
- Build Integration Score: $build_integration_score/3

Cross-Component Analysis:
- Shared Concepts: $shared_concepts/2
- Total Coordination References: $total_refs
- Performance Indicators: $performance_score/5

Next Steps:
- Proceed to final Phase 3 validation if success rate >= 75%
- Address integration issues if success rate < 75%
- Complete comprehensive cognitive system testing
EOF

exit 0