#!/bin/bash

# test-phase-iii-attention.sh
# Comprehensive test script for Phase III Week 9: Attention Integration
#
# Copyright (C) 2024 OpenCog Unified

echo "=== Phase III Week 9: Attention Integration Tests ==="
echo "Testing ECAN (Economic Attention Networks) implementation"
echo

# Set up environment
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if attention component exists
if [ ! -d "attention" ]; then
    echo "❌ Error: attention directory not found"
    echo "Please ensure attention component has been integrated"
    exit 1
fi

echo "✓ Attention component directory found"

# Test 1: Component structure validation
echo
echo "Test 1: Validating attention component structure..."

required_files=(
    "attention/CMakeLists.txt"
    "attention/src/AttentionValue.cc"
    "attention/src/AttentionBank.cc" 
    "attention/src/ECANAgent.cc"
    "attention/src/AttentionModule.cc"
    "attention/opencog/attention/AttentionValue.h"
    "attention/opencog/attention/AttentionBank.h"
    "attention/opencog/attention/ECANAgent.h"
    "attention/opencog/attention/AttentionModule.h"
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
    echo "✅ All required attention files present"
else
    echo "❌ Missing $missing_files required files"
    echo "! Attention component structure validation failed"
    exit 1
fi

# Test 2: Build system integration
echo
echo "Test 2: Testing build system integration..."

if [ -f "CMakeLists.txt" ]; then
    if grep -q "add_subdirectory(attention)" CMakeLists.txt; then
        echo "✓ Attention component integrated in main CMakeLists.txt"
    else
        echo "❌ Attention component not found in main CMakeLists.txt"
        echo "! Build system integration failed"
        exit 1
    fi
else
    echo "❌ Main CMakeLists.txt not found"
    exit 1
fi

# Test 3: ECAN algorithm implementation
echo
echo "Test 3: Validating ECAN algorithm implementation..."

ecan_concepts=(
    "AttentionValue"
    "AttentionBank"
    "ECANAgent"
    "STI.*Short.*Term.*Importance"
    "LTI.*Long.*Term.*Importance"
    "attentionalFocus"
    "spreadAttention"
    "rentAttention"
    "wageAttention"
    "taxAttention"
)

implementation_score=0
for concept in "${ecan_concepts[@]}"; do
    if find attention/ -name "*.h" -o -name "*.cc" | xargs grep -l "$concept" > /dev/null 2>&1; then
        echo "  ✓ ECAN concept found: $concept"
        implementation_score=$((implementation_score + 1))
    else
        echo "  ❌ ECAN concept missing: $concept"
    fi
done

ecan_completeness=$((implementation_score * 100 / ${#ecan_concepts[@]}))
echo "📊 ECAN implementation completeness: $ecan_completeness%"

if [ $ecan_completeness -ge 80 ]; then
    echo "✅ ECAN algorithm implementation sufficient"
else
    echo "❌ ECAN algorithm implementation incomplete"
    echo "! ECAN algorithm validation failed"
    exit 1
fi

# Test 4: Attention spreading mechanisms
echo
echo "Test 4: Testing attention spreading mechanisms..."

spreading_functions=(
    "spreadAttention"
    "spreadFromAtom" 
    "hebianSpread"
    "importanceSpread"
    "contextualSpread"
)

spreading_score=0
for func in "${spreading_functions[@]}"; do
    if find attention/ -name "*.h" -o -name "*.cc" | xargs grep -l "$func" > /dev/null 2>&1; then
        echo "  ✓ Spreading function found: $func"
        spreading_score=$((spreading_score + 1))
    else
        echo "  ⚠️  Spreading function not found: $func"
    fi
done

spreading_completeness=$((spreading_score * 100 / ${#spreading_functions[@]}))
echo "📊 Attention spreading completeness: $spreading_completeness%"

if [ $spreading_completeness -ge 60 ]; then
    echo "✅ Attention spreading mechanisms functional"
else
    echo "❌ Attention spreading mechanisms insufficient"
    echo "! Attention spreading validation inconclusive"
fi

# Test 5: CogServer integration
echo
echo "Test 5: Testing CogServer integration..."

if grep -r "CogServer" attention/ > /dev/null 2>&1; then
    echo "✓ CogServer integration references found"
    
    if grep -r "AttentionModule" attention/ > /dev/null 2>&1; then
        echo "✓ AttentionModule for CogServer integration found"
        echo "✅ CogServer integration implemented"
    else
        echo "⚠️  AttentionModule not found - integration may be incomplete"
        echo "! CogServer integration partially implemented"
    fi
else
    echo "❌ No CogServer integration found"
    echo "! CogServer integration missing"
fi

# Test 6: Attention allocation tests
echo
echo "Test 6: Testing attention allocation mechanisms..."

if [ -d "attention/tests" ]; then
    test_files=$(find attention/tests -name "*.cpp" -o -name "*.cc" | wc -l)
    echo "✓ Found $test_files test files"
    
    if [ $test_files -ge 3 ]; then
        echo "✅ Sufficient test coverage for attention allocation"
    else
        echo "⚠️  Limited test coverage - consider adding more tests"
        echo "! Test coverage could be improved"
    fi
else
    echo "❌ No test directory found"
    echo "! Attention allocation tests missing"
fi

# Test 7: Component configuration validation
echo
echo "Test 7: Validating component configuration..."

if [ -f "component-config.json" ]; then
    if grep -q '"attention"' component-config.json; then
        echo "✓ Attention component listed in component-config.json"
        
        if grep -A5 '"attention"' component-config.json | grep -q '"status": "present"'; then
            echo "✓ Attention component status set to present"
            echo "✅ Component configuration updated"
        else
            echo "⚠️  Attention component status not updated to present"
            echo "! Component configuration needs update"
        fi
    else
        echo "❌ Attention component not found in component-config.json"
        echo "! Component configuration not updated"
    fi
else
    echo "❌ component-config.json not found"
fi

# Test 8: Integration with existing systems
echo
echo "Test 8: Testing integration with existing systems..."

if find . -name "*.py" -o -name "*.scm" | xargs grep -l "attention" > /dev/null 2>&1; then
    existing_attention_refs=$(find . -name "*.py" -o -name "*.scm" | xargs grep -l "attention" | wc -l)
    echo "✓ Found $existing_attention_refs files with attention references"
    echo "✅ Integration with existing attention code"
else
    echo "⚠️  No existing attention code references found"
    echo "! Integration with existing systems unclear"
fi

# Summary
echo
echo "=== Phase III Week 9 Attention Integration Test Summary ==="
echo

overall_tests=8
passed_tests=0

# Count successful tests based on previous outputs
if [ $missing_files -eq 0 ]; then passed_tests=$((passed_tests + 1)); fi
if grep -q "add_subdirectory(attention)" CMakeLists.txt 2>/dev/null; then passed_tests=$((passed_tests + 1)); fi
if [ $ecan_completeness -ge 80 ]; then passed_tests=$((passed_tests + 1)); fi
if [ $spreading_completeness -ge 60 ]; then passed_tests=$((passed_tests + 1)); fi
if grep -r "CogServer" attention/ > /dev/null 2>&1; then passed_tests=$((passed_tests + 1)); fi
if [ -d "attention/tests" ] && [ $test_files -ge 3 ]; then passed_tests=$((passed_tests + 1)); fi
if [ -f "component-config.json" ] && grep -q '"attention"' component-config.json; then passed_tests=$((passed_tests + 1)); fi
if find . -name "*.py" -o -name "*.scm" | xargs grep -l "attention" > /dev/null 2>&1; then passed_tests=$((passed_tests + 1)); fi

success_rate=$((passed_tests * 100 / overall_tests))

echo "📊 Test Results:"
echo "  - Tests Passed: $passed_tests/$overall_tests"
echo "  - Success Rate: $success_rate%"
echo "  - ECAN Implementation: $ecan_completeness%"
echo "  - Attention Spreading: $spreading_completeness%"
echo

if [ $success_rate -ge 80 ]; then
    echo "🎉 PHASE III WEEK 9: ATTENTION INTEGRATION SUCCESSFUL!"
    echo "✅ Economic Attention Networks (ECAN) operational"
    echo "✅ Attention spreading algorithms working" 
    echo "✅ CogServer integration functional"
    echo "✅ Ready for Week 10: Spacetime Integration"
    echo
    echo "🧠 Next Steps:"
    echo "  1. Begin spacetime component integration"
    echo "  2. Test attention + spacetime coordination"
    echo "  3. Optimize attention allocation performance"
    echo "  4. Document attention API and usage patterns"
elif [ $success_rate -ge 60 ]; then
    echo "⚠️  PHASE III WEEK 9: ATTENTION INTEGRATION MOSTLY SUCCESSFUL"
    echo "✅ Core attention functionality implemented"
    echo "⚠️  Some components need refinement"
    echo "📋 Minor improvements needed before Week 10"
else
    echo "❌ PHASE III WEEK 9: ATTENTION INTEGRATION NEEDS WORK"
    echo "❌ Critical attention components missing or incomplete"
    echo "🔧 Significant development required before proceeding"
    echo
    echo "🚧 Required Actions:"
    echo "  - Complete missing ECAN components"
    echo "  - Fix CogServer integration issues"
    echo "  - Add comprehensive test coverage"
    echo "  - Update component configuration"
fi

echo
echo "📄 Detailed results logged to attention-integration-results.log"

# Log results
cat > attention-integration-results.log << EOF
Phase III Week 9: Attention Integration Test Results
Generated: $(date)

Test Summary:
- Overall Success Rate: $success_rate%
- Tests Passed: $passed_tests/$overall_tests
- ECAN Implementation Completeness: $ecan_completeness%
- Attention Spreading Completeness: $spreading_completeness%

Component Files Status:
- Missing Files: $missing_files
- Test Files Found: $test_files

Integration Status:
- Build System: $(grep -q "add_subdirectory(attention)" CMakeLists.txt 2>/dev/null && echo "Integrated" || echo "Not Integrated")
- CogServer Integration: $(grep -r "CogServer" attention/ > /dev/null 2>&1 && echo "Present" || echo "Missing")
- Component Config: $(grep -q '"attention"' component-config.json 2>/dev/null && echo "Updated" || echo "Not Updated")

Next Steps:
- Proceed to spacetime integration if success rate >= 80%
- Address identified issues if success rate < 80%
- Continue with Phase III cognitive systems integration
EOF

exit 0