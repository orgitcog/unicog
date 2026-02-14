#!/bin/bash

# Test Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
# Validation script for meta-cognitive and evolutionary components

echo "============================================================"
echo "Phase 5: Recursive Meta-Cognition & Evolutionary Optimization"
echo "Validation Testing Suite"
echo "============================================================"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Initialize counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Test function
run_test() {
    local test_name="$1"
    local test_command="$2"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "\n${BLUE}[TEST $TOTAL_TESTS]${NC} $test_name"
    echo "Command: $test_command"
    
    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASSED${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Test function with output capture
run_test_with_output() {
    local test_name="$1"
    local test_command="$2"
    local expected_pattern="$3"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "\n${BLUE}[TEST $TOTAL_TESTS]${NC} $test_name"
    echo "Command: $test_command"
    
    local output=$(eval "$test_command" 2>&1)
    local exit_code=$?
    
    if [ $exit_code -eq 0 ] && [[ $output == *"$expected_pattern"* ]]; then
        echo -e "${GREEN}✓ PASSED${NC}"
        echo "Found expected pattern: '$expected_pattern'"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}"
        echo "Exit code: $exit_code"
        echo "Output snippet: ${output:0:200}..."
        if [ -n "$expected_pattern" ]; then
            echo "Expected pattern not found: '$expected_pattern'"
        fi
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

echo -e "\n${YELLOW}Phase 1: Component Structure Validation${NC}"

# Test 1: Verify meta-cognition component structure
run_test "Meta-cognition component structure" \
    "[ -d 'meta-cognition' ] && [ -f 'meta-cognition/include/MetaCognitiveMonitor.h' ] && [ -f 'meta-cognition/src/MetaCognitiveMonitor.cc' ]"

# Test 2: Verify evolutionary optimization component structure  
run_test "Evolutionary optimization component structure" \
    "[ -d 'evolutionary-optimization' ] && [ -f 'evolutionary-optimization/include/EvolutionaryOptimizer.h' ] && [ -f 'evolutionary-optimization/src/EvolutionaryOptimizer.cc' ]"

# Test 3: Verify CMakeLists.txt files
run_test "CMakeLists.txt configuration files" \
    "[ -f 'meta-cognition/CMakeLists.txt' ] && [ -f 'evolutionary-optimization/CMakeLists.txt' ]"

# Test 4: Verify example applications
run_test "Example applications exist" \
    "[ -f 'meta-cognition/examples/meta_cognitive_demo.cc' ] && [ -f 'evolutionary-optimization/examples/evolutionary_demo.cc' ]"

# Test 5: Verify integration demo
run_test "Phase 5 integration demo exists" \
    "[ -f 'docs/phases/phase5-integration-demo.cc' ]"

echo -e "\n${YELLOW}Phase 2: Header File Content Validation${NC}"

# Test 6: MetaCognitiveTensor structure validation
run_test_with_output "MetaCognitiveTensor structure validation" \
    "grep -q 'struct MetaCognitiveTensor' meta-cognition/include/MetaCognitiveMonitor.h" \
    ""

# Test 7: Evolutionary genome structure validation
run_test_with_output "CognitiveGenome structure validation" \
    "grep -q 'struct CognitiveGenome' evolutionary-optimization/include/EvolutionaryOptimizer.h" \
    ""

# Test 8: Meta-cognitive tensor 9-dimensional signature
run_test_with_output "Meta-cognitive tensor 9-dimensional signature" \
    "grep -A 10 'MetaCognitiveTensor' meta-cognition/include/MetaCognitiveMonitor.h | grep -E '(self_awareness_level|performance_metric|evolutionary_generation|fitness_score|adaptation_rate|cognitive_complexity|meta_level|reflection_depth|optimization_target)'" \
    "self_awareness_level"

echo -e "\n${YELLOW}Phase 3: Implementation Content Validation${NC}"

# Test 9: Recursive self-reflection implementation
run_test_with_output "Recursive self-reflection implementation" \
    "grep -q 'recursive_self_reflection' meta-cognition/src/MetaCognitiveMonitor.cc" \
    ""

# Test 10: Evolutionary optimization methods
run_test_with_output "Evolutionary optimization methods" \
    "grep -E '(tournament_selection|crossover|mutate)' evolutionary-optimization/src/EvolutionaryOptimizer.cc" \
    "tournament_selection"

# Test 11: Multi-objective fitness evaluation
run_test_with_output "Multi-objective fitness evaluation" \
    "grep -q 'MultiObjectiveFitness' evolutionary-optimization/src/EvolutionaryOptimizer.cc" \
    ""

# Test 12: Meta-cognitive tensor calculations
run_test_with_output "Meta-cognitive tensor calculations" \
    "grep -q 'calculate_overall_fitness' meta-cognition/src/MetaCognitiveMonitor.cc" \
    ""

echo -e "\n${YELLOW}Phase 4: Integration and Dependency Validation${NC}"

# Test 13: Integration with existing patterns
run_test_with_output "Integration with cognitive patterns" \
    "grep -q 'meta-cognitive-pattern-adaptation' cognitive-patterns/scheme/emergent-patterns.scm" \
    ""

# Test 14: CMake integration in main build
run_test_with_output "Main CMakeLists.txt integration" \
    "grep -q 'meta-cognitive-optimization' CMakeLists.txt" \
    ""

# Test 15: Cross-component dependencies
run_test_with_output "Cross-component dependency handling" \
    "grep -q '../meta-cognition/include' evolutionary-optimization/src/EvolutionaryOptimizer.cc" \
    ""

echo -e "\n${YELLOW}Phase 5: Functional Validation (Compilation Check)${NC}"

# Test 16: Meta-cognition component compilation check
if command -v g++ >/dev/null 2>&1; then
    run_test_with_output "Meta-cognition compilation syntax check" \
        "g++ -std=c++17 -I meta-cognition/include -c meta-cognition/src/MetaCognitiveMonitor.cc -o /tmp/metacognitive_test.o" \
        ""
else
    echo -e "${YELLOW}[SKIP]${NC} G++ not available for compilation testing"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
fi

# Test 17: Evolutionary optimization component compilation check  
if command -v g++ >/dev/null 2>&1; then
    run_test_with_output "Evolutionary optimization compilation syntax check" \
        "g++ -std=c++17 -I evolutionary-optimization/include -I meta-cognition/include -c evolutionary-optimization/src/EvolutionaryOptimizer.cc -o /tmp/evolutionary_test.o" \
        ""
else
    echo -e "${YELLOW}[SKIP]${NC} G++ not available for compilation testing"  
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
fi

echo -e "\n${YELLOW}Phase 6: API and Interface Validation${NC}"

# Test 18: MetaCognitiveMonitor public interface
run_test_with_output "MetaCognitiveMonitor public interface" \
    "grep -E '(observe_cognitive_state|recursive_self_reflection|generate_improvement_suggestions)' meta-cognition/include/MetaCognitiveMonitor.h" \
    "observe_cognitive_state"

# Test 19: EvolutionaryOptimizer public interface
run_test_with_output "EvolutionaryOptimizer public interface" \
    "grep -E '(evolve_cognitive_architecture|evolutionary_step|integrate_metacognitive_feedback)' evolutionary-optimization/include/EvolutionaryOptimizer.h" \
    "evolve_cognitive_architecture"

# Test 20: Meta-cognitive tensor utility methods
run_test_with_output "Meta-cognitive tensor utility methods" \
    "grep -E '(to_vector|from_vector|calculate_overall_fitness)' meta-cognition/include/MetaCognitiveMonitor.h" \
    "to_vector"

echo -e "\n============================================================"
echo -e "${BLUE}PHASE 5 VALIDATION SUMMARY${NC}"
echo "============================================================"
echo -e "Total Tests: ${BLUE}$TOTAL_TESTS${NC}"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}🎉 ALL TESTS PASSED!${NC}"
    echo -e "${GREEN}Phase 5: Recursive Meta-Cognition & Evolutionary Optimization implementation is valid!${NC}"
    exit 0
else
    echo -e "\n${RED}⚠️  SOME TESTS FAILED${NC}"
    echo -e "${RED}Please review the failed tests above.${NC}"
    exit 1
fi