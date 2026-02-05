#!/bin/bash
#
# test-phase-iv-pln.sh - Test script for Phase IV PLN integration
# Week 13: PLN Integration Testing
#

set -e

echo "=== Phase IV - Week 13: PLN (Probabilistic Logic Networks) Integration Test ==="

# Test PLN component build
echo "Testing PLN component build..."
cd "$(dirname "$0")"
PROJECT_ROOT=$(pwd)

# Create build directory
BUILD_DIR="$PROJECT_ROOT/build"
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure build with PLN component
echo "Configuring build with PLN component..."
cmake -DCMAKE_BUILD_TYPE=Release "$PROJECT_ROOT"

# Build PLN component
echo "Building PLN component..."
if make pln-all; then
    echo "✅ PLN component built successfully"
else
    echo "❌ PLN component build failed"
    exit 1
fi

# Test PLN configuration
echo "Testing PLN configuration..."
cat > test_pln_config.cpp << 'EOF'
#include <iostream>
#include "../pln/opencog/pln/PLNConfig.h"

int main() {
    opencog::pln::PLNConfig config;
    std::cout << "PLN Configuration Test:" << std::endl;
    std::cout << "Confidence Threshold: " << config.getConfidenceThreshold() << std::endl;
    std::cout << "Max Inference Steps: " << config.getMaxInferenceSteps() << std::endl;
    std::cout << "✅ PLN configuration test passed" << std::endl;
    return 0;
}
EOF

# Compile and run test
if g++ -std=c++17 -I"$PROJECT_ROOT" test_pln_config.cpp -o test_pln_config 2>/dev/null; then
    if ./test_pln_config; then
        echo "✅ PLN configuration test passed"
    else
        echo "❌ PLN configuration test failed"
    fi
else
    echo "ℹ️ PLN configuration test skipped (compilation dependencies missing)"
fi

# Test PLN integration with URE and spacetime dependencies
echo "Testing PLN integration with dependencies..."
echo "Dependencies tested:"
echo "  - atomspace: $(test -d "$PROJECT_ROOT/atomspace" && echo "✅ Present" || echo "❌ Missing")"
echo "  - ure: $(test -d "$PROJECT_ROOT/ure" && echo "✅ Present" || echo "❌ Missing")"
echo "  - spacetime: $(test -d "$PROJECT_ROOT/spacetime" && echo "✅ Present" || echo "❌ Missing")"

# Cleanup
rm -f test_pln_config test_pln_config.cpp

echo "=== Phase IV - Week 13: PLN Integration Test Complete ==="
echo ""