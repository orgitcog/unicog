#!/bin/bash
# Simple test to verify GGML/Tensor implementation compiles and runs
# This is called by the neural-symbolic-integration workflow

set -e

echo "🔧 Testing GGML Tensor Kernel Implementation..."

cd "$(dirname "$0")"

# Check if GGML/Tensor directory exists
if [[ ! -d "GGML/Tensor" ]]; then
    echo "❌ GGML/Tensor directory not found"
    exit 1
fi

echo "  ✅ GGML/Tensor directory found"

# Check if required files exist
required_files=("GGML/Tensor/fold_rules.h" "GGML/Tensor/fold_rules.cc" 
                "GGML/Tensor/ann_rules.h" "GGML/Tensor/ann_rules.cc"
                "GGML/Tensor/test_tensor_rules.cc")

for file in "${required_files[@]}"; do
    if [[ ! -f "$file" ]]; then
        echo "❌ Required file not found: $file"
        exit 1
    fi
done

echo "  ✅ All required files present"

# Compile the tensor kernels
echo "  Compiling tensor kernels..."
cd GGML/Tensor

g++ -std=c++17 -c fold_rules.cc -o fold_rules.o 2>&1 || {
    echo "❌ Failed to compile fold_rules.cc"
    exit 1
}

g++ -std=c++17 -c ann_rules.cc -o ann_rules.o 2>&1 || {
    echo "❌ Failed to compile ann_rules.cc"
    exit 1
}

echo "  ✅ Tensor kernels compiled successfully"

# Build and run tests
echo "  Building and running tests..."
g++ -std=c++17 -o test_tensor_rules test_tensor_rules.cc fold_rules.o ann_rules.o 2>&1 || {
    echo "❌ Failed to build test_tensor_rules"
    exit 1
}

./test_tensor_rules > /tmp/tensor_test_output.log 2>&1 || {
    echo "❌ Test execution failed"
    cat /tmp/tensor_test_output.log
    exit 1
}

echo "  ✅ All tests passed"

# Check test output for success markers
if grep -q "All tests passed successfully" /tmp/tensor_test_output.log; then
    echo "  ✅ Test validation confirmed"
else
    echo "❌ Test output validation failed"
    exit 1
fi

# Clean up
rm -f fold_rules.o ann_rules.o test_tensor_rules

cd ../..

echo ""
echo "🎉 GGML Tensor Kernel Implementation: SUCCESS"
echo "✅ fold_rules.h/cc: Complete and functional"
echo "✅ ann_rules.h/cc: Complete and functional"
echo "✅ Neural-symbolic integration: READY"
echo ""

exit 0
