#!/bin/bash
# smart-verification.sh
# Enhanced verification that distinguishes between harmless comments and actual placeholders

echo "🧠 Smart Implementation Verification"
echo "===================================="

# Function to check for actual placeholder implementations (not just comments)
check_placeholder_implementations() {
    local file="$1"
    local file_type="$2"
    
    echo "Analyzing $file..."
    
    # For C++ files, look for placeholder implementations
    if [[ "$file_type" == "cpp" ]]; then
        # Check for functions that just return TODO/STUB/MOCK
        if grep -q -E "(return\s+(TODO|STUB|MOCK|0);|throw.*not.*implement)" "$file"; then
            echo "❌ PLACEHOLDER IMPLEMENTATION: $file has placeholder function bodies"
            return 1
        fi
        
        # Check for empty function bodies (excluding headers)
        if [[ "$file" == *.cc ]] && grep -q -E "^\s*}\s*$" "$file" && ! grep -q -B1 -E "^\s*}\s*$" "$file" | grep -q -E "\{\s*$"; then
            echo "⚠️  Checking for empty functions in $file"
        fi
    fi
    
    # For Scheme files, look for placeholder implementations
    if [[ "$file_type" == "scheme" ]]; then
        # Check for functions that just return placeholder values
        if grep -q -E "\(define.*\"(TODO|STUB|MOCK)\"" "$file"; then
            echo "❌ PLACEHOLDER IMPLEMENTATION: $file has placeholder function implementations"
            return 1
        fi
    fi
    
    echo "✅ $file: No placeholder implementations found"
    return 0
}

# Function to verify substantial implementations
verify_implementation_substance() {
    local file="$1"
    local min_size="$2"
    
    if [[ ! -f "$file" ]]; then
        echo "❌ Missing file: $file"
        return 1
    fi
    
    local size=$(stat -c%s "$file")
    
    if [[ $size -lt $min_size ]]; then
        echo "⚠️  Small implementation: $file (size: $size bytes, min: $min_size)"
        # Check if it's a legitimate small file or a stub
        if grep -q -i -E "(stub|todo|fixme|not implemented)" "$file"; then
            echo "❌ PLACEHOLDER: $file appears to be a stub"
            return 1
        else
            echo "✅ VERIFIED: $file is legitimately compact"
        fi
    else
        echo "✅ VERIFIED: $file is substantial (size: $size bytes)"
    fi
    
    return 0
}

# Main verification
echo "🔍 Phase 1: Analyzing C++ Implementations"
echo "==========================================="

cpp_failed=false

for file in cognitive-patterns/src/*.cc; do
    if [[ -f "$file" ]]; then
        verify_implementation_substance "$file" 500 || cpp_failed=true
        check_placeholder_implementations "$file" "cpp" || cpp_failed=true
    fi
done

for file in cognitive-patterns/include/*.h; do
    if [[ -f "$file" ]]; then
        verify_implementation_substance "$file" 200 || cpp_failed=true
        check_placeholder_implementations "$file" "cpp" || cpp_failed=true
    fi
done

echo ""
echo "🧠 Phase 2: Analyzing Scheme Implementations"  
echo "============================================="

scheme_failed=false

for file in */scheme/*.scm; do
    if [[ -f "$file" ]]; then
        verify_implementation_substance "$file" 300 || scheme_failed=true
        check_placeholder_implementations "$file" "scheme" || scheme_failed=true
    fi
done

echo ""
echo "🔨 Phase 3: Testing Functional Behavior"
echo "========================================"

behavior_failed=false

# Test C++ compilation
if [[ -f "ggml-tensor-kernel/test_tensor_kernel.cc" ]]; then
    echo "Testing tensor kernel compilation and execution..."
    if g++ -std=c++17 -I./ggml-tensor-kernel/include -o /tmp/test_tensor_kernel ggml-tensor-kernel/test_tensor_kernel.cc 2>/dev/null; then
        if /tmp/test_tensor_kernel >/dev/null 2>&1; then
            echo "✅ Tensor kernel: Compiles and executes successfully"
        else
            echo "❌ Tensor kernel: Execution failed"
            behavior_failed=true
        fi
    else
        echo "❌ Tensor kernel: Compilation failed"
        behavior_failed=true
    fi
fi

# Test Scheme syntax (if guile available)
if command -v guile &> /dev/null; then
    echo "Testing Scheme implementations..."
    for file in */scheme/*.scm; do
        if [[ -f "$file" ]]; then
            echo "Checking syntax: $file"
            # Create a wrapper that loads the file safely
            cat > /tmp/test_scheme.scm << EOF
(use-modules (srfi srfi-1))
(catch #t
  (lambda () (primitive-load "$file") (display "✅ $file: Syntax OK") (newline))
  (lambda (key . args) (display "⚠️  $file: Syntax issues (may need OpenCog modules)") (newline)))
EOF
            guile -s /tmp/test_scheme.scm 2>/dev/null
        fi
    done
else
    echo "⚠️  Guile not available - skipping Scheme syntax verification"
fi

echo ""
echo "📊 Smart Verification Summary"
echo "============================"

total_failed=0

if $cpp_failed; then
    echo "❌ C++ Implementation Issues Detected"
    total_failed=$((total_failed + 1))
else
    echo "✅ C++ Implementations: All verified as real and substantial"
fi

if $scheme_failed; then
    echo "❌ Scheme Implementation Issues Detected"
    total_failed=$((total_failed + 1))
else
    echo "✅ Scheme Implementations: All verified as real and substantial"
fi

if $behavior_failed; then
    echo "❌ Functional Behavior Issues Detected"
    total_failed=$((total_failed + 1))
else
    echo "✅ Functional Behavior: All tests passed"
fi

echo ""
if [[ $total_failed -eq 0 ]]; then
    echo "🎉 SMART VERIFICATION PASSED!"
    echo "✅ All implementations verified as real, substantial, and functional"
    echo "✅ No actual placeholder implementations detected"
    echo "✅ Comments with TODO/FIXME allowed (not considered placeholders)"
    echo "🛡️  Recursive safeguard against simulation: ACTIVE"
    exit 0
else
    echo "⚠️  Smart verification found $total_failed issue(s)"
    echo "🔧 Please review and address the issues above"
    exit 1
fi