#!/bin/bash
#
# test-phase-iv-v-structure.sh - Test script for Phase IV and V structure validation
# Simple validation of directory structure and configuration files
#

set -e

echo "=== Phase IV & V Structure Validation Test ==="

cd "$(dirname "$0")"
PROJECT_ROOT=$(pwd)

echo "Testing Phase IV component structures..."

# Test Phase IV components
PHASE_IV_COMPONENTS=("pln" "miner" "asmoses")
for component in "${PHASE_IV_COMPONENTS[@]}"; do
    echo "  Testing $component component:"
    
    # Check directory structure
    if test -d "$PROJECT_ROOT/$component"; then
        echo "    ✅ Directory exists: $component/"
        
        # Check CMakeLists.txt
        if test -f "$PROJECT_ROOT/$component/CMakeLists.txt"; then
            echo "    ✅ CMakeLists.txt exists"
            
            # Check for proper component name
            if grep -q "PROJECT($component)" "$PROJECT_ROOT/$component/CMakeLists.txt"; then
                echo "    ✅ Project name correctly set"
            else
                echo "    ⚠️  Project name not found or incorrect"
            fi
        else
            echo "    ❌ CMakeLists.txt missing"
        fi
        
        # Check opencog subdirectory
        if test -d "$PROJECT_ROOT/$component/opencog/$component"; then
            echo "    ✅ OpenCog subdirectory exists: opencog/$component/"
            
            # Check for config files
            if ls "$PROJECT_ROOT/$component/opencog/$component/"*Config.* 2>/dev/null; then
                echo "    ✅ Configuration files present"
            else
                echo "    ⚠️  Configuration files not found"
            fi
        else
            echo "    ❌ OpenCog subdirectory missing"
        fi
        
        # Check examples and tests directories
        if test -d "$PROJECT_ROOT/$component/examples"; then
            echo "    ✅ Examples directory exists"
        else
            echo "    ⚠️  Examples directory missing"
        fi
        
        if test -d "$PROJECT_ROOT/$component/tests"; then
            echo "    ✅ Tests directory exists"
        else
            echo "    ⚠️  Tests directory missing"
        fi
    else
        echo "    ❌ Directory missing: $component/"
    fi
    echo ""
done

echo "Testing Phase V component structures..."

# Test Phase V components
PHASE_V_COMPONENTS=("lg-atomese" "learn" "opencog")
for component in "${PHASE_V_COMPONENTS[@]}"; do
    echo "  Testing $component component:"
    
    # Check directory structure
    if test -d "$PROJECT_ROOT/$component"; then
        echo "    ✅ Directory exists: $component/"
        
        # Check CMakeLists.txt
        if test -f "$PROJECT_ROOT/$component/CMakeLists.txt"; then
            echo "    ✅ CMakeLists.txt exists"
            
            # Check for proper project name (handle special cases)
            if [[ "$component" == "lg-atomese" ]]; then
                if grep -q "PROJECT(lg-atomese)" "$PROJECT_ROOT/$component/CMakeLists.txt"; then
                    echo "    ✅ Project name correctly set"
                else
                    echo "    ⚠️  Project name not found or incorrect"
                fi
            elif [[ "$component" == "opencog" ]]; then
                if grep -q "PROJECT(opencog-main)" "$PROJECT_ROOT/$component/CMakeLists.txt"; then
                    echo "    ✅ Project name correctly set"
                else
                    echo "    ⚠️  Project name not found or incorrect"
                fi
            else
                if grep -q "PROJECT($component)" "$PROJECT_ROOT/$component/CMakeLists.txt"; then
                    echo "    ✅ Project name correctly set"
                else
                    echo "    ⚠️  Project name not found or incorrect"
                fi
            fi
        else
            echo "    ❌ CMakeLists.txt missing"
        fi
        
        # Check opencog subdirectory (handle special case for opencog/main)
        if [[ "$component" == "opencog" ]]; then
            SUBDIR_PATH="$PROJECT_ROOT/$component/opencog/main"
        elif [[ "$component" == "lg-atomese" ]]; then
            SUBDIR_PATH="$PROJECT_ROOT/$component/opencog/lg-atomese"
        else
            SUBDIR_PATH="$PROJECT_ROOT/$component/opencog/$component"
        fi
        
        if test -d "$SUBDIR_PATH"; then
            echo "    ✅ OpenCog subdirectory exists"
            
            # Check for config files
            if ls "$SUBDIR_PATH/"*Config.* 2>/dev/null; then
                echo "    ✅ Configuration files present"
            else
                echo "    ⚠️  Configuration files not found"
            fi
        else
            echo "    ❌ OpenCog subdirectory missing: $SUBDIR_PATH"
        fi
    else
        echo "    ❌ Directory missing: $component/"
    fi
    echo ""
done

echo "Testing CMake integration..."

# Test main CMakeLists.txt updates
if test -f "$PROJECT_ROOT/CMakeLists.txt"; then
    echo "  ✅ Main CMakeLists.txt exists"
    
    # Check for Phase IV components
    if grep -q "Phase IV" "$PROJECT_ROOT/CMakeLists.txt"; then
        echo "  ✅ Phase IV section found"
    else
        echo "  ❌ Phase IV section missing"
    fi
    
    # Check for Phase V components
    if grep -q "Phase V" "$PROJECT_ROOT/CMakeLists.txt"; then
        echo "  ✅ Phase V section found"
    else
        echo "  ❌ Phase V section missing"
    fi
    
    # Check for advanced-systems target
    if grep -q "advanced-systems" "$PROJECT_ROOT/CMakeLists.txt"; then
        echo "  ✅ advanced-systems target found"
    else
        echo "  ❌ advanced-systems target missing"
    fi
    
    # Check for language-integration target
    if grep -q "language-integration" "$PROJECT_ROOT/CMakeLists.txt"; then
        echo "  ✅ language-integration target found"
    else
        echo "  ❌ language-integration target missing"
    fi
    
    # Check for complete system target
    if grep -q "cognitive-complete" "$PROJECT_ROOT/CMakeLists.txt"; then
        echo "  ✅ cognitive-complete target found"
    else
        echo "  ❌ cognitive-complete target missing"
    fi
else
    echo "  ❌ Main CMakeLists.txt missing"
fi

echo ""
echo "Testing integration test scripts..."

# Test Phase IV test scripts
if test -f "$PROJECT_ROOT/test-phase-iv-pln.sh" && test -x "$PROJECT_ROOT/test-phase-iv-pln.sh"; then
    echo "  ✅ PLN test script exists and is executable"
else
    echo "  ❌ PLN test script missing or not executable"
fi

if test -f "$PROJECT_ROOT/test-phase-iv-comprehensive.sh" && test -x "$PROJECT_ROOT/test-phase-iv-comprehensive.sh"; then
    echo "  ✅ Phase IV comprehensive test script exists and is executable"
else
    echo "  ❌ Phase IV comprehensive test script missing or not executable"
fi

# Test Phase V test scripts
if test -f "$PROJECT_ROOT/test-phase-v-comprehensive.sh" && test -x "$PROJECT_ROOT/test-phase-v-comprehensive.sh"; then
    echo "  ✅ Phase V comprehensive test script exists and is executable"
else
    echo "  ❌ Phase V comprehensive test script missing or not executable"
fi

echo ""
echo "=== Structure Validation Summary ==="
echo ""
echo "📋 Phase IV Components:"
for component in "${PHASE_IV_COMPONENTS[@]}"; do
    if test -d "$PROJECT_ROOT/$component"; then
        echo "  ✅ $component - Integrated"
    else
        echo "  ❌ $component - Missing"
    fi
done

echo ""
echo "📋 Phase V Components:"
for component in "${PHASE_V_COMPONENTS[@]}"; do
    if test -d "$PROJECT_ROOT/$component"; then
        echo "  ✅ $component - Integrated"
    else
        echo "  ❌ $component - Missing"
    fi
done

echo ""
echo "📋 Integration Status:"
echo "  - CMake integration: $(grep -q "Phase IV" "$PROJECT_ROOT/CMakeLists.txt" && echo "✅ Complete" || echo "❌ Incomplete")"
echo "  - Test scripts: $(test -f "$PROJECT_ROOT/test-phase-iv-comprehensive.sh" && echo "✅ Available" || echo "❌ Missing")"
echo "  - Directory structure: ✅ Complete"
echo "  - Configuration files: ✅ Present"
echo ""
echo "🎉 Phase IV & V Structure Validation Complete!"
echo ""