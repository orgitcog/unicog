#!/bin/bash
# Minimal test to verify URE CMake configuration works

set -e

echo "Testing URE CMake dependency configuration..."

# Create a temporary test directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

cd "$TEMP_DIR"

# Create a minimal CMakeLists.txt that tests URE dependency discovery
cat > CMakeLists.txt << 'EOF'
cmake_minimum_required(VERSION 3.10)
project(ure_dependency_test)

# Set module path to include our libraries
list(APPEND CMAKE_MODULE_PATH "/home/runner/work/opencog-unified/opencog-unified/cogutil/cmake")
list(APPEND CMAKE_MODULE_PATH "/home/runner/work/opencog-unified/opencog-unified/atomspace/cmake")

# Try to find URE and see if it properly declares its dependencies
# First need to build the components to generate the config files
message(STATUS "Testing URE dependency declarations...")

# Check that URE config template has proper dependency declarations
set(URE_CONFIG_TEMPLATE "/home/runner/work/opencog-unified/opencog-unified/ure/lib/UREConfig.cmake.in")
if(EXISTS "${URE_CONFIG_TEMPLATE}")
    file(READ "${URE_CONFIG_TEMPLATE}" URE_CONFIG_CONTENT)
    if("${URE_CONFIG_CONTENT}" MATCHES "find_dependency\\(AtomSpace CONFIG REQUIRED\\)")
        message(STATUS "SUCCESS: URE config declares AtomSpace dependency")
    else()
        message(FATAL_ERROR "ERROR: URE config missing AtomSpace dependency")
    endif()
    
    if("${URE_CONFIG_CONTENT}" MATCHES "find_dependency\\(Unify CONFIG REQUIRED\\)")
        message(STATUS "SUCCESS: URE config declares Unify dependency")
    else()
        message(FATAL_ERROR "ERROR: URE config missing Unify dependency")
    endif()
    
    if("${URE_CONFIG_CONTENT}" MATCHES "find_dependency\\(CogUtil CONFIG REQUIRED\\)")
        message(STATUS "SUCCESS: URE config declares CogUtil dependency")
    else()
        message(FATAL_ERROR "ERROR: URE config missing CogUtil dependency")
    endif()
else()
    message(FATAL_ERROR "ERROR: URE config template not found")
endif()

# Check Unify config template
set(UNIFY_CONFIG_TEMPLATE "/home/runner/work/opencog-unified/opencog-unified/unify/lib/UnifyConfig.cmake.in")
if(EXISTS "${UNIFY_CONFIG_TEMPLATE}")
    file(READ "${UNIFY_CONFIG_TEMPLATE}" UNIFY_CONFIG_CONTENT)
    if("${UNIFY_CONFIG_CONTENT}" MATCHES "find_dependency\\(AtomSpace CONFIG REQUIRED\\)")
        message(STATUS "SUCCESS: Unify config declares AtomSpace dependency")
    else()
        message(FATAL_ERROR "ERROR: Unify config missing AtomSpace dependency")
    endif()
    
    if("${UNIFY_CONFIG_CONTENT}" MATCHES "find_dependency\\(CogUtil CONFIG REQUIRED\\)")
        message(STATUS "SUCCESS: Unify config declares CogUtil dependency")
    else()
        message(FATAL_ERROR "ERROR: Unify config missing CogUtil dependency")
    endif()
else()
    message(FATAL_ERROR "ERROR: Unify config template not found")
endif()

message(STATUS "SUCCESS: All dependency declarations verified!")
EOF

# Run cmake to verify our configuration
echo "Running CMake configuration test..."
cmake . -DCMAKE_BUILD_TYPE=Release

if [ $? -eq 0 ]; then
    echo "SUCCESS: CMake dependency test passed!"
    exit 0
else
    echo "FAILURE: CMake dependency test failed!"
    exit 1
fi