# ATenSpace: ATen Tensor-based AtomSpace for OpenCog
# Integrated into OpenCog Unified monorepo
# Original source: https://github.com/o9nn/ATenSpace

cmake_minimum_required(VERSION 3.14)

# When building as part of opencog-unified, project is already set
if(NOT PROJECT_NAME)
    project(ATenSpace
        VERSION 0.1.0
        DESCRIPTION "ATen Tensor-based adaptation of OpenCog AtomSpace"
        LANGUAGES CXX CUDA
    )
endif()

# Set C++ standard (inherit from parent if set)
if(NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()

# Build options
option(ATENSPACE_BUILD_TESTS "Build ATenSpace tests" ON)
option(ATENSPACE_BUILD_EXAMPLES "Build ATenSpace examples" ON)
option(ATENSPACE_USE_CUDA "Enable CUDA support" OFF)

message(STATUS "")
message(STATUS "=== ATenSpace: Tensor-based AtomSpace ===")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# ----------------------------------------------------------
# Find PyTorch/ATen
# ----------------------------------------------------------

# Try to find Torch/LibTorch
find_package(Torch QUIET)

if(Torch_FOUND)
    message(STATUS "ATenSpace: Found PyTorch ${Torch_VERSION}")
    set(ATENSPACE_TORCH_FOUND TRUE)
else()
    # Try finding via Python
    find_package(Python3 COMPONENTS Interpreter Development)
    if(Python3_FOUND)
        execute_process(
            COMMAND ${Python3_EXECUTABLE} -c "import torch; print(torch.utils.cmake_prefix_path)"
            OUTPUT_VARIABLE TORCH_CMAKE_PREFIX
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )
        if(TORCH_CMAKE_PREFIX)
            list(APPEND CMAKE_PREFIX_PATH ${TORCH_CMAKE_PREFIX})
            find_package(Torch QUIET)
            if(Torch_FOUND)
                message(STATUS "ATenSpace: Found PyTorch via Python at ${TORCH_CMAKE_PREFIX}")
                set(ATENSPACE_TORCH_FOUND TRUE)
            endif()
        endif()
    endif()
endif()

if(NOT ATENSPACE_TORCH_FOUND)
    message(STATUS "ATenSpace: PyTorch/LibTorch not found - building header-only components")
    message(STATUS "  To enable full build, install PyTorch or set Torch_DIR")
    set(ATENSPACE_HEADER_ONLY TRUE)
endif()

# ----------------------------------------------------------
# CUDA Support (optional)
# ----------------------------------------------------------

if(ATENSPACE_USE_CUDA)
    include(CheckLanguage)
    check_language(CUDA)
    if(CMAKE_CUDA_COMPILER)
        enable_language(CUDA)
        find_package(CUDAToolkit)
        if(CUDAToolkit_FOUND)
            message(STATUS "ATenSpace: CUDA ${CUDAToolkit_VERSION} enabled")
            set(ATENSPACE_CUDA_FOUND TRUE)
        endif()
    else()
        message(STATUS "ATenSpace: CUDA requested but not found")
    endif()
endif()

# ----------------------------------------------------------
# ATenSpace Core Library
# ----------------------------------------------------------

set(ATENSPACE_ATOMSPACE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/aten/src/ATen/atomspace)

# Header files
set(ATENSPACE_HEADERS
    ${ATENSPACE_ATOMSPACE_DIR}/Atom.h
    ${ATENSPACE_ATOMSPACE_DIR}/AtomSpace.h
    ${ATENSPACE_ATOMSPACE_DIR}/ATenSpace.h
    ${ATENSPACE_ATOMSPACE_DIR}/TimeServer.h
    ${ATENSPACE_ATOMSPACE_DIR}/AttentionBank.h
    ${ATENSPACE_ATOMSPACE_DIR}/Serializer.h
    ${ATENSPACE_ATOMSPACE_DIR}/PatternMatcher.h
    ${ATENSPACE_ATOMSPACE_DIR}/TruthValue.h
    ${ATENSPACE_ATOMSPACE_DIR}/ForwardChainer.h
    ${ATENSPACE_ATOMSPACE_DIR}/BackwardChainer.h
    ${ATENSPACE_ATOMSPACE_DIR}/ECAN.h
)

# Create interface library for header-only usage
add_library(atenspace-headers INTERFACE)
target_include_directories(atenspace-headers INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/aten/src>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/aten/src/ATen>
    $<INSTALL_INTERFACE:include>
)

# If Torch is found, build the full library
if(ATENSPACE_TORCH_FOUND)
    # Create atenspace library
    add_library(atenspace INTERFACE)
    target_include_directories(atenspace INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/aten/src>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/aten/src/ATen>
        $<INSTALL_INTERFACE:include>
    )
    target_link_libraries(atenspace INTERFACE ${TORCH_LIBRARIES})
    target_compile_features(atenspace INTERFACE cxx_std_17)

    # Add CUDA flags if available
    if(ATENSPACE_CUDA_FOUND)
        target_compile_definitions(atenspace INTERFACE ATENSPACE_CUDA_ENABLED=1)
    endif()

    # Build examples if requested
    if(ATENSPACE_BUILD_EXAMPLES)
        add_executable(atenspace-example ${ATENSPACE_ATOMSPACE_DIR}/example.cpp)
        target_link_libraries(atenspace-example atenspace)
        set_target_properties(atenspace-example PROPERTIES CXX_STANDARD 17)

        add_executable(atenspace-example-advanced ${ATENSPACE_ATOMSPACE_DIR}/example_advanced.cpp)
        target_link_libraries(atenspace-example-advanced atenspace)
        set_target_properties(atenspace-example-advanced PROPERTIES CXX_STANDARD 17)

        add_executable(atenspace-example-pln ${ATENSPACE_ATOMSPACE_DIR}/example_pln.cpp)
        target_link_libraries(atenspace-example-pln atenspace)
        set_target_properties(atenspace-example-pln PROPERTIES CXX_STANDARD 17)

        add_executable(atenspace-example-ecan ${ATENSPACE_ATOMSPACE_DIR}/example_ecan.cpp)
        target_link_libraries(atenspace-example-ecan atenspace)
        set_target_properties(atenspace-example-ecan PROPERTIES CXX_STANDARD 17)
    endif()

    # Build tests if requested
    if(ATENSPACE_BUILD_TESTS)
        add_executable(atenspace-test ${ATENSPACE_ATOMSPACE_DIR}/test.cpp)
        target_link_libraries(atenspace-test atenspace)
        set_target_properties(atenspace-test PROPERTIES CXX_STANDARD 17)

        add_executable(atenspace-test-advanced ${ATENSPACE_ATOMSPACE_DIR}/test_advanced.cpp)
        target_link_libraries(atenspace-test-advanced atenspace)
        set_target_properties(atenspace-test-advanced PROPERTIES CXX_STANDARD 17)

        add_executable(atenspace-test-pln ${ATENSPACE_ATOMSPACE_DIR}/test_pln.cpp)
        target_link_libraries(atenspace-test-pln atenspace)
        set_target_properties(atenspace-test-pln PROPERTIES CXX_STANDARD 17)

        add_executable(atenspace-test-ecan ${ATENSPACE_ATOMSPACE_DIR}/test_ecan.cpp)
        target_link_libraries(atenspace-test-ecan atenspace)
        set_target_properties(atenspace-test-ecan PROPERTIES CXX_STANDARD 17)

        # Add tests to CTest
        enable_testing()
        add_test(NAME ATenSpace_Core COMMAND atenspace-test)
        add_test(NAME ATenSpace_Advanced COMMAND atenspace-test-advanced)
        add_test(NAME ATenSpace_PLN COMMAND atenspace-test-pln)
        add_test(NAME ATenSpace_ECAN COMMAND atenspace-test-ecan)
    endif()
endif()

# ----------------------------------------------------------
# Installation
# ----------------------------------------------------------

# Install headers
install(FILES ${ATENSPACE_HEADERS}
    DESTINATION include/ATen/atomspace
)

# Install library targets
if(ATENSPACE_TORCH_FOUND)
    install(TARGETS atenspace atenspace-headers
        EXPORT ATenSpaceTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )
else()
    install(TARGETS atenspace-headers
        EXPORT ATenSpaceTargets
        INCLUDES DESTINATION include
    )
endif()

# ----------------------------------------------------------
# Status Summary
# ----------------------------------------------------------

message(STATUS "")
message(STATUS "ATenSpace Configuration Summary:")
message(STATUS "  PyTorch/LibTorch: ${ATENSPACE_TORCH_FOUND}")
message(STATUS "  CUDA support: ${ATENSPACE_CUDA_FOUND}")
message(STATUS "  Build tests: ${ATENSPACE_BUILD_TESTS}")
message(STATUS "  Build examples: ${ATENSPACE_BUILD_EXAMPLES}")
if(ATENSPACE_HEADER_ONLY)
    message(STATUS "  Mode: Header-only (install PyTorch for full build)")
else()
    message(STATUS "  Mode: Full build with examples and tests")
endif()
message(STATUS "=== ATenSpace configuration complete ===")
message(STATUS "")
