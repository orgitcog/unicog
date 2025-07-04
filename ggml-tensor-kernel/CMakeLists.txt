cmake_minimum_required(VERSION 3.10)
project(ggml-tensor-kernel)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find OpenCog dependencies
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

# Find or build GGML
option(BUILD_GGML_FROM_SOURCE "Build GGML from source" ON)

if(BUILD_GGML_FROM_SOURCE)
    include(FetchContent)
    FetchContent_Declare(
        ggml
        GIT_REPOSITORY https://github.com/ggerganov/ggml.git
        GIT_TAG        master
        GIT_SHALLOW    TRUE
    )
    FetchContent_MakeAvailable(ggml)
    set(GGML_LIBRARIES ggml)
    set(GGML_INCLUDE_DIRS ${ggml_SOURCE_DIR}/include)
else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GGML REQUIRED ggml)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${GGML_INCLUDE_DIRS}
    ${CMAKE_PREFIX_PATH}/include
)

# Create the unified tensor kernel library (minimal version for now)
add_library(ggml-tensor-kernel-minimal SHARED
    test_tensor_kernel.cc
)

# Link libraries
target_link_libraries(ggml-tensor-kernel-minimal
    ${GGML_LIBRARIES}
)

# Create executable test
add_executable(test-tensor-kernel test_tensor_kernel.cc)
target_link_libraries(test-tensor-kernel ${GGML_LIBRARIES})

# Install targets
install(TARGETS ggml-tensor-kernel-minimal test-tensor-kernel
    LIBRARY DESTINATION lib/opencog
    RUNTIME DESTINATION bin
)

# Install headers
install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Install scheme files
install(DIRECTORY scheme/
    DESTINATION share/guile/site/3.0/opencog/
    FILES_MATCHING PATTERN "*.scm"
)