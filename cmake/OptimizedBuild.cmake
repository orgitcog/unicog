# OptimizedBuild.cmake - Advanced Build Optimization Module
# Implements cutting-edge build optimization techniques

# Enable ccache for faster rebuilds
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    message(STATUS "Found ccache: ${CCACHE_PROGRAM}")
    set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
    set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
    
    # Configure ccache for optimal performance
    set(ENV{CCACHE_COMPRESS} "true")
    set(ENV{CCACHE_COMPRESSLEVEL} "6")
    set(ENV{CCACHE_MAXSIZE} "500M")
endif()

# Enable Link Time Optimization (LTO) for Release builds
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    include(CheckIPOSupported)
    check_ipo_supported(RESULT ipo_supported OUTPUT ipo_error)
    
    if(ipo_supported)
        message(STATUS "Enabling Link Time Optimization (LTO)")
        set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
    else()
        message(STATUS "LTO not supported: ${ipo_error}")
    endif()
endif()

# Parallel build optimization
include(ProcessorCount)
ProcessorCount(N)
if(NOT N EQUAL 0)
    set(CMAKE_BUILD_PARALLEL_LEVEL ${N})
    message(STATUS "Building with ${N} parallel jobs")
endif()

# Precompiled headers for common includes
function(add_precompiled_headers target)
    target_precompile_headers(${target} PRIVATE
        <memory>
        <vector>
        <string>
        <map>
        <unordered_map>
        <algorithm>
        <functional>
        <iostream>
    )
endfunction()

# Unity build support for faster compilation
set(CMAKE_UNITY_BUILD_BATCH_SIZE 16)

# Compiler optimization flags
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(
        -march=native
        -mtune=native
        -ffast-math
        -funroll-loops
        -fomit-frame-pointer
    )
    
    if(CMAKE_BUILD_TYPE STREQUAL "Release")
        add_compile_options(-O3)
    endif()
endif()

# Modern CMake target property helpers
function(optimize_target target)
    # Set modern C++ standard
    target_compile_features(${target} PRIVATE cxx_std_17)
    
    # Enable warnings
    if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
        target_compile_options(${target} PRIVATE
            -Wall -Wextra -Wpedantic
            -Wno-unused-parameter
        )
    endif()
    
    # Add precompiled headers
    add_precompiled_headers(${target})
    
    # Enable unity build if supported
    set_target_properties(${target} PROPERTIES
        UNITY_BUILD ON
        UNITY_BUILD_MODE BATCH
    )
endfunction()

# Cache configuration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Installation optimization
set(CMAKE_SKIP_INSTALL_RPATH FALSE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

message(STATUS "Advanced build optimizations enabled")
