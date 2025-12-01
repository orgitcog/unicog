# Modern CMake Configuration for OpenCog Unified
# Provides best practices and utilities for consistent builds

cmake_minimum_required(VERSION 3.16)

# Set modern C++ standards
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Enable position independent code
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Export compile commands for tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Color diagnostics
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-fdiagnostics-color=always)
endif()

# Optimization flags
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    add_compile_options(-O3 -march=native -DNDEBUG)
elseif(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_options(-O0 -g3 -ggdb)
    if(ENABLE_COVERAGE)
        add_compile_options(--coverage)
        add_link_options(--coverage)
    endif()
endif()

# Warning flags
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(
        -Wall
        -Wextra
        -Wpedantic
        -Wconversion
        -Wshadow
        -Wnon-virtual-dtor
        -Wold-style-cast
        -Wcast-align
        -Wunused
        -Woverloaded-virtual
        -Wnull-dereference
        -Wdouble-promotion
        -Wformat=2
    )
endif()

# Function to add modern library target
function(add_opencog_library TARGET_NAME)
    cmake_parse_arguments(
        ARG
        ""
        ""
        "SOURCES;HEADERS;DEPENDENCIES;INCLUDE_DIRS"
        ${ARGN}
    )
    
    add_library(${TARGET_NAME} ${ARG_SOURCES} ${ARG_HEADERS})
    
    target_include_directories(${TARGET_NAME}
        PUBLIC
            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
            $<INSTALL_INTERFACE:include>
        PRIVATE
            ${ARG_INCLUDE_DIRS}
    )
    
    if(ARG_DEPENDENCIES)
        target_link_libraries(${TARGET_NAME} PUBLIC ${ARG_DEPENDENCIES})
    endif()
    
    target_compile_features(${TARGET_NAME} PUBLIC cxx_std_17)
    
    # Install rules
    install(TARGETS ${TARGET_NAME}
        EXPORT ${TARGET_NAME}Targets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )
    
    if(ARG_HEADERS)
        install(FILES ${ARG_HEADERS} DESTINATION include/${TARGET_NAME})
    endif()
endfunction()

# Function to add modern executable target
function(add_opencog_executable TARGET_NAME)
    cmake_parse_arguments(
        ARG
        ""
        ""
        "SOURCES;DEPENDENCIES"
        ${ARGN}
    )
    
    add_executable(${TARGET_NAME} ${ARG_SOURCES})
    
    if(ARG_DEPENDENCIES)
        target_link_libraries(${TARGET_NAME} PRIVATE ${ARG_DEPENDENCIES})
    endif()
    
    target_compile_features(${TARGET_NAME} PRIVATE cxx_std_17)
    
    install(TARGETS ${TARGET_NAME}
        RUNTIME DESTINATION bin
    )
endfunction()

# Function to add modern test target
function(add_opencog_test TEST_NAME)
    cmake_parse_arguments(
        ARG
        ""
        ""
        "SOURCES;DEPENDENCIES"
        ${ARGN}
    )
    
    add_executable(${TEST_NAME} ${ARG_SOURCES})
    
    if(ARG_DEPENDENCIES)
        target_link_libraries(${TEST_NAME} PRIVATE ${ARG_DEPENDENCIES})
    endif()
    
    target_compile_features(${TEST_NAME} PRIVATE cxx_std_17)
    
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
    
    # Set test properties
    set_tests_properties(${TEST_NAME} PROPERTIES
        TIMEOUT 300
        LABELS "unit"
    )
endfunction()

# Print configuration summary
message(STATUS "=== OpenCog Build Configuration ===")
message(STATUS "Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ Compiler: ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "Install Prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "===================================")
