# Evolutionary Optimization Component CMakeLists.txt
# Phase 5: Recursive Meta-Cognition & Evolutionary Optimization

cmake_minimum_required(VERSION 3.10)

# Project configuration
set(COMPONENT_NAME "evolutionary-optimization")
project(${COMPONENT_NAME} VERSION 1.0.0 LANGUAGES CXX)

# C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../meta-cognition/include)

# Find required dependencies
find_package(Threads REQUIRED)

# Source files
set(SOURCES
    src/EvolutionaryOptimizer.cc
)

# Header files
set(HEADERS
    include/EvolutionaryOptimizer.h
)

# Create the evolutionary optimization library
add_library(${COMPONENT_NAME} ${SOURCES} ${HEADERS})

# Link libraries
target_link_libraries(${COMPONENT_NAME} 
    Threads::Threads
)

# Dependencies on meta-cognition component
if(TARGET meta-cognition)
    target_link_libraries(${COMPONENT_NAME} meta-cognition)
    add_dependencies(${COMPONENT_NAME} meta-cognition)
endif()

# Set target properties
set_target_properties(${COMPONENT_NAME} PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PUBLIC_HEADER "${HEADERS}"
)

# Installation rules
install(TARGETS ${COMPONENT_NAME}
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/opencog/evolutionary-optimization
)

# Install header files
install(DIRECTORY include/ DESTINATION include/opencog/evolutionary-optimization
    FILES_MATCHING PATTERN "*.h"
)

# Example applications
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Unit tests
if(BUILD_TESTS)
    add_subdirectory(tests)
endif()

# Component information
message(STATUS "Configured evolutionary-optimization component for Phase 5")
message(STATUS "  Sources: ${SOURCES}")
message(STATUS "  Headers: ${HEADERS}")

# Export targets for integration with other components
export(TARGETS ${COMPONENT_NAME} FILE ${COMPONENT_NAME}-targets.cmake)