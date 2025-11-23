# Meta-Cognition Component CMakeLists.txt
# Phase 5: Recursive Meta-Cognition & Evolutionary Optimization

cmake_minimum_required(VERSION 3.10)

# Project configuration
set(COMPONENT_NAME "meta-cognition")
project(${COMPONENT_NAME} VERSION 1.0.0 LANGUAGES CXX)

# C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Find required dependencies
find_package(Threads REQUIRED)

# Source files
set(SOURCES
    src/MetaCognitiveMonitor.cc
    src/CoreSelfAwareness.cc
)

# Header files
set(HEADERS
    include/MetaCognitiveMonitor.h
    include/CoreSelfAwareness.h
)

# Create the meta-cognition library
add_library(${COMPONENT_NAME} ${SOURCES} ${HEADERS})

# Link libraries
target_link_libraries(${COMPONENT_NAME} 
    Threads::Threads
)

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
    PUBLIC_HEADER DESTINATION include/opencog/meta-cognition
)

# Install header files
install(DIRECTORY include/ DESTINATION include/opencog/meta-cognition
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
message(STATUS "Configured meta-cognition component for Phase 5")
message(STATUS "  Sources: ${SOURCES}")
message(STATUS "  Headers: ${HEADERS}")

# Export targets for integration with other components
export(TARGETS ${COMPONENT_NAME} FILE ${COMPONENT_NAME}-targets.cmake)