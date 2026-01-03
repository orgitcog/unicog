cmake_minimum_required(VERSION 3.10)
project(spacetime VERSION 1.0.0)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Find atomspace (required)
find_path(ATOMSPACE_INCLUDE_DIR opencog/atomspace/AtomSpace.h)
if(NOT ATOMSPACE_INCLUDE_DIR)
    message(STATUS "AtomSpace headers not found, using relative path")
    set(ATOMSPACE_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../atomspace)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../cogutil
)

# SpaceTime library sources
set(SPACETIME_SOURCES
    src/SpaceTimeMap.cc
    src/SpatialReasoning.cc
    src/TemporalReasoning.cc
    src/SpaceTimeAtom.cc
    src/GeometricAlgorithms.cc
    src/TemporalSequence.cc
)

# Create spacetime library
add_library(spacetime SHARED ${SPACETIME_SOURCES})
ADD_DEPENDENCIES(spacetime atombase atomcore atomspace atomvalue cogutil truthvalue)


# Link dependencies  
target_link_libraries(spacetime
    atomspace
)

# Install targets
install(TARGETS spacetime
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

# Install headers
install(DIRECTORY opencog/spacetime/
    DESTINATION include/opencog/spacetime
    FILES_MATCHING PATTERN "*.h"
)

# Add tests if BUILD_TESTS is enabled
option(BUILD_TESTS "Build spacetime tests" ON)
if(BUILD_TESTS)
    add_subdirectory(tests)
endif()