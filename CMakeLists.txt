cmake_minimum_required(VERSION 3.10)
project(opencog-unified VERSION 1.0.0)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Create deps directory if it doesn't exist
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/deps)

# Include dependencies if directory exists and has CMakeLists.txt
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/deps/CMakeLists.txt")
    add_subdirectory(deps)
endif()

# Include dependencies (commented out as deps directory doesn't exist)
# add_subdirectory(deps)

# Include unified cognitive modules
add_subdirectory(cogutil)
add_subdirectory(atomspace)
add_subdirectory(cogserver)

# Define inter-module dependencies explicitly
add_dependencies(atomspace cogutil)
add_dependencies(cogserver atomspace)

# Phase II: Recursive Cognitive Expansion modules
# Create directories for new components
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/cognitive-patterns)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/distributed-cognition) 
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/cognitive-visualization)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tutorial-automation)

# Add Phase II modules if they have CMakeLists.txt
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cognitive-patterns/CMakeLists.txt")
    add_subdirectory(cognitive-patterns)
endif()
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/distributed-cognition/CMakeLists.txt")
    add_subdirectory(distributed-cognition)
endif()
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cognitive-visualization/CMakeLists.txt")
    add_subdirectory(cognitive-visualization)
endif()
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tutorial-automation/CMakeLists.txt")
    add_subdirectory(tutorial-automation)
endif()

# Global cognitive build targets
add_custom_target(cognitive ALL DEPENDS cogutil atomspace cogserver)