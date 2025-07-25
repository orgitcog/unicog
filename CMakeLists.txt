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

# Phase I: Core Extensions (Week 1)
# Add atomspace-rocks if it exists and has CMakeLists.txt
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/atomspace-rocks/CMakeLists.txt")
    add_subdirectory(atomspace-rocks)
endif()

# Phase I: Core Extensions (Week 3)
# Add moses if it exists and has CMakeLists.txt
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/moses/CMakeLists.txt")
    add_subdirectory(moses)
endif()

# Define inter-module dependencies explicitly
add_dependencies(atomspace cogutil)
add_dependencies(cogserver atomspace)
# atomspace-rocks depends on atomspace
if(TARGET atomspace-rocks)
    add_dependencies(atomspace-rocks atomspace)
endif()
# moses depends on cogutil
if(TARGET moses)
    add_dependencies(moses cogutil)
endif()

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

# Add unified GGML tensor kernel
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ggml-tensor-kernel/CMakeLists.txt")
    add_subdirectory(ggml-tensor-kernel)
endif()

# Add agentic kernels catalog
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/agentic-kernels-catalog/CMakeLists.txt")
    add_subdirectory(agentic-kernels-catalog)
endif()

# Global cognitive build targets
add_custom_target(cognitive ALL DEPENDS cogutil atomspace cogserver)

# Add comprehensive target that includes agentic catalog
add_custom_target(cognitive-complete 
    DEPENDS cognitive agentic-kernels-catalog
    COMMENT "Building complete cognitive system with agentic kernels catalog")