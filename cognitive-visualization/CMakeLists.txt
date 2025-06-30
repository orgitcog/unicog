# Phase II.4: Interactive Cognitive Visualization
# Real-time introspection with adaptive attention overlays

cmake_minimum_required(VERSION 3.10)
project(cognitive-visualization)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories from parent modules
include_directories(${CMAKE_SOURCE_DIR}/cogutil/opencog)
include_directories(${CMAKE_SOURCE_DIR}/atomspace/opencog)
include_directories(${CMAKE_SOURCE_DIR}/cogserver/opencog)
include_directories(${CMAKE_SOURCE_DIR}/cognitive-patterns/include)
include_directories(${CMAKE_SOURCE_DIR}/distributed-cognition/include)

# Source files for cognitive visualization
set(VISUALIZATION_SOURCES
    src/CognitiveVisualizer.cc
    src/AttentionOverlay.cc
    src/HypergraphRenderer.cc
    src/RealTimeIntrospector.cc
)

# Header files
set(VISUALIZATION_HEADERS
    include/CognitiveVisualizer.h
    include/AttentionOverlay.h
    include/HypergraphRenderer.h
    include/RealTimeIntrospector.h
)

# Create include directory
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/web)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scheme)

# Create the cognitive visualization library
add_library(cognitive-visualization SHARED ${VISUALIZATION_SOURCES})

# Install headers
install(FILES ${VISUALIZATION_HEADERS} DESTINATION include/opencog/cognitive-visualization)

# Install library
install(TARGETS cognitive-visualization DESTINATION lib)

# Install web interface files
install(DIRECTORY web/ DESTINATION share/opencog/web/cognitive-visualization/)

# Install Scheme files
install(DIRECTORY scheme/ DESTINATION share/opencog/scm/cognitive-visualization/)