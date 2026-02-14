# Phase II.1: Perceptual Input Layer (Scheme/C++ Integration)
# Recursive Cognitive Expansion - Perceptual Input Processing

cmake_minimum_required(VERSION 3.10)
project(cognitive-patterns)

# Find required packages
find_package(PkgConfig REQUIRED)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories from parent modules
include_directories(${CMAKE_SOURCE_DIR}/cogutil/opencog)
include_directories(${CMAKE_SOURCE_DIR}/atomspace/opencog)
include_directories(${CMAKE_SOURCE_DIR}/cogserver/opencog)

# APRFE Integration: Enhanced cognitive-patterns with Adaptive Fusion Engine
set(PATTERN_SOURCES
    src/AdaptivePatternRecognitionFusionEngine.cc
    src/PerceptualInputProcessor.cc
    src/RecursivePatternEncoder.cc
    src/HypergraphPatternExtractor.cc
    src/CognitiveObjectEncoder.cc
)

# Enhanced header files including APRFE
set(PATTERN_HEADERS
    include/AdaptivePatternRecognitionFusionEngine.h
    include/PerceptualInputProcessor.h
    include/RecursivePatternEncoder.h
    include/HypergraphPatternExtractor.h
    include/CognitiveObjectEncoder.h
)

# Create include directory
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scheme)

# Create the enhanced cognitive patterns library with APRFE
add_library(cognitive-patterns SHARED ${PATTERN_SOURCES})
ADD_DEPENDENCIES(cognitive-patterns atombase atomcore atomspace value cogutil truthvalue)


# Link with parent modules (when they're available)
# target_link_libraries(cognitive-patterns cogutil atomspace)

# Install headers
install(FILES ${PATTERN_HEADERS} DESTINATION include/opencog/cognitive-patterns)

# Install library
install(TARGETS cognitive-patterns DESTINATION lib)

# Install Scheme files
install(DIRECTORY scheme/ DESTINATION share/opencog/scm/cognitive-patterns/)