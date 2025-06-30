# Phase II.5: Tutorial and Automation Layer
# Interactive chatbot tutorials and automated testing

cmake_minimum_required(VERSION 3.10)
project(tutorial-automation)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories from parent modules
include_directories(${CMAKE_SOURCE_DIR}/cogutil/opencog)
include_directories(${CMAKE_SOURCE_DIR}/atomspace/opencog)
include_directories(${CMAKE_SOURCE_DIR}/cogserver/opencog)
include_directories(${CMAKE_SOURCE_DIR}/cognitive-patterns/include)
include_directories(${CMAKE_SOURCE_DIR}/distributed-cognition/include)
include_directories(${CMAKE_SOURCE_DIR}/cognitive-visualization/include)

# Source files for tutorial automation
set(TUTORIAL_SOURCES
    src/InteractiveChatbot.cc
    src/NeuralSymbolicTutorial.cc
    src/AutomatedTesting.cc
    src/TutorialContentGenerator.cc
)

# Header files
set(TUTORIAL_HEADERS
    include/InteractiveChatbot.h
    include/NeuralSymbolicTutorial.h
    include/AutomatedTesting.h
    include/TutorialContentGenerator.h
)

# Create include directory
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scheme)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tutorials)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests)

# Create the tutorial automation library
add_library(tutorial-automation SHARED ${TUTORIAL_SOURCES})

# Install headers
install(FILES ${TUTORIAL_HEADERS} DESTINATION include/opencog/tutorial-automation)

# Install library
install(TARGETS tutorial-automation DESTINATION lib)

# Install tutorials and tests
install(DIRECTORY tutorials/ DESTINATION share/opencog/tutorials/)
install(DIRECTORY tests/ DESTINATION share/opencog/tests/)
install(DIRECTORY scheme/ DESTINATION share/opencog/scm/tutorial-automation/)