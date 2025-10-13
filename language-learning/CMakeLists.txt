cmake_minimum_required(VERSION 3.10)
project(language-learning)

# This is a Python-based project that uses setup.py
# We'll create a minimal CMake wrapper to integrate it into the unified build

# Find Python
find_package(Python3 REQUIRED COMPONENTS Interpreter)

# Require CogUtil as a dependency
find_package(CogUtil REQUIRED)
if(NOT COGUTIL_FOUND)
    message(FATAL_ERROR "CogUtil is required but not found. Please install CogUtil first.")
endif()

# Define installation target
add_custom_target(language-learning ALL
    COMMAND ${Python3_EXECUTABLE} setup.py build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building language-learning Python package"
)

# Install target
install(CODE "
    execute_process(
        COMMAND ${Python3_EXECUTABLE} setup.py install --prefix=${CMAKE_INSTALL_PREFIX}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
")

# Add this target to the language processing phase
set_target_properties(language-learning PROPERTIES
    FOLDER "Language Processing"
)