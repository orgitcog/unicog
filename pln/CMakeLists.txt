#
# PLN (Probabilistic Logic Networks) CMake file
#
# Week 13: PLN Integration - Probabilistic reasoning with ure and spacetime dependencies
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(pln)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required dependencies
# Use find_path instead of find_package for unified build
find_path(COGUTIL_INCLUDE_DIR opencog/util/Config.h)
if(NOT COGUTIL_INCLUDE_DIR)
    message(STATUS "CogUtil headers not found, using relative path")
    set(COGUTIL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../cogutil)
endif()

find_path(ATOMSPACE_INCLUDE_DIR opencog/atomspace/AtomSpace.h)
if(NOT ATOMSPACE_INCLUDE_DIR)
    message(STATUS "AtomSpace headers not found, using relative path")
    set(ATOMSPACE_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../atomspace)
endif()

# Find URE (Unified Rule Engine) - required for PLN inference
find_path(URE_INCLUDE_DIR opencog/ure/Rule.h)
if(NOT URE_INCLUDE_DIR)
    message(STATUS "URE headers not found, using relative path")
    set(URE_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../ure)
endif()

# Find spacetime - required for spatial-temporal reasoning
find_path(SPACETIME_INCLUDE_DIR opencog/spacetime/SpaceTimeAtom.h)
if(NOT SPACETIME_INCLUDE_DIR)
    message(STATUS "SpaceTime headers not found, using relative path")
    set(SPACETIME_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../spacetime)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${URE_INCLUDE_DIR}
    ${SPACETIME_INCLUDE_DIR}
)

# PLN core source files
file(GLOB_RECURSE PLN_SOURCES
    "opencog/pln/*.cc"
    "opencog/pln/*.cpp"
)

file(GLOB_RECURSE PLN_HEADERS
    "opencog/pln/*.h" 
    "opencog/pln/*.hpp"
)

# Create PLN library
if(PLN_SOURCES)
    add_library(pln SHARED ${PLN_SOURCES})
ADD_DEPENDENCIES(pln atombase atomcore atomspace atomvalue cogutil spacetime truthvalue unify ure)

    
    # Link dependencies - using target names for unified build
    target_link_libraries(pln
        cogutil
        atomspace
        ure
        spacetime
    )
    
    # Set library properties
    set_target_properties(pln PROPERTIES
        VERSION ${OPENCOG_VERSION}
        SOVERSION ${OPENCOG_SOVERSION}
    )
    
    # Install library and headers
    install(TARGETS pln
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
    
    install(FILES ${PLN_HEADERS}
        DESTINATION include/opencog/pln
    )
endif()

# PLN executables
file(GLOB PLN_EXECUTABLES "examples/*.cc")
foreach(executable_source ${PLN_EXECUTABLES})
    get_filename_component(executable_name ${executable_source} NAME_WE)
    add_executable(${executable_name} ${executable_source})
    target_link_libraries(${executable_name} pln cogutil atomspace ure spacetime)
    install(TARGETS ${executable_name} DESTINATION bin)
endforeach()

# Add tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    add_subdirectory(tests)
endif()

# Create PLN-specific target
add_custom_target(pln-all
    DEPENDS pln
    COMMENT "Building PLN (Probabilistic Logic Networks) component"
)

MESSAGE(STATUS "PLN (Probabilistic Logic Networks) component configured")