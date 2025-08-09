#
# OpenCog Main Integration CMake file
#
# Week 19: opencog Final Integration - Complete system integration
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(opencog-main)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required dependencies
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# OpenCog main integration source files
file(GLOB_RECURSE OPENCOG_MAIN_SOURCES
    "opencog/main/*.cc"
    "opencog/main/*.cpp"
)

file(GLOB_RECURSE OPENCOG_MAIN_HEADERS
    "opencog/main/*.h"
    "opencog/main/*.hpp"
)

# Create opencog-main library
if(OPENCOG_MAIN_SOURCES)
    add_library(opencog-main SHARED ${OPENCOG_MAIN_SOURCES})
    
    # Link dependencies
    target_link_libraries(opencog-main
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
    )
    
    # Set library properties
    set_target_properties(opencog-main PROPERTIES
        VERSION ${OPENCOG_VERSION}
        SOVERSION ${OPENCOG_SOVERSION}
    )
    
    # Install library and headers
    install(TARGETS opencog-main
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
    
    install(FILES ${OPENCOG_MAIN_HEADERS}
        DESTINATION include/opencog/main
    )
endif()

# OpenCog main executables
file(GLOB OPENCOG_MAIN_EXECUTABLES "examples/*.cc")
foreach(executable_source ${OPENCOG_MAIN_EXECUTABLES})
    get_filename_component(executable_name ${executable_source} NAME_WE)
    add_executable(${executable_name} ${executable_source})
    target_link_libraries(${executable_name} opencog-main ${COGUTIL_LIBRARY} ${ATOMSPACE_LIBRARY})
    install(TARGETS ${executable_name} DESTINATION bin)
endforeach()

# Add tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    add_subdirectory(tests)
endif()

# Create opencog-main-specific target
add_custom_target(opencog-main-all
    DEPENDS opencog-main
    COMMENT "Building OpenCog main integration component"
)

MESSAGE(STATUS "OpenCog main integration component configured")