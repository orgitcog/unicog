#
# Learn CMake file - Unsupervised Learning for OpenCog
#
# Week 18: learn Integration - Unsupervised learning with atomspace and cogserver dependencies
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(learn)

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

# Learn core source files
file(GLOB_RECURSE LEARN_SOURCES
    "opencog/learn/*.cc"
    "opencog/learn/*.cpp"
)

file(GLOB_RECURSE LEARN_HEADERS
    "opencog/learn/*.h"
    "opencog/learn/*.hpp"
)

# Create learn library
if(LEARN_SOURCES)
    add_library(learn SHARED ${LEARN_SOURCES})
    
    # Link dependencies
    target_link_libraries(learn
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
    )
    
    # Set library properties
    set_target_properties(learn PROPERTIES
        VERSION ${OPENCOG_VERSION}
        SOVERSION ${OPENCOG_SOVERSION}
    )
    
    # Install library and headers
    install(TARGETS learn
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
    
    install(FILES ${LEARN_HEADERS}
        DESTINATION include/opencog/learn
    )
endif()

# Learn executables
file(GLOB LEARN_EXECUTABLES "examples/*.cc")
foreach(executable_source ${LEARN_EXECUTABLES})
    get_filename_component(executable_name ${executable_source} NAME_WE)
    add_executable(${executable_name} ${executable_source})
    target_link_libraries(${executable_name} learn ${COGUTIL_LIBRARY} ${ATOMSPACE_LIBRARY})
    install(TARGETS ${executable_name} DESTINATION bin)
endforeach()

# Add tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    add_subdirectory(tests)
endif()

# Create learn-specific target
add_custom_target(learn-all
    DEPENDS learn
    COMMENT "Building learn (Unsupervised Learning) component"
)

MESSAGE(STATUS "learn (Unsupervised Learning) component configured")