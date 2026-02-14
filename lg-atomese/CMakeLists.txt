#
# lg-atomese CMake file - Link Grammar to AtomSpace conversion
#
# Week 17: lg-atomese Integration - Link Grammar parsing with atomspace dependency
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(lg-atomese)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required dependencies
find_package(CogUtil QUIET)

# In monorepo context, handle missing CogUtil
IF(NOT COGUTIL_FOUND)
	SET(COGUTIL_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/cogutil)
ENDIF()

# AtomSpace is a critical dependency for lg-atomese
# In monorepo context, AtomSpace is available via parent build
find_package(AtomSpace 5.0.3 CONFIG QUIET)
if(AtomSpace_FOUND)
    message(STATUS "AtomSpace found.")
    add_definitions(-DHAVE_ATOMSPACE)
    set(HAVE_ATOMSPACE 1)
else()
    # In monorepo context, set AtomSpace variables manually
    message(STATUS "AtomSpace not found as package, using monorepo structure")
    set(ATOMSPACE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/atomspace)
    set(HAVE_ATOMSPACE 1)
    add_definitions(-DHAVE_ATOMSPACE)
    include_directories(${ATOMSPACE_INCLUDE_DIR})
endif()

# Try to find Link Grammar
find_package(PkgConfig)
if(PKG_CONFIG_FOUND)
    pkg_check_modules(LINK_GRAMMAR link-grammar)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

if(LINK_GRAMMAR_FOUND)
    include_directories(${LINK_GRAMMAR_INCLUDE_DIRS})
endif()

# lg-atomese core source files
file(GLOB_RECURSE LG_ATOMESE_SOURCES
    "opencog/lg-atomese/*.cc"
    "opencog/lg-atomese/*.cpp"
)

file(GLOB_RECURSE LG_ATOMESE_HEADERS
    "opencog/lg-atomese/*.h"
    "opencog/lg-atomese/*.hpp"
)

# Create lg-atomese library - only if we have AtomSpace and sources
if(LG_ATOMESE_SOURCES AND HAVE_ATOMSPACE)
    add_library(lg-atomese SHARED ${LG_ATOMESE_SOURCES})
ADD_DEPENDENCIES(lg-atomese atombase atomcore atomspace value cogutil truthvalue)

    
    # Link dependencies
    target_link_libraries(lg-atomese
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
    )
    
    if(LINK_GRAMMAR_FOUND)
        target_link_libraries(lg-atomese ${LINK_GRAMMAR_LIBRARIES})
    endif()
    
    # Set library properties
    set_target_properties(lg-atomese PROPERTIES
        VERSION ${OPENCOG_VERSION}
        SOVERSION ${OPENCOG_SOVERSION}
    )
    
    # Install library and headers
    install(TARGETS lg-atomese
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
    
    install(FILES ${LG_ATOMESE_HEADERS}
        DESTINATION include/opencog/lg-atomese
    )
endif()

# lg-atomese executables
file(GLOB LG_ATOMESE_EXECUTABLES "examples/*.cc")
foreach(executable_source ${LG_ATOMESE_EXECUTABLES})
    get_filename_component(executable_name ${executable_source} NAME_WE)
    add_executable(${executable_name} ${executable_source})
    target_link_libraries(${executable_name} lg-atomese ${COGUTIL_LIBRARY} ${ATOMSPACE_LIBRARY})
    install(TARGETS ${executable_name} DESTINATION bin)
endforeach()

# Add tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    add_subdirectory(tests)
endif()

# Create lg-atomese-specific target
add_custom_target(lg-atomese-all
    DEPENDS lg-atomese
    COMMENT "Building lg-atomese (Link Grammar to AtomSpace) component"
)

# Print configuration summary
if(HAVE_ATOMSPACE)
    MESSAGE(STATUS "lg-atomese (Link Grammar to AtomSpace) component configured")
    MESSAGE(STATUS "  AtomSpace dependency: SATISFIED")
    MESSAGE(STATUS "  lg-atomese requires: atomspace")
else()
    MESSAGE(WARNING "lg-atomese component NOT configured - missing required AtomSpace dependency")
endif()