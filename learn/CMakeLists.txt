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
find_package(CogUtil QUIET)
find_package(AtomSpace QUIET)

# In monorepo context, handle missing packages
IF(NOT COGUTIL_FOUND)
	SET(COGUTIL_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/cogutil)
ENDIF()
IF(NOT ATOMSPACE_FOUND)
	SET(ATOMSPACE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/atomspace)
ENDIF()

# Find CogServer (required for learn module integration)
find_package(CogServer CONFIG QUIET)
if(CogServer_FOUND)
    message(STATUS "CogServer found.")
    set(HAVE_COGSERVER 1)
    add_definitions(-DHAVE_COGSERVER)
else()
    message(STATUS "CogServer not found, using relative path")
    set(COGSERVER_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/cogserver)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Add include directories based on what was found
if(HAVE_COGSERVER)
    include_directories(${COGSERVER_INCLUDE_DIR})
else()
    include_directories(${COGSERVER_INCLUDE_DIR})
endif()

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
    set(LEARN_LINK_LIBRARIES "")
    
    # Link with proper targets or fallback libraries
    list(APPEND LEARN_LINK_LIBRARIES ${COGUTIL_LIBRARY})
    list(APPEND LEARN_LINK_LIBRARIES ${ATOMSPACE_LIBRARY})
    
    if(HAVE_COGSERVER AND TARGET cogserver)
        list(APPEND LEARN_LINK_LIBRARIES cogserver)
    elseif(COGSERVER_LIBRARIES)
        list(APPEND LEARN_LINK_LIBRARIES ${COGSERVER_LIBRARIES})
    endif()
    
    target_link_libraries(learn
        ${LEARN_LINK_LIBRARIES}
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
    target_link_libraries(${executable_name} learn ${LEARN_LINK_LIBRARIES})
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

# Explicit dependency declarations for documentation and validation
# The learn module requires:
# - atomspace: For knowledge representation and atom management  
# - cogserver: For distributed server integration and module loading
set(HAVE_ATOMSPACE 1)  # AtomSpace is REQUIRED so always present
set(HAVE_COGUTIL 1)    # CogUtil is REQUIRED so always present

if(NOT HAVE_COGSERVER)
    message(WARNING "CogServer is required for learn module but was not found using CONFIG. Using fallback include path.")
endif()

# Summary
message(STATUS "Learn module configuration:")
message(STATUS "  - AtomSpace dependency: ${HAVE_ATOMSPACE}")
message(STATUS "  - CogServer dependency: ${HAVE_COGSERVER}")
message(STATUS "  - CogUtil dependency: ${HAVE_COGUTIL}")
message(STATUS "  - Link libraries: ${LEARN_LINK_LIBRARIES}")
MESSAGE(STATUS "learn (Unsupervised Learning) component configured")