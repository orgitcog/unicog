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

# Find required dependencies - in monorepo context
find_package(CogUtil QUIET)
find_package(AtomSpace QUIET)
find_package(CogServer CONFIG QUIET)
find_package(AttentionModule CONFIG QUIET)
find_package(URE CONFIG QUIET)
find_package(LGAtomese CONFIG QUIET)

# In monorepo context, handle missing packages
IF(NOT COGUTIL_FOUND)
	SET(COGUTIL_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/cogutil)
ENDIF()
IF(NOT ATOMSPACE_FOUND)
	SET(ATOMSPACE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/atomspace)
ENDIF()
IF(NOT CogServer_FOUND)
	SET(COGSERVER_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/cogserver)
	INCLUDE_DIRECTORIES(${COGSERVER_INCLUDE_DIR})
ENDIF()
IF(NOT URE_FOUND)
	SET(URE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/ure)
	INCLUDE_DIRECTORIES(${URE_INCLUDE_DIR})
ENDIF()
IF(NOT AttentionModule_FOUND)
	SET(ATTENTION_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/attention)
	INCLUDE_DIRECTORIES(${ATTENTION_INCLUDE_DIR})
ENDIF()
IF(NOT LGAtomese_FOUND)
	SET(LGATOMESE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/lg-atomese)
	INCLUDE_DIRECTORIES(${LGATOMESE_INCLUDE_DIR})
ENDIF()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Add conditional include directories for optional dependencies
if(CogServer_FOUND)
    include_directories(${COGSERVER_INCLUDE_DIR})
elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../cogserver")
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../cogserver)
endif()

if(AttentionModule_FOUND)
    include_directories(${ATTENTION_INCLUDE_DIR})
elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../attention")
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../attention)
endif()

if(URE_FOUND)
    include_directories(${URE_INCLUDE_DIR})
elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../ure")
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ure)
endif()

if(LGAtomese_FOUND)
    include_directories(${LGATOMESE_INCLUDE_DIR})
elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../lg-atomese")
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../lg-atomese)
endif()

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
    set(OPENCOG_LINK_LIBRARIES
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
    )
    
    # Add conditional dependencies based on availability
    if(TARGET server)
        list(APPEND OPENCOG_LINK_LIBRARIES server)
    elseif(COGSERVER_LIBRARIES)
        list(APPEND OPENCOG_LINK_LIBRARIES ${COGSERVER_LIBRARIES})
    endif()
    
    if(TARGET attention)
        list(APPEND OPENCOG_LINK_LIBRARIES attention)
    elseif(ATTENTION_LIBRARIES)
        list(APPEND OPENCOG_LINK_LIBRARIES ${ATTENTION_LIBRARIES})
    endif()
    
    if(TARGET ure)
        list(APPEND OPENCOG_LINK_LIBRARIES ure)
    elseif(URE_LIBRARIES)
        list(APPEND OPENCOG_LINK_LIBRARIES ${URE_LIBRARIES})
    endif()
    
    if(TARGET lg-atomese)
        list(APPEND OPENCOG_LINK_LIBRARIES lg-atomese)
    elseif(LGATOMESE_LIBRARIES)
        list(APPEND OPENCOG_LINK_LIBRARIES ${LGATOMESE_LIBRARIES})
    endif()
    
    target_link_libraries(opencog-main ${OPENCOG_LINK_LIBRARIES})
    
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
    
    # Create alias for consistency with dependency system
    add_library(opencog ALIAS opencog-main)
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

# Explicit dependency declarations for documentation and validation
# The opencog module requires:
# - atomspace: For knowledge representation and atom management (REQUIRED)
# - cogutil: For core utilities (REQUIRED)
# - cogserver: For distributed server integration and management (OPTIONAL)
# - attention: For ECAN attention allocation system (OPTIONAL)
# - ure: For unified rule engine and reasoning (OPTIONAL)
# - lg-atomese: For Link Grammar parsing integration (OPTIONAL)

MESSAGE(STATUS "OpenCog main integration component configured")
MESSAGE(STATUS "  - Required dependencies: CogUtil, AtomSpace")
MESSAGE(STATUS "  - Optional dependencies: CogServer, Attention, URE, LG-Atomese")
if(OPENCOG_LINK_LIBRARIES)
    MESSAGE(STATUS "  - Link libraries: ${OPENCOG_LINK_LIBRARIES}")
endif()