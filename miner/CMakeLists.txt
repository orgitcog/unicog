#
# Miner CMake file - Pattern Mining for OpenCog
#
# Week 14: miner Integration - Pattern mining algorithms with ure dependency
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(miner)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required dependencies
find_package(CogUtil QUIET)
find_package(AtomSpace QUIET)
find_package(URE QUIET)

# In monorepo context, handle missing packages
IF(NOT COGUTIL_FOUND)
	SET(COGUTIL_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/cogutil)
ENDIF()
IF(NOT ATOMSPACE_FOUND)
	SET(ATOMSPACE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/atomspace)
ENDIF()
IF(NOT URE_FOUND)
	SET(URE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/ure)
ENDIF()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${URE_INCLUDE_DIR}
)

# Miner core source files
file(GLOB_RECURSE MINER_SOURCES
    "opencog/miner/*.cc"
    "opencog/miner/*.cpp"
)

file(GLOB_RECURSE MINER_HEADERS
    "opencog/miner/*.h"
    "opencog/miner/*.hpp"
)

# Create miner library
if(MINER_SOURCES)
    add_library(miner SHARED ${MINER_SOURCES})
    
    # Link dependencies
    target_link_libraries(miner
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
        ${URE_LIBRARY}
    )
    
    # Set library properties
    set_target_properties(miner PROPERTIES
        VERSION ${OPENCOG_VERSION}
        SOVERSION ${OPENCOG_SOVERSION}
    )
    
    # Install library and headers
    install(TARGETS miner
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
    
    install(FILES ${MINER_HEADERS}
        DESTINATION include/opencog/miner
    )
endif()

# Miner executables
file(GLOB MINER_EXECUTABLES "examples/*.cc")
foreach(executable_source ${MINER_EXECUTABLES})
    get_filename_component(executable_name ${executable_source} NAME_WE)
    add_executable(${executable_name} ${executable_source})
    target_link_libraries(${executable_name} miner ${COGUTIL_LIBRARY} ${ATOMSPACE_LIBRARY} ${URE_LIBRARY})
    install(TARGETS ${executable_name} DESTINATION bin)
endforeach()

# Add tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    add_subdirectory(tests)
endif()

# Create miner-specific target
add_custom_target(miner-all
    DEPENDS miner
    COMMENT "Building miner (Pattern Mining) component"
)

MESSAGE(STATUS "Miner (Pattern Mining) component configured")