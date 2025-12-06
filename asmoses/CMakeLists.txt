#
# ASMoses CMake file - AtomSpace MOSES Integration
#
# Week 15: asmoses Integration - AtomSpace evolutionary learning with ure dependency
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(asmoses)

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

# ASMoses core source files
file(GLOB_RECURSE ASMOSES_SOURCES
    "opencog/asmoses/*.cc"
    "opencog/asmoses/*.cpp"
)

file(GLOB_RECURSE ASMOSES_HEADERS
    "opencog/asmoses/*.h"
    "opencog/asmoses/*.hpp"
)

# Create asmoses library
if(ASMOSES_SOURCES)
    add_library(asmoses SHARED ${ASMOSES_SOURCES})
    
    # Link dependencies
    target_link_libraries(asmoses
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
        ${URE_LIBRARY}
    )
    
    # Set library properties
    set_target_properties(asmoses PROPERTIES
        VERSION ${OPENCOG_VERSION}
        SOVERSION ${OPENCOG_SOVERSION}
    )
    
    # Install library and headers
    install(TARGETS asmoses
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
    
    install(FILES ${ASMOSES_HEADERS}
        DESTINATION include/opencog/asmoses
    )
endif()

# ASMoses executables
file(GLOB ASMOSES_EXECUTABLES "examples/*.cc")
foreach(executable_source ${ASMOSES_EXECUTABLES})
    get_filename_component(executable_name ${executable_source} NAME_WE)
    add_executable(${executable_name} ${executable_source})
    target_link_libraries(${executable_name} asmoses ${COGUTIL_LIBRARY} ${ATOMSPACE_LIBRARY} ${URE_LIBRARY})
    install(TARGETS ${executable_name} DESTINATION bin)
endforeach()

# Add tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    add_subdirectory(tests)
endif()

# Create asmoses-specific target
add_custom_target(asmoses-all
    DEPENDS asmoses
    COMMENT "Building ASMoses (AtomSpace MOSES) component"
)

MESSAGE(STATUS "ASMoses (AtomSpace MOSES) component configured")