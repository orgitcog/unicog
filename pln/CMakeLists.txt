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
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/opencog
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
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
    
    # Link dependencies
    target_link_libraries(pln
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
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
    target_link_libraries(${executable_name} pln ${COGUTIL_LIBRARY} ${ATOMSPACE_LIBRARY})
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