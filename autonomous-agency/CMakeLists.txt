CMAKE_MINIMUM_REQUIRED(VERSION 3.10)

# Autonomous Agency Module Configuration
PROJECT(AutonomousAgency)

# In a monorepo, OpenCog components are available via parent CMake
# Find required OpenCog components (optional for monorepo builds)
FIND_PACKAGE(OpenCog QUIET)

# Include OpenCog directories if found, otherwise use parent scope
IF(OpenCog_FOUND)
    INCLUDE_DIRECTORIES(
        ${OPENCOG_INCLUDE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}
    )
ELSE()
    # In monorepo context, use parent-level includes
    INCLUDE_DIRECTORIES(
        ${CMAKE_SOURCE_DIR}/atomspace/opencog
        ${CMAKE_SOURCE_DIR}/cogutil/opencog
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}
    )
ENDIF()

# Set C++ standard
SET(CMAKE_CXX_STANDARD 17)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# Compiler flags for autonomous agency components
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -O3")

# Bootstrap Module
SET(BOOTSTRAP_SOURCES
    bootstrap/entropic-drift-detector.cpp
    bootstrap/self-healing-atomspace.cpp
    bootstrap/bootstrap-resource-manager.cpp
)

SET(BOOTSTRAP_HEADERS
    bootstrap/entropic-drift-detector.hpp
    bootstrap/self-healing-atomspace.hpp
    bootstrap/bootstrap-resource-manager.hpp
)

# Agentic Loops Module (to be implemented)
SET(AGENTIC_LOOPS_SOURCES
    # Will be added in Stage 2
)

# Virtual Engine Module (to be implemented)
SET(VIRTUAL_ENGINE_SOURCES
    # Will be added in Stage 3
)

# Autopoiesis Module (to be implemented)
SET(AUTOPOIESIS_SOURCES
    # Will be added in Stage 4
)

# Autognosis Module (to be implemented)
SET(AUTOGNOSIS_SOURCES
    # Will be added in Stage 5
)

# Metacycles Module (to be implemented)
SET(METACYCLES_SOURCES
    # Will be added in Stage 6
)

# Autogenesis Module (to be implemented)
SET(AUTOGENESIS_SOURCES
    # Will be added in Stage 7
)

# Create autonomous agency library
ADD_LIBRARY(autonomous-agency STATIC
    ${BOOTSTRAP_SOURCES}
    ${AGENTIC_LOOPS_SOURCES}
    ${VIRTUAL_ENGINE_SOURCES}
    ${AUTOPOIESIS_SOURCES}
    ${AUTOGNOSIS_SOURCES}
    ${METACYCLES_SOURCES}
    ${AUTOGENESIS_SOURCES}
)
ADD_DEPENDENCIES(autonomous-agency atombase atomcore atomspace atomvalue attention cogutil truthvalue)


# Link required libraries
TARGET_LINK_LIBRARIES(autonomous-agency
    ${OPENCOG_LIBRARIES}
    atomspace
    attention
    pthread
)

# Install headers
INSTALL(FILES ${BOOTSTRAP_HEADERS}
    DESTINATION include/opencog/autonomous-agency/bootstrap
)

# Install library
INSTALL(TARGETS autonomous-agency
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Test executables
IF(CXXTEST_FOUND)
    # Bootstrap tests
    ADD_CXXTEST(BootstrapEntropicDriftTest tests/test-entropic-drift-detector.cpp)
    TARGET_LINK_LIBRARIES(BootstrapEntropicDriftTest autonomous-agency ${OPENCOG_LIBRARIES})
    
    ADD_CXXTEST(BootstrapSelfHealingTest tests/test-self-healing-atomspace.cpp)
    TARGET_LINK_LIBRARIES(BootstrapSelfHealingTest autonomous-agency ${OPENCOG_LIBRARIES})
    
    ADD_CXXTEST(BootstrapResourceManagerTest tests/test-bootstrap-resource-manager.cpp)
    TARGET_LINK_LIBRARIES(BootstrapResourceManagerTest autonomous-agency ${OPENCOG_LIBRARIES})
ENDIF(CXXTEST_FOUND)

# Python bindings (if available)
IF(HAVE_CYTHON)
    # Will be added for Python interface
ENDIF(HAVE_CYTHON)

# Documentation
IF(DOXYGEN_FOUND)
    CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/docs/Doxyfile.in
                   ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    ADD_CUSTOM_TARGET(autonomous-agency-doc
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating autonomous agency API documentation with Doxygen" VERBATIM
    )
ENDIF(DOXYGEN_FOUND)

# Enable testing
ENABLE_TESTING()

# Integration tests
ADD_EXECUTABLE(autonomous-agency-integration-test
    tests/integration-test.cpp
)

TARGET_LINK_LIBRARIES(autonomous-agency-integration-test
    autonomous-agency
    ${OPENCOG_LIBRARIES}
)

ADD_TEST(NAME AutonomousAgencyIntegrationTest 
         COMMAND autonomous-agency-integration-test)

# Examples
ADD_EXECUTABLE(bootstrap-example
    examples/bootstrap-example.cpp
)

TARGET_LINK_LIBRARIES(bootstrap-example
    autonomous-agency
    ${OPENCOG_LIBRARIES}
)

# Configuration message
MESSAGE(STATUS "Autonomous Agency Module Configuration:")
MESSAGE(STATUS "  Bootstrap components: ENABLED")
MESSAGE(STATUS "  Agentic loops: PLANNED (Stage 2)")
MESSAGE(STATUS "  Virtual engine: PLANNED (Stage 3)")
MESSAGE(STATUS "  Autopoiesis: PLANNED (Stage 4)")
MESSAGE(STATUS "  Autognosis: PLANNED (Stage 5)")
MESSAGE(STATUS "  Metacycles: PLANNED (Stage 6)")
MESSAGE(STATUS "  Autogenesis: PLANNED (Stage 7)")
MESSAGE(STATUS "  Tests: ${CXXTEST_FOUND}")
MESSAGE(STATUS "  Documentation: ${DOXYGEN_FOUND}")