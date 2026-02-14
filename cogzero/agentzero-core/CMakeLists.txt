# Agent-Zero Core Module CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

project(AgentZeroCore
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Core Orchestration Engine for OpenCog Integration"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required OpenCog packages
# When part of monorepo, parent will have set these variables
if(DEFINED COGZERO_COGUTIL_TARGET)
    # Monorepo build - use parent-provided targets
    set(COGUTIL_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo cogutil target")
else()
    # Standalone build - use pkg-config
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(COGUTIL REQUIRED cogutil)
    if (COGUTIL_FOUND)
        message(STATUS "Found cogutil version ${COGUTIL_VERSION}")
        include_directories(${COGUTIL_INCLUDE_DIRS})
        link_directories(${COGUTIL_LIBRARY_DIRS})
    endif()
endif()

if(DEFINED COGZERO_ATOMSPACE_TARGET)
    set(ATOMSPACE_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo atomspace target")
else()
    pkg_check_modules(ATOMSPACE REQUIRED atomspace)
    if (ATOMSPACE_FOUND)
        message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
        include_directories(${ATOMSPACE_INCLUDE_DIRS})
        link_directories(${ATOMSPACE_LIBRARY_DIRS})
    endif()
endif()

if(DEFINED COGZERO_COGSERVER_TARGET)
    set(COGSERVER_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo cogserver target")
else()
    if(NOT DEFINED PkgConfig_FOUND)
        find_package(PkgConfig REQUIRED)
    endif()
    pkg_check_modules(COGSERVER cogserver)
    if (COGSERVER_FOUND)
        message(STATUS "Found cogserver version ${COGSERVER_VERSION}")
        include_directories(${COGSERVER_INCLUDE_DIRS})
        link_directories(${COGSERVER_LIBRARY_DIRS})
    else()
        message(STATUS "CogServer not found - some features will be disabled")
        set(COGSERVER_FOUND FALSE)
    endif()
endif()

# Optional dependencies - check monorepo first
if(DEFINED COGZERO_ATTENTION_TARGET)
    set(ATTENTION_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo attention target")
else()
    if(NOT DEFINED PkgConfig_FOUND)
        find_package(PkgConfig REQUIRED)
    endif()
    pkg_check_modules(ATTENTION attention)
    if (ATTENTION_FOUND)
        message(STATUS "Found attention version ${ATTENTION_VERSION}")
        include_directories(${ATTENTION_INCLUDE_DIRS})
        link_directories(${ATTENTION_LIBRARY_DIRS})
    else()
        message(STATUS "Attention module not found via pkg-config, looking for system includes")
        # Look for attention headers in system paths
        find_path(ATTENTION_INCLUDE_PATH
            NAMES opencog/attentionbank/bank/AttentionBank.h
            PATHS /usr/local/include /usr/include
            PATH_SUFFIXES opencog
        )
        if(ATTENTION_INCLUDE_PATH)
            message(STATUS "Found attention headers at ${ATTENTION_INCLUDE_PATH}")
            include_directories(${ATTENTION_INCLUDE_PATH})
            set(ATTENTION_FOUND TRUE)
        endif()
    endif()
endif()

if(DEFINED COGZERO_PLN_TARGET)
    set(PLN_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo pln target")
else()
    if(NOT DEFINED PkgConfig_FOUND)
        find_package(PkgConfig REQUIRED)
    endif()
    pkg_check_modules(PLN pln)
    if (PLN_FOUND)
        message(STATUS "Found PLN version ${PLN_VERSION}")
        include_directories(${PLN_INCLUDE_DIRS})
        link_directories(${PLN_LIBRARY_DIRS})
    else()
        message(STATUS "PLN module not found via pkg-config, looking for system includes")
        find_path(PLN_INCLUDE_PATH
            NAMES opencog/pln/PLN.h
            PATHS /usr/local/include /usr/include
            PATH_SUFFIXES opencog
        )
        if(PLN_INCLUDE_PATH)
            message(STATUS "Found PLN headers at ${PLN_INCLUDE_PATH}")
            include_directories(${PLN_INCLUDE_PATH})
            set(PLN_FOUND TRUE)
        endif()
    endif()
endif()

if(DEFINED COGZERO_URE_TARGET)
    set(URE_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo ure target")
else()
    if(NOT DEFINED PkgConfig_FOUND)
        find_package(PkgConfig REQUIRED)
    endif()
    pkg_check_modules(URE ure)
    if (URE_FOUND)
        message(STATUS "Found URE version ${URE_VERSION}")
        include_directories(${URE_INCLUDE_DIRS})
        link_directories(${URE_LIBRARY_DIRS})
    else()
        message(STATUS "URE module not found via pkg-config, looking for system includes")
        find_path(URE_INCLUDE_PATH
            NAMES opencog/ure/Rule.h
            PATHS /usr/local/include /usr/include
            PATH_SUFFIXES opencog
        )
        if(URE_INCLUDE_PATH)
            message(STATUS "Found URE headers at ${URE_INCLUDE_PATH}")
            include_directories(${URE_INCLUDE_PATH})
            set(URE_FOUND TRUE)
        endif()
    endif()
endif()

if(DEFINED COGZERO_MINER_TARGET)
    set(MINER_FOUND TRUE)
    message(STATUS "AgentZero-Core: Using monorepo miner target")
else()
    if(NOT DEFINED PkgConfig_FOUND)
        find_package(PkgConfig REQUIRED)
    endif()
    pkg_check_modules(MINER miner)
    if (MINER_FOUND)
        message(STATUS "Found miner version ${MINER_VERSION}")
        include_directories(${MINER_INCLUDE_DIRS})
        link_directories(${MINER_LIBRARY_DIRS})
    else()
        message(STATUS "Miner module not found - pattern discovery features may be limited")
        set(MINER_FOUND FALSE)
    endif()
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Source files
set(AGENTZERO_CORE_SOURCES
    src/AgentZeroCore.cpp
    src/CognitiveLoop.cpp  
    src/TaskManager.cpp
    src/KnowledgeIntegrator.cpp
    src/ReasoningEngine.cpp
    src/ActionExecutor.cpp
    src/ActionScheduler.cpp
    src/MetaPlanner.cpp
    src/SelfModification.cpp
)

# Header files
set(AGENTZERO_CORE_HEADERS
    include/opencog/agentzero/AgentZeroCore.h
    include/opencog/agentzero/CognitiveLoop.h
    include/opencog/agentzero/TaskManager.h
    include/opencog/agentzero/KnowledgeIntegrator.h
    include/opencog/agentzero/ReasoningEngine.h
    include/opencog/agentzero/ActionExecutor.h
    include/opencog/agentzero/ActionScheduler.h
    include/opencog/agentzero/MetaPlanner.h
    include/opencog/agentzero/SelfModification.h
)

# Create library (when source files exist)
if(AGENTZERO_CORE_SOURCES)
    add_library(agentzero-core SHARED
        ${AGENTZERO_CORE_SOURCES}
        ${AGENTZERO_CORE_HEADERS}
    )

    # Link required dependencies (use monorepo targets if available)
    if(DEFINED COGZERO_COGUTIL_TARGET)
        target_link_libraries(agentzero-core ${COGZERO_COGUTIL_TARGET})
    else()
        target_link_libraries(agentzero-core ${COGUTIL_LIBRARIES})
    endif()
    
    if(DEFINED COGZERO_ATOMSPACE_TARGET)
        target_link_libraries(agentzero-core ${COGZERO_ATOMSPACE_TARGET})
    else()
        target_link_libraries(agentzero-core ${ATOMSPACE_LIBRARIES})
    endif()
    
    target_link_libraries(agentzero-core ${Boost_LIBRARIES})
    
    # Link cogserver if available
    if(COGSERVER_FOUND)
        if(DEFINED COGZERO_COGSERVER_TARGET)
            target_link_libraries(agentzero-core ${COGZERO_COGSERVER_TARGET})
        elseif(COGSERVER_LIBRARIES)
            target_link_libraries(agentzero-core ${COGSERVER_LIBRARIES})
        endif()
        target_compile_definitions(agentzero-core PRIVATE HAVE_COGSERVER=1)
        message(STATUS "AgentZero-Core: Linking cogserver")
    endif()
    
    # Link attention if available
    if(ATTENTION_FOUND)
        if(DEFINED COGZERO_ATTENTION_TARGET)
            target_link_libraries(agentzero-core ${COGZERO_ATTENTION_TARGET})
        elseif(ATTENTION_LIBRARIES)
            target_link_libraries(agentzero-core ${ATTENTION_LIBRARIES})
        endif()
        target_compile_definitions(agentzero-core PRIVATE HAVE_ATTENTION_BANK=1)
        message(STATUS "AgentZero-Core: Linking attention")
    endif()
    
    # Link PLN if available
    if(PLN_FOUND)
        if(DEFINED COGZERO_PLN_TARGET)
            target_link_libraries(agentzero-core ${COGZERO_PLN_TARGET})
        elseif(PLN_LIBRARIES)
            target_link_libraries(agentzero-core ${PLN_LIBRARIES})
        endif()
        target_compile_definitions(agentzero-core PRIVATE HAVE_PLN=1)
        message(STATUS "AgentZero-Core: Linking PLN")
    endif()
    
    # Link URE if available
    if(URE_FOUND)
        if(DEFINED COGZERO_URE_TARGET)
            target_link_libraries(agentzero-core ${COGZERO_URE_TARGET})
        elseif(URE_LIBRARIES)
            target_link_libraries(agentzero-core ${URE_LIBRARIES})
        endif()
        target_compile_definitions(agentzero-core PRIVATE HAVE_URE=1)
        message(STATUS "AgentZero-Core: Linking URE")
    endif()
    
    # Link Miner if available
    if(MINER_FOUND)
        if(DEFINED COGZERO_MINER_TARGET)
            target_link_libraries(agentzero-core ${COGZERO_MINER_TARGET})
        elseif(MINER_LIBRARIES)
            target_link_libraries(agentzero-core ${MINER_LIBRARIES})
        endif()
        target_compile_definitions(agentzero-core PRIVATE HAVE_MINER=1)
        message(STATUS "AgentZero-Core: Linking miner")
    endif()

    target_include_directories(agentzero-core PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    )

    # Installation
    install(TARGETS agentzero-core
        EXPORT AgentZeroCppTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )

    install(DIRECTORY include/
        DESTINATION include
        FILES_MATCHING PATTERN "*.h"
    )
endif()

# Tests (when implemented)
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Status message
message(STATUS "Agent-Zero Core module configuration complete")
message(STATUS "Source files implemented - ready for build and testing")