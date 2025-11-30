#
# Master URE CMake file.
#
# General organization:
# -- check for different compilers, OS'es
# -- search for various required & optional libraries/tools
# -- decide what to build based on above results.
# -- configure various config files.
# -- print pretty summary
#
# cogutils already requires 2.8.12.2, so may as well ask for that.
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12.2)
IF (COMMAND CMAKE_POLICY)
	CMAKE_POLICY(SET CMP0003 NEW)
ENDIF (COMMAND CMAKE_POLICY)

IF(CMAKE_VERSION VERSION_GREATER 3.0.2)
	CMAKE_POLICY(SET CMP0037 OLD)
	CMAKE_POLICY(SET CMP0042 NEW)
ENDIF(CMAKE_VERSION VERSION_GREATER 3.0.2)
IF (CMAKE_VERSION VERSION_GREATER 3.9.0)
	CMAKE_POLICY(SET CMP0068 NEW)
ENDIF (CMAKE_VERSION VERSION_GREATER 3.9.0)

PROJECT(ure)

# ----------------------------------------------------------
# User-modifiable options. Feel free to change these!
#
# Uncomment to be in Release mode [default].
# SET(CMAKE_BUILD_TYPE Release)

# Uncomment to build in debug mode.
# SET(CMAKE_BUILD_TYPE Debug)

# Uncomment to be in coverage testing mode.
# SET(CMAKE_BUILD_TYPE Coverage)

# Uncomment to build in profile mode.
# SET(CMAKE_BUILD_TYPE Profile)

# Uncomment to build in release mode with debug information.
# SET(CMAKE_BUILD_TYPE RelWithDebInfo)

# default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
                -DPROJECT_BINARY_DIR="${CMAKE_BINARY_DIR}")

# ===============================================================
# Detect different compilers and OS'es, tweak flags as necessary.

# The default case for non-profile builds is to use shared libraries. So don't
# use explicit SHARED in the ADD_LIBRARY calls in CMakeLists.txt instances or
# this flag won't work since it only affects the default.
IF (CMAKE_BUILD_TYPE STREQUAL "Profile")
	SET(BUILD_SHARED_LIBS OFF)
ELSE (CMAKE_BUILD_TYPE STREQUAL "Profile")
	SET(BUILD_SHARED_LIBS ON)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "Profile")

# ===============================================================
# Check for existance of various required, optional packages.
# Listed in alphabetical order, more or less.
# CogUtil must come first, because it supplies various FindXXX macros.

# Add the 'lib' dir to cmake's module search path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/lib/")

# Cogutil
FIND_PACKAGE(CogUtil CONFIG)
IF (COGUTIL_FOUND)
	MESSAGE(STATUS "CogUtil version ${COGUTIL_VERSION} found.")
	ADD_DEFINITIONS(-DHAVE_COGUTIL)
	SET(HAVE_COGUTIL 1)
ELSE (COGUTIL_FOUND)
	MESSAGE(FATAL_ERROR "CogUtil missing: it is needed!")
ENDIF (COGUTIL_FOUND)

# AtomSpace
# In monorepo context, AtomSpace is available via parent build
FIND_PACKAGE(AtomSpace 5.0.3 CONFIG QUIET)
IF (ATOMSPACE_FOUND)
	MESSAGE(STATUS "AtomSpace found.")
	ADD_DEFINITIONS(-DHAVE_ATOMSPACE)
	SET(HAVE_ATOMSPACE 1)
ELSE ()
	# In monorepo context, set AtomSpace variables manually
	MESSAGE(STATUS "AtomSpace not found as package, using monorepo structure")
	SET(ATOMSPACE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/atomspace)
	SET(HAVE_ATOMSPACE 1)
	ADD_DEFINITIONS(-DHAVE_ATOMSPACE)
	INCLUDE_DIRECTORIES(${ATOMSPACE_INCLUDE_DIR})
ENDIF ()

# Unify
# In monorepo context, Unify is available via parent build
FIND_PACKAGE(Unify CONFIG QUIET)
IF (UNIFY_FOUND)
	MESSAGE(STATUS "Unify version ${UNIFY_VERSION} found.")
	ADD_DEFINITIONS(-DHAVE_UNIFY)
	SET(HAVE_UNIFY 1)
ELSE ()
	# In monorepo context, set Unify variables manually
	MESSAGE(STATUS "Unify not found as package, using monorepo structure")
	SET(UNIFY_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/unify)
	SET(HAVE_UNIFY 1)
	ADD_DEFINITIONS(-DHAVE_UNIFY)
	INCLUDE_DIRECTORIES(${UNIFY_INCLUDE_DIR})
ENDIF ()

# add the 'cmake' directory from cogutil to search path
list(APPEND CMAKE_MODULE_PATH  ${COGUTIL_DATA_DIR}/cmake)
include(OpenCogGccOptions)
include(OpenCogLibOptions)
include(OpenCogInstallOptions)
include(Summary)

# ----------------------------------------------------------
# Check for boost. We need dynamic-linked, threaded libs by default.
SET(Boost_USE_STATIC_LIBS OFF)
SET(Boost_USE_MULTITHREADED ON)
SET(MIN_BOOST 1.46)

# Required boost packages
FIND_PACKAGE(Boost ${MIN_BOOST} REQUIRED)

IF(Boost_FOUND)
	SET(Boost_FOUND_SAVE 1)
ELSE(Boost_FOUND)
	MESSAGE(FATAL_ERROR "Boost ${MIN_BOOST} or newer is needed to build OpenCog!")
ENDIF(Boost_FOUND)

MESSAGE(STATUS "Boost version ${Boost_VERSION} found.")

# Arghhh. Except cmake is treating above as required, not optional. #$%**&
IF(Boost_FOUND_SAVE)
	SET(Boost_FOUND 1)
ENDIF(Boost_FOUND_SAVE)

# ----------------------------------------------------------
# Needed for unit tests.

FIND_PACKAGE(Cxxtest)
IF (CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest found.")
ELSE (CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest missing: needed for unit tests.")
ENDIF (CXXTEST_FOUND)

# ----------------------------------------------------------
# This is required for Guile, Python

include(OpenCogFindGuile)
include(OpenCogFindPython)

# ----------------------------------------------------------
# Optional, currently needed only to hush up DRD in util/Logger.cc
FIND_PACKAGE(VALGRIND)
IF (VALGRIND_FOUND)
	MESSAGE(STATUS "VALGRIND was found.")
	IF (VALGRIND_INCLUDE_DIR)
		MESSAGE(STATUS "VALGRIND devel headers found.")
		ADD_DEFINITIONS(-DHAVE_VALGRIND)
	ELSE (VALGRIND_INCLUDE_DIR)
		MESSAGE(STATUS "VALGRIND devel headers NOT FOUND: needed for thread debugging.")
	ENDIF (VALGRIND_INCLUDE_DIR)
ELSE (VALGRIND_FOUND)
	MESSAGE(STATUS "VALGRIND missing: needed for thread debugging.")
ENDIF (VALGRIND_FOUND)

# ===============================================================
# Get ure version

# NOTE: This is the official semantic-version, as it is derived from
# a version-control independent means of declaring versioning.
#
# NOTE: Policy CMP0048 (project VERSION command) requires CMake >= 3.0.2
# but we currently require only 2.8.12.2 for compatibility reasons.
# When minimum version is updated to 3.0.2+, we can use:
# PROJECT(AtomSpace VERSION ${SEMANTIC_VERSION})
#
# In monorepo context, adjust path to version.h
IF(EXISTS "${CMAKE_SOURCE_DIR}/opencog/ure/version.h")
	FILE(READ "${CMAKE_SOURCE_DIR}/opencog/ure/version.h" _URE_H_CONTENTS)
ELSEIF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/opencog/ure/version.h")
	FILE(READ "${CMAKE_CURRENT_SOURCE_DIR}/opencog/ure/version.h" _URE_H_CONTENTS)
ELSE()
	# Default version if file not found
	SET(_URE_H_CONTENTS "#define URE_VERSION_STRING \"1.0.0\"")
ENDIF()
STRING(REGEX MATCH
	"#define URE_VERSION_STRING \"([0-9]+[.0-9]+[.0-9]+)\""
        _ "${_URE_H_CONTENTS}")
SET(SEMANTIC_VERSION ${CMAKE_MATCH_1})

# ===================================================================
# Global includes

# -------------------------------------------------
# Include configuration.

# Set default include paths.
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR} ${Boost_INCLUDE_DIRS}
	${COGUTIL_INCLUDE_DIR} ${ATOMSPACE_INCLUDE_DIR})

# Macros that define how atom types get declared.
# In monorepo context, use cmake files from atomspace
IF(EXISTS "${ATOMSPACE_DATA_DIR}/cmake/OpenCogMacros.cmake")
	INCLUDE("${ATOMSPACE_DATA_DIR}/cmake/OpenCogMacros.cmake")
	INCLUDE("${ATOMSPACE_DATA_DIR}/cmake/OpenCogGuile.cmake")
	INCLUDE("${ATOMSPACE_DATA_DIR}/cmake/OpenCogCython.cmake")
ELSEIF(EXISTS "${CMAKE_SOURCE_DIR}/atomspace/cmake/OpenCogMacros.cmake")
	INCLUDE("${CMAKE_SOURCE_DIR}/atomspace/cmake/OpenCogMacros.cmake")
	INCLUDE("${CMAKE_SOURCE_DIR}/atomspace/cmake/OpenCogGuile.cmake")
	INCLUDE("${CMAKE_SOURCE_DIR}/atomspace/cmake/OpenCogCython.cmake")
ELSE()
	MESSAGE(STATUS "OpenCog cmake macros not found, some features may be disabled")
ENDIF()

# ==========================================================
# Decide what to build, based on the packages found.

IF(HAVE_UNIFY AND HAVE_ATOMSPACE AND HAVE_COGUTIL)
	SET(HAVE_URE 1)
ENDIF(HAVE_UNIFY AND HAVE_ATOMSPACE AND HAVE_COGUTIL)

ADD_SUBDIRECTORY(opencog)
ADD_SUBDIRECTORY(lib)

IF (CXXTEST_FOUND)
	ADD_CUSTOM_TARGET(tests)
	ADD_SUBDIRECTORY(tests EXCLUDE_FROM_ALL)
	IF (CMAKE_BUILD_TYPE STREQUAL "Coverage")
		# doing coverage stuff while running tests if this is the Coverage build
		# Find lcov for coverage reporting
		FIND_PROGRAM(LCOV_EXECUTABLE lcov)
		FIND_PROGRAM(GENHTML_EXECUTABLE genhtml)
		
		IF(NOT LCOV_EXECUTABLE)
			MESSAGE(WARNING "lcov not found! Coverage reports will not be generated.")
		ENDIF()
		
		# Option to run coverage per test or combined
		OPTION(COVERAGE_PER_TEST "Run coverage analysis per test (slower but more detailed)" OFF)
		
		ADD_CUSTOM_TARGET(covtest
			WORKING_DIRECTORY tests
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
			COMMENT "Running tests with coverage..."
		)
		ADD_CUSTOM_TARGET(check
			DEPENDS covtest
			WORKING_DIRECTORY ./
			# This script combines the coverage analysis of each test,
			# then creates html in tests/lcov
			# COMMAND genhtml -o ../lcov -t "All AtomSpace unit tests" *.info
			COMMAND ${PROJECT_SOURCE_DIR}/scripts/combine_lcov.sh
			COMMENT "Generating lcov report..."
		)
	ELSE (CMAKE_BUILD_TYPE STREQUAL "Coverage")
		# Coverage is disabled; test normally
		ADD_CUSTOM_TARGET(check
			DEPENDS tests
			WORKING_DIRECTORY tests
			# COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process $(ARGS)
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process --output-on-failure $(ARGS)
			COMMENT "Running tests..."
		)
	ENDIF (CMAKE_BUILD_TYPE STREQUAL "Coverage")

	# When CMP00037 is finally turned off, just remove these two lines.
	ADD_CUSTOM_TARGET(test)
	ADD_DEPENDENCIES(test check)

	ADD_CUSTOM_TARGET(test_python
		WORKING_DIRECTORY tests/cython
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Python and Cython tests..."
	)
	ADD_CUSTOM_TARGET(test_guile
		WORKING_DIRECTORY tests/scm
		COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
		COMMENT "Running Guile and Python-bridge tests..."
	)
ENDIF (CXXTEST_FOUND)

IF (NOT WIN32)
	ADD_CUSTOM_TARGET (ure-examples
		# using CMAKE_BUILD_TOOL results in teh cryptic error message
		# warning: jobserver unavailable: using -j1.  Add `+' to parent make rule.
		# This is because make doesn't know how to pass jobserver args to
		# the submake.  So, instead, just use $(MAKE) (with round parens)
		# -- that will do the right thing.
		# COMMAND ${CMAKE_BUILD_TOOL}
		COMMAND $(MAKE)
		WORKING_DIRECTORY examples
		COMMENT "Building URE examples"
	)
ENDIF (NOT WIN32)

ADD_CUSTOM_TARGET(cscope-ure
	COMMAND find opencog examples tests -name '*.cc' -o -name '*.h' -o -name '*.cxxtest' -o -name '*.scm' > ${CMAKE_SOURCE_DIR}/cscope.files
	COMMAND cscope -b
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	COMMENT "Generating CScope database for URE"
)

# ===============================================================
# Packaging
## Architecture the package is for.
## Detect architecture in a cross-platform way
IF(UNIX AND NOT APPLE)
	# Try dpkg first (Debian/Ubuntu)
	EXECUTE_PROCESS(COMMAND dpkg --print-architecture
		OUTPUT_VARIABLE PACKAGE_ARCHITECTURE
		OUTPUT_STRIP_TRAILING_WHITESPACE
		ERROR_QUIET
		RESULT_VARIABLE DPKG_RESULT)
	
	# If dpkg fails, use uname
	IF(NOT DPKG_RESULT EQUAL 0)
		EXECUTE_PROCESS(COMMAND uname -m
			OUTPUT_VARIABLE PACKAGE_ARCHITECTURE
			OUTPUT_STRIP_TRAILING_WHITESPACE)
		# Convert common uname outputs to Debian equivalents
		IF(PACKAGE_ARCHITECTURE STREQUAL "x86_64")
			SET(PACKAGE_ARCHITECTURE "amd64")
		ELSEIF(PACKAGE_ARCHITECTURE STREQUAL "i686")
			SET(PACKAGE_ARCHITECTURE "i386")
		ENDIF()
	ENDIF()
ELSEIF(APPLE)
	SET(PACKAGE_ARCHITECTURE "darwin")
ELSE()
	SET(PACKAGE_ARCHITECTURE "unknown")
ENDIF()
STRING(TIMESTAMP UTC_DATE %Y%m%d UTC)
# If 'sudo make install' is run before 'make package', then install_manifest.txt
# will be owned by root. Creating the file during configuration stage ensures
# that is owned by the builder thus avoiding 'Permission denied' error when
# packaging
FILE(WRITE "${PROJECT_BINARY_DIR}/install_manifest.txt")

## Cpack configuration
SET(CPACK_GENERATOR "DEB")
SET(CPACK_PACKAGE_CONTACT "opencog@googlegroups.com")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
SET(CPACK_PACKAGE_DIRECTORY "${CMAKE_BINARY_DIR}/packages")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Unified Rule Engine")
SET(CPACK_PACKAGE_NAME "ure-dev")
SET(CPACK_PACKAGE_VENDOR "opencog.org")
SET(CPACK_PACKAGE_VERSION "${SEMANTIC_VERSION}-${UTC_DATE}")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
SET(CPACK_PACKAGE_FILE_NAME
	"${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}_${PACKAGE_ARCHITECTURE}")
SET(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local")

## Debian specific configurations
SET(DEPENDENCY_LIST
	"guile-2.2-dev (>= 2.2.2)"
	"python3-dev (>= 3.6.7)"
	"libboost-random-dev (>= ${MIN_BOOST})"
	"libstdc++6 (>= 4.7)"
	"libcogutil-dev (>= 2.0.2)"
	"atomspace-dev (>= 5.0.3)"
)

STRING(REPLACE ";" ", " MAIN_DEPENDENCIES "${DEPENDENCY_LIST}")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "${MAIN_DEPENDENCIES}")
SET(CPACK_DEBIAN_PACKAGE_SECTION "libdevel")
SET(CPACK_DEBIAN_PACKAGE_HOMEPAGE "http://opencog.org")
INCLUDE(CPack)

# ===================================================================
# Show a summary of what we found, what we will do.

SUMMARY_ADD("URE" "Unified Rule Engine" HAVE_URE)
SUMMARY_ADD("Python bindings" "Python (cython) bindings" HAVE_CYTHON)
SUMMARY_ADD("Python tests" "Python bindings nose tests" HAVE_NOSETESTS)
SUMMARY_ADD("Scheme bindings" "Scheme bindings and shell" HAVE_GUILE)
SUMMARY_ADD("Unit tests" "Unit tests" CXXTEST_FOUND)
SUMMARY_SHOW()
