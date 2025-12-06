SET(CMAKE_SHARED_LIBRARY_SUFFIX .so)

ADD_SUBDIRECTORY (modules)

INCLUDE_DIRECTORIES(
	${GUILE_INCLUDE_DIR}
)

# Collect all available source files for the smob library
SET(SMOB_SOURCES
	SchemeEval.cc
	SchemeModule.cc
	SchemePrimitive.cc
	SchemeSmob.cc
	SchemeSmobAtom.cc
	SchemeSmobAS.cc
	SchemeSmobGC.cc
	SchemeSmobNew.cc
	SchemeSmobPrint.cc
	SchemeSmobTV.cc
	SchemeSmobValue.cc
)

# Verify that at least some source files exist before creating the target
SET(EXISTING_SOURCES "")
FOREACH(src_file ${SMOB_SOURCES})
	IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${src_file}")
		LIST(APPEND EXISTING_SOURCES ${src_file})
	ELSE()
		MESSAGE(WARNING "Source file not found: ${src_file}")
	ENDIF()
ENDFOREACH()

IF(NOT EXISTING_SOURCES)
	MESSAGE(FATAL_ERROR "No source files found for smob target in opencog/guile. "
		"Ensure required source files exist or update SMOB_SOURCES list in CMakeLists.txt.")
ENDIF()

ADD_LIBRARY (smob ${EXISTING_SOURCES})

TARGET_LINK_LIBRARIES(smob

	# We want to list every possible library that defines some
	# atom type, so that the library ctors will run, and add those
	# atom types to the classserver. If this is not done, then the
	# classserver will fail to run atom factories, viz, will fail
	# to run the ctors for C++-backed atom types, resulting in
	# screwball failures.
	${NO_AS_NEEDED}
	clearbox
	join
	pattern
	execution
	columnvec
	atomflow
	atomcore
	foreign
	atombase
	truthvalue
	atomspace
	${GUILE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

ADD_GUILE_EXTENSION(ATOMSPACE_SCM_CONFIG smob "opencog-ext-path-smob")

INSTALL (TARGETS smob
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	SchemeEval.h
	SchemeModule.h
	SchemePrimitive.h
	SchemeSmob.h
	DESTINATION "include/opencog/guile"
)
