#
# Build the RocksDB driver for a single AtomSpace
#

ADD_LIBRARY (persist-monospace SHARED
	MonoIO.cc
	MonoStorage.cc
	MonoPersistSCM.cc
)

ADD_DEPENDENCIES(persist-monospace atombase atomcore atomspace atomvalue cogutil truthvalue)

TARGET_LINK_LIBRARIES(persist-monospace
	${ATOMSPACE_STORAGE_LIBRARIES}
	${ATOMSPACE_LIBRARIES}
	rocksdb
)

ADD_GUILE_EXTENSION(SCM_CONFIG_ROCKS persist-monospace
	"opencog-ext-path-persist-mono")

# The EXPORT is need to autogenerate CMake boilerplate in the lib
# directory that lets other packages FIND_PACKAGE(AtomSpaceMonoSpace)
INSTALL (TARGETS persist-monospace
	EXPORT AtomSpaceMonoSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
   MonoStorage.h
   DESTINATION "include/opencog/persist/monospace"
)
