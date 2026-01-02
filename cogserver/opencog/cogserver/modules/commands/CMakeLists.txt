
ADD_LIBRARY (builtinreqs MODULE
	BuiltinRequestsModule.cc
	ModuleManagement.cc
	ShutdownRequest.cc
)
ADD_DEPENDENCIES(builtinreqs atomspace cogutil network server)


TARGET_LINK_LIBRARIES(builtinreqs
	server
)

INSTALL (TARGETS builtinreqs
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/modules")

# ------------------------------------------------------------
