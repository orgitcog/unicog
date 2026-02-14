
ADD_LIBRARY (w-thru-proxy SHARED
	ThruCommands.cc
	WriteThruProxy.cc
)
ADD_DEPENDENCIES(w-thru-proxy atombase atomcore atomspace value cogutil network server truthvalue)


TARGET_LINK_LIBRARIES(w-thru-proxy
	network
	server
	${ATOMSPACE_LIBRARY}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS w-thru-proxy
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/modules")

# --------------------------------------

ADD_LIBRARY (r-thru-proxy SHARED
	ReadThruProxy.cc
	ThruCommands.cc
)
ADD_DEPENDENCIES(r-thru-proxy atombase atomcore atomspace value cogutil network server truthvalue)


TARGET_LINK_LIBRARIES(r-thru-proxy
	network
	server
	${ATOMSPACE_LIBRARY}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS r-thru-proxy
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/modules")

# --------------------------------------

INSTALL (FILES
	Proxy.h
	ReadThruProxy.h
	ThruCommands.h
	WriteThruProxy.h
	DESTINATION "include/opencog/cogserver/proxy/"
)
