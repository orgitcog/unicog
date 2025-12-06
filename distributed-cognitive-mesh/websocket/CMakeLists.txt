# WebSocket Real-time Cognitive Streams

# WebSocket headers and sources
set(WEBSOCKET_SOURCES
    CognitiveWebSocketServer.cc
    WebSocketBridge.cc
    RealtimeCognitiveStreams.cc
)

set(WEBSOCKET_HEADERS
    CognitiveWebSocketServer.h
    WebSocketBridge.h
    RealtimeCognitiveStreams.h
)

# Check if source files exist
set(WEBSOCKET_SOURCES_EXIST TRUE)
foreach(SOURCE_FILE ${WEBSOCKET_SOURCES})
    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${SOURCE_FILE}")
        set(WEBSOCKET_SOURCES_EXIST FALSE)
        break()
    endif()
endforeach()

# Only create library if sources exist
if(WEBSOCKET_SOURCES_EXIST)
    # Create library
    add_library(websocket-cognitive SHARED ${WEBSOCKET_SOURCES})

    # Link dependencies
    target_link_libraries(websocket-cognitive
        cognitive-mesh-api
        ${Boost_LIBRARIES}
        pthread
    )

    # Install
    install(TARGETS websocket-cognitive
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
    )

    install(FILES ${WEBSOCKET_HEADERS}
        DESTINATION include/distributed-cognitive-mesh/websocket
    )
else()
    message(STATUS "WebSocket cognitive sources not found - skipping websocket-cognitive library build")
endif()

# Install WebSocket client/server examples
install(DIRECTORY client/ server/
    DESTINATION share/distributed-cognitive-mesh/websocket/
)