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

# Install WebSocket client/server examples
install(DIRECTORY client/ server/
    DESTINATION share/distributed-cognitive-mesh/websocket/
)