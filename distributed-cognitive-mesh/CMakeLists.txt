# Phase 4: Distributed Cognitive Mesh API & Embodiment Layer
cmake_minimum_required(VERSION 3.10)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/api
    ${CMAKE_CURRENT_SOURCE_DIR}/embodiment
    ${CMAKE_CURRENT_SOURCE_DIR}/unity3d
    ${CMAKE_CURRENT_SOURCE_DIR}/ros
    ${CMAKE_CURRENT_SOURCE_DIR}/websocket
    ${CMAKE_SOURCE_DIR}/distributed-cognition/include
    ${CMAKE_SOURCE_DIR}/cognitive-das
    ${CMAKE_SOURCE_DIR}/cognitive-visualization/include
)

# API Framework
add_subdirectory(api)

# Embodiment Systems
add_subdirectory(embodiment)

# Unity3D Integration
add_subdirectory(unity3d)

# ROS Integration
add_subdirectory(ros)

# WebSocket Streams
add_subdirectory(websocket)

# Tests
add_subdirectory(tests)

# Phase 4 Main Library
add_library(distributed-cognitive-mesh SHARED
    api/CognitiveMeshAPI.cc
    embodiment/EmbodimentTensorSystem.cc
    unity3d/Unity3DCognitiveInterface.cc
    ros/ROSCognitiveNode.cc
    websocket/CognitiveWebSocketServer.cc
)

# Link dependencies
target_link_libraries(distributed-cognitive-mesh
    distributed-cognition
    cognitive-visualization
    ${Boost_LIBRARIES}
)

# Install targets
install(TARGETS distributed-cognitive-mesh
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

# Install headers
install(DIRECTORY api/ embodiment/ unity3d/ ros/ websocket/
    DESTINATION include/distributed-cognitive-mesh
    FILES_MATCHING PATTERN "*.h"
)