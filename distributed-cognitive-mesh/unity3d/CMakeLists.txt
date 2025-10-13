# Unity3D Cognitive Integration

# Unity3D interface headers and sources
set(UNITY3D_SOURCES
    Unity3DCognitiveInterface.cc
    Unity3DDataBridge.cc
    Unity3DTensorStreaming.cc
)

set(UNITY3D_HEADERS
    Unity3DCognitiveInterface.h
    Unity3DDataBridge.h
    Unity3DTensorStreaming.h
)

# Create library
add_library(unity3d-cognitive SHARED ${UNITY3D_SOURCES})

# Link dependencies
target_link_libraries(unity3d-cognitive
    cognitive-mesh-api
    ${Boost_LIBRARIES}
)

# Install
install(TARGETS unity3d-cognitive
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(FILES ${UNITY3D_HEADERS}
    DESTINATION include/distributed-cognitive-mesh/unity3d
)

# Install Unity3D scripts and C# components
install(DIRECTORY scripts/ csharp/
    DESTINATION share/distributed-cognitive-mesh/unity3d/
)