# Distributed Cognitive Mesh API

# Find required packages
find_package(Boost REQUIRED COMPONENTS system thread)

# API headers and source
set(API_SOURCES
    CognitiveMeshAPI.cc
    StatePropagatonAPI.cc
    TaskOrchestrationAPI.cc
    CognitiveStateSync.cc
)

set(API_HEADERS
    CognitiveMeshAPI.h
    StatePropagatonAPI.h  
    TaskOrchestrationAPI.h
    CognitiveStateSync.h
    EmbodimentTensorSignature.h
)

# Create library
add_library(cognitive-mesh-api SHARED ${API_SOURCES})

# Link dependencies
target_link_libraries(cognitive-mesh-api
    ${Boost_LIBRARIES}
    pthread
)

# Install
install(TARGETS cognitive-mesh-api
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(FILES ${API_HEADERS}
    DESTINATION include/distributed-cognitive-mesh/api
)