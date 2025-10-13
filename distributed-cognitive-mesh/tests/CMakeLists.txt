# Distributed Cognitive Mesh Tests

# Test sources
set(TEST_SOURCES
    integration/test_cognitive_mesh_api.cc
    integration/test_embodiment_tensor.cc
    integration/test_unity3d_integration.cc
    integration/test_ros_integration.cc
    integration/test_websocket_streams.cc
    unit/test_tensor_signature.cc
    unit/test_feedback_loop.cc
    performance/test_api_performance.cc
)

# Find test framework
find_package(Boost REQUIRED COMPONENTS unit_test_framework)

# Create test executables
foreach(TEST_SOURCE ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_SOURCE})
    
    target_link_libraries(${TEST_NAME}
        distributed-cognitive-mesh
        cognitive-mesh-api
        unity3d-cognitive
        ros-cognitive
        websocket-cognitive
        embodiment-systems
        ${Boost_LIBRARIES}
    )
    
    # Add to test suite
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
endforeach()

# Install tests
install(TARGETS 
    test_cognitive_mesh_api
    test_embodiment_tensor
    test_unity3d_integration
    test_ros_integration
    test_websocket_streams
    test_tensor_signature
    test_feedback_loop
    test_api_performance
    RUNTIME DESTINATION bin/tests
)