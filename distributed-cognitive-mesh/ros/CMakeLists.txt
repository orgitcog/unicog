# ROS Cognitive Node Architecture

# ROS interface headers and sources  
set(ROS_SOURCES
    ROSCognitiveNode.cc
    ROSBridge.cc
    ROSSensorProcessor.cc
    ROSMotorController.cc
)

set(ROS_HEADERS
    ROSCognitiveNode.h
    ROSBridge.h
    ROSSensorProcessor.h
    ROSMotorController.h
)

# Create library
add_library(ros-cognitive SHARED ${ROS_SOURCES})

# Link dependencies
target_link_libraries(ros-cognitive
    cognitive-mesh-api
    ${Boost_LIBRARIES}
)

# Install
install(TARGETS ros-cognitive
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib  
)

install(FILES ${ROS_HEADERS}
    DESTINATION include/distributed-cognitive-mesh/ros
)

# Install ROS configuration files
install(DIRECTORY launch/ msg/ srv/ cfg/
    DESTINATION share/distributed-cognitive-mesh/ros/
)