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

# Check if source files exist
set(ROS_SOURCES_EXIST TRUE)
foreach(SOURCE_FILE ${ROS_SOURCES})
    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${SOURCE_FILE}")
        set(ROS_SOURCES_EXIST FALSE)
        break()
    endif()
endforeach()

# Only create library if sources exist
if(ROS_SOURCES_EXIST)
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
else()
    message(STATUS "ROS cognitive sources not found - skipping ros-cognitive library build")
endif()

# Install ROS configuration files
install(DIRECTORY launch/ msg/ srv/ cfg/
    DESTINATION share/distributed-cognitive-mesh/ros/
)