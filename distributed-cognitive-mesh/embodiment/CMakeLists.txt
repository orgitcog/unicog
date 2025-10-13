# Embodiment Systems

# Embodiment headers and sources
set(EMBODIMENT_SOURCES
    EmbodimentTensorSystem.cc
    SensoryMotorFeedbackLoop.cc
    EmbodimentStateManager.cc
)

set(EMBODIMENT_HEADERS
    EmbodimentTensorSystem.h
    SensoryMotorFeedbackLoop.h
    EmbodimentStateManager.h
)

# Create library
add_library(embodiment-systems SHARED ${EMBODIMENT_SOURCES})

# Link dependencies
target_link_libraries(embodiment-systems
    cognitive-mesh-api
    ${Boost_LIBRARIES}
)

# Install
install(TARGETS embodiment-systems
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(FILES ${EMBODIMENT_HEADERS}
    DESTINATION include/distributed-cognitive-mesh/embodiment
)