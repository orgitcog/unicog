cmake_minimum_required(VERSION 3.10)

# Test executable for spacetime system
add_executable(spacetime_tests
    test_spacetime_map.cpp
    test_spatial_reasoning.cpp
    test_temporal_reasoning.cpp
    test_spacetime_integration.cpp
)

# Link with spacetime library and dependencies
target_link_libraries(spacetime_tests 
    spacetime
    atomspace
)

# Include test headers
target_include_directories(spacetime_tests PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../opencog
)

# Add test
enable_testing()
add_test(NAME spacetime_unit_tests COMMAND spacetime_tests)