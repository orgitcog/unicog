# Neural-Symbolic Integration Component
# Part of OpenCog Unified Phase III Development

cmake_minimum_required(VERSION 3.10)

project(NeuralSymbolicIntegration)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(include)

# Source files
set(SOURCES
    src/NeuralSymbolicBridge.cc
)

# Headers
set(HEADERS
    include/NeuralSymbolicBridge.h
)

# Create library
add_library(neural_symbolic_integration STATIC ${SOURCES} ${HEADERS})

# Set target properties
target_include_directories(neural_symbolic_integration PUBLIC include)

# Install targets
install(TARGETS neural_symbolic_integration
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin)

install(FILES ${HEADERS} DESTINATION include/opencog/neural-symbolic)

# Example/Test executable (optional)
add_executable(neural_symbolic_demo demo/neural_symbolic_demo.cc)
target_link_libraries(neural_symbolic_demo neural_symbolic_integration)

# Print configuration
message(STATUS "Neural-Symbolic Integration component configured")
message(STATUS "  - C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "  - Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  - Sources: ${SOURCES}")