# Evolutionary Optimization Examples CMakeLists.txt

cmake_minimum_required(VERSION 3.10)

# Example executables
add_executable(evolutionary_demo evolutionary_demo.cc)
target_link_libraries(evolutionary_demo evolutionary-optimization meta-cognition Threads::Threads)

# Installation
install(TARGETS evolutionary_demo DESTINATION bin/examples)

# Set up proper include paths
target_include_directories(evolutionary_demo PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../../meta-cognition/include
)