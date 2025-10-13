# Meta-Cognition Examples CMakeLists.txt

cmake_minimum_required(VERSION 3.10)

# Example executables
add_executable(meta_cognitive_demo meta_cognitive_demo.cc)
target_link_libraries(meta_cognitive_demo meta-cognition Threads::Threads)

# Installation
install(TARGETS meta_cognitive_demo DESTINATION bin/examples)

# Set up proper include paths
target_include_directories(meta_cognitive_demo PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
)