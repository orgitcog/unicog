cmake_minimum_required(VERSION 3.10)
project(GGML_Tensor_Kernels)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Source files
set(TENSOR_SOURCES
    fold_rules.cc
    ann_rules.cc
)

# Header files
set(TENSOR_HEADERS
    fold_rules.h
    ann_rules.h
)

# Build static library
add_library(ggml_tensor_kernels STATIC ${TENSOR_SOURCES})

# Include directories
target_include_directories(ggml_tensor_kernels PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Test executable
add_executable(test_tensor_rules test_tensor_rules.cc)
target_link_libraries(test_tensor_rules ggml_tensor_kernels)

# Install targets
install(TARGETS ggml_tensor_kernels
        ARCHIVE DESTINATION lib)

install(FILES ${TENSOR_HEADERS}
        DESTINATION include/GGML/Tensor)

# Enable testing
enable_testing()
add_test(NAME TensorRulesTest COMMAND test_tensor_rules)
