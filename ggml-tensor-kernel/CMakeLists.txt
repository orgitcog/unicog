cmake_minimum_required(VERSION 3.10)
project(ggml-tensor-kernel-minimal)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find or build GGML
option(BUILD_GGML_FROM_SOURCE "Build GGML from source" ON)

if(BUILD_GGML_FROM_SOURCE)
    include(FetchContent)
    FetchContent_Declare(
        ggml
        GIT_REPOSITORY https://github.com/ggerganov/ggml.git
        GIT_TAG        master
        GIT_SHALLOW    TRUE
    )
    FetchContent_MakeAvailable(ggml)
    set(GGML_LIBRARIES ggml)
    set(GGML_INCLUDE_DIRS ${ggml_SOURCE_DIR}/include)
else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GGML REQUIRED ggml)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${GGML_INCLUDE_DIRS}
)

# Add minimal AtomSpace stubs for testing
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/atomspace_stub.h "
#ifndef ATOMSPACE_STUB_H
#define ATOMSPACE_STUB_H
#include <memory>
#include <vector>
#include <unordered_set>
#include <string>

namespace opencog {
    typedef uint64_t Type;
    
    // Atom types for cognitive primitives
    static const Type NODE_TYPE = 1;
    static const Type LINK_TYPE = 2;
    static const Type CONCEPT_NODE = 3;
    static const Type PREDICATE_NODE = 4; 
    static const Type NUMBER_NODE = 5;
    static const Type EVALUATION_LINK = 6;
    static const Type LIST_LINK = 7;
    static const Type INHERITANCE_LINK = 8;
    static const Type SIMILARITY_LINK = 9;
    static const Type AND_LINK = 10;
    static const Type OR_LINK = 11;
    static const Type SET_LINK = 12;
    
    class Handle {
    public:
        static const Handle UNDEFINED;
        Handle() = default;
        Handle(uint64_t id) : uuid_(id) {}
        uint64_t uuid_ = 0;
        bool operator==(const Handle& other) const { return uuid_ == other.uuid_; }
        bool operator!=(const Handle& other) const { return !(*this == other); }
    };
    
    typedef std::unordered_set<Handle> HandleSet;
    
    class AtomSpace {
    public:
        AtomSpace() = default;
        virtual ~AtomSpace() = default;
        
        // Stub methods for cognitive primitive integration
        Handle add_node(Type type, const std::string& name) {
            return Handle(++next_uuid_);
        }
        
        Handle add_link(Type type, const std::vector<Handle>& outgoing) {
            return Handle(++next_uuid_);
        }
        
    private:
        static uint64_t next_uuid_;
    };
}

namespace std {
    template<>
    struct hash<opencog::Handle> {
        size_t operator()(const opencog::Handle& h) const {
            return hash<uint64_t>()(h.uuid_);
        }
    };
}
#endif
")

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/atomspace_stub.cc "
#include \"atomspace_stub.h\"

namespace opencog {
    const Handle Handle::UNDEFINED = Handle(0);
    uint64_t AtomSpace::next_uuid_ = 100;
}
")

include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Create the minimal tensor kernel implementation
add_library(ggml-tensor-kernel-minimal SHARED
    src/TensorKernel_minimal.cc
    src/AtomSpaceTensorMapper_minimal.cc
    src/AttentionAllocator_minimal.cc
    src/CognitivePrimitive.cc
    ${CMAKE_CURRENT_BINARY_DIR}/atomspace_stub.cc
)

target_link_libraries(ggml-tensor-kernel-minimal
    ${GGML_LIBRARIES}
)

# Create executable test
add_executable(test-tensor-kernel-minimal test_tensor_kernel_minimal.cc)
target_link_libraries(test-tensor-kernel-minimal ggml-tensor-kernel-minimal ${GGML_LIBRARIES})

# Create cognitive primitive test
add_executable(test-cognitive-primitive test_cognitive_primitive_implementation.cc)
target_link_libraries(test-cognitive-primitive ggml-tensor-kernel-minimal ${GGML_LIBRARIES})

# Install targets
install(TARGETS ggml-tensor-kernel-minimal test-tensor-kernel-minimal test-cognitive-primitive
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
)