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

# Add comprehensive AtomSpace stubs for testing
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/atomspace_stub.h "
#ifndef ATOMSPACE_STUB_H
#define ATOMSPACE_STUB_H
#include <memory>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <stdexcept>

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
    static const Type VARIABLE_NODE = 13;
    static const Type LAMBDA_LINK = 14;
    static const Type MEMBER_LINK = 15;
    static const Type SUBSET_LINK = 16;
    
    // Forward declarations
    class Atom;
    class Node;
    class Link;
    
    class Handle {
    public:
        static const Handle UNDEFINED;
        Handle() = default;
        Handle(uint64_t id) : uuid_(id) {}
        Handle(std::shared_ptr<Atom> atom) : atom_(atom) {}
        
        uint64_t uuid_ = 0;
        bool operator==(const Handle& other) const;
        bool operator!=(const Handle& other) const { return !(*this == other); }
        bool operator<(const Handle& other) const;
        
        std::shared_ptr<Atom> atom() const { return atom_; }
        
    private:
        std::shared_ptr<Atom> atom_;
    };
    
    typedef std::unordered_set<Handle> HandleSet;
    typedef std::vector<Handle> HandleSeq;
    
    // TruthValue stub
    class TruthValue {
    public:
        TruthValue(double mean = 0.0, double confidence = 0.0) 
            : mean_(mean), confidence_(confidence) {}
        
        double get_mean() const { return mean_; }
        double get_confidence() const { return confidence_; }
        
    private:
        double mean_;
        double confidence_;
    };
    typedef std::shared_ptr<TruthValue> TruthValuePtr;
    
    // Atom base class
    class Atom : public std::enable_shared_from_this<Atom> {
    public:
        Atom(Type t) : type_(t), truth_value_(std::make_shared<TruthValue>()) {}
        virtual ~Atom() = default;
        
        Type get_type() const { return type_; }
        bool is_node() const { return type_ < 100; }  // Simple heuristic
        bool is_link() const { return type_ >= 100; }
        
        TruthValuePtr getTruthValue() const { return truth_value_; }
        void setTruthValue(const TruthValuePtr& tv) { truth_value_ = tv; }
        
        virtual std::string to_string() const = 0;
        
    protected:
        Type type_;
        TruthValuePtr truth_value_;
    };
    
    // Node implementation
    class Node : public Atom {
    public:
        Node(Type t, const std::string& name) : Atom(t), name_(name) {}
        
        const std::string& get_name() const { return name_; }
        
        std::string to_string() const override {
            return \"(\" + std::to_string(type_) + \" \\\"\" + name_ + \"\\\")\";
        }
        
    private:
        std::string name_;
    };
    
    // Link implementation
    class Link : public Atom {
    public:
        Link(Type t, const HandleSeq& outgoing) : Atom(t), outgoing_(outgoing) {}
        
        const HandleSeq& getOutgoingSet() const { return outgoing_; }
        size_t get_arity() const { return outgoing_.size(); }
        
        std::string to_string() const override {
            std::string result = \"(\" + std::to_string(type_);
            for (const auto& h : outgoing_) {
                if (h.atom()) {
                    result += \" \" + h.atom()->to_string();
                }
            }
            result += \")\";
            return result;
        }
        
    private:
        HandleSeq outgoing_;
    };
    
    // AtomSpace implementation
    class AtomSpace {
    public:
        AtomSpace() = default;
        virtual ~AtomSpace() = default;
        
        Handle add_node(Type type, const std::string& name) {
            auto node = std::make_shared<Node>(type, name);
            atoms_[next_uuid_] = node;
            Handle h(next_uuid_++);
            h = Handle(node);
            return h;
        }
        
        Handle add_link(Type type, const HandleSeq& outgoing) {
            auto link = std::make_shared<Link>(type, outgoing);
            atoms_[next_uuid_] = link;
            Handle h(next_uuid_++);
            h = Handle(link);
            return h;
        }
        
        Handle get_handle(Type type, const std::string& name) {
            for (const auto& [id, atom] : atoms_) {
                if (atom->get_type() == type && atom->is_node()) {
                    auto node = std::dynamic_pointer_cast<Node>(atom);
                    if (node && node->get_name() == name) {
                        return Handle(atom);
                    }
                }
            }
            return Handle::UNDEFINED;
        }
        
        size_t get_size() const { return atoms_.size(); }
        
    private:
        static uint64_t next_uuid_;
        std::unordered_map<uint64_t, std::shared_ptr<Atom>> atoms_;
    };
    
    typedef std::shared_ptr<AtomSpace> AtomSpacePtr;
    
    // Value base class stub
    class Value {
    public:
        virtual ~Value() = default;
    };
    typedef std::shared_ptr<Value> ValuePtr;
    
    // FloatValue stub
    class FloatValue : public Value {
    public:
        FloatValue(const std::vector<double>& values) : values_(values) {}
        const std::vector<double>& value() const { return values_; }
        
    private:
        std::vector<double> values_;
    };
    typedef std::shared_ptr<FloatValue> FloatValuePtr;
    
    // Logger stub
    class Logger {
    public:
        static Logger& logger() {
            static Logger instance;
            return instance;
        }
        
        void debug(const std::string& msg) {}
        void info(const std::string& msg) {}
        void warn(const std::string& msg) {}
        void error(const std::string& msg) {}
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

// Handle comparison implementation
inline bool opencog::Handle::operator==(const opencog::Handle& other) const {
    if (atom_ && other.atom_) {
        return atom_ == other.atom_;
    }
    return uuid_ == other.uuid_;
}

inline bool opencog::Handle::operator<(const opencog::Handle& other) const {
    if (atom_ && other.atom_) {
        return atom_ < other.atom_;
    }
    return uuid_ < other.uuid_;
}

#endif // ATOMSPACE_STUB_H
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