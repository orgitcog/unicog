/*
 * CognitivePrimitive.cc
 *
 * Implementation of 5-dimensional cognitive primitive tensor
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#include <sstream>
#include <cmath>
#include <algorithm>
#include "atomspace_stub.h"

#include "../include/opencog/tensor/CognitivePrimitive.h"

// Logger stub for minimal implementation
namespace opencog {
    class Logger {
    public:
        static Logger& getInstance() {
            static Logger instance;
            return instance;
        }
        void info(const char* fmt, ...) { printf("[INFO] "); printf(fmt, ""); printf("\n"); }
        void error(const char* fmt, ...) { printf("[ERROR] "); printf(fmt, ""); printf("\n"); }
        void warn(const char* fmt, ...) { printf("[WARN] "); printf(fmt, ""); printf("\n"); }
        void debug(const char* fmt, ...) { printf("[DEBUG] "); printf(fmt, ""); printf("\n"); }
    };
    
    Logger& logger() { return Logger::getInstance(); }
}

using namespace opencog;

// Static tensor shape definition: [modality(4), depth(3), context(3), salience(1), autonomy(1)]
const std::vector<size_t> CognitivePrimitiveTensor::PRIMITIVE_SHAPE = {4, 3, 3, 1, 1};

CognitivePrimitiveTensor::CognitivePrimitiveTensor(ggml_context* ctx, const std::string& name)
    : context_(ctx), primitive_name_(name), 
      modality_(ModalityType::SYMBOLIC), depth_(DepthType::SURFACE), 
      context_type_(ContextType::LOCAL), salience_(0.5f), autonomy_index_(0.5f)
{
    if (!ctx) {
        throw std::invalid_argument("GGML context cannot be null");
    }
    
    // Create 5D tensor with the cognitive primitive shape
    tensor_ = ggml_new_tensor_4d(ctx, GGML_TYPE_F32, 
                                PRIMITIVE_SHAPE[0], PRIMITIVE_SHAPE[1], 
                                PRIMITIVE_SHAPE[2], PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4]);
    
    if (!tensor_) {
        throw std::runtime_error("Failed to create cognitive primitive tensor");
    }
    
    // Initialize tensor with zeros
    ggml_set_zero(tensor_);
    
    logger().info("Created cognitive primitive tensor: %s", name.c_str());
}

CognitivePrimitiveTensor::~CognitivePrimitiveTensor()
{
    // GGML context manages tensor memory
    tensor_ = nullptr;
}

void CognitivePrimitiveTensor::set_modality(ModalityType modality)
{
    modality_ = modality;
    encode_to_tensor();
}

void CognitivePrimitiveTensor::set_depth(DepthType depth)
{
    depth_ = depth;
    encode_to_tensor();
}

void CognitivePrimitiveTensor::set_context(ContextType context)
{
    context_type_ = context;
    encode_to_tensor();
}

void CognitivePrimitiveTensor::set_salience(float salience)
{
    if (salience < 0.0f || salience > 1.0f) {
        throw std::invalid_argument("Salience must be in range [0.0, 1.0]");
    }
    salience_ = salience;
    encode_to_tensor();
}

void CognitivePrimitiveTensor::set_autonomy_index(float autonomy)
{
    if (autonomy < 0.0f || autonomy > 1.0f) {
        throw std::invalid_argument("Autonomy index must be in range [0.0, 1.0]");
    }
    autonomy_index_ = autonomy;
    encode_to_tensor();
}

void CognitivePrimitiveTensor::encode_to_tensor()
{
    if (!tensor_) {
        logger().error("Cannot encode to null tensor");
        return;
    }
    
    // Get tensor data pointer
    float* data = ggml_get_data_f32(tensor_);
    
    // Clear tensor data
    size_t total_elements = get_total_elements();
    std::fill(data, data + total_elements, 0.0f);
    
    // Encode modality (one-hot encoding in first dimension)
    size_t modality_idx = static_cast<size_t>(modality_);
    if (modality_idx < PRIMITIVE_SHAPE[0]) {
        // Calculate index in flattened tensor for modality
        size_t idx = modality_idx * PRIMITIVE_SHAPE[1] * PRIMITIVE_SHAPE[2] * PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4];
        data[idx] = 1.0f;
    }
    
    // Encode depth (one-hot encoding in second dimension)
    size_t depth_idx = static_cast<size_t>(depth_);
    if (depth_idx < PRIMITIVE_SHAPE[1]) {
        size_t idx = depth_idx * PRIMITIVE_SHAPE[2] * PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4];
        data[idx] = 1.0f;
    }
    
    // Encode context (one-hot encoding in third dimension)
    size_t context_idx = static_cast<size_t>(context_type_);
    if (context_idx < PRIMITIVE_SHAPE[2]) {
        size_t idx = context_idx * PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4];
        data[idx] = 1.0f;
    }
    
    // Encode salience (continuous value in fourth dimension)
    size_t salience_base_idx = PRIMITIVE_SHAPE[0] * PRIMITIVE_SHAPE[1] * PRIMITIVE_SHAPE[2] * 0;
    data[salience_base_idx] = salience_;
    
    // Encode autonomy index (continuous value in fifth dimension)
    size_t autonomy_base_idx = PRIMITIVE_SHAPE[0] * PRIMITIVE_SHAPE[1] * PRIMITIVE_SHAPE[2] * PRIMITIVE_SHAPE[3] * 0;
    data[autonomy_base_idx] = autonomy_index_;
    
    logger().debug("Encoded cognitive primitive: %s", to_string().c_str());
}

void CognitivePrimitiveTensor::decode_from_tensor()
{
    if (!tensor_) {
        logger().error("Cannot decode from null tensor");
        return;
    }
    
    float* data = ggml_get_data_f32(tensor_);
    
    // Decode modality (find max in first dimension)
    float max_modality = -1.0f;
    size_t best_modality = 0;
    for (size_t i = 0; i < PRIMITIVE_SHAPE[0]; ++i) {
        size_t idx = i * PRIMITIVE_SHAPE[1] * PRIMITIVE_SHAPE[2] * PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4];
        if (data[idx] > max_modality) {
            max_modality = data[idx];
            best_modality = i;
        }
    }
    modality_ = static_cast<ModalityType>(best_modality);
    
    // Decode depth (find max in second dimension)
    float max_depth = -1.0f;
    size_t best_depth = 0;
    for (size_t i = 0; i < PRIMITIVE_SHAPE[1]; ++i) {
        size_t idx = i * PRIMITIVE_SHAPE[2] * PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4];
        if (data[idx] > max_depth) {
            max_depth = data[idx];
            best_depth = i;
        }
    }
    depth_ = static_cast<DepthType>(best_depth);
    
    // Decode context (find max in third dimension)
    float max_context = -1.0f;
    size_t best_context = 0;
    for (size_t i = 0; i < PRIMITIVE_SHAPE[2]; ++i) {
        size_t idx = i * PRIMITIVE_SHAPE[3] * PRIMITIVE_SHAPE[4];
        if (data[idx] > max_context) {
            max_context = data[idx];
            best_context = i;
        }
    }
    context_type_ = static_cast<ContextType>(best_context);
    
    // Decode salience
    size_t salience_idx = PRIMITIVE_SHAPE[0] * PRIMITIVE_SHAPE[1] * PRIMITIVE_SHAPE[2] * 0;
    salience_ = std::max(0.0f, std::min(1.0f, data[salience_idx]));
    
    // Decode autonomy index
    size_t autonomy_idx = PRIMITIVE_SHAPE[0] * PRIMITIVE_SHAPE[1] * PRIMITIVE_SHAPE[2] * PRIMITIVE_SHAPE[3] * 0;
    autonomy_index_ = std::max(0.0f, std::min(1.0f, data[autonomy_idx]));
    
    logger().debug("Decoded cognitive primitive: %s", to_string().c_str());
}

bool CognitivePrimitiveTensor::validate_tensor_shape() const
{
    if (!tensor_) {
        return false;
    }
    
    // Check if tensor dimensions match expected shape
    auto shape = get_shape();
    if (shape.size() != PRIMITIVE_SHAPE.size()) {
        return false;
    }
    
    for (size_t i = 0; i < shape.size(); ++i) {
        if (shape[i] != PRIMITIVE_SHAPE[i]) {
            return false;
        }
    }
    
    return true;
}

bool CognitivePrimitiveTensor::validate_primitive_values() const
{
    return (salience_ >= 0.0f && salience_ <= 1.0f) &&
           (autonomy_index_ >= 0.0f && autonomy_index_ <= 1.0f);
}

std::vector<size_t> CognitivePrimitiveTensor::get_shape() const
{
    if (!tensor_) {
        return {};
    }
    
    std::vector<size_t> shape;
    for (int i = 0; i < ggml_n_dims(tensor_); ++i) {
        shape.push_back(tensor_->ne[i]);
    }
    return shape;
}

size_t CognitivePrimitiveTensor::get_total_elements() const
{
    size_t total = 1;
    for (size_t dim : PRIMITIVE_SHAPE) {
        total *= dim;
    }
    return total;
}

size_t CognitivePrimitiveTensor::calculate_degrees_of_freedom() const
{
    // DOF = modality_choices + depth_choices + context_choices + salience_continuous + autonomy_continuous
    // For categorical variables: DOF = n_categories - 1
    // For continuous variables: DOF = 1
    return (PRIMITIVE_SHAPE[0] - 1) + (PRIMITIVE_SHAPE[1] - 1) + (PRIMITIVE_SHAPE[2] - 1) + 1 + 1;
}

std::vector<int> CognitivePrimitiveTensor::get_prime_factorization() const
{
    // Prime factorization mapping for tensor signature
    // Using small primes to represent different dimensions
    std::vector<int> primes;
    
    // Modality dimension: primes 2, 3, 5, 7
    const std::vector<int> modality_primes = {2, 3, 5, 7};
    primes.push_back(modality_primes[static_cast<int>(modality_)]);
    
    // Depth dimension: primes 11, 13, 17
    const std::vector<int> depth_primes = {11, 13, 17};
    primes.push_back(depth_primes[static_cast<int>(depth_)]);
    
    // Context dimension: primes 19, 23, 29
    const std::vector<int> context_primes = {19, 23, 29};
    primes.push_back(context_primes[static_cast<int>(context_type_)]);
    
    // Salience and autonomy: mapped to primes based on quantized values
    int salience_prime = 31 + static_cast<int>(salience_ * 10);
    int autonomy_prime = 41 + static_cast<int>(autonomy_index_ * 10);
    primes.push_back(salience_prime);
    primes.push_back(autonomy_prime);
    
    return primes;
}

std::string CognitivePrimitiveTensor::to_string() const
{
    std::ostringstream oss;
    oss << "CognitivePrimitive[" << primitive_name_ << "]: ";
    oss << "modality=" << CognitivePrimitiveUtils::modality_to_string(modality_) << ", ";
    oss << "depth=" << CognitivePrimitiveUtils::depth_to_string(depth_) << ", ";
    oss << "context=" << CognitivePrimitiveUtils::context_to_string(context_type_) << ", ";
    oss << "salience=" << salience_ << ", ";
    oss << "autonomy=" << autonomy_index_;
    return oss.str();
}

Handle CognitivePrimitiveTensor::to_atomspace_node(AtomSpace* as) const
{
    if (!as) {
        return Handle::UNDEFINED;
    }
    
    // Create a ConceptNode for the cognitive primitive
    Handle primitive_node = as->add_node(CONCEPT_NODE, primitive_name_);
    
    // Add properties as EvaluationLinks
    Handle modality_predicate = as->add_node(PREDICATE_NODE, "modality");
    Handle modality_value = as->add_node(CONCEPT_NODE, CognitivePrimitiveUtils::modality_to_string(modality_));
    as->add_link(EVALUATION_LINK, {modality_predicate, primitive_node, modality_value});
    
    Handle depth_predicate = as->add_node(PREDICATE_NODE, "depth");
    Handle depth_value = as->add_node(CONCEPT_NODE, CognitivePrimitiveUtils::depth_to_string(depth_));
    as->add_link(EVALUATION_LINK, {depth_predicate, primitive_node, depth_value});
    
    Handle context_predicate = as->add_node(PREDICATE_NODE, "context");
    Handle context_value = as->add_node(CONCEPT_NODE, CognitivePrimitiveUtils::context_to_string(context_type_));
    as->add_link(EVALUATION_LINK, {context_predicate, primitive_node, context_value});
    
    // Add numerical values as NumberNodes
    Handle salience_predicate = as->add_node(PREDICATE_NODE, "salience");
    Handle salience_value = as->add_node(NUMBER_NODE, std::to_string(salience_));
    as->add_link(EVALUATION_LINK, {salience_predicate, primitive_node, salience_value});
    
    Handle autonomy_predicate = as->add_node(PREDICATE_NODE, "autonomy_index");
    Handle autonomy_value = as->add_node(NUMBER_NODE, std::to_string(autonomy_index_));
    as->add_link(EVALUATION_LINK, {autonomy_predicate, primitive_node, autonomy_value});
    
    return primitive_node;
}

std::unique_ptr<CognitivePrimitiveTensor> CognitivePrimitiveTensor::from_atomspace_node(
    ggml_context* ctx, const Handle& handle)
{
    // Stub implementation for minimal build
    if (!ctx) {
        return nullptr;
    }
    
    // Create a default primitive for testing
    auto primitive = std::make_unique<CognitivePrimitiveTensor>(ctx, "recovered-primitive");
    primitive->set_modality(ModalityType::SYMBOLIC);
    primitive->set_depth(DepthType::SEMANTIC);
    primitive->set_context(ContextType::LOCAL);
    primitive->set_salience(0.5f);
    primitive->set_autonomy_index(0.5f);
    
    return primitive;
}

// CognitivePrimitiveRegistry implementation

CognitivePrimitiveRegistry::CognitivePrimitiveRegistry(ggml_context* ctx)
    : context_(ctx)
{
    if (!ctx) {
        throw std::invalid_argument("GGML context cannot be null");
    }
}

CognitivePrimitiveRegistry::~CognitivePrimitiveRegistry()
{
    primitives_.clear();
}

bool CognitivePrimitiveRegistry::register_primitive(const std::string& name,
                                                  ModalityType modality,
                                                  DepthType depth,
                                                  ContextType context,
                                                  float salience,
                                                  float autonomy)
{
    if (primitives_.find(name) != primitives_.end()) {
        logger().warn("Primitive %s already registered", name.c_str());
        return false;
    }
    
    try {
        auto primitive = std::make_unique<CognitivePrimitiveTensor>(context_, name);
        primitive->set_modality(modality);
        primitive->set_depth(depth);
        primitive->set_context(context);
        primitive->set_salience(salience);
        primitive->set_autonomy_index(autonomy);
        
        primitives_[name] = std::move(primitive);
        logger().info("Registered cognitive primitive: %s", name.c_str());
        return true;
    } catch (const std::exception& e) {
        logger().error("Failed to register primitive %s: %s", name.c_str(), e.what());
        return false;
    }
}

CognitivePrimitiveTensor* CognitivePrimitiveRegistry::get_primitive(const std::string& name)
{
    auto it = primitives_.find(name);
    return (it != primitives_.end()) ? it->second.get() : nullptr;
}

std::vector<std::string> CognitivePrimitiveRegistry::list_primitives() const
{
    std::vector<std::string> names;
    for (const auto& pair : primitives_) {
        names.push_back(pair.first);
    }
    return names;
}

bool CognitivePrimitiveRegistry::validate_all_primitives() const
{
    for (const auto& pair : primitives_) {
        if (!pair.second->validate_tensor_shape() || 
            !pair.second->validate_primitive_values()) {
            return false;
        }
    }
    return true;
}

// Utility functions implementation

namespace CognitivePrimitiveUtils {

std::unique_ptr<CognitivePrimitiveTensor> create_standard_primitive(
    ggml_context* ctx, const std::string& name)
{
    auto primitive = std::make_unique<CognitivePrimitiveTensor>(ctx, name);
    // Set default values
    primitive->set_modality(ModalityType::SYMBOLIC);
    primitive->set_depth(DepthType::SEMANTIC);
    primitive->set_context(ContextType::LOCAL);
    primitive->set_salience(0.5f);
    primitive->set_autonomy_index(0.5f);
    return primitive;
}

std::string modality_to_string(ModalityType modality)
{
    switch (modality) {
        case ModalityType::VISUAL: return "visual";
        case ModalityType::AUDITORY: return "auditory";
        case ModalityType::TEXTUAL: return "textual";
        case ModalityType::SYMBOLIC: return "symbolic";
        default: return "unknown";
    }
}

std::string depth_to_string(DepthType depth)
{
    switch (depth) {
        case DepthType::SURFACE: return "surface";
        case DepthType::SEMANTIC: return "semantic";
        case DepthType::PRAGMATIC: return "pragmatic";
        default: return "unknown";
    }
}

std::string context_to_string(ContextType context)
{
    switch (context) {
        case ContextType::LOCAL: return "local";
        case ContextType::GLOBAL: return "global";
        case ContextType::TEMPORAL: return "temporal";
        default: return "unknown";
    }
}

ModalityType string_to_modality(const std::string& str)
{
    if (str == "visual") return ModalityType::VISUAL;
    if (str == "auditory") return ModalityType::AUDITORY;
    if (str == "textual") return ModalityType::TEXTUAL;
    if (str == "symbolic") return ModalityType::SYMBOLIC;
    throw std::invalid_argument("Unknown modality: " + str);
}

DepthType string_to_depth(const std::string& str)
{
    if (str == "surface") return DepthType::SURFACE;
    if (str == "semantic") return DepthType::SEMANTIC;
    if (str == "pragmatic") return DepthType::PRAGMATIC;
    throw std::invalid_argument("Unknown depth: " + str);
}

ContextType string_to_context(const std::string& str)
{
    if (str == "local") return ContextType::LOCAL;
    if (str == "global") return ContextType::GLOBAL;
    if (str == "temporal") return ContextType::TEMPORAL;
    throw std::invalid_argument("Unknown context: " + str);
}

bool validate_primitive_signature(const CognitivePrimitiveTensor& primitive)
{
    return primitive.validate_tensor_shape() && primitive.validate_primitive_values();
}

} // namespace CognitivePrimitiveUtils