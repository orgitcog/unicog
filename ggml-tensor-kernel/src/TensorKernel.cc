/*
 * TensorKernel.cc
 *
 * Implementation of the unified GGML tensor kernel
 */

#include <opencog/tensor/TensorKernel.h>
#include <opencog/util/Logger.h>
#include <iostream>

using namespace opencog;

// Static instance for singleton pattern
static std::unique_ptr<TensorKernel> default_instance = nullptr;

// Standard tensor shapes definitions
namespace opencog {
namespace StandardTensorShapes {
    const TensorShape COGUTIL_SHAPE("cogutil", {64, 32, 16}, "Core utilities tensor field");
    const TensorShape ATOMSPACE_SHAPE("atomspace", {1024, 512, 256}, "AtomSpace hypergraph tensor field");
    const TensorShape COGSERVER_SHAPE("cogserver", {128, 64, 32}, "Distributed cognitive server tensor field");
    const TensorShape RELEX_SHAPE("relex", {256, 128, 64}, "RelEx NLP tensor field");
    const TensorShape PLN_SHAPE("pln", {512, 256, 128}, "PLN reasoning tensor field");
    const TensorShape ECAN_SHAPE("ecan", {256, 128, 64}, "ECAN attention tensor field");
    const TensorShape MOSES_SHAPE("moses", {512, 256, 128}, "MOSES evolution tensor field");
    const TensorShape GHOST_SHAPE("ghost", {256, 128, 64}, "GHOST chat engine tensor field");
    const TensorShape LOVING_AI_SHAPE("loving_ai", {128, 64, 32}, "Loving AI tensor field");
    const TensorShape GAME_AI_SHAPE("game_ai", {256, 128, 64}, "Game AI tensor field");
}
}

// CognitiveModule implementation
void CognitiveModule::initialize_tensor(ggml_context* ctx) {
    if (!ctx) {
        logger().error("TensorKernel") << "Cannot initialize tensor with null context";
        return;
    }
    
    // Create tensor with the specified shape
    std::vector<int64_t> dims(shape.dimensions.begin(), shape.dimensions.end());
    
    switch (dims.size()) {
        case 1:
            tensor = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, dims[0]);
            break;
        case 2:
            tensor = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, dims[0], dims[1]);
            break;
        case 3:
            tensor = ggml_new_tensor_3d(ctx, GGML_TYPE_F32, dims[0], dims[1], dims[2]);
            break;
        default:
            tensor = ggml_new_tensor(ctx, GGML_TYPE_F32, dims.size(), dims.data());
            break;
    }
    
    if (tensor) {
        ggml_set_name(tensor, name.c_str());
        logger().debug("TensorKernel") << "Initialized tensor for module: " << name;
    } else {
        logger().error("TensorKernel") << "Failed to initialize tensor for module: " << name;
    }
}

ggml_tensor* CognitiveModule::execute(ggml_context* ctx, const std::vector<ggml_tensor*>& inputs) {
    if (!operation) {
        logger().warn("TensorKernel") << "No operation defined for module: " << name;
        return tensor;
    }
    
    return operation(ctx, inputs);
}

// TensorKernel implementation
TensorKernel::TensorKernel(AtomSpace* as) 
    : atomspace_(as), main_context_(nullptr), unified_tensor_field_(nullptr), initialized_(false) {
    
    // Initialize components
    registry_ = std::make_unique<TensorRegistry>();
    atom_mapper_ = std::make_unique<AtomSpaceTensorMapper>(atomspace_, main_context_);
    attention_allocator_ = std::make_unique<AttentionAllocator>(atomspace_, main_context_);
    grammar_registry_ = std::make_unique<GrammarRegistry>(atomspace_, main_context_);
    distributed_executor_ = std::make_unique<DistributedExecutor>();
    neural_symbolic_bridge_ = std::make_unique<NeuralSymbolicBridge>(atomspace_, main_context_);
}

TensorKernel::~TensorKernel() {
    cleanup();
}

bool TensorKernel::initialize() {
    if (initialized_) {
        logger().warn("TensorKernel") << "Already initialized";
        return true;
    }
    
    // Initialize GGML context
    size_t mem_size = 256 * 1024 * 1024; // 256MB
    struct ggml_init_params params = {
        .mem_size = mem_size,
        .mem_buffer = nullptr,
        .no_alloc = false,
    };
    
    main_context_ = ggml_init(params);
    if (!main_context_) {
        logger().error("TensorKernel") << "Failed to initialize GGML context";
        return false;
    }
    
    // Update component contexts
    atom_mapper_ = std::make_unique<AtomSpaceTensorMapper>(atomspace_, main_context_);
    attention_allocator_ = std::make_unique<AttentionAllocator>(atomspace_, main_context_);
    grammar_registry_ = std::make_unique<GrammarRegistry>(atomspace_, main_context_);
    neural_symbolic_bridge_ = std::make_unique<NeuralSymbolicBridge>(atomspace_, main_context_);
    
    // Initialize components
    atom_mapper_->initialize_encoder();
    attention_allocator_->initialize(1000); // Initialize with 1000 atoms capacity
    neural_symbolic_bridge_->initialize();
    
    // Register standard modules
    register_standard_modules();
    
    // Create unified tensor field
    unified_tensor_field_ = ggml_new_tensor_2d(main_context_, GGML_TYPE_F32, 1024, 1024);
    if (unified_tensor_field_) {
        ggml_set_name(unified_tensor_field_, "unified_tensor_field");
    }
    
    initialized_ = true;
    logger().info("TensorKernel") << "Successfully initialized unified tensor kernel";
    return true;
}

void TensorKernel::cleanup() {
    if (main_context_) {
        ggml_free(main_context_);
        main_context_ = nullptr;
    }
    
    modules_.clear();
    initialized_ = false;
    
    logger().info("TensorKernel") << "Cleaned up tensor kernel";
}

void TensorKernel::register_module(const std::string& name, const TensorShape& shape) {
    auto module = std::make_unique<CognitiveModule>(name, shape);
    
    if (main_context_) {
        module->initialize_tensor(main_context_);
    }
    
    // Register shape in registry
    registry_->register_shape(name, shape);
    
    modules_[name] = std::move(module);
    logger().debug("TensorKernel") << "Registered module: " << name;
}

void TensorKernel::register_standard_modules() {
    using namespace StandardTensorShapes;
    
    register_module("cogutil", COGUTIL_SHAPE);
    register_module("atomspace", ATOMSPACE_SHAPE);
    register_module("cogserver", COGSERVER_SHAPE);
    register_module("relex", RELEX_SHAPE);
    register_module("pln", PLN_SHAPE);
    register_module("ecan", ECAN_SHAPE);
    register_module("moses", MOSES_SHAPE);
    register_module("ghost", GHOST_SHAPE);
    register_module("loving_ai", LOVING_AI_SHAPE);
    register_module("game_ai", GAME_AI_SHAPE);
    
    logger().info("TensorKernel") << "Registered " << modules_.size() << " standard modules";
}

CognitiveModule* TensorKernel::get_module(const std::string& name) {
    auto it = modules_.find(name);
    if (it != modules_.end()) {
        return it->second.get();
    }
    return nullptr;
}

ggml_tensor* TensorKernel::create_tensor(const std::string& name, const std::vector<size_t>& shape) {
    if (!main_context_) {
        logger().error("TensorKernel") << "Cannot create tensor without initialized context";
        return nullptr;
    }
    
    std::vector<int64_t> dims(shape.begin(), shape.end());
    ggml_tensor* tensor = nullptr;
    
    switch (dims.size()) {
        case 1:
            tensor = ggml_new_tensor_1d(main_context_, GGML_TYPE_F32, dims[0]);
            break;
        case 2:
            tensor = ggml_new_tensor_2d(main_context_, GGML_TYPE_F32, dims[0], dims[1]);
            break;
        case 3:
            tensor = ggml_new_tensor_3d(main_context_, GGML_TYPE_F32, dims[0], dims[1], dims[2]);
            break;
        default:
            tensor = ggml_new_tensor(main_context_, GGML_TYPE_F32, dims.size(), dims.data());
            break;
    }
    
    if (tensor) {
        ggml_set_name(tensor, name.c_str());
        logger().debug("TensorKernel") << "Created tensor: " << name;
    }
    
    return tensor;
}

ggml_tensor* TensorKernel::map_atomspace_to_tensor(const HandleSet& atoms) {
    if (!atom_mapper_) {
        logger().error("TensorKernel") << "AtomSpace mapper not initialized";
        return nullptr;
    }
    
    return atom_mapper_->atoms_to_tensor(atoms);
}

ggml_tensor* TensorKernel::apply_attention_mask(ggml_tensor* input, const std::string& attention_type) {
    if (!attention_allocator_) {
        logger().error("TensorKernel") << "Attention allocator not initialized";
        return nullptr;
    }
    
    return attention_allocator_->apply_attention_mask(input, attention_type);
}

Handle TensorKernel::execute_grammar_operation(const std::string& grammar_name, 
                                             const std::vector<Handle>& inputs) {
    if (!grammar_registry_) {
        logger().error("TensorKernel") << "Grammar registry not initialized";
        return Handle::UNDEFINED;
    }
    
    return grammar_registry_->execute_grammar_operation(grammar_name, inputs);
}

std::vector<Handle> TensorKernel::tensor_to_atoms(ggml_tensor* tensor) {
    if (!atom_mapper_) {
        logger().error("TensorKernel") << "AtomSpace mapper not initialized";
        return std::vector<Handle>();
    }
    
    HandleSet atoms = atom_mapper_->tensor_to_atoms(tensor);
    return std::vector<Handle>(atoms.begin(), atoms.end());
}

void TensorKernel::enable_distributed_execution(const std::vector<std::string>& node_addresses) {
    if (!distributed_executor_) {
        logger().error("TensorKernel") << "Distributed executor not initialized";
        return;
    }
    
    // Register nodes
    for (size_t i = 0; i < node_addresses.size(); ++i) {
        std::string node_id = "node_" + std::to_string(i);
        distributed_executor_->register_node(node_id, node_addresses[i], 1000, 1024*1024*1024);
    }
    
    distributed_executor_->start_executor();
    logger().info("TensorKernel") << "Enabled distributed execution with " << node_addresses.size() << " nodes";
}

void TensorKernel::execute_distributed(const std::string& operation_name, 
                                      const std::vector<ggml_tensor*>& inputs) {
    if (!distributed_executor_) {
        logger().error("TensorKernel") << "Distributed executor not initialized";
        return;
    }
    
    // Convert tensors to string identifiers for distributed execution
    std::vector<std::string> input_ids;
    for (size_t i = 0; i < inputs.size(); ++i) {
        input_ids.push_back("input_" + std::to_string(i));
    }
    
    std::vector<std::string> output_ids = {"output_0"};
    
    distributed_executor_->execute_distributed_operation(operation_name, input_ids, output_ids);
}

void TensorKernel::update_attention_weights(const std::vector<double>& feedback) {
    if (!attention_allocator_) {
        logger().error("TensorKernel") << "Attention allocator not initialized";
        return;
    }
    
    attention_allocator_->update_attention_weights(feedback);
}

ggml_tensor* TensorKernel::get_attention_mask(const std::string& context) {
    if (!attention_allocator_) {
        logger().error("TensorKernel") << "Attention allocator not initialized";
        return nullptr;
    }
    
    return attention_allocator_->compute_attention_mask(context);
}

std::vector<std::string> TensorKernel::get_registered_modules() const {
    std::vector<std::string> names;
    for (const auto& pair : modules_) {
        names.push_back(pair.first);
    }
    return names;
}

void TensorKernel::print_tensor_info(const std::string& name) const {
    auto it = modules_.find(name);
    if (it != modules_.end()) {
        const auto& module = it->second;
        std::cout << "Module: " << module->name << std::endl;
        std::cout << "Shape: ";
        for (size_t dim : module->shape.dimensions) {
            std::cout << dim << " ";
        }
        std::cout << std::endl;
        std::cout << "Total elements: " << module->shape.total_elements() << std::endl;
        std::cout << "Description: " << module->shape.description << std::endl;
    } else {
        std::cout << "Module not found: " << name << std::endl;
    }
}

// Static methods
std::unique_ptr<TensorKernel> TensorKernel::create_instance(AtomSpace* as) {
    return std::make_unique<TensorKernel>(as);
}

TensorKernel* TensorKernel::get_default_instance() {
    if (!default_instance) {
        default_instance = std::make_unique<TensorKernel>();
        default_instance->initialize();
    }
    return default_instance.get();
}