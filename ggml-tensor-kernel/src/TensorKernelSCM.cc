/*
 * TensorKernelSCM.cc
 *
 * Scheme bindings for the unified tensor kernel
 */

#include <opencog/tensor/TensorKernel.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeModule.h>

using namespace opencog;

class TensorKernelSCM : public ModuleWrap {
private:
    static TensorKernel* kernel_instance;
    
public:
    TensorKernelSCM() : ModuleWrap("opencog tensor-kernel") {}
    
    void init();
    
    // Scheme function implementations
    static Handle create_tensor_kernel(AtomSpace* as);
    static Handle register_cognitive_module(const std::string& name, const std::vector<int>& shape);
    static Handle execute_tensor_operation(const std::string& operation_name, const HandleSeq& inputs);
    static Handle map_atoms_to_tensor(const HandleSeq& atoms);
    static Handle allocate_attention(const HandleSeq& atoms, const std::string& context);
    static Handle execute_grammar_function(const std::string& function_name, const HandleSeq& inputs);
    static std::string tensor_kernel_info();
};

TensorKernel* TensorKernelSCM::kernel_instance = nullptr;

void TensorKernelSCM::init() {
    // Initialize the tensor kernel if not already done
    if (!kernel_instance) {
        kernel_instance = TensorKernel::get_default_instance();
    }
    
    // Register scheme functions
    define_scheme_primitive("cog-create-tensor-kernel", &TensorKernelSCM::create_tensor_kernel, this, "tensor-kernel");
    define_scheme_primitive("cog-register-cognitive-module", &TensorKernelSCM::register_cognitive_module, this, "tensor-kernel");
    define_scheme_primitive("cog-execute-tensor-operation", &TensorKernelSCM::execute_tensor_operation, this, "tensor-kernel");
    define_scheme_primitive("cog-map-atoms-to-tensor", &TensorKernelSCM::map_atoms_to_tensor, this, "tensor-kernel");
    define_scheme_primitive("cog-allocate-attention", &TensorKernelSCM::allocate_attention, this, "tensor-kernel");
    define_scheme_primitive("cog-execute-grammar-function", &TensorKernelSCM::execute_grammar_function, this, "tensor-kernel");
    define_scheme_primitive("cog-tensor-kernel-info", &TensorKernelSCM::tensor_kernel_info, this, "tensor-kernel");
}

Handle TensorKernelSCM::create_tensor_kernel(AtomSpace* as) {
    if (!kernel_instance) {
        kernel_instance = TensorKernel::get_default_instance();
    }
    
    // Return a symbolic representation of the tensor kernel
    return as->add_node(CONCEPT_NODE, "TensorKernel");
}

Handle TensorKernelSCM::register_cognitive_module(const std::string& name, const std::vector<int>& shape) {
    if (!kernel_instance) {
        return Handle::UNDEFINED;
    }
    
    std::vector<size_t> tensor_shape(shape.begin(), shape.end());
    TensorShape ts(name, tensor_shape, "Registered from Scheme");
    kernel_instance->register_module(name, ts);
    
    return kernel_instance->get_atomspace()->add_node(CONCEPT_NODE, "Module:" + name);
}

Handle TensorKernelSCM::execute_tensor_operation(const std::string& operation_name, const HandleSeq& inputs) {
    if (!kernel_instance) {
        return Handle::UNDEFINED;
    }
    
    // Convert handles to tensor representation and execute
    std::vector<Handle> input_vector(inputs.begin(), inputs.end());
    Handle result = kernel_instance->execute_grammar_operation(operation_name, input_vector);
    
    return result;
}

Handle TensorKernelSCM::map_atoms_to_tensor(const HandleSeq& atoms) {
    if (!kernel_instance) {
        return Handle::UNDEFINED;
    }
    
    HandleSet atom_set(atoms.begin(), atoms.end());
    ggml_tensor* tensor = kernel_instance->map_atomspace_to_tensor(atom_set);
    
    if (tensor) {
        // Return a symbolic representation of the tensor
        return kernel_instance->get_atomspace()->add_node(CONCEPT_NODE, "Tensor:" + std::string(ggml_get_name(tensor)));
    }
    
    return Handle::UNDEFINED;
}

Handle TensorKernelSCM::allocate_attention(const HandleSeq& atoms, const std::string& context) {
    if (!kernel_instance) {
        return Handle::UNDEFINED;
    }
    
    HandleSet atom_set(atoms.begin(), atoms.end());
    ggml_tensor* attention_mask = kernel_instance->get_attention_mask(context);
    
    if (attention_mask) {
        return kernel_instance->get_atomspace()->add_node(CONCEPT_NODE, "AttentionMask:" + context);
    }
    
    return Handle::UNDEFINED;
}

Handle TensorKernelSCM::execute_grammar_function(const std::string& function_name, const HandleSeq& inputs) {
    if (!kernel_instance) {
        return Handle::UNDEFINED;
    }
    
    std::vector<Handle> input_vector(inputs.begin(), inputs.end());
    return kernel_instance->execute_grammar_operation(function_name, input_vector);
}

std::string TensorKernelSCM::tensor_kernel_info() {
    if (!kernel_instance) {
        return "Tensor kernel not initialized";
    }
    
    std::string info = "Unified GGML Tensor Kernel\n";
    info += "Status: " + std::string(kernel_instance->is_initialized() ? "Initialized" : "Not initialized") + "\n";
    
    auto modules = kernel_instance->get_registered_modules();
    info += "Registered modules: " + std::to_string(modules.size()) + "\n";
    
    for (const auto& module : modules) {
        info += "  - " + module + "\n";
    }
    
    return info;
}

extern "C" {
void opencog_tensor_kernel_init(void);
}

void opencog_tensor_kernel_init(void) {
    static TensorKernelSCM tensor_kernel_scm;
    tensor_kernel_scm.module_init();
}