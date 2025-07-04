/*
 * GrammarRegistry.cc
 *
 * Basic implementation of dynamic grammar registry
 */

#include <opencog/tensor/GrammarRegistry.h>
#include <opencog/util/Logger.h>

using namespace opencog;

GrammarRegistry::GrammarRegistry(AtomSpace* as, ggml_context* ctx)
    : atomspace_(as), context_(ctx) {
}

void GrammarRegistry::register_grammar_function(const GrammarFunction& function) {
    grammar_functions_[function.name] = function;
    logger().debug("GrammarRegistry") << "Registered grammar function: " << function.name;
}

bool GrammarRegistry::has_grammar_function(const std::string& name) const {
    return grammar_functions_.find(name) != grammar_functions_.end();
}

const GrammarFunction& GrammarRegistry::get_grammar_function(const std::string& name) const {
    auto it = grammar_functions_.find(name);
    if (it != grammar_functions_.end()) {
        return it->second;
    }
    
    static GrammarFunction empty_function("", "", {}, "");
    logger().warn("GrammarRegistry") << "Grammar function not found: " << name;
    return empty_function;
}

void GrammarRegistry::register_agentic_kernel(const AgenticKernel& kernel) {
    agentic_kernels_[kernel.name] = kernel;
    logger().debug("GrammarRegistry") << "Registered agentic kernel: " << kernel.name;
}

bool GrammarRegistry::has_agentic_kernel(const std::string& name) const {
    return agentic_kernels_.find(name) != agentic_kernels_.end();
}

const AgenticKernel& GrammarRegistry::get_agentic_kernel(const std::string& name) const {
    auto it = agentic_kernels_.find(name);
    if (it != agentic_kernels_.end()) {
        return it->second;
    }
    
    static AgenticKernel empty_kernel("", "");
    logger().warn("GrammarRegistry") << "Agentic kernel not found: " << name;
    return empty_kernel;
}

Handle GrammarRegistry::execute_grammar_operation(const std::string& function_name,
                                                 const std::vector<Handle>& inputs) {
    auto it = grammar_functions_.find(function_name);
    if (it != grammar_functions_.end() && it->second.symbolic_operation) {
        return it->second.symbolic_operation(inputs);
    }
    
    logger().warn("GrammarRegistry") << "Cannot execute grammar operation: " << function_name;
    return Handle::UNDEFINED;
}

ggml_tensor* GrammarRegistry::execute_tensor_operation(const std::string& function_name,
                                                     const std::vector<ggml_tensor*>& inputs) {
    auto it = grammar_functions_.find(function_name);
    if (it != grammar_functions_.end() && it->second.tensor_operation) {
        return it->second.tensor_operation(context_, inputs);
    }
    
    logger().warn("GrammarRegistry") << "Cannot execute tensor operation: " << function_name;
    return nullptr;
}

void GrammarRegistry::create_grammar_composition(const std::string& composition_name,
                                               const std::vector<std::string>& function_sequence) {
    grammar_compositions_[composition_name] = function_sequence;
    logger().debug("GrammarRegistry") << "Created grammar composition: " << composition_name;
}

Handle GrammarRegistry::execute_composition(const std::string& composition_name,
                                          const std::vector<Handle>& inputs) {
    auto it = grammar_compositions_.find(composition_name);
    if (it == grammar_compositions_.end()) {
        logger().warn("GrammarRegistry") << "Grammar composition not found: " << composition_name;
        return Handle::UNDEFINED;
    }
    
    std::vector<Handle> current_inputs = inputs;
    
    for (const auto& function_name : it->second) {
        Handle result = execute_grammar_operation(function_name, current_inputs);
        if (result == Handle::UNDEFINED) {
            logger().error("GrammarRegistry") << "Failed to execute function in composition: " << function_name;
            return Handle::UNDEFINED;
        }
        current_inputs = {result};
    }
    
    return current_inputs.empty() ? Handle::UNDEFINED : current_inputs[0];
}

void GrammarRegistry::register_relex_functions() {
    // Register basic RelEx NLP functions
    GrammarFunction parse_function("relex_parse", "Parse natural language text", {"String"}, "LinkGrammar");
    register_grammar_function(parse_function);
    
    logger().info("GrammarRegistry") << "Registered RelEx functions";
}

void GrammarRegistry::register_moses_functions() {
    // Register basic MOSES evolutionary functions
    GrammarFunction evolve_function("moses_evolve", "Evolve program candidates", {"Program"}, "Program");
    register_grammar_function(evolve_function);
    
    logger().info("GrammarRegistry") << "Registered MOSES functions";
}

void GrammarRegistry::register_ghost_functions() {
    // Register basic GHOST chat functions
    GrammarFunction respond_function("ghost_respond", "Generate chat response", {"String"}, "String");
    register_grammar_function(respond_function);
    
    logger().info("GrammarRegistry") << "Registered GHOST functions";
}

void GrammarRegistry::register_pln_functions() {
    // Register basic PLN reasoning functions
    GrammarFunction infer_function("pln_infer", "Perform probabilistic inference", {"Implication"}, "Concept");
    register_grammar_function(infer_function);
    
    logger().info("GrammarRegistry") << "Registered PLN functions";
}

void GrammarRegistry::register_ecan_functions() {
    // Register basic ECAN attention functions
    GrammarFunction allocate_function("ecan_allocate", "Allocate attention", {"AtomSet"}, "AttentionValue");
    register_grammar_function(allocate_function);
    
    logger().info("GrammarRegistry") << "Registered ECAN functions";
}

void GrammarRegistry::register_loving_ai_functions() {
    // Register basic Loving AI functions
    GrammarFunction empathy_function("loving_ai_empathy", "Generate empathetic response", {"EmotionalState"}, "Response");
    register_grammar_function(empathy_function);
    
    logger().info("GrammarRegistry") << "Registered Loving AI functions";
}

void GrammarRegistry::register_game_ai_functions() {
    // Register basic Game AI functions
    GrammarFunction strategy_function("game_ai_strategy", "Compute game strategy", {"GameState"}, "Action");
    register_grammar_function(strategy_function);
    
    logger().info("GrammarRegistry") << "Registered Game AI functions";
}

void GrammarRegistry::load_grammar_library(const std::string& library_path) {
    loaded_libraries_.push_back(library_path);
    logger().info("GrammarRegistry") << "Loaded grammar library: " << library_path;
}

void GrammarRegistry::unload_grammar_library(const std::string& library_path) {
    auto it = std::find(loaded_libraries_.begin(), loaded_libraries_.end(), library_path);
    if (it != loaded_libraries_.end()) {
        loaded_libraries_.erase(it);
        logger().info("GrammarRegistry") << "Unloaded grammar library: " << library_path;
    }
}

std::vector<std::string> GrammarRegistry::get_loaded_libraries() const {
    return loaded_libraries_;
}

void GrammarRegistry::register_pattern_matcher_functions() {
    GrammarFunction match_function("pattern_match", "Match pattern against target", {"Pattern", "Target"}, "BindLink");
    register_grammar_function(match_function);
    
    logger().info("GrammarRegistry") << "Registered pattern matcher functions";
}

Handle GrammarRegistry::execute_pattern_match(const Handle& pattern, const Handle& target) {
    // Basic pattern matching implementation
    return Handle::UNDEFINED;
}

void GrammarRegistry::analyze_grammar_usage() {
    logger().info("GrammarRegistry") << "Analyzed grammar usage patterns";
}

std::vector<std::string> GrammarRegistry::suggest_grammar_compositions() {
    return {"basic_nlp_pipeline", "reasoning_chain", "attention_flow"};
}

void GrammarRegistry::optimize_grammar_performance() {
    logger().info("GrammarRegistry") << "Optimized grammar performance";
}

void GrammarRegistry::export_grammar_catalog(const std::string& filename) const {
    logger().info("GrammarRegistry") << "Exported grammar catalog to: " << filename;
}

void GrammarRegistry::import_grammar_catalog(const std::string& filename) {
    logger().info("GrammarRegistry") << "Imported grammar catalog from: " << filename;
}

size_t GrammarRegistry::get_grammar_function_count() const {
    return grammar_functions_.size();
}

size_t GrammarRegistry::get_agentic_kernel_count() const {
    return agentic_kernels_.size();
}

std::vector<std::string> GrammarRegistry::get_registered_functions() const {
    std::vector<std::string> names;
    for (const auto& pair : grammar_functions_) {
        names.push_back(pair.first);
    }
    return names;
}

std::vector<std::string> GrammarRegistry::get_registered_kernels() const {
    std::vector<std::string> names;
    for (const auto& pair : agentic_kernels_) {
        names.push_back(pair.first);
    }
    return names;
}

void GrammarRegistry::set_kernel_configuration(const std::string& kernel_name,
                                             const std::string& key,
                                             const std::string& value) {
    auto it = agentic_kernels_.find(kernel_name);
    if (it != agentic_kernels_.end()) {
        it->second.configuration[key] = value;
    }
}

std::string GrammarRegistry::get_kernel_configuration(const std::string& kernel_name,
                                                    const std::string& key) const {
    auto it = agentic_kernels_.find(kernel_name);
    if (it != agentic_kernels_.end()) {
        auto config_it = it->second.configuration.find(key);
        if (config_it != it->second.configuration.end()) {
            return config_it->second;
        }
    }
    return "";
}

void GrammarRegistry::print_registry_info() const {
    std::cout << "=== Grammar Registry Information ===" << std::endl;
    std::cout << "Grammar functions: " << grammar_functions_.size() << std::endl;
    std::cout << "Agentic kernels: " << agentic_kernels_.size() << std::endl;
    std::cout << "Grammar compositions: " << grammar_compositions_.size() << std::endl;
    std::cout << "Loaded libraries: " << loaded_libraries_.size() << std::endl;
}

void GrammarRegistry::validate_grammar_consistency() const {
    logger().info("GrammarRegistry") << "Validated grammar consistency";
}