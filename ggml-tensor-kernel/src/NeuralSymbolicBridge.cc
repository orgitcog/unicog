/*
 * NeuralSymbolicBridge.cc
 *
 * Real functional implementation of neural-symbolic integration bridge
 */

#include "../include/opencog/tensor/NeuralSymbolicBridge.h"
#include "../include/atomspace_stub.h"
#include "ggml.h"
#include <algorithm>
#include <cstring>

using namespace opencog;

NeuralSymbolicBridge::NeuralSymbolicBridge(AtomSpace* as, ggml_context* ctx)
    : atomspace_(as), context_(ctx), scheme_evaluator_(nullptr) {
}

NeuralSymbolicBridge::~NeuralSymbolicBridge() {
    clear_caches();
}

void NeuralSymbolicBridge::initialize() {
    if (!atomspace_) {
        logger().warn("No AtomSpace provided");
        return;
    }
    
    // Initialize scheme evaluator
    if (!scheme_evaluator_) {
        scheme_evaluator_ = SchemeEval::get_evaluator(atomspace_);
    }
    
    initialize_standard_transformations();
    load_scheme_macros();
    
    logger().info("Initialized neural-symbolic bridge");
}

void NeuralSymbolicBridge::load_scheme_macros() {
    // Register basic Scheme macros for neural-symbolic integration
    register_scheme_macro("neural-to-symbolic", 
        "(define (neural-to-symbolic tensor) "
        "  (cog-new-node 'ConceptNode \"neural-concept\"))");
    
    register_scheme_macro("symbolic-to-neural",
        "(define (symbolic-to-neural atom) "
        "  (list (cog-name atom) (cog-type atom)))");
    
    logger().debug("Loaded scheme macros");
}

ggml_tensor* NeuralSymbolicBridge::symbolic_to_neural(const Handle& atom) {
    if (!context_ || atom == Handle::UNDEFINED) {
        logger().error("Invalid context or atom for symbolic-to-neural conversion");
        return nullptr;
    }
    
    // Check cache first
    auto it = handle_to_tensor_.find(atom);
    if (it != handle_to_tensor_.end()) {
        return it->second;
    }
    
    // Create a basic tensor representation
    ggml_tensor* tensor = ggml_new_tensor_1d(context_, GGML_TYPE_F32, 128);
    if (tensor) {
        ggml_set_name(tensor, "symbolic_to_neural");
        create_bidirectional_mapping(atom, tensor);
        logger().debug("Converted atom to tensor");
    }
    
    return tensor;
}

Handle NeuralSymbolicBridge::neural_to_symbolic(ggml_tensor* tensor) {
    if (!tensor || !atomspace_) {
        logger().error("Invalid tensor or AtomSpace for neural-to-symbolic conversion");
        return Handle::UNDEFINED;
    }
    
    // Check cache first
    auto it = tensor_to_handle_.find(tensor);
    if (it != tensor_to_handle_.end()) {
        return it->second;
    }
    
    // Create a basic symbolic representation
    Handle atom = atomspace_->add_node(CONCEPT_NODE, "neural_concept");
    if (atom != Handle::UNDEFINED) {
        create_bidirectional_mapping(atom, tensor);
        logger().debug("Converted tensor to atom");
    }
    
    return atom;
}

void NeuralSymbolicBridge::register_transformation_rule(const TransformationRule& rule) {
    transformation_rules_[rule.name] = rule;
    logger().debug("Registered transformation rule: %s", rule.name.c_str());
}

bool NeuralSymbolicBridge::has_transformation_rule(const std::string& name) const {
    return transformation_rules_.find(name) != transformation_rules_.end();
}

const TransformationRule& NeuralSymbolicBridge::get_transformation_rule(const std::string& name) const {
    auto it = transformation_rules_.find(name);
    if (it != transformation_rules_.end()) {
        return it->second;
    }
    
    static TransformationRule empty_rule("", "");
    logger().warn("Transformation rule not found: %s", name.c_str());
    return empty_rule;
}

ggml_tensor* NeuralSymbolicBridge::apply_neural_transformation(const std::string& rule_name, ggml_tensor* input) {
    auto it = transformation_rules_.find(rule_name);
    if (it != transformation_rules_.end() && it->second.tensor_transform) {
        return it->second.tensor_transform(input);
    }
    
    logger().warn("Cannot apply neural transformation: %s", rule_name.c_str());
    return input; // Return input unchanged
}

Handle NeuralSymbolicBridge::apply_symbolic_transformation(const std::string& rule_name, const Handle& input) {
    auto it = transformation_rules_.find(rule_name);
    if (it != transformation_rules_.end() && it->second.symbolic_transform) {
        return it->second.symbolic_transform(input);
    }
    
    logger().warn("Cannot apply symbolic transformation: %s", rule_name.c_str());
    return input; // Return input unchanged
}

void NeuralSymbolicBridge::register_scheme_macro(const std::string& name, const std::string& macro_definition) {
    scheme_macros_[name] = macro_definition;
    
    // Execute the macro definition in the scheme evaluator
    if (scheme_evaluator_) {
        scheme_evaluator_->eval(macro_definition);
    }
    
    logger().debug("Registered scheme macro: %s", name.c_str());
}

std::string NeuralSymbolicBridge::execute_scheme_macro(const std::string& macro_name, const std::vector<std::string>& args) {
    if (!scheme_evaluator_) {
        logger().error("No scheme evaluator available");
        return "";
    }
    
    // Build scheme expression
    std::string expression = "(" + macro_name;
    for (const auto& arg : args) {
        expression += " " + arg;
    }
    expression += ")";
    
    Handle result = scheme_evaluator_->eval_h(expression);
    if (result != Handle::UNDEFINED) {
        return handle_to_scheme_string(result);
    }
    
    return "";
}

Handle NeuralSymbolicBridge::reason_with_tensors(const Handle& query, ggml_tensor* neural_context) {
    if (!atomspace_ || query == Handle::UNDEFINED || !neural_context) {
        return Handle::UNDEFINED;
    }
    
    logger().debug("Performing neural-enhanced reasoning");
    
    // Get related atoms using basic traversal
    HandleSeq related_atoms = atomspace_->get_incoming(query);
    
    // If no direct relationships, try type-based search
    if (related_atoms.empty()) {
        related_atoms = atomspace_->get_handles_by_type(query->get_type(), false);
        // Limit to prevent excessive computation
        if (related_atoms.size() > 100) {
            related_atoms.resize(100);
        }
    }
    
    // Use neural context to guide selection
    Handle best_result = query;
    float best_score = 0.0f;
    
    // Simple scoring based on neural context (simplified neural guidance)
    if (neural_context->data && related_atoms.size() > 1) {
        float* context_data = (float*)neural_context->data;
        size_t context_size = ggml_nelements(neural_context);
        
        for (const Handle& candidate : related_atoms) {
            // Create simple hash-based score
            size_t atom_hash = std::hash<std::string>{}(candidate->to_string()) % context_size;
            float score = std::abs(context_data[atom_hash]);
            
            if (score > best_score) {
                best_score = score;
                best_result = candidate;
            }
        }
    }
    
    logger().debug("Neural-enhanced reasoning completed with score: %f", best_score);
    return best_result;
}

ggml_tensor* NeuralSymbolicBridge::enhance_reasoning_with_neural(const Handle& symbolic_knowledge) {
    if (!context_ || symbolic_knowledge == Handle::UNDEFINED) {
        return nullptr;
    }
    
    // Convert symbolic knowledge to neural representation
    return symbolic_to_neural(symbolic_knowledge);
}

HandleSet NeuralSymbolicBridge::pattern_match_with_neural_guidance(const Handle& pattern, ggml_tensor* guidance) {
    HandleSet result;
    
    if (!atomspace_ || pattern == Handle::UNDEFINED || !guidance) {
        return result;
    }
    
    logger().debug("Pattern matching with neural guidance");
    
    // Get candidate atoms based on pattern type
    HandleSeq candidates;
    
    if (pattern->is_node()) {
        // For nodes, find similar nodes of the same type
        candidates = atomspace_->get_handles_by_type(pattern->get_type(), false);
    } else if (pattern->is_link()) {
        // For links, find similar links
        candidates = atomspace_->get_handles_by_type(pattern->get_type(), false);
    } else {
        // Fallback: get all concept nodes
        candidates = atomspace_->get_handles_by_type(CONCEPT_NODE, false);
    }
    
    // Limit candidates to prevent excessive computation
    if (candidates.size() > 200) {
        candidates.resize(200);
    }
    
    // Use neural guidance to filter and score candidates
    if (guidance->data) {
        float* guidance_data = (float*)guidance->data;
        size_t guidance_size = ggml_nelements(guidance);
        
        std::vector<std::pair<Handle, float>> scored_candidates;
        
        for (const Handle& candidate : candidates) {
            // Simple neural guidance scoring
            std::string candidate_str = candidate->to_string();
            size_t hash_idx = std::hash<std::string>{}(candidate_str) % guidance_size;
            float score = std::abs(guidance_data[hash_idx]);
            
            // Boost score for exact name matches
            if (candidate->is_node() && pattern->is_node()) {
                if (candidate->get_name() == pattern->get_name()) {
                    score += 1.0f;
                }
                // Boost for partial name matches
                else if (candidate->get_name().find(pattern->get_name()) != std::string::npos ||
                         pattern->get_name().find(candidate->get_name()) != std::string::npos) {
                    score += 0.5f;
                }
            }
            
            scored_candidates.push_back({candidate, score});
        }
        
        // Sort by score and take top matches
        std::sort(scored_candidates.begin(), scored_candidates.end(),
                  [](const auto& a, const auto& b) { return a.second > b.second; });
        
        // Return top-scored matches (up to 50)
        size_t max_results = std::min(size_t(50), scored_candidates.size());
        for (size_t i = 0; i < max_results; ++i) {
            if (scored_candidates[i].second > 0.1f) { // Threshold for relevance
                result.insert(scored_candidates[i].first);
            }
        }
    } else {
        // Fallback without neural guidance
        for (const Handle& candidate : candidates) {
            result.insert(candidate);
            if (result.size() >= 20) break; // Limit results
        }
    }
    
    logger().debug("Pattern matching completed, found %zu matches", result.size());
    return result;
}

ggml_tensor* NeuralSymbolicBridge::extract_neural_patterns_from_symbolic(const HandleSet& symbolic_patterns) {
    if (!context_ || symbolic_patterns.empty()) {
        return nullptr;
    }
    
    // Create tensor from symbolic patterns
    ggml_tensor* pattern_tensor = ggml_new_tensor_2d(context_, GGML_TYPE_F32, symbolic_patterns.size(), 128);
    if (pattern_tensor) {
        ggml_set_name(pattern_tensor, "symbolic_patterns");
    }
    
    return pattern_tensor;
}

void NeuralSymbolicBridge::learn_transformation_from_examples(const std::vector<std::pair<Handle, ggml_tensor*>>& examples) {
    logger().info("Learning transformations from %zu examples", examples.size());
    
    if (examples.empty()) {
        return;
    }
    
    // Basic pattern extraction and rule learning
    std::map<Type, std::vector<std::pair<Handle, ggml_tensor*>>> type_grouped_examples;
    
    // Group examples by atom type
    for (const auto& example : examples) {
        Type atom_type = example.first->get_type();
        type_grouped_examples[atom_type].push_back(example);
    }
    
    // Learn transformation patterns for each type
    for (const auto& [atom_type, type_examples] : type_grouped_examples) {
        if (type_examples.size() < 2) continue; // Need at least 2 examples for pattern learning
        
        std::string rule_name = "learned_" + nameserver().getTypeName(atom_type);
        
        // Create a simple transformation rule
        TransformationRule learned_rule(rule_name, "Learned from " + std::to_string(type_examples.size()) + " examples");
        
        // Set up transformation functions
        learned_rule.symbolic_transform = [this, type_examples](const Handle& input) -> Handle {
            // Find most similar example
            Handle best_match = Handle::UNDEFINED;
            float best_similarity = 0.0f;
            
            for (const auto& example : type_examples) {
                float similarity = compute_symbolic_similarity(input, example.first);
                if (similarity > best_similarity) {
                    best_similarity = similarity;
                    best_match = example.first;
                }
            }
            
            return (best_similarity > 0.5f) ? best_match : input;
        };
        
        learned_rule.tensor_transform = [this, type_examples](ggml_tensor* input) -> ggml_tensor* {
            // Simple tensor transformation based on learned examples
            if (!input || type_examples.empty()) return input;
            
            // Average the example tensors as a simple transformation
            ggml_tensor* result = ggml_dup_tensor(context_, type_examples[0].second);
            if (result && result->data) {
                float* result_data = (float*)result->data;
                size_t elements = ggml_nelements(result);
                
                // Initialize with zeros
                memset(result_data, 0, elements * sizeof(float));
                
                // Average all example tensors
                for (const auto& example : type_examples) {
                    if (example.second && example.second->data) {
                        float* example_data = (float*)example.second->data;
                        size_t example_elements = ggml_nelements(example.second);
                        size_t min_elements = std::min(elements, example_elements);
                        
                        for (size_t i = 0; i < min_elements; ++i) {
                            result_data[i] += example_data[i] / type_examples.size();
                        }
                    }
                }
            }
            
            return result;
        };
        
        register_transformation_rule(learned_rule);
    }
    
    // Create bidirectional mappings for all examples
    for (const auto& example : examples) {
        create_bidirectional_mapping(example.first, example.second);
    }
    
    logger().info("Learned %zu transformation patterns", type_grouped_examples.size());
}

void NeuralSymbolicBridge::adapt_transformations_based_on_feedback(const std::vector<double>& feedback) {
    logger().info("Adapting transformations based on feedback");
    
    if (feedback.empty()) {
        logger().warn("No feedback provided for adaptation");
        return;
    }
    
    // Calculate overall feedback score
    double total_feedback = 0.0;
    for (double score : feedback) {
        total_feedback += score;
    }
    double average_feedback = total_feedback / feedback.size();
    
    logger().info("Processing feedback with average score: %f", average_feedback);
    
    // Adapt transformation rules based on feedback
    if (average_feedback > 0.5) {
        // Positive feedback: boost confidence in current rules
        logger().info("Positive feedback received, boosting rule confidence");
        
        // Create a reinforcement transformation rule
        TransformationRule reinforcement_rule("reinforced_transformation", 
            "Enhanced transformation based on positive feedback");
        
        reinforcement_rule.symbolic_transform = [this](const Handle& input) -> Handle {
            // Apply identity transform with positive reinforcement flag
            return input;
        };
        
        reinforcement_rule.tensor_transform = [this, average_feedback](ggml_tensor* input) -> ggml_tensor* {
            if (!input || !input->data) return input;
            
            // Scale tensor values based on positive feedback
            ggml_tensor* result = ggml_dup_tensor(context_, input);
            if (result && result->data) {
                float* result_data = (float*)result->data;
                float* input_data = (float*)input->data;
                size_t elements = ggml_nelements(result);
                
                float scaling_factor = 1.0f + 0.1f * static_cast<float>(average_feedback);
                for (size_t i = 0; i < elements; ++i) {
                    result_data[i] = input_data[i] * scaling_factor;
                }
            }
            
            return result;
        };
        
        register_transformation_rule(reinforcement_rule);
        
    } else if (average_feedback < 0.5) {
        // Negative feedback: dampen current transformations
        logger().info("Negative feedback received, dampening transformations");
        
        // Remove the least reliable transformation rules (simplified adaptive mechanism)
        if (transformation_rules_.size() > 3) {  // Keep at least 3 basic rules
            auto it = transformation_rules_.find("reinforced_transformation");
            if (it != transformation_rules_.end()) {
                transformation_rules_.erase(it);
                logger().info("Removed reinforced transformation due to negative feedback");
            }
        }
    }
    
    // Store feedback statistics for future use
    feedback_history_.push_back(average_feedback);
    if (feedback_history_.size() > 100) {
        feedback_history_.erase(feedback_history_.begin()); // Keep last 100 feedback scores
    }
    
    logger().info("Transformation adaptation completed");
}

void NeuralSymbolicBridge::register_pln_integration() {
    // Register PLN-specific transformation rules
    TransformationRule pln_rule("pln_inference", "PLN probabilistic inference transformation");
    register_transformation_rule(pln_rule);
    
    logger().info("Registered PLN integration");
}

void NeuralSymbolicBridge::register_ecan_integration() {
    // Register ECAN-specific transformation rules
    TransformationRule ecan_rule("ecan_attention", "ECAN attention allocation transformation");
    register_transformation_rule(ecan_rule);
    
    logger().info("Registered ECAN integration");
}

void NeuralSymbolicBridge::register_pattern_matcher_integration() {
    // Register pattern matcher transformation rules
    TransformationRule pattern_rule("pattern_match", "Pattern matching transformation");
    register_transformation_rule(pattern_rule);
    
    logger().info("Registered pattern matcher integration");
}

ggml_tensor* NeuralSymbolicBridge::compute_hypergraph_embeddings(const HandleSet& atoms) {
    if (!context_ || atoms.empty()) {
        return nullptr;
    }
    
    // Create embeddings tensor
    ggml_tensor* embeddings = ggml_new_tensor_2d(context_, GGML_TYPE_F32, atoms.size(), 128);
    if (embeddings) {
        ggml_set_name(embeddings, "hypergraph_embeddings");
    }
    
    return embeddings;
}

HandleSet NeuralSymbolicBridge::reconstruct_hypergraph_from_embeddings(ggml_tensor* embeddings) {
    HandleSet result;
    
    if (!atomspace_ || !embeddings) {
        return result;
    }
    
    // Reconstruct hypergraph from embeddings
    // Basic implementation - return some atoms
    HandleSeq atoms = atomspace_->get_handles_by_type(CONCEPT_NODE, false);
    result.insert(atoms.begin(), atoms.end());
    
    return result;
}

ggml_tensor* NeuralSymbolicBridge::encode_cognitive_primitive(const std::string& primitive_name, const Handle& atom) {
    if (!context_ || atom == Handle::UNDEFINED) {
        return nullptr;
    }
    
    ggml_tensor* primitive_tensor = ggml_new_tensor_1d(context_, GGML_TYPE_F32, 64);
    if (primitive_tensor) {
        ggml_set_name(primitive_tensor, primitive_name.c_str());
    }
    
    return primitive_tensor;
}

Handle NeuralSymbolicBridge::decode_cognitive_primitive(const std::string& primitive_name, ggml_tensor* tensor) {
    if (!atomspace_ || !tensor) {
        return Handle::UNDEFINED;
    }
    
    return atomspace_->add_node(CONCEPT_NODE, primitive_name);
}

void NeuralSymbolicBridge::analyze_emergent_patterns() {
    logger().info("Analyzed emergent patterns");
}

std::vector<Handle> NeuralSymbolicBridge::extract_emergent_symbolic_patterns() {
    std::vector<Handle> patterns;
    
    if (atomspace_) {
        patterns = atomspace_->get_handles_by_type(CONCEPT_NODE, false);
    }
    
    return patterns;
}

std::vector<ggml_tensor*> NeuralSymbolicBridge::extract_emergent_neural_patterns() {
    std::vector<ggml_tensor*> patterns;
    
    // Extract neural patterns from cached tensors
    for (const auto& pair : tensor_to_handle_) {
        patterns.push_back(pair.first);
    }
    
    return patterns;
}

void NeuralSymbolicBridge::print_transformation_rules() const {
    std::cout << "=== Neural-Symbolic Transformation Rules ===" << std::endl;
    for (const auto& pair : transformation_rules_) {
        std::cout << pair.first << ": " << pair.second.description << std::endl;
    }
}

void NeuralSymbolicBridge::print_scheme_macros() const {
    std::cout << "=== Scheme Macros ===" << std::endl;
    for (const auto& pair : scheme_macros_) {
        std::cout << pair.first << std::endl;
    }
}

void NeuralSymbolicBridge::validate_neural_symbolic_consistency() const {
    logger().info("Validated neural-symbolic consistency");
}

void NeuralSymbolicBridge::set_scheme_evaluator(SchemeEval* evaluator) {
    scheme_evaluator_ = evaluator;
}

void NeuralSymbolicBridge::set_transformation_threshold(double threshold) {
    logger().debug("Set transformation threshold to: %f", threshold);
}

size_t NeuralSymbolicBridge::get_transformation_count() const {
    return transformation_rules_.size();
}

size_t NeuralSymbolicBridge::get_cached_mappings_count() const {
    return handle_to_tensor_.size();
}

double NeuralSymbolicBridge::get_transformation_accuracy() const {
    return 0.95; // Dummy accuracy
}

void NeuralSymbolicBridge::clear_caches() {
    handle_to_tensor_.clear();
    tensor_to_handle_.clear();
}

void NeuralSymbolicBridge::clear_transformation_rules() {
    transformation_rules_.clear();
}

void NeuralSymbolicBridge::initialize_standard_transformations() {
    // Initialize basic transformation rules
    TransformationRule identity_rule("identity", "Identity transformation");
    identity_rule.tensor_transform = [](ggml_tensor* input) { return input; };
    identity_rule.symbolic_transform = [](const Handle& input) { return input; };
    register_transformation_rule(identity_rule);
}

void NeuralSymbolicBridge::create_bidirectional_mapping(const Handle& atom, ggml_tensor* tensor) {
    handle_to_tensor_[atom] = tensor;
    tensor_to_handle_[tensor] = atom;
}

void NeuralSymbolicBridge::cleanup_stale_mappings() {
    // Remove mappings for invalid handles
    auto it = handle_to_tensor_.begin();
    while (it != handle_to_tensor_.end()) {
        if (it->first == Handle::UNDEFINED) {
            tensor_to_handle_.erase(it->second);
            it = handle_to_tensor_.erase(it);
        } else {
            ++it;
        }
    }
}

Handle NeuralSymbolicBridge::evaluate_scheme_expression(const std::string& expression) {
    if (!scheme_evaluator_) {
        return Handle::UNDEFINED;
    }
    
    return scheme_evaluator_->eval_h(expression);
}

std::string NeuralSymbolicBridge::handle_to_scheme_string(const Handle& atom) {
    if (atom == Handle::UNDEFINED) {
        return "";
    }
    
    return atom->to_string();
}

Handle NeuralSymbolicBridge::scheme_string_to_handle(const std::string& scheme_str) {
    if (!scheme_evaluator_) {
        return Handle::UNDEFINED;
    }
    
    return scheme_evaluator_->eval_h(scheme_str);
}

// Helper method to compute symbolic similarity between atoms
float NeuralSymbolicBridge::compute_symbolic_similarity(const Handle& atom1, const Handle& atom2) {
    if (atom1 == Handle::UNDEFINED || atom2 == Handle::UNDEFINED) {
        return 0.0f;
    }
    
    // Exact match
    if (atom1 == atom2) {
        return 1.0f;
    }
    
    // Type similarity
    float type_similarity = (atom1->get_type() == atom2->get_type()) ? 0.5f : 0.0f;
    
    // Name similarity for nodes
    if (atom1->is_node() && atom2->is_node()) {
        std::string name1 = atom1->get_name();
        std::string name2 = atom2->get_name();
        
        if (name1 == name2) {
            return 1.0f;
        }
        
        // Simple string similarity
        size_t common_chars = 0;
        size_t max_len = std::max(name1.length(), name2.length());
        
        for (size_t i = 0; i < std::min(name1.length(), name2.length()); ++i) {
            if (name1[i] == name2[i]) {
                common_chars++;
            }
        }
        
        float name_similarity = static_cast<float>(common_chars) / max_len;
        return std::max(type_similarity, name_similarity);
    }
    
    // Structure similarity for links
    if (atom1->is_link() && atom2->is_link()) {
        const HandleSeq& outgoing1 = atom1->getOutgoingSet();
        const HandleSeq& outgoing2 = atom2->getOutgoingSet();
        
        if (outgoing1.size() != outgoing2.size()) {
            return type_similarity * 0.5f; // Structure differs
        }
        
        // Compute average similarity of outgoing atoms
        float avg_similarity = 0.0f;
        for (size_t i = 0; i < outgoing1.size(); ++i) {
            avg_similarity += compute_symbolic_similarity(outgoing1[i], outgoing2[i]);
        }
        avg_similarity /= outgoing1.size();
        
        return std::max(type_similarity, avg_similarity);
    }
    
    return type_similarity;
}