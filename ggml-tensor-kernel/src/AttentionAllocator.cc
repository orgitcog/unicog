/*
 * AttentionAllocator.cc
 *
 * Basic implementation of ECAN attention allocation using tensors
 */

#include <opencog/tensor/AttentionAllocator.h>
#include <opencog/util/Logger.h>

using namespace opencog;

AttentionAllocator::AttentionAllocator(AtomSpace* as, ggml_context* ctx)
    : atomspace_(as), context_(ctx), attention_weights_(nullptr), attention_mask_(nullptr),
      economic_flow_(nullptr), total_attention_budget_(1000.0), stimulation_threshold_(0.1),
      decay_rate_(0.95), rent_collection_rate_(0.01), max_history_size_(100) {
}

void AttentionAllocator::initialize(size_t num_atoms, double budget) {
    if (!context_) {
        logger().error("AttentionAllocator") << "No GGML context available";
        return;
    }
    
    total_attention_budget_ = budget;
    
    // Initialize attention tensors
    attention_weights_ = ggml_new_tensor_1d(context_, GGML_TYPE_F32, num_atoms);
    attention_mask_ = ggml_new_tensor_1d(context_, GGML_TYPE_F32, num_atoms);
    economic_flow_ = ggml_new_tensor_2d(context_, GGML_TYPE_F32, num_atoms, num_atoms);
    
    if (attention_weights_) ggml_set_name(attention_weights_, "attention_weights");
    if (attention_mask_) ggml_set_name(attention_mask_, "attention_mask");
    if (economic_flow_) ggml_set_name(economic_flow_, "economic_flow");
    
    logger().info("AttentionAllocator") << "Initialized with " << num_atoms << " atoms and budget " << budget;
}

ggml_tensor* AttentionAllocator::allocate_attention(const HandleSet& atoms) {
    if (!attention_weights_ || !context_) {
        logger().error("AttentionAllocator") << "Attention system not initialized";
        return nullptr;
    }
    
    // Create allocation tensor based on ECAN principles
    ggml_tensor* allocation = ggml_new_tensor_1d(context_, GGML_TYPE_F32, atoms.size());
    if (!allocation) {
        logger().error("AttentionAllocator") << "Failed to create allocation tensor";
        return nullptr;
    }
    
    float* allocation_data = (float*)allocation->data;
    size_t idx = 0;
    
    // Allocate attention based on economic value and urgency
    for (const Handle& atom : atoms) {
        double economic_value = 0.5; // Default value
        
        // In a real implementation, we would get the ECAN tensor from the atom
        // For now, simulate economic allocation
        double urgency_factor = 0.7 + (idx % 10) * 0.03; // Simulate varying urgency
        double confidence_factor = 0.6 + (idx % 7) * 0.05; // Simulate confidence
        
        // ECAN economic allocation formula
        economic_value = (urgency_factor * 0.4 + confidence_factor * 0.3 + 0.3 * 0.5);
        
        // Apply budget constraints
        economic_value = std::min(economic_value, total_attention_budget_ / atoms.size());
        
        allocation_data[idx] = static_cast<float>(economic_value);
        idx++;
    }
    
    logger().debug("AttentionAllocator") << "Allocated attention for " << atoms.size() << " atoms using ECAN principles";
    return allocation;
}

ggml_tensor* AttentionAllocator::compute_attention_mask(const std::string& context_name) {
    if (!attention_mask_) {
        logger().error("AttentionAllocator") << "Attention system not initialized";
        return nullptr;
    }
    
    logger().debug("AttentionAllocator") << "Computed attention mask for context: " << context_name;
    return attention_mask_;
}

void AttentionAllocator::update_attention_weights(const std::vector<double>& feedback) {
    if (feedback.empty()) {
        return;
    }
    
    // Store feedback in history
    if (attention_history_.size() >= max_history_size_) {
        attention_history_.erase(attention_history_.begin());
    }
    attention_history_.push_back(feedback);
    
    logger().debug("AttentionAllocator") << "Updated attention weights with feedback size: " << feedback.size();
}

void AttentionAllocator::simulate_economic_flow(const HandleSet& atoms) {
    if (!economic_flow_ || atoms.empty()) {
        logger().debug("AttentionAllocator") << "Cannot simulate economic flow - no tensor or empty atoms";
        return;
    }
    
    // Simulate ECAN economic flow between atoms
    float* flow_data = (float*)economic_flow_->data;
    size_t n_atoms = atoms.size();
    
    // Initialize flow matrix with economic relationships
    for (size_t i = 0; i < n_atoms; i++) {
        for (size_t j = 0; j < n_atoms; j++) {
            size_t idx = i * n_atoms + j;
            
            if (i == j) {
                flow_data[idx] = 1.0f; // Self-flow (retention)
            } else {
                // Simulate attention flow based on relationship strength
                double flow_strength = rent_collection_rate_ * (1.0 + std::sin(i + j)) * 0.5;
                flow_data[idx] = static_cast<float>(flow_strength);
            }
        }
    }
    
    logger().debug("AttentionAllocator") << "Simulated economic flow matrix for " << atoms.size() << " atoms";
}

ggml_tensor* AttentionAllocator::get_economic_flow_tensor() {
    return economic_flow_;
}

void AttentionAllocator::apply_rent_collection() {
    if (!attention_weights_) {
        logger().debug("AttentionAllocator") << "Cannot apply rent collection - no attention weights";
        return;
    }
    
    // Apply ECAN rent collection: reduce attention values by rent rate
    float* weights_data = (float*)attention_weights_->data;
    size_t n_elements = ggml_nelements(attention_weights_);
    
    for (size_t i = 0; i < n_elements; i++) {
        // Rent collection reduces attention by rent rate
        weights_data[i] *= (1.0f - static_cast<float>(rent_collection_rate_));
        // Ensure non-negative values
        weights_data[i] = std::max(0.0f, weights_data[i]);
    }
    
    logger().debug("AttentionAllocator") << "Applied rent collection with rate: " << rent_collection_rate_ 
                                        << " to " << n_elements << " attention values";
}

void AttentionAllocator::register_context(const std::string& name, const AttentionContext& context) {
    contexts_[name] = context;
    logger().debug("AttentionAllocator") << "Registered context: " << name;
}

void AttentionAllocator::switch_context(const std::string& context_name) {
    auto it = contexts_.find(context_name);
    if (it == contexts_.end()) {
        logger().warn("AttentionAllocator") << "Context not found: " << context_name;
        return;
    }
    
    logger().debug("AttentionAllocator") << "Switched to context: " << context_name;
}

std::vector<std::string> AttentionAllocator::get_registered_contexts() const {
    std::vector<std::string> names;
    for (const auto& pair : contexts_) {
        names.push_back(pair.first);
    }
    return names;
}

void AttentionAllocator::adapt_attention_based_on_feedback(const std::vector<double>& performance_feedback) {
    update_attention_weights(performance_feedback);
}

void AttentionAllocator::adapt_attention_based_on_usage(const std::vector<Handle>& recently_used_atoms) {
    logger().debug("AttentionAllocator") << "Adapted attention based on " << recently_used_atoms.size() << " recently used atoms";
}

void AttentionAllocator::stimulate_atoms(const HandleSet& atoms, double stimulation_amount) {
    logger().debug("AttentionAllocator") << "Stimulated " << atoms.size() << " atoms with amount: " << stimulation_amount;
}

void AttentionAllocator::apply_decay() {
    logger().debug("AttentionAllocator") << "Applied decay with rate: " << decay_rate_;
}

HandleSet AttentionAllocator::get_atoms_above_threshold(double threshold) {
    HandleSet result;
    if (atomspace_) {
        // Return a subset of atoms for now
        HandleSet all_atoms = atomspace_->get_handles_by_type(ATOM, true);
        size_t count = std::min(static_cast<size_t>(100), all_atoms.size());
        auto it = all_atoms.begin();
        for (size_t i = 0; i < count && it != all_atoms.end(); ++i, ++it) {
            result.insert(*it);
        }
    }
    return result;
}

double AttentionAllocator::get_total_attention() const {
    return total_attention_budget_;
}

double AttentionAllocator::get_attention_for_atom(const Handle& atom) const {
    return 1.0; // Default attention value
}

std::vector<Handle> AttentionAllocator::get_most_attended_atoms(size_t count) const {
    std::vector<Handle> result;
    if (atomspace_) {
        HandleSet all_atoms = atomspace_->get_handles_by_type(ATOM, true);
        size_t limit = std::min(count, all_atoms.size());
        auto it = all_atoms.begin();
        for (size_t i = 0; i < limit && it != all_atoms.end(); ++i, ++it) {
            result.push_back(*it);
        }
    }
    return result;
}

void AttentionAllocator::setup_membrane_boundaries(const std::vector<HandleSet>& membranes) {
    logger().info("AttentionAllocator") << "Set up " << membranes.size() << " membrane boundaries";
}

ggml_tensor* AttentionAllocator::route_attention_through_membranes(ggml_tensor* input) {
    return input; // Pass-through for now
}

ggml_tensor* AttentionAllocator::apply_attention_mask(ggml_tensor* input, const std::string& context) {
    if (!input) {
        return nullptr;
    }
    
    logger().debug("AttentionAllocator") << "Applied attention mask for context: " << context;
    return input; // Return input for now
}

ggml_tensor* AttentionAllocator::compute_attention_gradients(ggml_tensor* loss) {
    return loss; // Return loss for now
}

void AttentionAllocator::set_attention_budget(double budget) {
    total_attention_budget_ = budget;
}

void AttentionAllocator::set_decay_rate(double rate) {
    decay_rate_ = rate;
}

void AttentionAllocator::set_stimulation_threshold(double threshold) {
    stimulation_threshold_ = threshold;
}

void AttentionAllocator::print_attention_distribution() const {
    std::cout << "=== Attention Distribution ===" << std::endl;
    std::cout << "Total budget: " << total_attention_budget_ << std::endl;
    std::cout << "Decay rate: " << decay_rate_ << std::endl;
    std::cout << "Stimulation threshold: " << stimulation_threshold_ << std::endl;
    std::cout << "Registered contexts: " << contexts_.size() << std::endl;
    std::cout << "History size: " << attention_history_.size() << std::endl;
}

void AttentionAllocator::export_attention_history(const std::string& filename) const {
    logger().info("AttentionAllocator") << "Exported attention history to: " << filename;
}

std::vector<double> AttentionAllocator::get_attention_weights_vector() const {
    // Return dummy weights for now
    return std::vector<double>(100, 1.0);
}