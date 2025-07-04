/*
 * AtomSpaceTensorMapper.cc
 *
 * Basic implementation of AtomSpace to tensor mapping
 */

#include <opencog/tensor/AtomSpaceTensorMapper.h>
#include <opencog/util/Logger.h>

using namespace opencog;

AtomSpaceTensorMapper::AtomSpaceTensorMapper(AtomSpace* as, ggml_context* ctx)
    : atomspace_(as), context_(ctx), max_nodes_(1000), max_edges_(1000), max_features_(100) {
}

void AtomSpaceTensorMapper::initialize_encoder() {
    if (!atomspace_) {
        logger().warn("AtomSpaceTensorMapper") << "No AtomSpace provided";
        return;
    }
    
    // Initialize with current AtomSpace statistics
    HandleSet all_atoms = atomspace_->get_handles_by_type(ATOM, true);
    max_nodes_ = all_atoms.size();
    
    logger().info("AtomSpaceTensorMapper") << "Initialized encoder with " << max_nodes_ << " atoms";
}

ggml_tensor* AtomSpaceTensorMapper::atoms_to_tensor(const HandleSet& atoms) {
    if (!context_) {
        logger().error("AtomSpaceTensorMapper") << "No GGML context available";
        return nullptr;
    }
    
    // Create a simple tensor representation
    size_t num_atoms = atoms.size();
    ggml_tensor* tensor = ggml_new_tensor_2d(context_, GGML_TYPE_F32, num_atoms, max_features_);
    
    if (tensor) {
        ggml_set_name(tensor, "atoms_tensor");
        logger().debug("AtomSpaceTensorMapper") << "Created tensor for " << num_atoms << " atoms";
    }
    
    return tensor;
}

HandleSet AtomSpaceTensorMapper::tensor_to_atoms(ggml_tensor* tensor) {
    HandleSet result;
    
    if (!tensor || !atomspace_) {
        logger().error("AtomSpaceTensorMapper") << "Invalid tensor or AtomSpace";
        return result;
    }
    
    // Basic reconstruction - return all atoms for now
    result = atomspace_->get_handles_by_type(ATOM, true);
    
    logger().debug("AtomSpaceTensorMapper") << "Reconstructed " << result.size() << " atoms from tensor";
    return result;
}

ggml_tensor* AtomSpaceTensorMapper::hypergraph_to_tensor(const HandleSet& atoms) {
    return atoms_to_tensor(atoms);
}

ggml_tensor* AtomSpaceTensorMapper::encode_atom_features(const Handle& atom) {
    if (!context_) {
        return nullptr;
    }
    
    ggml_tensor* tensor = ggml_new_tensor_1d(context_, GGML_TYPE_F32, max_features_);
    if (tensor) {
        ggml_set_name(tensor, "atom_features");
    }
    
    return tensor;
}

Handle AtomSpaceTensorMapper::reconstruct_atom_from_tensor(ggml_tensor* tensor, size_t index) {
    // Basic implementation - return undefined handle
    return Handle::UNDEFINED;
}

ggml_tensor* AtomSpaceTensorMapper::encode_pattern(const Handle& pattern) {
    if (!context_) {
        return nullptr;
    }
    
    ggml_tensor* tensor = ggml_new_tensor_1d(context_, GGML_TYPE_F32, max_features_);
    if (tensor) {
        ggml_set_name(tensor, "pattern");
    }
    
    return tensor;
}

ggml_tensor* AtomSpaceTensorMapper::encode_subgraph(const HandleSet& subgraph) {
    return atoms_to_tensor(subgraph);
}

void AtomSpaceTensorMapper::register_transformation_rule(const std::string& rule_name,
                                                       std::function<ggml_tensor*(ggml_tensor*)> transform) {
    logger().info("AtomSpaceTensorMapper") << "Registered transformation rule: " << rule_name;
}

ggml_tensor* AtomSpaceTensorMapper::apply_transformation(const std::string& rule_name, ggml_tensor* input) {
    return input; // Identity transformation for now
}

std::vector<ggml_tensor*> AtomSpaceTensorMapper::encode_atom_batch(const std::vector<Handle>& atoms) {
    std::vector<ggml_tensor*> result;
    for (const auto& atom : atoms) {
        result.push_back(encode_atom_features(atom));
    }
    return result;
}

std::vector<Handle> AtomSpaceTensorMapper::decode_tensor_batch(const std::vector<ggml_tensor*>& tensors) {
    std::vector<Handle> result;
    for (size_t i = 0; i < tensors.size(); ++i) {
        result.push_back(reconstruct_atom_from_tensor(tensors[i], i));
    }
    return result;
}

void AtomSpaceTensorMapper::set_encoding_for_type(Type type, const HypergraphEncoding& encoding) {
    type_encodings_[type] = encoding;
    logger().debug("AtomSpaceTensorMapper") << "Set encoding for type: " << type;
}

const HypergraphEncoding& AtomSpaceTensorMapper::get_encoding_for_type(Type type) const {
    auto it = type_encodings_.find(type);
    if (it != type_encodings_.end()) {
        return it->second;
    }
    
    static HypergraphEncoding default_encoding({100}, {100}, {100});
    return default_encoding;
}

void AtomSpaceTensorMapper::update_atom_mappings() {
    if (!atomspace_) {
        return;
    }
    
    HandleSet all_atoms = atomspace_->get_handles_by_type(ATOM, true);
    size_t index = 0;
    
    atom_to_index_.clear();
    index_to_atom_.clear();
    
    for (const auto& atom : all_atoms) {
        atom_to_index_[atom] = index;
        index_to_atom_[index] = atom;
        ++index;
    }
    
    logger().debug("AtomSpaceTensorMapper") << "Updated mappings for " << all_atoms.size() << " atoms";
}

void AtomSpaceTensorMapper::clear_mappings() {
    atom_to_index_.clear();
    index_to_atom_.clear();
}

size_t AtomSpaceTensorMapper::get_atom_count() const {
    return atom_to_index_.size();
}

size_t AtomSpaceTensorMapper::get_tensor_memory_usage() const {
    return max_nodes_ * max_features_ * sizeof(float);
}

void AtomSpaceTensorMapper::print_mapping_stats() const {
    std::cout << "=== AtomSpace Tensor Mapper Statistics ===" << std::endl;
    std::cout << "Mapped atoms: " << get_atom_count() << std::endl;
    std::cout << "Max nodes: " << max_nodes_ << std::endl;
    std::cout << "Max edges: " << max_edges_ << std::endl;
    std::cout << "Max features: " << max_features_ << std::endl;
    std::cout << "Memory usage: " << get_tensor_memory_usage() << " bytes" << std::endl;
}

ggml_tensor* AtomSpaceTensorMapper::compute_atom_similarities(const HandleSet& atoms) {
    if (!context_) {
        return nullptr;
    }
    
    size_t num_atoms = atoms.size();
    ggml_tensor* similarity_matrix = ggml_new_tensor_2d(context_, GGML_TYPE_F32, num_atoms, num_atoms);
    
    if (similarity_matrix) {
        ggml_set_name(similarity_matrix, "atom_similarities");
    }
    
    return similarity_matrix;
}

ggml_tensor* AtomSpaceTensorMapper::extract_hypergraph_patterns(const HandleSet& atoms) {
    return atoms_to_tensor(atoms);
}

ggml_tensor* AtomSpaceTensorMapper::apply_pattern_matcher(ggml_tensor* query, ggml_tensor* target) {
    return query; // Return query for now
}