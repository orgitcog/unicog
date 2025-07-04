/*
 * AtomSpaceTensorMapper.h
 *
 * Maps AtomSpace hypergraph structures to GGML tensor operations
 */

#ifndef _OPENCOG_ATOMSPACE_TENSOR_MAPPER_H
#define _OPENCOG_ATOMSPACE_TENSOR_MAPPER_H

#include <memory>
#include <vector>
#include <unordered_map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>

#include "ggml.h"

namespace opencog {

/**
 * HypergraphEncoding represents how hypergraph patterns are encoded as tensors
 */
struct HypergraphEncoding {
    std::vector<size_t> node_dimensions;
    std::vector<size_t> edge_dimensions;  
    std::vector<size_t> feature_dimensions;
    std::string encoding_type;
    
    HypergraphEncoding() = default;
    HypergraphEncoding(const std::vector<size_t>& nodes, 
                      const std::vector<size_t>& edges,
                      const std::vector<size_t>& features,
                      const std::string& type = "standard")
        : node_dimensions(nodes), edge_dimensions(edges), 
          feature_dimensions(features), encoding_type(type) {}
};

/**
 * AtomSpaceTensorMapper converts between AtomSpace hypergraph and GGML tensors
 */
class AtomSpaceTensorMapper {
private:
    AtomSpace* atomspace_;
    ggml_context* context_;
    
    // Mapping caches
    std::unordered_map<Handle, size_t> atom_to_index_;
    std::unordered_map<size_t, Handle> index_to_atom_;
    
    // Encoding configurations
    std::unordered_map<Type, HypergraphEncoding> type_encodings_;
    
    // Tensor field dimensions
    size_t max_nodes_;
    size_t max_edges_;
    size_t max_features_;
    
public:
    AtomSpaceTensorMapper(AtomSpace* as, ggml_context* ctx);
    ~AtomSpaceTensorMapper() = default;
    
    // Initialize encoder with AtomSpace statistics
    void initialize_encoder();
    
    // AtomSpace to tensor conversion
    ggml_tensor* atoms_to_tensor(const HandleSet& atoms);
    ggml_tensor* hypergraph_to_tensor(const HandleSet& atoms);
    ggml_tensor* encode_atom_features(const Handle& atom);
    
    // Tensor to AtomSpace conversion
    HandleSet tensor_to_atoms(ggml_tensor* tensor);
    Handle reconstruct_atom_from_tensor(ggml_tensor* tensor, size_t index);
    
    // Hypergraph pattern encoding
    ggml_tensor* encode_pattern(const Handle& pattern);
    ggml_tensor* encode_subgraph(const HandleSet& subgraph);
    
    // Neural-symbolic transformation rules
    void register_transformation_rule(const std::string& rule_name,
                                    std::function<ggml_tensor*(ggml_tensor*)> transform);
    ggml_tensor* apply_transformation(const std::string& rule_name, ggml_tensor* input);
    
    // Batch operations
    std::vector<ggml_tensor*> encode_atom_batch(const std::vector<Handle>& atoms);
    std::vector<Handle> decode_tensor_batch(const std::vector<ggml_tensor*>& tensors);
    
    // Configuration
    void set_encoding_for_type(Type type, const HypergraphEncoding& encoding);
    const HypergraphEncoding& get_encoding_for_type(Type type) const;
    
    // Update mappings
    void update_atom_mappings();
    void clear_mappings();
    
    // Statistics and debugging
    size_t get_atom_count() const;
    size_t get_tensor_memory_usage() const;
    void print_mapping_stats() const;
    
    // Tensor operations for AtomSpace
    ggml_tensor* compute_atom_similarities(const HandleSet& atoms);
    ggml_tensor* extract_hypergraph_patterns(const HandleSet& atoms);
    ggml_tensor* apply_pattern_matcher(ggml_tensor* query, ggml_tensor* target);
};

} // namespace opencog

#endif // _OPENCOG_ATOMSPACE_TENSOR_MAPPER_H