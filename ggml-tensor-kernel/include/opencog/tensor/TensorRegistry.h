/*
 * TensorRegistry.h
 *
 * Registry for managing tensor shapes and operations across cognitive modules
 */

#ifndef _OPENCOG_TENSOR_REGISTRY_H
#define _OPENCOG_TENSOR_REGISTRY_H

#include <memory>
#include <vector>
#include <unordered_map>
#include <string>
#include <functional>

#include "ggml.h"

namespace opencog {

struct TensorShape;

/**
 * TensorRegistry manages tensor shapes and operations for all cognitive modules
 */
class TensorRegistry {
private:
    std::unordered_map<std::string, TensorShape> registered_shapes_;
    std::unordered_map<std::string, std::function<ggml_tensor*(ggml_context*, const std::vector<ggml_tensor*>&)>> operations_;
    
public:
    TensorRegistry() = default;
    ~TensorRegistry() = default;
    
    // Shape registration
    void register_shape(const std::string& name, const TensorShape& shape);
    bool has_shape(const std::string& name) const;
    const TensorShape& get_shape(const std::string& name) const;
    std::vector<std::string> get_registered_shapes() const;
    
    // Operation registration
    void register_operation(const std::string& name, 
                          std::function<ggml_tensor*(ggml_context*, const std::vector<ggml_tensor*>&)> operation);
    bool has_operation(const std::string& name) const;
    ggml_tensor* execute_operation(const std::string& name, ggml_context* ctx, 
                                 const std::vector<ggml_tensor*>& inputs);
    
    // Catalog management
    void export_catalog(const std::string& filename) const;
    void import_catalog(const std::string& filename);
    
    // Dynamic composition
    std::vector<size_t> compose_shapes(const std::vector<std::string>& shape_names) const;
    
    // Clear registry
    void clear();
    
    // Statistics
    size_t get_total_parameters() const;
    void print_registry_info() const;
};

} // namespace opencog

#endif // _OPENCOG_TENSOR_REGISTRY_H