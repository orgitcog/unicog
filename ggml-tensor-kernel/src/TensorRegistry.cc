/*
 * TensorRegistry.cc
 *
 * Implementation of the tensor registry for cognitive modules
 */

#include <opencog/tensor/TensorRegistry.h>
#include <opencog/tensor/TensorKernel.h>
#include <opencog/util/Logger.h>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace opencog;

void TensorRegistry::register_shape(const std::string& name, const TensorShape& shape) {
    registered_shapes_[name] = shape;
    logger().debug("TensorRegistry") << "Registered shape: " << name;
}

bool TensorRegistry::has_shape(const std::string& name) const {
    return registered_shapes_.find(name) != registered_shapes_.end();
}

const TensorShape& TensorRegistry::get_shape(const std::string& name) const {
    auto it = registered_shapes_.find(name);
    if (it != registered_shapes_.end()) {
        return it->second;
    }
    
    static TensorShape empty_shape("", {}, "");
    logger().warn("TensorRegistry") << "Shape not found: " << name;
    return empty_shape;
}

std::vector<std::string> TensorRegistry::get_registered_shapes() const {
    std::vector<std::string> names;
    for (const auto& pair : registered_shapes_) {
        names.push_back(pair.first);
    }
    return names;
}

void TensorRegistry::register_operation(const std::string& name, 
                                       std::function<ggml_tensor*(ggml_context*, const std::vector<ggml_tensor*>&)> operation) {
    operations_[name] = operation;
    logger().debug("TensorRegistry") << "Registered operation: " << name;
}

bool TensorRegistry::has_operation(const std::string& name) const {
    return operations_.find(name) != operations_.end();
}

ggml_tensor* TensorRegistry::execute_operation(const std::string& name, ggml_context* ctx, 
                                             const std::vector<ggml_tensor*>& inputs) {
    auto it = operations_.find(name);
    if (it != operations_.end()) {
        return it->second(ctx, inputs);
    }
    
    logger().warn("TensorRegistry") << "Operation not found: " << name;
    return nullptr;
}

void TensorRegistry::export_catalog(const std::string& filename) const {
    std::ofstream file(filename);
    if (!file.is_open()) {
        logger().error("TensorRegistry") << "Cannot open file for export: " << filename;
        return;
    }
    
    file << "# Tensor Registry Catalog\n";
    file << "# Format: name:dimensions:description\n\n";
    
    for (const auto& pair : registered_shapes_) {
        const auto& shape = pair.second;
        file << shape.module_name << ":";
        
        for (size_t i = 0; i < shape.dimensions.size(); ++i) {
            file << shape.dimensions[i];
            if (i < shape.dimensions.size() - 1) {
                file << "x";
            }
        }
        
        file << ":" << shape.description << "\n";
    }
    
    file.close();
    logger().info("TensorRegistry") << "Exported catalog to: " << filename;
}

void TensorRegistry::import_catalog(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        logger().error("TensorRegistry") << "Cannot open file for import: " << filename;
        return;
    }
    
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty() || line[0] == '#') {
            continue;
        }
        
        std::istringstream iss(line);
        std::string name, dimensions_str, description;
        
        if (std::getline(iss, name, ':') && 
            std::getline(iss, dimensions_str, ':') && 
            std::getline(iss, description)) {
            
            // Parse dimensions
            std::vector<size_t> dimensions;
            std::istringstream dim_stream(dimensions_str);
            std::string dim_str;
            
            while (std::getline(dim_stream, dim_str, 'x')) {
                dimensions.push_back(std::stoul(dim_str));
            }
            
            TensorShape shape(name, dimensions, description);
            register_shape(name, shape);
        }
    }
    
    file.close();
    logger().info("TensorRegistry") << "Imported catalog from: " << filename;
}

std::vector<size_t> TensorRegistry::compose_shapes(const std::vector<std::string>& shape_names) const {
    std::vector<size_t> composed_shape;
    
    for (const auto& name : shape_names) {
        if (has_shape(name)) {
            const auto& shape = get_shape(name);
            composed_shape.insert(composed_shape.end(), 
                                 shape.dimensions.begin(), 
                                 shape.dimensions.end());
        }
    }
    
    return composed_shape;
}

void TensorRegistry::clear() {
    registered_shapes_.clear();
    operations_.clear();
    logger().info("TensorRegistry") << "Cleared registry";
}

size_t TensorRegistry::get_total_parameters() const {
    size_t total = 0;
    for (const auto& pair : registered_shapes_) {
        total += pair.second.total_elements();
    }
    return total;
}

void TensorRegistry::print_registry_info() const {
    std::cout << "=== Tensor Registry Information ===" << std::endl;
    std::cout << "Registered shapes: " << registered_shapes_.size() << std::endl;
    std::cout << "Registered operations: " << operations_.size() << std::endl;
    std::cout << "Total parameters: " << get_total_parameters() << std::endl;
    std::cout << std::endl;
    
    std::cout << "Shapes:" << std::endl;
    for (const auto& pair : registered_shapes_) {
        const auto& shape = pair.second;
        std::cout << "  " << shape.module_name << ": ";
        for (size_t i = 0; i < shape.dimensions.size(); ++i) {
            std::cout << shape.dimensions[i];
            if (i < shape.dimensions.size() - 1) {
                std::cout << "x";
            }
        }
        std::cout << " (" << shape.total_elements() << " elements)";
        if (!shape.description.empty()) {
            std::cout << " - " << shape.description;
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
    
    std::cout << "Operations:" << std::endl;
    for (const auto& pair : operations_) {
        std::cout << "  " << pair.first << std::endl;
    }
}