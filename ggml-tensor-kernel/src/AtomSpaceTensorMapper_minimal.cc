/*
 * AtomSpaceTensorMapper_minimal.cc
 *
 * Minimal AtomSpace to Tensor mapping implementation with advanced serialization
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <ctime>
#include <cstring>

#include "ggml.h"
#include "ggml-cpu.h"
#include "atomspace_stub.h"

namespace opencog {

// Advanced Atom Types for comprehensive mapping  
// (Note: These are for documentation only - actual types come from atomspace_stub.h)

// Serialization format for Atoms and tensors
struct AtomSerializationData {
    uint64_t atom_id;
    uint32_t atom_type;
    std::vector<float> features;
    std::vector<uint64_t> outgoing_ids;
    std::string name;
    float truth_value_strength;
    float truth_value_confidence;
};

struct TensorSerializationData {
    std::vector<int64_t> dimensions;
    std::vector<float> data;
    std::string tensor_name;
    uint64_t timestamp;
    std::string encoding_type;
};

// Advanced AtomSpace to Tensor Mapper with comprehensive type support
class AdvancedAtomSpaceTensorMapper {
private:
    ggml_context* context_;
    std::unordered_map<Handle, AtomSerializationData> atom_database_;
    std::unordered_map<std::string, TensorSerializationData> tensor_database_;
    
    // Type-specific encoding dimensions
    std::unordered_map<uint32_t, std::vector<size_t>> type_encoding_dims_;
    
    // Feature extraction parameters
    static constexpr size_t BASE_FEATURE_SIZE = 64;
    static constexpr size_t TYPE_FEATURE_SIZE = 16;
    static constexpr size_t STRUCTURE_FEATURE_SIZE = 32;
    static constexpr size_t SEMANTIC_FEATURE_SIZE = 16;

public:
    AdvancedAtomSpaceTensorMapper(ggml_context* ctx) : context_(ctx) {
        initialize_type_encodings();
    }

    void initialize_type_encodings() {
        // Define encoding dimensions for each atom type
        type_encoding_dims_[NODE_TYPE] = {BASE_FEATURE_SIZE};
        type_encoding_dims_[LINK_TYPE] = {BASE_FEATURE_SIZE, 8}; // Links have outgoing structure
        type_encoding_dims_[CONCEPT_NODE] = {BASE_FEATURE_SIZE + SEMANTIC_FEATURE_SIZE};
        type_encoding_dims_[PREDICATE_NODE] = {BASE_FEATURE_SIZE + SEMANTIC_FEATURE_SIZE};
        type_encoding_dims_[EVALUATION_LINK] = {BASE_FEATURE_SIZE, 16}; // Predicate + args
        type_encoding_dims_[INHERITANCE_LINK] = {BASE_FEATURE_SIZE, 8}; // Parent-child
        type_encoding_dims_[SIMILARITY_LINK] = {BASE_FEATURE_SIZE, 8};
        type_encoding_dims_[AND_LINK] = {BASE_FEATURE_SIZE, 16}; // Logic operations
        type_encoding_dims_[OR_LINK] = {BASE_FEATURE_SIZE, 16};
        type_encoding_dims_[LIST_LINK] = {BASE_FEATURE_SIZE, 32}; // Variable length
        type_encoding_dims_[SET_LINK] = {BASE_FEATURE_SIZE, 32};
    }

    // Comprehensive atom registration with full metadata
    void register_atom_complete(const Handle& atom, uint32_t type, const std::string& name = "",
                               const std::vector<Handle>& outgoing = {},
                               float tv_strength = 1.0f, float tv_confidence = 1.0f) {
        AtomSerializationData data;
        data.atom_id = atom.uuid_;
        data.atom_type = type;
        data.name = name;
        data.truth_value_strength = tv_strength;
        data.truth_value_confidence = tv_confidence;
        
        // Convert outgoing handles to IDs
        for (const Handle& out : outgoing) {
            data.outgoing_ids.push_back(out.uuid_);
        }
        
        // Generate comprehensive feature vector
        data.features = generate_comprehensive_features(atom, type, name, outgoing, tv_strength, tv_confidence);
        
        atom_database_[atom] = data;
    }

    // Generate comprehensive feature vector for an atom
    std::vector<float> generate_comprehensive_features(const Handle& atom, uint32_t type, 
                                                     const std::string& name,
                                                     const std::vector<Handle>& outgoing,
                                                     float tv_strength, float tv_confidence) {
        std::vector<float> features;
        features.reserve(BASE_FEATURE_SIZE + TYPE_FEATURE_SIZE + STRUCTURE_FEATURE_SIZE + SEMANTIC_FEATURE_SIZE);
        
        // Base atom features (64 dimensions)
        for (size_t i = 0; i < BASE_FEATURE_SIZE; ++i) {
            float value = 0.0f;
            
            if (i < 8) {
                // Atom ID encoding
                value = static_cast<float>((atom.uuid_ >> (i * 8)) & 0xFF) / 255.0f;
            } else if (i < 16) {
                // Truth value encoding
                value = (i == 8) ? tv_strength : tv_confidence;
            } else if (i < 32) {
                // Positional encoding
                float angle = atom.uuid_ * (i - 15) * 0.1f;
                value = (i % 2 == 0) ? std::sin(angle) : std::cos(angle);
            } else if (i < 48) {
                // Name hash encoding (if available)
                if (!name.empty()) {
                    std::hash<std::string> hasher;
                    size_t name_hash = hasher(name);
                    value = static_cast<float>((name_hash >> ((i - 32) * 2)) & 0x3) / 3.0f;
                }
            } else {
                // Structure-dependent features
                value = std::tanh(static_cast<float>(outgoing.size() * (i - 47)) * 0.1f);
            }
            
            features.push_back(value);
        }
        
        // Type-specific features (16 dimensions)
        for (size_t i = 0; i < TYPE_FEATURE_SIZE; ++i) {
            float value = 0.0f;
            
            if (i < 8) {
                // One-hot-ish encoding for type
                value = (type == (i + 1)) ? 1.0f : 0.0f;
            } else {
                // Type-dependent transformations
                value = std::sin(type * (i - 7) * 0.5f);
            }
            
            features.push_back(value);
        }
        
        // Structural features (32 dimensions)
        for (size_t i = 0; i < STRUCTURE_FEATURE_SIZE; ++i) {
            float value = 0.0f;
            
            if (i < outgoing.size() && i < 16) {
                // Outgoing atom influence
                value = static_cast<float>(outgoing[i].uuid_ % 1000) / 1000.0f;
            } else if (i >= 16 && i < 24) {
                // Arity encoding
                size_t arity = outgoing.size();
                value = (i == 16 + (arity % 8)) ? 1.0f : 0.0f;
            } else {
                // Graph topology features
                value = std::tanh(static_cast<float>(outgoing.size()) * (i - 23) * 0.2f);
            }
            
            features.push_back(value);
        }
        
        // Semantic features (16 dimensions)
        for (size_t i = 0; i < SEMANTIC_FEATURE_SIZE; ++i) {
            float value = 0.0f;
            
            if (!name.empty()) {
                // Name-based semantic encoding
                if (i < name.length() && i < 8) {
                    value = static_cast<float>(name[i]) / 127.0f;
                } else {
                    // Name length and complexity features
                    value = (i == 8) ? std::tanh(static_cast<float>(name.length()) * 0.1f) : 0.0f;
                }
            }
            
            features.push_back(value);
        }
        
        return features;
    }

    // Convert comprehensive atom set to tensor with full type support
    ggml_tensor* atoms_to_comprehensive_tensor(const HandleSet& atoms) {
        if (!context_ || atoms.empty()) return nullptr;
        
        size_t num_atoms = atoms.size();
        size_t total_feature_size = BASE_FEATURE_SIZE + TYPE_FEATURE_SIZE + STRUCTURE_FEATURE_SIZE + SEMANTIC_FEATURE_SIZE;
        
        ggml_tensor* tensor = ggml_new_tensor_2d(context_, GGML_TYPE_F32, total_feature_size, num_atoms);
        if (!tensor) return nullptr;
        
        ggml_set_name(tensor, "comprehensive_atoms_tensor");
        
        float* data = (float*)tensor->data;
        size_t atom_idx = 0;
        
        for (const Handle& atom : atoms) {
            std::vector<float> features;
            
            auto it = atom_database_.find(atom);
            if (it != atom_database_.end()) {
                features = it->second.features;
            } else {
                // Generate default features for unknown atoms
                features = generate_comprehensive_features(atom, NODE_TYPE, "", {}, 1.0f, 1.0f);
            }
            
            // Copy features to tensor
            for (size_t i = 0; i < std::min(features.size(), total_feature_size); ++i) {
                data[atom_idx * total_feature_size + i] = features[i];
            }
            
            atom_idx++;
        }
        
        return tensor;
    }

    // Advanced tensor to atoms reconstruction
    HandleSet comprehensive_tensor_to_atoms(ggml_tensor* tensor) {
        HandleSet atoms;
        if (!tensor || !tensor->data) return atoms;
        
        size_t num_atoms = tensor->ne[1];
        size_t total_feature_size = tensor->ne[0];
        float* data = (float*)tensor->data;
        
        for (size_t atom_idx = 0; atom_idx < num_atoms; ++atom_idx) {
            // Extract atom ID from first 8 features
            uint64_t reconstructed_id = 0;
            for (size_t i = 0; i < 8; ++i) {
                uint8_t byte_val = static_cast<uint8_t>(data[atom_idx * total_feature_size + i] * 255.0f);
                reconstructed_id |= (static_cast<uint64_t>(byte_val) << (i * 8));
            }
            
            Handle reconstructed_atom(reconstructed_id);
            atoms.insert(reconstructed_atom);
            
            // Optionally reconstruct full atom data
            reconstruct_atom_metadata(reconstructed_atom, data + atom_idx * total_feature_size, total_feature_size);
        }
        
        return atoms;
    }

    // Reconstruct atom metadata from tensor features
    void reconstruct_atom_metadata(const Handle& atom, float* features, size_t feature_size) {
        if (feature_size < BASE_FEATURE_SIZE + TYPE_FEATURE_SIZE) return;
        
        AtomSerializationData data;
        data.atom_id = atom.uuid_;
        
        // Extract truth values
        data.truth_value_strength = features[8];
        data.truth_value_confidence = features[9];
        
        // Extract type from type features
        data.atom_type = NODE_TYPE; // Default
        for (size_t i = 0; i < 8 && i < TYPE_FEATURE_SIZE; ++i) {
            if (features[BASE_FEATURE_SIZE + i] > 0.5f) {
                data.atom_type = i + 1;
                break;
            }
        }
        
        // Store comprehensive features
        data.features.assign(features, features + feature_size);
        
        atom_database_[atom] = data;
    }

    // Serialize atom database to file
    bool serialize_atoms_to_file(const std::string& filename) {
        std::ofstream file(filename, std::ios::binary);
        if (!file.is_open()) return false;
        
        // Write header
        uint32_t version = 1;
        uint32_t num_atoms = atom_database_.size();
        file.write(reinterpret_cast<const char*>(&version), sizeof(version));
        file.write(reinterpret_cast<const char*>(&num_atoms), sizeof(num_atoms));
        
        // Write atom data
        for (const auto& pair : atom_database_) {
            const AtomSerializationData& data = pair.second;
            
            // Write basic data
            file.write(reinterpret_cast<const char*>(&data.atom_id), sizeof(data.atom_id));
            file.write(reinterpret_cast<const char*>(&data.atom_type), sizeof(data.atom_type));
            file.write(reinterpret_cast<const char*>(&data.truth_value_strength), sizeof(data.truth_value_strength));
            file.write(reinterpret_cast<const char*>(&data.truth_value_confidence), sizeof(data.truth_value_confidence));
            
            // Write name
            uint32_t name_length = data.name.length();
            file.write(reinterpret_cast<const char*>(&name_length), sizeof(name_length));
            if (name_length > 0) {
                file.write(data.name.c_str(), name_length);
            }
            
            // Write features
            uint32_t feature_count = data.features.size();
            file.write(reinterpret_cast<const char*>(&feature_count), sizeof(feature_count));
            file.write(reinterpret_cast<const char*>(data.features.data()), feature_count * sizeof(float));
            
            // Write outgoing IDs
            uint32_t outgoing_count = data.outgoing_ids.size();
            file.write(reinterpret_cast<const char*>(&outgoing_count), sizeof(outgoing_count));
            file.write(reinterpret_cast<const char*>(data.outgoing_ids.data()), outgoing_count * sizeof(uint64_t));
        }
        
        file.close();
        return true;
    }

    // Deserialize atom database from file
    bool deserialize_atoms_from_file(const std::string& filename) {
        std::ifstream file(filename, std::ios::binary);
        if (!file.is_open()) return false;
        
        // Read header
        uint32_t version, num_atoms;
        file.read(reinterpret_cast<char*>(&version), sizeof(version));
        file.read(reinterpret_cast<char*>(&num_atoms), sizeof(num_atoms));
        
        if (version != 1) return false;
        
        atom_database_.clear();
        
        // Read atom data
        for (uint32_t i = 0; i < num_atoms; ++i) {
            AtomSerializationData data;
            
            // Read basic data
            file.read(reinterpret_cast<char*>(&data.atom_id), sizeof(data.atom_id));
            file.read(reinterpret_cast<char*>(&data.atom_type), sizeof(data.atom_type));
            file.read(reinterpret_cast<char*>(&data.truth_value_strength), sizeof(data.truth_value_strength));
            file.read(reinterpret_cast<char*>(&data.truth_value_confidence), sizeof(data.truth_value_confidence));
            
            // Read name
            uint32_t name_length;
            file.read(reinterpret_cast<char*>(&name_length), sizeof(name_length));
            if (name_length > 0) {
                data.name.resize(name_length);
                file.read(&data.name[0], name_length);
            }
            
            // Read features
            uint32_t feature_count;
            file.read(reinterpret_cast<char*>(&feature_count), sizeof(feature_count));
            data.features.resize(feature_count);
            file.read(reinterpret_cast<char*>(data.features.data()), feature_count * sizeof(float));
            
            // Read outgoing IDs
            uint32_t outgoing_count;
            file.read(reinterpret_cast<char*>(&outgoing_count), sizeof(outgoing_count));
            data.outgoing_ids.resize(outgoing_count);
            file.read(reinterpret_cast<char*>(data.outgoing_ids.data()), outgoing_count * sizeof(uint64_t));
            
            Handle atom(data.atom_id);
            atom_database_[atom] = data;
        }
        
        file.close();
        return true;
    }

    // Serialize tensor to file
    bool serialize_tensor_to_file(ggml_tensor* tensor, const std::string& filename, const std::string& tensor_name = "") {
        if (!tensor || !tensor->data) return false;
        
        std::ofstream file(filename, std::ios::binary);
        if (!file.is_open()) return false;
        
        TensorSerializationData data;
        
        // Fill tensor data
        for (int i = 0; i < GGML_MAX_DIMS; ++i) {
            data.dimensions.push_back(tensor->ne[i]);
        }
        
        size_t total_elements = ggml_nelements(tensor);
        data.data.resize(total_elements);
        memcpy(data.data.data(), tensor->data, total_elements * sizeof(float));
        
        data.tensor_name = tensor_name.empty() ? "tensor" : tensor_name;
        data.timestamp = std::time(nullptr);
        data.encoding_type = "comprehensive_atoms";
        
        // Write header
        uint32_t version = 1;
        file.write(reinterpret_cast<const char*>(&version), sizeof(version));
        
        // Write dimensions
        uint32_t dim_count = data.dimensions.size();
        file.write(reinterpret_cast<const char*>(&dim_count), sizeof(dim_count));
        file.write(reinterpret_cast<const char*>(data.dimensions.data()), dim_count * sizeof(int64_t));
        
        // Write tensor data
        uint64_t data_count = data.data.size();
        file.write(reinterpret_cast<const char*>(&data_count), sizeof(data_count));
        file.write(reinterpret_cast<const char*>(data.data.data()), data_count * sizeof(float));
        
        // Write metadata
        uint32_t name_length = data.tensor_name.length();
        file.write(reinterpret_cast<const char*>(&name_length), sizeof(name_length));
        file.write(data.tensor_name.c_str(), name_length);
        
        file.write(reinterpret_cast<const char*>(&data.timestamp), sizeof(data.timestamp));
        
        uint32_t encoding_length = data.encoding_type.length();
        file.write(reinterpret_cast<const char*>(&encoding_length), sizeof(encoding_length));
        file.write(data.encoding_type.c_str(), encoding_length);
        
        file.close();
        tensor_database_[data.tensor_name] = data;
        return true;
    }

    // Deserialize tensor from file
    ggml_tensor* deserialize_tensor_from_file(const std::string& filename) {
        std::ifstream file(filename, std::ios::binary);
        if (!file.is_open() || !context_) return nullptr;
        
        // Read header
        uint32_t version;
        file.read(reinterpret_cast<char*>(&version), sizeof(version));
        if (version != 1) return nullptr;
        
        TensorSerializationData data;
        
        // Read dimensions
        uint32_t dim_count;
        file.read(reinterpret_cast<char*>(&dim_count), sizeof(dim_count));
        data.dimensions.resize(dim_count);
        file.read(reinterpret_cast<char*>(data.dimensions.data()), dim_count * sizeof(int64_t));
        
        // Read tensor data
        uint64_t data_count;
        file.read(reinterpret_cast<char*>(&data_count), sizeof(data_count));
        data.data.resize(data_count);
        file.read(reinterpret_cast<char*>(data.data.data()), data_count * sizeof(float));
        
        // Read metadata
        uint32_t name_length;
        file.read(reinterpret_cast<char*>(&name_length), sizeof(name_length));
        data.tensor_name.resize(name_length);
        file.read(&data.tensor_name[0], name_length);
        
        file.read(reinterpret_cast<char*>(&data.timestamp), sizeof(data.timestamp));
        
        uint32_t encoding_length;
        file.read(reinterpret_cast<char*>(&encoding_length), sizeof(encoding_length));
        data.encoding_type.resize(encoding_length);
        file.read(&data.encoding_type[0], encoding_length);
        
        // Create tensor
        ggml_tensor* tensor = nullptr;
        if (data.dimensions.size() >= 2) {
            tensor = ggml_new_tensor_2d(context_, GGML_TYPE_F32, data.dimensions[0], data.dimensions[1]);
        } else if (data.dimensions.size() == 1) {
            tensor = ggml_new_tensor_1d(context_, GGML_TYPE_F32, data.dimensions[0]);
        }
        
        if (tensor && !data.data.empty()) {
            memcpy(tensor->data, data.data.data(), data.data.size() * sizeof(float));
            ggml_set_name(tensor, data.tensor_name.c_str());
        }
        
        file.close();
        tensor_database_[data.tensor_name] = data;
        return tensor;
    }

    // Get database statistics
    void print_database_stats() {
        std::cout << "=== AtomSpace Tensor Mapper Statistics ===" << std::endl;
        std::cout << "Total atoms in database: " << atom_database_.size() << std::endl;
        std::cout << "Total tensors in database: " << tensor_database_.size() << std::endl;
        
        // Type distribution
        std::unordered_map<uint32_t, size_t> type_counts;
        for (const auto& pair : atom_database_) {
            type_counts[pair.second.atom_type]++;
        }
        
        std::cout << "\nAtom type distribution:" << std::endl;
        for (const auto& pair : type_counts) {
            std::cout << "  Type " << pair.first << ": " << pair.second << " atoms" << std::endl;
        }
        
        std::cout << "===================================" << std::endl;
    }

    // Clear all data
    void clear_database() {
        atom_database_.clear();
        tensor_database_.clear();
    }

    // Accessor methods
    size_t get_atom_count() const { return atom_database_.size(); }
    size_t get_tensor_count() const { return tensor_database_.size(); }
    
    const std::unordered_map<Handle, AtomSerializationData>& get_atom_database() const {
        return atom_database_;
    }
};

} // namespace opencog