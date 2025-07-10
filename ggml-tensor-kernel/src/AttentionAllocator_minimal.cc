/*
 * AttentionAllocator_minimal.cc
 *
 * Advanced ECAN-based recursive attention allocation with meta-pattern detection
 */

#include <iostream>
#include <algorithm>
#include <cmath>
#include <random>
#include <queue>
#include <unordered_map>
#include <ctime>
#include <numeric>

#include "ggml.h"
#include "ggml-cpu.h"
#include "atomspace_stub.h"

namespace opencog {

// Attention Value Structure
struct AttentionValue {
    float sti = 0.0f;    // Short-term importance
    float lti = 0.0f;    // Long-term importance  
    float vlti = 0.0f;   // Very long-term importance
    float rent = 0.0f;   // Attentional focus rental
    
    AttentionValue() = default;
    AttentionValue(float s, float l, float v = 0.0f, float r = 0.0f) 
        : sti(s), lti(l), vlti(v), rent(r) {}
    
    float total_importance() const {
        return sti + lti + vlti;
    }
};

// Economic Attention Allocation Context
struct ECANContext {
    float total_sti_funds = 10000.0f;
    float total_lti_funds = 10000.0f;
    float rent_rate = 0.1f;
    float decay_rate = 0.01f;
    float minimum_sti = 0.1f;
    float minimum_lti = 0.1f;
    size_t attentional_focus_boundary = 100;
    
    // Meta-pattern thresholds
    float pattern_emergence_threshold = 0.7f;
    float recursive_depth_limit = 5;
    float meta_pattern_amplification = 1.5f;
};

// Meta-Pattern Detection Structure
struct MetaPattern {
    std::vector<Handle> constituent_atoms;
    float coherence_score = 0.0f;
    float emergence_strength = 0.0f;
    int pattern_type = 0; // 0=structural, 1=semantic, 2=temporal, 3=recursive
    size_t detection_count = 1;
    uint64_t first_detected = 0;
    uint64_t last_reinforced = 0;
    
    MetaPattern(const std::vector<Handle>& atoms, float coherence, int type)
        : constituent_atoms(atoms), coherence_score(coherence), pattern_type(type) {
        first_detected = last_reinforced = std::time(nullptr);
    }
    
    void reinforce(float additional_strength) {
        emergence_strength += additional_strength;
        detection_count++;
        last_reinforced = std::time(nullptr);
    }
};

// Advanced Recursive Attention Allocator
class AdvancedRecursiveAttentionAllocator {
private:
    ggml_context* context_;
    std::unordered_map<Handle, AttentionValue> attention_bank_;
    ECANContext ecan_config_;
    
    // Meta-pattern detection
    std::vector<MetaPattern> detected_meta_patterns_;
    std::unordered_map<Handle, std::vector<size_t>> atom_to_patterns_;
    
    // Recursive attention computation
    std::unordered_map<Handle, std::vector<float>> recursive_attention_history_;
    
    // Economy simulation
    float current_sti_pool_ = 0.0f;
    float current_lti_pool_ = 0.0f;

public:
    AdvancedRecursiveAttentionAllocator(ggml_context* ctx) : context_(ctx) {
        current_sti_pool_ = ecan_config_.total_sti_funds;
        current_lti_pool_ = ecan_config_.total_lti_funds;
    }

    // Initialize attention for atom set
    void initialize_attention_economy(const HandleSet& atoms) {
        attention_bank_.clear();
        
        // Distribute initial attention based on estimated importance
        float base_sti = ecan_config_.total_sti_funds / atoms.size();
        float base_lti = ecan_config_.total_lti_funds / atoms.size();
        
        for (const Handle& atom : atoms) {
            // Add some variance based on atom properties
            std::hash<Handle> hasher;
            size_t hash_val = hasher(atom);
            
            float sti_variance = (hash_val % 100) / 500.0f; // ±0.2 variance
            float lti_variance = ((hash_val >> 8) % 100) / 500.0f;
            
            AttentionValue av(
                base_sti + base_sti * sti_variance,
                base_lti + base_lti * lti_variance
            );
            
            attention_bank_[atom] = av;
        }
        
        std::cout << "Initialized attention economy for " << atoms.size() << " atoms" << std::endl;
    }

    // Core recursive attention allocation algorithm
    ggml_tensor* compute_recursive_attention(const HandleSet& atoms, int max_depth = 5) {
        if (!context_ || atoms.empty()) return nullptr;
        
        size_t num_atoms = atoms.size();
        max_depth = std::min(max_depth, static_cast<int>(ecan_config_.recursive_depth_limit));
        
        // Create attention evolution tensor [atoms x depth x features]
        ggml_tensor* attention_tensor = ggml_new_tensor_3d(context_, GGML_TYPE_F32, 
                                                          num_atoms, max_depth, 4); // STI, LTI, VLTI, RENT
        
        float* data = (float*)attention_tensor->data;
        std::vector<Handle> atom_list(atoms.begin(), atoms.end());
        
        // Initialize first layer with current attention values
        for (size_t i = 0; i < num_atoms; ++i) {
            Handle atom = atom_list[i];
            AttentionValue av = get_attention_value(atom);
            
            size_t base_idx = i * max_depth * 4; // atom * depth * features
            data[base_idx + 0] = av.sti;        // STI at depth 0
            data[base_idx + 1] = av.lti;        // LTI at depth 0
            data[base_idx + 2] = av.vlti;       // VLTI at depth 0
            data[base_idx + 3] = av.rent;       // RENT at depth 0
        }
        
        // Recursive attention propagation
        for (int depth = 1; depth < max_depth; ++depth) {
            compute_attention_propagation_layer(data, atom_list, depth, max_depth);
            apply_economic_constraints(data, atom_list, depth, max_depth);
            detect_and_amplify_meta_patterns(data, atom_list, depth, max_depth);
        }
        
        // Update attention bank with final values
        update_attention_bank_from_tensor(data, atom_list, max_depth);
        
        ggml_set_name(attention_tensor, "recursive_attention_evolution");
        return attention_tensor;
    }

    // Compute attention propagation for a specific layer
    void compute_attention_propagation_layer(float* data, const std::vector<Handle>& atoms, 
                                           int depth, int max_depth) {
        size_t num_atoms = atoms.size();
        
        for (size_t i = 0; i < num_atoms; ++i) {
            Handle atom_i = atoms[i];
            
            // Base indices for current and previous layer
            size_t curr_base = i * max_depth * 4 + depth * 4;
            size_t prev_base = i * max_depth * 4 + (depth - 1) * 4;
            
            // Get previous layer values
            float prev_sti = data[prev_base + 0];
            float prev_lti = data[prev_base + 1];
            float prev_vlti = data[prev_base + 2];
            float prev_rent = data[prev_base + 3];
            
            // Compute influences from other atoms
            float sti_influence = 0.0f;
            float lti_influence = 0.0f;
            float vlti_influence = 0.0f;
            
            for (size_t j = 0; j < num_atoms; ++j) {
                if (i == j) continue;
                
                Handle atom_j = atoms[j];
                size_t other_prev_base = j * max_depth * 4 + (depth - 1) * 4;
                
                // Compute relationship strength
                float relationship = compute_relationship_strength(atom_i, atom_j);
                
                // Attention flow based on relationship and economic pressure
                float other_sti = data[other_prev_base + 0];
                float other_lti = data[other_prev_base + 1];
                float other_vlti = data[other_prev_base + 2];
                
                float flow_rate = relationship * ecan_config_.rent_rate;
                
                sti_influence += other_sti * flow_rate * 0.1f;  // STI flows quickly
                lti_influence += other_lti * flow_rate * 0.05f; // LTI flows slower
                vlti_influence += other_vlti * flow_rate * 0.01f; // VLTI flows very slowly
            }
            
            // Apply decay and influence
            float decay = std::pow(1.0f - ecan_config_.decay_rate, depth);
            
            data[curr_base + 0] = (prev_sti * decay) + sti_influence;   // STI
            data[curr_base + 1] = (prev_lti * decay) + lti_influence;   // LTI  
            data[curr_base + 2] = (prev_vlti * decay) + vlti_influence; // VLTI
            data[curr_base + 3] = prev_rent + (prev_sti * ecan_config_.rent_rate * depth); // RENT
        }
    }

    // Apply economic constraints and normalize funds
    void apply_economic_constraints(float* data, const std::vector<Handle>& atoms, 
                                  int depth, int max_depth) {
        size_t num_atoms = atoms.size();
        
        // Calculate total funds at this layer
        float total_sti = 0.0f;
        float total_lti = 0.0f;
        
        for (size_t i = 0; i < num_atoms; ++i) {
            size_t curr_base = i * max_depth * 4 + depth * 4;
            total_sti += data[curr_base + 0];
            total_lti += data[curr_base + 1];
        }
        
        // Normalize to maintain economic constraints
        if (total_sti > ecan_config_.total_sti_funds) {
            float sti_scale = ecan_config_.total_sti_funds / total_sti;
            for (size_t i = 0; i < num_atoms; ++i) {
                size_t curr_base = i * max_depth * 4 + depth * 4;
                data[curr_base + 0] *= sti_scale;
            }
        }
        
        if (total_lti > ecan_config_.total_lti_funds) {
            float lti_scale = ecan_config_.total_lti_funds / total_lti;
            for (size_t i = 0; i < num_atoms; ++i) {
                size_t curr_base = i * max_depth * 4 + depth * 4;
                data[curr_base + 1] *= lti_scale;
            }
        }
        
        // Enforce minimum values
        for (size_t i = 0; i < num_atoms; ++i) {
            size_t curr_base = i * max_depth * 4 + depth * 4;
            data[curr_base + 0] = std::max(data[curr_base + 0], ecan_config_.minimum_sti);
            data[curr_base + 1] = std::max(data[curr_base + 1], ecan_config_.minimum_lti);
        }
    }

    // Detect meta-patterns and amplify attention
    void detect_and_amplify_meta_patterns(float* data, const std::vector<Handle>& atoms, 
                                         int depth, int max_depth) {
        if (depth < 2) return; // Need at least 2 layers for pattern detection
        
        size_t num_atoms = atoms.size();
        
        // Detect emergent patterns in attention flow
        for (size_t i = 0; i < num_atoms; ++i) {
            std::vector<float> attention_trajectory;
            
            // Extract attention trajectory for this atom
            for (int d = 0; d <= depth; ++d) {
                size_t base = i * max_depth * 4 + d * 4;
                float total_attention = data[base + 0] + data[base + 1] + data[base + 2];
                attention_trajectory.push_back(total_attention);
            }
            
            // Detect patterns in trajectory
            float coherence = compute_trajectory_coherence(attention_trajectory);
            
            if (coherence > ecan_config_.pattern_emergence_threshold) {
                // Pattern detected - amplify attention
                Handle atom = atoms[i];
                amplify_meta_pattern_attention(data, i, depth, max_depth, coherence);
                
                // Record meta-pattern
                record_meta_pattern(atom, coherence, depth);
            }
        }
        
        // Detect inter-atom patterns
        detect_collective_meta_patterns(data, atoms, depth, max_depth);
    }

    // Compute coherence in attention trajectory
    float compute_trajectory_coherence(const std::vector<float>& trajectory) {
        if (trajectory.size() < 3) return 0.0f;
        
        // Compute auto-correlation and trend consistency
        float mean = 0.0f;
        for (float val : trajectory) mean += val;
        mean /= trajectory.size();
        
        float variance = 0.0f;
        for (float val : trajectory) {
            variance += (val - mean) * (val - mean);
        }
        variance /= trajectory.size();
        
        if (variance == 0.0f) return 1.0f; // Perfect coherence (constant)
        
        // Compute trend strength
        float trend_sum = 0.0f;
        for (size_t i = 1; i < trajectory.size(); ++i) {
            trend_sum += trajectory[i] - trajectory[i-1];
        }
        
        float trend_strength = std::abs(trend_sum) / (trajectory.size() - 1);
        float normalized_trend = std::tanh(trend_strength / std::sqrt(variance));
        
        return normalized_trend;
    }

    // Amplify attention for detected meta-patterns
    void amplify_meta_pattern_attention(float* data, size_t atom_idx, int depth, 
                                      int max_depth, float coherence) {
        size_t curr_base = atom_idx * max_depth * 4 + depth * 4;
        
        float amplification = ecan_config_.meta_pattern_amplification * coherence;
        
        data[curr_base + 0] *= amplification; // Amplify STI
        data[curr_base + 2] += coherence * 10.0f; // Boost VLTI for long-term pattern memory
    }

    // Detect collective meta-patterns among multiple atoms
    void detect_collective_meta_patterns(float* data, const std::vector<Handle>& atoms, 
                                       int depth, int max_depth) {
        size_t num_atoms = atoms.size();
        
        // Look for synchronized attention changes
        for (size_t i = 0; i < num_atoms; ++i) {
            for (size_t j = i + 1; j < num_atoms; ++j) {
                float synchronization = compute_attention_synchronization(data, i, j, depth, max_depth);
                
                if (synchronization > 0.8f) {
                    // High synchronization detected - boost both atoms
                    size_t base_i = i * max_depth * 4 + depth * 4;
                    size_t base_j = j * max_depth * 4 + depth * 4;
                    
                    float boost = synchronization * 0.2f;
                    data[base_i + 1] += boost; // Boost LTI
                    data[base_j + 1] += boost;
                    
                    // Record collective pattern
                    record_collective_meta_pattern({atoms[i], atoms[j]}, synchronization, depth);
                }
            }
        }
    }

    // Compute attention synchronization between two atoms
    float compute_attention_synchronization(float* data, size_t atom_i, size_t atom_j, 
                                          int depth, int max_depth) {
        if (depth < 2) return 0.0f;
        
        std::vector<float> traj_i, traj_j;
        
        for (int d = 0; d <= depth; ++d) {
            size_t base_i = atom_i * max_depth * 4 + d * 4;
            size_t base_j = atom_j * max_depth * 4 + d * 4;
            
            float total_i = data[base_i + 0] + data[base_i + 1] + data[base_i + 2];
            float total_j = data[base_j + 0] + data[base_j + 1] + data[base_j + 2];
            
            traj_i.push_back(total_i);
            traj_j.push_back(total_j);
        }
        
        // Compute correlation
        return compute_correlation(traj_i, traj_j);
    }

    // Compute correlation between two trajectories
    float compute_correlation(const std::vector<float>& a, const std::vector<float>& b) {
        if (a.size() != b.size() || a.empty()) return 0.0f;
        
        float mean_a = 0.0f, mean_b = 0.0f;
        for (size_t i = 0; i < a.size(); ++i) {
            mean_a += a[i];
            mean_b += b[i];
        }
        mean_a /= a.size();
        mean_b /= b.size();
        
        float num = 0.0f, den_a = 0.0f, den_b = 0.0f;
        for (size_t i = 0; i < a.size(); ++i) {
            float diff_a = a[i] - mean_a;
            float diff_b = b[i] - mean_b;
            num += diff_a * diff_b;
            den_a += diff_a * diff_a;
            den_b += diff_b * diff_b;
        }
        
        float denominator = std::sqrt(den_a * den_b);
        return (denominator > 0.0f) ? num / denominator : 0.0f;
    }

    // Record detected meta-pattern
    void record_meta_pattern(const Handle& atom, float coherence, int depth) {
        // Check if pattern already exists
        for (MetaPattern& pattern : detected_meta_patterns_) {
            if (pattern.constituent_atoms.size() == 1 && 
                pattern.constituent_atoms[0] == atom) {
                pattern.reinforce(coherence);
                return;
            }
        }
        
        // Create new meta-pattern
        MetaPattern new_pattern({atom}, coherence, 3); // Type 3 = recursive
        new_pattern.emergence_strength = coherence * depth;
        detected_meta_patterns_.push_back(new_pattern);
        
        // Update atom-to-pattern mapping
        atom_to_patterns_[atom].push_back(detected_meta_patterns_.size() - 1);
    }

    // Record collective meta-pattern
    void record_collective_meta_pattern(const std::vector<Handle>& atoms, float coherence, int depth) {
        MetaPattern new_pattern(atoms, coherence, 0); // Type 0 = structural
        new_pattern.emergence_strength = coherence * depth * atoms.size();
        detected_meta_patterns_.push_back(new_pattern);
        
        // Update mappings
        size_t pattern_idx = detected_meta_patterns_.size() - 1;
        for (const Handle& atom : atoms) {
            atom_to_patterns_[atom].push_back(pattern_idx);
        }
    }

    // Update attention bank from tensor computation
    void update_attention_bank_from_tensor(float* data, const std::vector<Handle>& atoms, int max_depth) {
        size_t num_atoms = atoms.size();
        int final_depth = max_depth - 1;
        
        for (size_t i = 0; i < num_atoms; ++i) {
            Handle atom = atoms[i];
            size_t final_base = i * max_depth * 4 + final_depth * 4;
            
            AttentionValue av(
                data[final_base + 0], // STI
                data[final_base + 1], // LTI
                data[final_base + 2], // VLTI
                data[final_base + 3]  // RENT
            );
            
            attention_bank_[atom] = av;
            
            // Store attention history
            std::vector<float> history;
            for (int d = 0; d < max_depth; ++d) {
                size_t base = i * max_depth * 4 + d * 4;
                history.push_back(data[base + 0] + data[base + 1] + data[base + 2]);
            }
            recursive_attention_history_[atom] = history;
        }
    }

    // Compute relationship strength between atoms
    float compute_relationship_strength(const Handle& atom_i, const Handle& atom_j) {
        // Simple hash-based relationship (can be enhanced with actual graph structure)
        uint64_t combined = atom_i.uuid_ ^ atom_j.uuid_;
        float base_strength = (combined % 1000) / 1000.0f;
        
        // Enhance with meta-pattern co-occurrence
        float pattern_boost = compute_pattern_cooccurrence(atom_i, atom_j);
        
        return std::min(1.0f, base_strength + pattern_boost);
    }

    // Compute pattern co-occurrence strength
    float compute_pattern_cooccurrence(const Handle& atom_i, const Handle& atom_j) {
        auto it_i = atom_to_patterns_.find(atom_i);
        auto it_j = atom_to_patterns_.find(atom_j);
        
        if (it_i == atom_to_patterns_.end() || it_j == atom_to_patterns_.end()) {
            return 0.0f;
        }
        
        // Count shared patterns
        size_t shared_patterns = 0;
        for (size_t pattern_i : it_i->second) {
            for (size_t pattern_j : it_j->second) {
                if (pattern_i == pattern_j) {
                    shared_patterns++;
                }
            }
        }
        
        if (shared_patterns > 0) {
            return std::min(0.5f, shared_patterns * 0.1f);
        }
        
        return 0.0f;
    }

    // Get attention value for an atom
    AttentionValue get_attention_value(const Handle& atom) {
        auto it = attention_bank_.find(atom);
        return (it != attention_bank_.end()) ? it->second : AttentionValue();
    }

    // Get attentional focus (top-K atoms by STI)
    std::vector<Handle> get_attentional_focus(size_t k = 0) {
        if (k == 0) k = ecan_config_.attentional_focus_boundary;
        
        std::vector<std::pair<Handle, float>> sti_pairs;
        for (const auto& pair : attention_bank_) {
            sti_pairs.emplace_back(pair.first, pair.second.sti);
        }
        
        std::partial_sort(sti_pairs.begin(), 
                         sti_pairs.begin() + std::min(k, sti_pairs.size()),
                         sti_pairs.end(),
                         [](const auto& a, const auto& b) { return a.second > b.second; });
        
        std::vector<Handle> focus;
        for (size_t i = 0; i < std::min(k, sti_pairs.size()); ++i) {
            focus.push_back(sti_pairs[i].first);
        }
        
        return focus;
    }

    // Print meta-pattern analysis
    void print_meta_pattern_analysis() {
        std::cout << "\n=== Meta-Pattern Analysis ===" << std::endl;
        std::cout << "Total detected meta-patterns: " << detected_meta_patterns_.size() << std::endl;
        
        // Sort patterns by emergence strength
        std::vector<size_t> pattern_indices(detected_meta_patterns_.size());
        std::iota(pattern_indices.begin(), pattern_indices.end(), 0);
        
        std::sort(pattern_indices.begin(), pattern_indices.end(), 
                 [this](size_t a, size_t b) {
                     return detected_meta_patterns_[a].emergence_strength > 
                            detected_meta_patterns_[b].emergence_strength;
                 });
        
        std::cout << "\nTop meta-patterns by emergence strength:" << std::endl;
        for (size_t i = 0; i < std::min(size_t(10), pattern_indices.size()); ++i) {
            const MetaPattern& pattern = detected_meta_patterns_[pattern_indices[i]];
            std::cout << "  Pattern " << pattern_indices[i] << ": ";
            std::cout << "atoms=" << pattern.constituent_atoms.size() << ", ";
            std::cout << "strength=" << pattern.emergence_strength << ", ";
            std::cout << "coherence=" << pattern.coherence_score << ", ";
            std::cout << "detected=" << pattern.detection_count << " times" << std::endl;
        }
        
        std::cout << "=========================" << std::endl;
    }

    // Get configuration
    const ECANContext& get_config() const { return ecan_config_; }
    ECANContext& get_config() { return ecan_config_; }
    
    // Get detected meta-patterns
    const std::vector<MetaPattern>& get_meta_patterns() const { return detected_meta_patterns_; }
    
    // Clear all data
    void clear_all() {
        attention_bank_.clear();
        detected_meta_patterns_.clear();
        atom_to_patterns_.clear();
        recursive_attention_history_.clear();
        current_sti_pool_ = ecan_config_.total_sti_funds;
        current_lti_pool_ = ecan_config_.total_lti_funds;
    }
};

} // namespace opencog