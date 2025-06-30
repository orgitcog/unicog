/*
 * HypergraphPatternExtractor.cc
 * 
 * Implementation of Phase II.2: Emergent Pattern Encoding
 */

#include "HypergraphPatternExtractor.h"
#include <algorithm>
#include <sstream>
#include <cmath>
#include <numeric>

namespace opencog {

HypergraphPatternExtractor::HypergraphPatternExtractor(double threshold, int max_complexity)
    : pattern_threshold_(threshold), max_pattern_complexity_(max_complexity),
      self_reflexivity_factor_(0.1)
{
    // Initialize pattern frequency tracking
    pattern_frequencies_.clear();
}

HypergraphPatternExtractor::~HypergraphPatternExtractor()
{
    // Cleanup if needed
}

std::vector<std::string> HypergraphPatternExtractor::extract_emergent_patterns(
    const std::vector<std::string>& hypergraph_nodes,
    const std::vector<std::pair<std::string, std::vector<std::string>>>& hypergraph_edges)
{
    std::vector<std::string> detected_patterns;

    // Look for recurring subgraph structures
    for (size_t i = 0; i < hypergraph_edges.size(); ++i) {
        const auto& edge1 = hypergraph_edges[i];
        
        // Find similar edge patterns
        for (size_t j = i + 1; j < hypergraph_edges.size(); ++j) {
            const auto& edge2 = hypergraph_edges[j];
            
            // Check for structural similarity
            if (edge1.second.size() == edge2.second.size() && 
                edge1.second.size() <= static_cast<size_t>(max_pattern_complexity_)) {
                
                // Calculate similarity based on node relationships
                double similarity = calculate_structural_similarity(edge1, edge2, hypergraph_nodes);
                
                if (similarity > pattern_threshold_) {
                    // Create pattern descriptor
                    std::stringstream pattern_desc;
                    pattern_desc << "pattern_" << edge1.first << "_" << edge2.first 
                                << "_sim_" << std::fixed << std::setprecision(2) << similarity;
                    
                    detected_patterns.push_back(pattern_desc.str());
                }
            }
        }
    }

    // Look for emergent hierarchical patterns
    auto hierarchical_patterns = detect_hierarchical_patterns(hypergraph_nodes, hypergraph_edges);
    detected_patterns.insert(detected_patterns.end(), 
                            hierarchical_patterns.begin(), 
                            hierarchical_patterns.end());

    // Update frequency tracking
    update_pattern_frequencies(detected_patterns);

    return detected_patterns;
}

std::vector<std::string> HypergraphPatternExtractor::reify_patterns_as_links(
    const std::vector<std::string>& detected_patterns)
{
    std::vector<std::string> reified_links;
    
    for (const auto& pattern : detected_patterns) {
        // Create reified hypergraph link identifier
        std::stringstream reified_link;
        reified_link << "reified_link_" << pattern << "_" << std::time(nullptr);
        
        reified_links.push_back(reified_link.str());
        
        // Track reification for self-reflexive learning
        pattern_frequencies_[pattern + "_reified"]++;
    }

    return reified_links;
}

void HypergraphPatternExtractor::self_reflexive_adaptation(
    const std::vector<std::pair<std::string, double>>& detection_history)
{
    if (detection_history.empty()) return;

    // Analyze detection success rates
    double avg_detection_quality = 0.0;
    for (const auto& entry : detection_history) {
        avg_detection_quality += entry.second;
    }
    avg_detection_quality /= detection_history.size();

    // Adapt threshold based on detection quality
    if (avg_detection_quality < 0.5) {
        // Lower threshold to detect more patterns
        pattern_threshold_ *= (1.0 - self_reflexivity_factor_);
        pattern_threshold_ = std::max(0.3, pattern_threshold_);
    } else if (avg_detection_quality > 0.8) {
        // Raise threshold to be more selective
        pattern_threshold_ *= (1.0 + self_reflexivity_factor_);
        pattern_threshold_ = std::min(0.9, pattern_threshold_);
    }

    // Adapt complexity limit based on pattern diversity
    int unique_patterns = pattern_frequencies_.size();
    if (unique_patterns < 5) {
        max_pattern_complexity_ = std::min(30, max_pattern_complexity_ + 2);
    } else if (unique_patterns > 20) {
        max_pattern_complexity_ = std::max(10, max_pattern_complexity_ - 1);
    }
}

std::vector<double> HypergraphPatternExtractor::generate_pattern_feedback(
    const std::vector<std::string>& reified_patterns)
{
    std::vector<double> feedback_signals;
    feedback_signals.reserve(reified_patterns.size());

    for (const auto& pattern : reified_patterns) {
        // Generate feedback strength based on pattern novelty and frequency
        auto freq_it = pattern_frequencies_.find(pattern);
        int frequency = (freq_it != pattern_frequencies_.end()) ? freq_it->second : 1;
        
        // Novel patterns (low frequency) generate stronger feedback
        double novelty_factor = 1.0 / (1.0 + std::log(frequency));
        
        // Pattern complexity influences feedback strength
        double complexity_factor = std::min(1.0, pattern.length() / 50.0);
        
        double feedback_strength = novelty_factor * complexity_factor;
        feedback_signals.push_back(feedback_strength);
    }

    return feedback_signals;
}

double HypergraphPatternExtractor::calculate_pattern_similarity(
    const std::string& pattern1, 
    const std::string& pattern2)
{
    // Simple Levenshtein-inspired similarity
    if (pattern1.empty() || pattern2.empty()) return 0.0;
    
    // Normalized edit distance
    size_t max_len = std::max(pattern1.length(), pattern2.length());
    size_t min_len = std::min(pattern1.length(), pattern2.length());
    
    // Count common characters
    size_t common_chars = 0;
    for (size_t i = 0; i < min_len; ++i) {
        if (pattern1[i] == pattern2[i]) {
            common_chars++;
        }
    }
    
    return static_cast<double>(common_chars) / max_len;
}

void HypergraphPatternExtractor::update_pattern_frequencies(
    const std::vector<std::string>& patterns)
{
    for (const auto& pattern : patterns) {
        pattern_frequencies_[pattern]++;
    }
}

// Helper methods
double HypergraphPatternExtractor::calculate_structural_similarity(
    const std::pair<std::string, std::vector<std::string>>& edge1,
    const std::pair<std::string, std::vector<std::string>>& edge2,
    const std::vector<std::string>& nodes)
{
    // Compare edge connectivity patterns
    if (edge1.second.size() != edge2.second.size()) return 0.0;
    
    // Count matching node relationships
    size_t matching_connections = 0;
    for (size_t i = 0; i < edge1.second.size(); ++i) {
        if (std::find(edge2.second.begin(), edge2.second.end(), edge1.second[i]) 
            != edge2.second.end()) {
            matching_connections++;
        }
    }
    
    return static_cast<double>(matching_connections) / edge1.second.size();
}

std::vector<std::string> HypergraphPatternExtractor::detect_hierarchical_patterns(
    const std::vector<std::string>& nodes,
    const std::vector<std::pair<std::string, std::vector<std::string>>>& edges)
{
    std::vector<std::string> hierarchical_patterns;
    
    // Look for parent-child relationship patterns
    std::map<std::string, std::vector<std::string>> node_children;
    
    for (const auto& edge : edges) {
        if (edge.second.size() >= 2) {
            // First node is parent, others are children
            node_children[edge.second[0]].insert(
                node_children[edge.second[0]].end(),
                edge.second.begin() + 1,
                edge.second.end());
        }
    }
    
    // Detect hierarchical patterns
    for (const auto& parent_children : node_children) {
        if (parent_children.second.size() >= 2) {
            std::stringstream hierarchy_pattern;
            hierarchy_pattern << "hierarchy_" << parent_children.first 
                             << "_children_" << parent_children.second.size();
            hierarchical_patterns.push_back(hierarchy_pattern.str());
        }
    }
    
    return hierarchical_patterns;
}

} // namespace opencog