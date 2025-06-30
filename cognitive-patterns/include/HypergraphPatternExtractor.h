/*
 * HypergraphPatternExtractor.h
 * 
 * Phase II.2: Emergent Pattern Encoding (Hypergraph Synergy)
 * Pattern extraction routines for self-reflexive learning
 */

#ifndef _OPENCOG_HYPERGRAPH_PATTERN_EXTRACTOR_H
#define _OPENCOG_HYPERGRAPH_PATTERN_EXTRACTOR_H

#include <memory>
#include <vector>
#include <map>
#include <string>

namespace opencog {

// Forward declarations (would normally include atomspace headers)
class Handle;
class AtomSpace;

/**
 * Hypergraph Pattern Extractor
 * 
 * Implements recursive pattern detection and reification in hypergraph structures.
 * Recursive pathway: Patterns detected → Reified into new hypergraph links 
 * → Fed back into perception loop
 */
class HypergraphPatternExtractor
{
private:
    // Pattern detection threshold
    double pattern_threshold_;
    
    // Maximum pattern complexity (number of nodes/links)
    int max_pattern_complexity_;
    
    // Pattern frequency tracking for emergence detection
    std::map<std::string, int> pattern_frequencies_;
    
    // Self-reflexive learning parameters
    double self_reflexivity_factor_;

public:
    HypergraphPatternExtractor(double threshold = 0.7, int max_complexity = 20);
    ~HypergraphPatternExtractor();

    /**
     * Extract emergent patterns from hypergraph structure
     * 
     * @param hypergraph_nodes Vector of node identifiers in the hypergraph
     * @param hypergraph_edges Vector of edge structures (node connections)
     * @return Vector of detected pattern structures
     */
    std::vector<std::string> extract_emergent_patterns(
        const std::vector<std::string>& hypergraph_nodes,
        const std::vector<std::pair<std::string, std::vector<std::string>>>& hypergraph_edges);

    /**
     * Reify detected patterns as new hypergraph links
     * Recursive implementation: Pattern becomes new cognitive object
     * 
     * @param detected_patterns Vector of pattern structures to reify
     * @return Vector of new hypergraph link identifiers
     */
    std::vector<std::string> reify_patterns_as_links(
        const std::vector<std::string>& detected_patterns);

    /**
     * Self-reflexive learning mechanism
     * Analyzes its own pattern detection behavior and adapts
     * 
     * @param detection_history History of pattern detections
     * @return Updated detection parameters
     */
    void self_reflexive_adaptation(
        const std::vector<std::pair<std::string, double>>& detection_history);

    /**
     * Feed patterns back into perception loop
     * Recursive pathway: reified patterns influence future perception
     * 
     * @param reified_patterns New hypergraph links from pattern reification
     * @return Feedback signal strengths for attention allocation
     */
    std::vector<double> generate_pattern_feedback(
        const std::vector<std::string>& reified_patterns);

    /**
     * Detect structural similarities and emergent hierarchies
     * 
     * @param pattern1 First pattern structure
     * @param pattern2 Second pattern structure  
     * @return Similarity score (0.0 to 1.0)
     */
    double calculate_pattern_similarity(
        const std::string& pattern1, 
        const std::string& pattern2);

    /**
     * Update pattern frequency tracking for emergence detection
     */
    void update_pattern_frequencies(const std::vector<std::string>& patterns);

    /**
     * Get current pattern emergence statistics
     */
    std::map<std::string, int> get_pattern_frequencies() const { 
        return pattern_frequencies_; 
    }

    /**
     * Set pattern detection threshold
     */
    void set_pattern_threshold(double threshold) { 
        pattern_threshold_ = threshold; 
    }

private:
    /**
     * Helper method to calculate structural similarity between edges
     */
    double calculate_structural_similarity(
        const std::pair<std::string, std::vector<std::string>>& edge1,
        const std::pair<std::string, std::vector<std::string>>& edge2,
        const std::vector<std::string>& nodes);

    /**
     * Helper method to detect hierarchical patterns in hypergraph
     */
    std::vector<std::string> detect_hierarchical_patterns(
        const std::vector<std::string>& nodes,
        const std::vector<std::pair<std::string, std::vector<std::string>>>& edges);
};

} // namespace opencog

#endif // _OPENCOG_HYPERGRAPH_PATTERN_EXTRACTOR_H