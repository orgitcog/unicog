/*
 * UnsupervisedLearner.cc - Implementation of Unsupervised Learning Algorithms
 */

#include "UnsupervisedLearner.h"
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FloatValue.h>
#include <algorithm>
#include <random>
#include <cmath>

namespace opencog {
namespace learn {

UnsupervisedLearner::UnsupervisedLearner(AtomSpace* atomspace, const LearnConfig& config) :
    atomspace_(atomspace), 
    config_(config),
    learning_iterations_(0),
    current_error_(1.0)
{
    logger().info("Unsupervised learner initialized with learning rate: %f, max iterations: %d",
                  config_.getLearningRate(), config_.getMaxIterations());
}

UnsupervisedLearner::~UnsupervisedLearner()
{
}

std::vector<Handle> UnsupervisedLearner::discoverPatterns(const std::vector<Handle>& input_atoms)
{
    logger().debug("Discovering patterns from %zu input atoms", input_atoms.size());
    
    std::vector<Handle> discovered_patterns;
    
    if (input_atoms.empty()) {
        return discovered_patterns;
    }
    
    // Group similar atoms together
    auto clusters = clusterAtoms(input_atoms, 3); // Create 3 clusters by default
    
    // Extract patterns from each cluster
    for (const auto& cluster : clusters) {
        if (cluster.size() >= 2) { // Need at least 2 atoms to form a pattern
            Handle pattern = createPatternFromCluster(cluster);
            if (pattern != Handle::UNDEFINED) {
                discovered_patterns.push_back(pattern);
                learned_knowledge_.push_back(pattern);
            }
        }
    }
    
    logger().info("Discovered %zu patterns from %zu clusters", 
                  discovered_patterns.size(), clusters.size());
    return discovered_patterns;
}

std::vector<std::vector<Handle>> UnsupervisedLearner::clusterAtoms(const std::vector<Handle>& atoms, int num_clusters)
{
    if (num_clusters == 0) {
        num_clusters = std::min(3, static_cast<int>(atoms.size())); // Default clustering
    }
    
    logger().debug("Clustering %zu atoms into %d clusters", atoms.size(), num_clusters);
    
    // Use simple k-means style clustering
    std::vector<std::vector<Handle>> clusters(num_clusters);
    
    // Simple clustering based on atom type and name similarity
    for (size_t i = 0; i < atoms.size(); ++i) {
        int cluster_id = i % num_clusters; // Simple round-robin assignment
        clusters[cluster_id].push_back(atoms[i]);
    }
    
    // Try to improve clustering by moving similar atoms together
    bool improved = true;
    int iterations = 0;
    while (improved && iterations < 10) {
        improved = false;
        iterations++;
        
        for (size_t i = 0; i < clusters.size(); ++i) {
            for (size_t j = 0; j < clusters[i].size(); ++j) {
                Handle atom = clusters[i][j];
                
                // Find best cluster for this atom
                int best_cluster = i;
                double best_similarity = 0.0;
                
                for (size_t k = 0; k < clusters.size(); ++k) {
                    if (k == i || clusters[k].empty()) continue;
                    
                    double similarity = calculateSimilarity(atom, clusters[k][0]);
                    if (similarity > best_similarity) {
                        best_similarity = similarity;
                        best_cluster = k;
                    }
                }
                
                // Move atom if we found a better cluster
                if (best_cluster != static_cast<int>(i) && best_similarity > 0.5) {
                    clusters[best_cluster].push_back(atom);
                    clusters[i].erase(clusters[i].begin() + j);
                    improved = true;
                    break;
                }
            }
        }
    }
    
    // Remove empty clusters
    clusters.erase(std::remove_if(clusters.begin(), clusters.end(),
                                  [](const std::vector<Handle>& cluster) { return cluster.empty(); }),
                   clusters.end());
    
    logger().debug("Final clustering resulted in %zu non-empty clusters", clusters.size());
    return clusters;
}

std::vector<Handle> UnsupervisedLearner::mineAssociations(const std::vector<Handle>& atoms, double min_support)
{
    logger().debug("Mining associations from %zu atoms with min support: %f", atoms.size(), min_support);
    
    std::vector<Handle> associations;
    
    // Find frequent atom pairs and create association rules
    std::map<std::pair<Handle, Handle>, int> pair_counts;
    
    for (size_t i = 0; i < atoms.size(); ++i) {
        for (size_t j = i + 1; j < atoms.size(); ++j) {
            std::pair<Handle, Handle> pair = {atoms[i], atoms[j]};
            pair_counts[pair]++;
        }
    }
    
    // Create association links for frequent pairs
    int min_count = static_cast<int>(atoms.size() * min_support);
    for (const auto& pair_count : pair_counts) {
        if (pair_count.second >= min_count) {
            HandleSeq association_outgoing = {pair_count.first.first, pair_count.first.second};
            Handle association = atomspace_->add_link(ASSOCIATIVE_LINK, association_outgoing);
            
            // Add confidence value
            double confidence = static_cast<double>(pair_count.second) / atoms.size();
            FloatValuePtr confidence_value = createFloatValue({confidence});
            association->setValue(atomspace_->add_node(PREDICATE_NODE, "confidence"), confidence_value);
            
            associations.push_back(association);
            learned_knowledge_.push_back(association);
        }
    }
    
    logger().info("Mined %zu associations", associations.size());
    return associations;
}

Handle UnsupervisedLearner::formConcepts(const std::vector<Handle>& examples)
{
    logger().debug("Forming concept from %zu examples", examples.size());
    
    if (examples.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Create a generalized concept from examples
    Handle concept = generalizePattern(examples);
    
    if (concept != Handle::UNDEFINED) {
        learned_knowledge_.push_back(concept);
    }
    
    return concept;
}

std::vector<Handle> UnsupervisedLearner::acquireKnowledge(const std::vector<Handle>& input_data)
{
    logger().info("Acquiring knowledge from %zu input data points", input_data.size());
    
    std::vector<Handle> new_knowledge;
    
    // Discover patterns
    auto patterns = discoverPatterns(input_data);
    new_knowledge.insert(new_knowledge.end(), patterns.begin(), patterns.end());
    
    // Mine associations
    auto associations = mineAssociations(input_data, 0.1);
    new_knowledge.insert(new_knowledge.end(), associations.begin(), associations.end());
    
    // Form concepts
    if (!input_data.empty()) {
        Handle concept = formConcepts(input_data);
        if (concept != Handle::UNDEFINED) {
            new_knowledge.push_back(concept);
        }
    }
    
    learning_iterations_++;
    current_error_ = 1.0 / (1.0 + learning_iterations_); // Simple error reduction
    
    logger().info("Acquired %zu pieces of new knowledge", new_knowledge.size());
    return new_knowledge;
}

void UnsupervisedLearner::incrementalLearn(const Handle& new_example)
{
    logger().debug("Incrementally learning from new example");
    
    // Find similar existing knowledge
    auto similar_atoms = findSimilarAtoms(new_example, 0.8);
    
    if (similar_atoms.empty()) {
        // Create new knowledge from this example
        learned_knowledge_.push_back(new_example);
    } else {
        // Update existing knowledge
        for (const auto& similar_atom : similar_atoms) {
            // Simple update: create association between new example and similar atom
            HandleSeq assoc_outgoing = {new_example, similar_atom};
            Handle association = atomspace_->add_link(ASSOCIATIVE_LINK, assoc_outgoing);
            learned_knowledge_.push_back(association);
        }
    }
    
    learning_iterations_++;
}

std::map<std::string, double> UnsupervisedLearner::getLearningStatistics() const
{
    std::map<std::string, double> stats;
    stats["learning_iterations"] = learning_iterations_;
    stats["current_error"] = current_error_;
    stats["learned_knowledge_count"] = learned_knowledge_.size();
    stats["learning_rate"] = config_.getLearningRate();
    
    return stats;
}

Handle UnsupervisedLearner::extractPattern(const std::vector<Handle>& atoms)
{
    if (atoms.empty() || !atomspace_) {
        return Handle::UNDEFINED;
    }
    
    // Simple pattern extraction - create a pattern link
    HandleSeq pattern_outgoing = atoms;
    Handle pattern = atomspace_->add_link(PATTERN_LINK, pattern_outgoing);
    
    return pattern;
}

std::vector<Handle> UnsupervisedLearner::findSimilarAtoms(const Handle& atom, double threshold)
{
    std::vector<Handle> similar_atoms;
    
    // Simple similarity check based on atom type and name
    for (const auto& learned_atom : learned_knowledge_) {
        double similarity = calculateSimilarity(atom, learned_atom);
        if (similarity >= threshold) {
            similar_atoms.push_back(learned_atom);
        }
    }
    
    return similar_atoms;
}

double UnsupervisedLearner::calculateSimilarity(const Handle& atom1, const Handle& atom2)
{
    if (atom1 == atom2) {
        return 1.0;
    }
    
    // Simple similarity based on type and name
    if (atom1->get_type() != atom2->get_type()) {
        return 0.0;
    }
    
    if (atom1->is_node() && atom2->is_node()) {
        std::string name1 = atom1->get_name();
        std::string name2 = atom2->get_name();
        
        // Simple string similarity (common prefix length)
        size_t common_prefix = 0;
        size_t min_len = std::min(name1.length(), name2.length());
        for (size_t i = 0; i < min_len; ++i) {
            if (name1[i] == name2[i]) {
                common_prefix++;
            } else {
                break;
            }
        }
        
        return static_cast<double>(common_prefix) / std::max(name1.length(), name2.length());
    }
    
    return 0.3; // Default similarity for different link types
}

Handle UnsupervisedLearner::createPatternFromCluster(const std::vector<Handle>& cluster)
{
    if (cluster.empty() || !atomspace_) {
        return Handle::UNDEFINED;
    }
    
    // Create a pattern that represents this cluster
    HandleSeq pattern_outgoing;
    for (const auto& atom : cluster) {
        pattern_outgoing.push_back(atom);
    }
    
    Handle pattern = atomspace_->add_link(PATTERN_LINK, pattern_outgoing);
    
    // Add quality assessment
    double quality = assessPatternQuality(pattern, cluster);
    FloatValuePtr quality_value = createFloatValue({quality});
    pattern->setValue(atomspace_->add_node(PREDICATE_NODE, "pattern_quality"), quality_value);
    
    return pattern;
}

Handle UnsupervisedLearner::generalizePattern(const std::vector<Handle>& examples)
{
    if (examples.empty() || !atomspace_) {
        return Handle::UNDEFINED;
    }
    
    // Simple generalization - create a concept that encompasses all examples
    Handle concept = atomspace_->add_node(CONCEPT_NODE, 
                                         "learned_concept_" + std::to_string(learning_iterations_));
    
    // Connect all examples to this concept
    for (const auto& example : examples) {
        HandleSeq inheritance_outgoing = {example, concept};
        atomspace_->add_link(INHERITANCE_LINK, inheritance_outgoing);
    }
    
    return concept;
}

bool UnsupervisedLearner::validateKnowledge(const Handle& knowledge_atom)
{
    // Simple validation - check if the atom is valid and not empty
    return knowledge_atom != Handle::UNDEFINED && 
           !knowledge_atom->get_name().empty();
}

double UnsupervisedLearner::assessPatternQuality(const Handle& pattern, const std::vector<Handle>& examples)
{
    // Simple quality assessment based on coverage and consistency
    if (examples.empty()) {
        return 0.0;
    }
    
    // Quality is higher for patterns that cover more examples
    double coverage = std::min(1.0, static_cast<double>(examples.size()) / 10.0);
    
    // Add some randomness to simulate more complex quality assessment
    double consistency = 0.7 + 0.3 * (std::hash<std::string>{}(pattern->get_name()) % 100) / 100.0;
    
    return (coverage + consistency) / 2.0;
}

} // namespace learn
} // namespace opencog