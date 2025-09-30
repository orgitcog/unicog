/*
 * UnsupervisedLearner.h - Unsupervised Learning Module for OpenCog
 *
 * Copyright (C) 2025 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_UNSUPERVISED_LEARNER_H
#define _OPENCOG_UNSUPERVISED_LEARNER_H

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <set>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace main {

/**
 * Pattern - Represents a discovered pattern in the data
 */
struct Pattern {
    Handle pattern_handle;
    double frequency;
    double surprise_value;
    double support;
    double confidence;
    std::vector<Handle> instances;
    std::string pattern_type;
    size_t pattern_size;
};

/**
 * Cluster - Represents a cluster of similar atoms/patterns
 */
struct Cluster {
    Handle cluster_center;
    std::vector<Handle> members;
    double cohesion;
    double separation;
    std::string cluster_label;
};

/**
 * LearningResult - Contains the results of unsupervised learning
 */
struct LearningResult {
    std::vector<Pattern> discovered_patterns;
    std::vector<Cluster> formed_clusters;
    std::map<Handle, Handle> concept_hierarchy;
    double learning_quality_score;
    size_t iterations_performed;
};

/**
 * UnsupervisedLearner - Unsupervised learning algorithms implementation
 * 
 * This class provides various unsupervised learning algorithms including
 * pattern mining, clustering, and concept formation integrated with
 * OpenCog's AtomSpace.
 */
class UnsupervisedLearner {
public:
    /**
     * Constructor
     * @param atomspace Pointer to the AtomSpace for learning
     */
    UnsupervisedLearner(AtomSpace* atomspace);
    
    /**
     * Destructor
     */
    ~UnsupervisedLearner();
    
    /**
     * Perform pattern mining on input data
     * @param input_atoms Vector of atoms to mine patterns from
     * @param min_support Minimum support threshold (0.0 to 1.0)
     * @param min_confidence Minimum confidence threshold (0.0 to 1.0)
     * @return Vector of discovered patterns
     */
    std::vector<Pattern> minePatterns(const std::vector<Handle>& input_atoms,
                                     double min_support = 0.01,
                                     double min_confidence = 0.5);
    
    /**
     * Perform clustering on input data
     * @param input_atoms Vector of atoms to cluster
     * @param num_clusters Target number of clusters (0 for automatic)
     * @param similarity_threshold Threshold for cluster membership
     * @return Vector of formed clusters
     */
    std::vector<Cluster> performClustering(const std::vector<Handle>& input_atoms,
                                          size_t num_clusters = 0,
                                          double similarity_threshold = 0.7);
    
    /**
     * Perform concept formation from patterns and clusters
     * @param patterns Vector of patterns
     * @param clusters Vector of clusters
     * @return Map of concept hierarchy (child -> parent)
     */
    std::map<Handle, Handle> formConcepts(const std::vector<Pattern>& patterns,
                                         const std::vector<Cluster>& clusters);
    
    /**
     * Run complete unsupervised learning pipeline
     * @param input_atoms Vector of atoms to learn from
     * @return Complete learning results
     */
    LearningResult runLearningPipeline(const std::vector<Handle>& input_atoms);
    
    /**
     * Set learning parameters
     */
    void setPatternMinSupport(double support) { pattern_min_support_ = support; }
    void setPatternMinConfidence(double confidence) { pattern_min_confidence_ = confidence; }
    void setMaxPatterns(size_t max_patterns) { max_patterns_ = max_patterns; }
    void setSurpriseThreshold(double threshold) { surprise_threshold_ = threshold; }
    void setClusteringMethod(const std::string& method) { clustering_method_ = method; }
    void setMaxIterations(size_t iterations) { max_iterations_ = iterations; }
    
    /**
     * Get learning statistics
     */
    size_t getTotalPatternsDiscovered() const { return total_patterns_discovered_; }
    size_t getTotalClustersFormed() const { return total_clusters_formed_; }
    size_t getTotalConceptsCreated() const { return total_concepts_created_; }
    double getAverageLearningTime() const {
        return learning_cycles_ > 0 ? total_learning_time_ / learning_cycles_ : 0.0;
    }

private:
    AtomSpace* atomspace_;
    
    // Learning parameters
    double pattern_min_support_;
    double pattern_min_confidence_;
    size_t max_patterns_;
    double surprise_threshold_;
    std::string clustering_method_;
    size_t max_iterations_;
    double convergence_threshold_;
    
    // Statistics
    size_t total_patterns_discovered_;
    size_t total_clusters_formed_;
    size_t total_concepts_created_;
    size_t learning_cycles_;
    double total_learning_time_;
    
    // Pattern mining methods
    std::map<HandleSeq, int> findFrequentItemsets(const std::vector<Handle>& atoms,
                                                  double min_support);
    std::vector<Pattern> generateAssociationRules(const std::map<HandleSeq, int>& itemsets,
                                                 double min_confidence);
    double calculateSurprise(const Pattern& pattern);
    std::vector<Handle> findPatternInstances(const HandleSeq& pattern_template,
                                            const std::vector<Handle>& search_space);
    
    // Clustering methods
    std::vector<Cluster> kMeansClustering(const std::vector<Handle>& atoms,
                                         size_t k);
    std::vector<Cluster> hierarchicalClustering(const std::vector<Handle>& atoms,
                                               double threshold);
    double calculateSimilarity(const Handle& atom1, const Handle& atom2);
    Handle findClusterCenter(const std::vector<Handle>& cluster_members);
    double calculateClusterCohesion(const Cluster& cluster);
    double calculateClusterSeparation(const Cluster& cluster,
                                    const std::vector<Cluster>& all_clusters);
    
    // Concept formation methods
    Handle createAbstractConcept(const std::vector<Handle>& instances,
                               const std::string& concept_type);
    std::vector<Handle> extractCommonFeatures(const std::vector<Handle>& instances);
    Handle generalizePattern(const Pattern& pattern);
    void buildConceptHierarchy(std::map<Handle, Handle>& hierarchy,
                             const std::vector<Handle>& concepts);
    
    // Utility methods
    std::string getAtomSignature(const Handle& atom);
    bool areAtomsCompatible(const Handle& atom1, const Handle& atom2);
    std::vector<Handle> extractAtomFeatures(const Handle& atom);
    double calculateInformationGain(const Handle& concept,
                                  const std::vector<Handle>& instances);
};

} // namespace main
} // namespace opencog

#endif // _OPENCOG_UNSUPERVISED_LEARNER_H