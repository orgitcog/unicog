/*
 * UnsupervisedLearner.h - Unsupervised Learning Algorithms for OpenCog
 *
 * Week 18: learn Integration - Unsupervised learning implementation
 */

#ifndef _OPENCOG_UNSUPERVISED_LEARNER_H
#define _OPENCOG_UNSUPERVISED_LEARNER_H

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Handle.h>
#include "LearnConfig.h"

namespace opencog {
namespace learn {

/**
 * Unsupervised Learning Engine
 * Implements various unsupervised learning algorithms for knowledge acquisition
 */
class UnsupervisedLearner
{
public:
    UnsupervisedLearner(AtomSpace* atomspace, const LearnConfig& config);
    virtual ~UnsupervisedLearner();
    
    // Pattern discovery methods
    std::vector<Handle> discoverPatterns(const std::vector<Handle>& input_atoms);
    
    // Clustering algorithms
    std::vector<std::vector<Handle>> clusterAtoms(const std::vector<Handle>& atoms, int num_clusters = 0);
    
    // Association rule mining
    std::vector<Handle> mineAssociations(const std::vector<Handle>& atoms, double min_support = 0.1);
    
    // Concept formation
    Handle formConcepts(const std::vector<Handle>& examples);
    
    // Knowledge acquisition from patterns
    std::vector<Handle> acquireKnowledge(const std::vector<Handle>& input_data);
    
    // Incremental learning
    void incrementalLearn(const Handle& new_example);
    
    // Get learned knowledge
    std::vector<Handle> getLearnedKnowledge() const { return learned_knowledge_; }
    
    // Statistics
    std::map<std::string, double> getLearningStatistics() const;
    
private:
    AtomSpace* atomspace_;
    LearnConfig config_;
    std::vector<Handle> learned_knowledge_;
    int learning_iterations_;
    double current_error_;
    
    // Core learning algorithms
    Handle extractPattern(const std::vector<Handle>& atoms);
    std::vector<Handle> findSimilarAtoms(const Handle& atom, double threshold = 0.7);
    double calculateSimilarity(const Handle& atom1, const Handle& atom2);
    
    // Clustering helpers
    std::vector<Handle> kMeansCluster(const std::vector<Handle>& atoms, int k);
    std::vector<Handle> hierarchicalCluster(const std::vector<Handle>& atoms);
    
    // Pattern extraction
    Handle createPatternFromCluster(const std::vector<Handle>& cluster);
    Handle generalizePattern(const std::vector<Handle>& examples);
    
    // Knowledge validation
    bool validateKnowledge(const Handle& knowledge_atom);
    double assessPatternQuality(const Handle& pattern, const std::vector<Handle>& examples);
};

} // namespace learn
} // namespace opencog

#endif // _OPENCOG_UNSUPERVISED_LEARNER_H