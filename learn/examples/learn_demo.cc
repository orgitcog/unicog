/*
 * learn_demo.cc - Demonstration of unsupervised learning functionality
 */

#include <iostream>
#include <vector>
#include <opencog/atomspace/AtomSpace.h>
#include "../opencog/learn/LearnConfig.h"
#include "../opencog/learn/UnsupervisedLearner.h"

using namespace opencog;
using namespace opencog::learn;

int main()
{
    std::cout << "=== Learn (Unsupervised Learning) Demo ===" << std::endl;
    
    // Create AtomSpace
    AtomSpace atomspace;
    
    // Create configuration
    LearnConfig config;
    std::cout << "Configuration loaded:" << std::endl;
    std::cout << "  Learning rate: " << config.getLearningRate() << std::endl;
    std::cout << "  Max iterations: " << config.getMaxIterations() << std::endl;
    
    // Create learner
    UnsupervisedLearner learner(&atomspace, config);
    
    // Create test data
    std::vector<Handle> test_data;
    test_data.push_back(atomspace.add_node(CONCEPT_NODE, "cat"));
    test_data.push_back(atomspace.add_node(CONCEPT_NODE, "dog"));
    test_data.push_back(atomspace.add_node(CONCEPT_NODE, "animal"));
    test_data.push_back(atomspace.add_node(CONCEPT_NODE, "mammal"));
    test_data.push_back(atomspace.add_node(CONCEPT_NODE, "pet"));
    test_data.push_back(atomspace.add_node(CONCEPT_NODE, "furry"));
    
    std::cout << "\nTest data created: " << test_data.size() << " atoms" << std::endl;
    
    // Discover patterns
    std::cout << "\nDiscovering patterns..." << std::endl;
    auto patterns = learner.discoverPatterns(test_data);
    std::cout << "Patterns discovered: " << patterns.size() << std::endl;
    
    for (size_t i = 0; i < patterns.size() && i < 3; ++i) {
        std::cout << "  Pattern " << (i+1) << ": " << patterns[i]->to_short_string() << std::endl;
    }
    
    // Cluster atoms
    std::cout << "\nClustering atoms..." << std::endl;
    auto clusters = learner.clusterAtoms(test_data, 2);
    std::cout << "Clusters created: " << clusters.size() << std::endl;
    
    for (size_t i = 0; i < clusters.size(); ++i) {
        std::cout << "  Cluster " << (i+1) << " size: " << clusters[i].size() << " atoms" << std::endl;
    }
    
    // Mine associations
    std::cout << "\nMining associations..." << std::endl;
    auto associations = learner.mineAssociations(test_data, 0.3);
    std::cout << "Associations found: " << associations.size() << std::endl;
    
    // Form concepts
    std::cout << "\nForming concepts..." << std::endl;
    Handle concept = learner.formConcepts(test_data);
    if (concept != Handle::UNDEFINED) {
        std::cout << "Concept formed: " << concept->to_short_string() << std::endl;
    }
    
    // Acquire knowledge
    std::cout << "\nAcquiring knowledge..." << std::endl;
    auto knowledge = learner.acquireKnowledge(test_data);
    std::cout << "Knowledge acquired: " << knowledge.size() << " items" << std::endl;
    
    // Incremental learning
    std::cout << "\nTesting incremental learning..." << std::endl;
    Handle new_example = atomspace.add_node(CONCEPT_NODE, "kitten");
    learner.incrementalLearn(new_example);
    std::cout << "Incremental learning completed" << std::endl;
    
    // Get statistics
    auto stats = learner.getLearningStatistics();
    std::cout << "\nLearning Statistics:" << std::endl;
    for (const auto& stat : stats) {
        std::cout << "  " << stat.first << ": " << stat.second << std::endl;
    }
    
    std::cout << "\nAtomSpace size after learning: " << atomspace.get_size() << " atoms" << std::endl;
    std::cout << "✅ Learn demo completed successfully!" << std::endl;
    
    return 0;
}