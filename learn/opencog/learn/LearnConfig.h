/*
 * LearnConfig.h - Configuration for Unsupervised Learning
 *
 * Week 18: learn Integration - Unsupervised learning with atomspace and cogserver dependencies
 */

#ifndef _OPENCOG_LEARN_CONFIG_H
#define _OPENCOG_LEARN_CONFIG_H

#include <opencog/util/Config.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace learn {

/**
 * Learn Configuration Manager
 * Handles configuration for unsupervised learning algorithms
 */
class LearnConfig
{
public:
    static const std::string DEFAULT_CONFIG_PATH;
    static const double DEFAULT_LEARNING_RATE;
    static const int DEFAULT_MAX_ITERATIONS;
    
    LearnConfig();
    virtual ~LearnConfig();
    
    // Configuration methods
    void loadConfig(const std::string& config_path = "");
    void setLearningRate(double learning_rate);
    void setMaxIterations(int max_iterations);
    
    // Getters
    double getLearningRate() const { return learning_rate; }
    int getMaxIterations() const { return max_iterations; }
    
private:
    double learning_rate;
    int max_iterations;
    std::string config_path;
};

} // namespace learn
} // namespace opencog

#endif // _OPENCOG_LEARN_CONFIG_H