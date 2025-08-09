/*
 * PLNConfig.h - Configuration for Probabilistic Logic Networks
 *
 * Week 13: PLN Integration - Probabilistic reasoning with ure and spacetime
 */

#ifndef _OPENCOG_PLN_CONFIG_H
#define _OPENCOG_PLN_CONFIG_H

#include <opencog/util/Config.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace pln {

/**
 * PLN Configuration Manager
 * Handles configuration for probabilistic logic networks
 */
class PLNConfig
{
public:
    static const std::string DEFAULT_CONFIG_PATH;
    static const double DEFAULT_CONFIDENCE_THRESHOLD;
    static const int DEFAULT_MAX_INFERENCE_STEPS;
    
    PLNConfig();
    virtual ~PLNConfig();
    
    // Configuration methods
    void loadConfig(const std::string& config_path = "");
    void setConfidenceThreshold(double threshold);
    void setMaxInferenceSteps(int max_steps);
    
    // Getters
    double getConfidenceThreshold() const { return confidence_threshold; }
    int getMaxInferenceSteps() const { return max_inference_steps; }
    
private:
    double confidence_threshold;
    int max_inference_steps;
    std::string config_path;
};

} // namespace pln
} // namespace opencog

#endif // _OPENCOG_PLN_CONFIG_H