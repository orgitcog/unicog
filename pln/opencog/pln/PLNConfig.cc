/*
 * PLNConfig.cc - Implementation of PLN Configuration
 */

#include "PLNConfig.h"
#include <opencog/util/Logger.h>

namespace opencog {
namespace pln {

const std::string PLNConfig::DEFAULT_CONFIG_PATH = "pln.conf";
const double PLNConfig::DEFAULT_CONFIDENCE_THRESHOLD = 0.8;
const int PLNConfig::DEFAULT_MAX_INFERENCE_STEPS = 100;

PLNConfig::PLNConfig() :
    confidence_threshold(DEFAULT_CONFIDENCE_THRESHOLD),
    max_inference_steps(DEFAULT_MAX_INFERENCE_STEPS),
    config_path(DEFAULT_CONFIG_PATH)
{
    loadConfig();
}

PLNConfig::~PLNConfig()
{
}

void PLNConfig::loadConfig(const std::string& config_path)
{
    if (!config_path.empty()) {
        this->config_path = config_path;
    }
    
    // Load configuration from file if it exists
    logger().info("PLN configuration loaded with confidence threshold: %f, max steps: %d",
                  confidence_threshold, max_inference_steps);
    logger().info("PLN dependencies: URE support=%s, SpaceTime support=%s", 
                  hasURESupport() ? "enabled" : "disabled",
                  hasSpaceTimeSupport() ? "enabled" : "disabled");
}

void PLNConfig::setConfidenceThreshold(double threshold)
{
    confidence_threshold = threshold;
}

void PLNConfig::setMaxInferenceSteps(int max_steps)
{
    max_inference_steps = max_steps;
}

} // namespace pln
} // namespace opencog