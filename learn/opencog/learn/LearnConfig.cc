/*
 * LearnConfig.cc - Implementation of Learn Configuration
 */

#include "LearnConfig.h"
#include <opencog/util/Logger.h>

namespace opencog {
namespace learn {

const std::string LearnConfig::DEFAULT_CONFIG_PATH = "learn.conf";
const double LearnConfig::DEFAULT_LEARNING_RATE = 0.01;
const int LearnConfig::DEFAULT_MAX_ITERATIONS = 1000;

LearnConfig::LearnConfig() :
    learning_rate(DEFAULT_LEARNING_RATE),
    max_iterations(DEFAULT_MAX_ITERATIONS),
    config_path(DEFAULT_CONFIG_PATH)
{
    loadConfig();
}

LearnConfig::~LearnConfig()
{
}

void LearnConfig::loadConfig(const std::string& config_path)
{
    if (!config_path.empty()) {
        this->config_path = config_path;
    }
    
    // Load configuration from file if it exists
    logger().info("Learn configuration loaded with learning rate: %f, max iterations: %d",
                  learning_rate, max_iterations);
}

void LearnConfig::setLearningRate(double learning_rate)
{
    this->learning_rate = learning_rate;
}

void LearnConfig::setMaxIterations(int max_iterations)
{
    this->max_iterations = max_iterations;
}

} // namespace learn
} // namespace opencog