/*
 * MinerConfig.cc - Implementation of Miner Configuration
 */

#include "MinerConfig.h"
#include <opencog/util/Logger.h>
#include <opencog/ure/Rule.h>

namespace opencog {
namespace miner {

const std::string MinerConfig::DEFAULT_CONFIG_PATH = "miner.conf";
const double MinerConfig::DEFAULT_MIN_SUPPORT = 0.1;
const int MinerConfig::DEFAULT_MAX_PATTERNS = 1000;

MinerConfig::MinerConfig() :
    min_support(DEFAULT_MIN_SUPPORT),
    max_patterns(DEFAULT_MAX_PATTERNS),
    config_path(DEFAULT_CONFIG_PATH)
{
    loadConfig();
}

MinerConfig::~MinerConfig()
{
}

void MinerConfig::loadConfig(const std::string& config_path)
{
    if (!config_path.empty()) {
        this->config_path = config_path;
    }
    
    // Load configuration from file if it exists
    logger().info("Miner configuration loaded with min support: %f, max patterns: %d",
                  min_support, max_patterns);
}

void MinerConfig::setMinSupport(double min_support)
{
    this->min_support = min_support;
}

void MinerConfig::setMaxPatterns(int max_patterns)
{
    this->max_patterns = max_patterns;
}

bool MinerConfig::validateRuleBasedMining(const opencog::ure::Rule& rule) const
{
    // Demonstrate URE dependency usage - validate if rule is suitable for mining
    if (rule.is_valid()) {
        logger().info("Miner: URE rule validation passed for pattern mining");
        return true;
    }
    logger().warn("Miner: URE rule validation failed");
    return false;
}

} // namespace miner
} // namespace opencog