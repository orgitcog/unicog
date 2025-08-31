/*
 * MinerConfig.h - Configuration for Pattern Mining
 *
 * Week 14: miner Integration - Pattern mining algorithms with ure dependency
 */

#ifndef _OPENCOG_MINER_CONFIG_H
#define _OPENCOG_MINER_CONFIG_H

#include <opencog/util/Config.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/ure/Rule.h>

namespace opencog {
namespace miner {

/**
 * Miner Configuration Manager
 * Handles configuration for pattern mining algorithms
 */
class MinerConfig
{
public:
    static const std::string DEFAULT_CONFIG_PATH;
    static const double DEFAULT_MIN_SUPPORT;
    static const int DEFAULT_MAX_PATTERNS;
    
    MinerConfig();
    virtual ~MinerConfig();
    
    // Configuration methods
    void loadConfig(const std::string& config_path = "");
    void setMinSupport(double min_support);
    void setMaxPatterns(int max_patterns);
    
    // URE integration for pattern mining
    bool validateRuleBasedMining(const opencog::ure::Rule& rule) const;
    
    // Getters
    double getMinSupport() const { return min_support; }
    int getMaxPatterns() const { return max_patterns; }
    
private:
    double min_support;
    int max_patterns;
    std::string config_path;
};

} // namespace miner
} // namespace opencog

#endif // _OPENCOG_MINER_CONFIG_H