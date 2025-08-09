/*
 * ASMosesConfig.h - Configuration for AtomSpace MOSES
 *
 * Week 15: asmoses Integration - AtomSpace evolutionary learning with ure dependency
 */

#ifndef _OPENCOG_ASMOSES_CONFIG_H
#define _OPENCOG_ASMOSES_CONFIG_H

#include <opencog/util/Config.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace asmoses {

/**
 * ASMoses Configuration Manager
 * Handles configuration for AtomSpace evolutionary learning
 */
class ASMosesConfig
{
public:
    static const std::string DEFAULT_CONFIG_PATH;
    static const int DEFAULT_POPULATION_SIZE;
    static const int DEFAULT_MAX_GENERATIONS;
    
    ASMosesConfig();
    virtual ~ASMosesConfig();
    
    // Configuration methods
    void loadConfig(const std::string& config_path = "");
    void setPopulationSize(int population_size);
    void setMaxGenerations(int max_generations);
    
    // Getters
    int getPopulationSize() const { return population_size; }
    int getMaxGenerations() const { return max_generations; }
    
private:
    int population_size;
    int max_generations;
    std::string config_path;
};

} // namespace asmoses
} // namespace opencog

#endif // _OPENCOG_ASMOSES_CONFIG_H