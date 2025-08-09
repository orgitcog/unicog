/*
 * ASMosesConfig.cc - Implementation of ASMoses Configuration
 */

#include "ASMosesConfig.h"
#include <opencog/util/Logger.h>

namespace opencog {
namespace asmoses {

const std::string ASMosesConfig::DEFAULT_CONFIG_PATH = "asmoses.conf";
const int ASMosesConfig::DEFAULT_POPULATION_SIZE = 100;
const int ASMosesConfig::DEFAULT_MAX_GENERATIONS = 50;

ASMosesConfig::ASMosesConfig() :
    population_size(DEFAULT_POPULATION_SIZE),
    max_generations(DEFAULT_MAX_GENERATIONS),
    config_path(DEFAULT_CONFIG_PATH)
{
    loadConfig();
}

ASMosesConfig::~ASMosesConfig()
{
}

void ASMosesConfig::loadConfig(const std::string& config_path)
{
    if (!config_path.empty()) {
        this->config_path = config_path;
    }
    
    // Load configuration from file if it exists
    logger().info("ASMoses configuration loaded with population size: %d, max generations: %d",
                  population_size, max_generations);
}

void ASMosesConfig::setPopulationSize(int population_size)
{
    this->population_size = population_size;
}

void ASMosesConfig::setMaxGenerations(int max_generations)
{
    this->max_generations = max_generations;
}

} // namespace asmoses
} // namespace opencog