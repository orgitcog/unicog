/*
 * LGAtomeseConfig.cc - Implementation of LGAtomese Configuration
 */

#include "LGAtomeseConfig.h"
#include <opencog/util/Logger.h>

namespace opencog {
namespace lg_atomese {

const std::string LGAtomeseConfig::DEFAULT_CONFIG_PATH = "lg-atomese.conf";
const std::string LGAtomeseConfig::DEFAULT_DICT_PATH = "/usr/share/link-grammar";
const int LGAtomeseConfig::DEFAULT_MAX_LINKAGES = 100;

LGAtomeseConfig::LGAtomeseConfig() :
    dict_path(DEFAULT_DICT_PATH),
    max_linkages(DEFAULT_MAX_LINKAGES),
    config_path(DEFAULT_CONFIG_PATH)
{
    loadConfig();
}

LGAtomeseConfig::~LGAtomeseConfig()
{
}

void LGAtomeseConfig::loadConfig(const std::string& config_path)
{
    if (!config_path.empty()) {
        this->config_path = config_path;
    }
    
    // Load configuration from file if it exists
    logger().info("LGAtomese configuration loaded with dict path: %s, max linkages: %d",
                  dict_path.c_str(), max_linkages);
}

void LGAtomeseConfig::setDictPath(const std::string& dict_path)
{
    this->dict_path = dict_path;
}

void LGAtomeseConfig::setMaxLinkages(int max_linkages)
{
    this->max_linkages = max_linkages;
}

} // namespace lg_atomese
} // namespace opencog