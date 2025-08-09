/*
 * LGAtomeseConfig.h - Configuration for Link Grammar to AtomSpace conversion
 *
 * Week 17: lg-atomese Integration - Link Grammar parsing with atomspace dependency
 */

#ifndef _OPENCOG_LG_ATOMESE_CONFIG_H
#define _OPENCOG_LG_ATOMESE_CONFIG_H

#include <opencog/util/Config.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace lg_atomese {

/**
 * LGAtomese Configuration Manager
 * Handles configuration for Link Grammar to AtomSpace conversion
 */
class LGAtomeseConfig
{
public:
    static const std::string DEFAULT_CONFIG_PATH;
    static const std::string DEFAULT_DICT_PATH;
    static const int DEFAULT_MAX_LINKAGES;
    
    LGAtomeseConfig();
    virtual ~LGAtomeseConfig();
    
    // Configuration methods
    void loadConfig(const std::string& config_path = "");
    void setDictPath(const std::string& dict_path);
    void setMaxLinkages(int max_linkages);
    
    // Getters
    const std::string& getDictPath() const { return dict_path; }
    int getMaxLinkages() const { return max_linkages; }
    
private:
    std::string dict_path;
    int max_linkages;
    std::string config_path;
};

} // namespace lg_atomese
} // namespace opencog

#endif // _OPENCOG_LG_ATOMESE_CONFIG_H