/*
 * OpenCogConfig.h - Configuration for OpenCog Main Integration
 *
 * Week 19: opencog Final Integration - Complete system integration
 */

#ifndef _OPENCOG_MAIN_CONFIG_H
#define _OPENCOG_MAIN_CONFIG_H

#include <opencog/util/Config.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
namespace main {

/**
 * OpenCog Main Configuration Manager
 * Handles configuration for complete system integration
 */
class OpenCogConfig
{
public:
    static const std::string DEFAULT_CONFIG_PATH;
    static const std::string DEFAULT_LOG_LEVEL;
    static const int DEFAULT_SERVER_PORT;
    
    OpenCogConfig();
    virtual ~OpenCogConfig();
    
    // Configuration methods
    void loadConfig(const std::string& config_path = "");
    void setLogLevel(const std::string& log_level);
    void setServerPort(int server_port);
    
    // Getters
    const std::string& getLogLevel() const { return log_level; }
    int getServerPort() const { return server_port; }
    
private:
    std::string log_level;
    int server_port;
    std::string config_path;
};

} // namespace main
} // namespace opencog

#endif // _OPENCOG_MAIN_CONFIG_H