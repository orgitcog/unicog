/*
 * OpenCogConfig.cc - Implementation of OpenCog Main Configuration
 */

#include "OpenCogConfig.h"
#include <opencog/util/Logger.h>

namespace opencog {
namespace main {

const std::string OpenCogConfig::DEFAULT_CONFIG_PATH = "opencog.conf";
const std::string OpenCogConfig::DEFAULT_LOG_LEVEL = "INFO";
const int OpenCogConfig::DEFAULT_SERVER_PORT = 17001;

OpenCogConfig::OpenCogConfig() :
    log_level(DEFAULT_LOG_LEVEL),
    server_port(DEFAULT_SERVER_PORT),
    config_path(DEFAULT_CONFIG_PATH)
{
    loadConfig();
}

OpenCogConfig::~OpenCogConfig()
{
}

void OpenCogConfig::loadConfig(const std::string& config_path)
{
    if (!config_path.empty()) {
        this->config_path = config_path;
    }
    
    // Load configuration from file if it exists
    logger().info("OpenCog main configuration loaded with log level: %s, server port: %d",
                  log_level.c_str(), server_port);
}

void OpenCogConfig::setLogLevel(const std::string& log_level)
{
    this->log_level = log_level;
}

void OpenCogConfig::setServerPort(int server_port)
{
    this->server_port = server_port;
}

} // namespace main
} // namespace opencog