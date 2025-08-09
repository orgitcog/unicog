/*
 * IntegrationCoordinator.h - OpenCog Final Integration Coordinator
 *
 * Week 19: opencog Final Integration - Complete system integration
 */

#ifndef _OPENCOG_INTEGRATION_COORDINATOR_H
#define _OPENCOG_INTEGRATION_COORDINATOR_H

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Handle.h>
#include "OpenCogConfig.h"

namespace opencog {
namespace main {

// Forward declarations
class LGAtomeseConfig;
class LGParser;
class LearnConfig;
class UnsupervisedLearner;

/**
 * Integration Coordinator
 * Coordinates all OpenCog components for complete system integration
 */
class IntegrationCoordinator
{
public:
    IntegrationCoordinator(AtomSpace* atomspace, const OpenCogConfig& config);
    virtual ~IntegrationCoordinator();
    
    // System initialization
    bool initializeSystem();
    void shutdownSystem();
    
    // Component integration methods
    bool integrateLGAtomese();
    bool integrateLearnModule();
    bool integrateAttentionSystem();
    bool integrateReasoningEngine();
    
    // End-to-end processing pipeline
    std::vector<Handle> processLanguageInput(const std::string& text);
    std::vector<Handle> performUnsupervisedLearning(const std::vector<Handle>& input_data);
    std::vector<Handle> executeReasoningCycle();
    
    // System validation
    bool validateSystemIntegration();
    bool runSystemTests();
    
    // System status and monitoring
    std::map<std::string, std::string> getSystemStatus() const;
    std::map<std::string, double> getSystemMetrics() const;
    
    // Component access
    AtomSpace* getAtomSpace() const { return atomspace_; }
    
    // Integration statistics
    struct IntegrationStats {
        int components_loaded;
        int components_active;
        int processing_cycles;
        double total_processing_time;
        bool system_ready;
    };
    
    IntegrationStats getIntegrationStats() const { return stats_; }
    
private:
    AtomSpace* atomspace_;
    OpenCogConfig config_;
    IntegrationStats stats_;
    
    // Component instances (using void* to avoid dependency issues)
    void* lg_parser_;
    void* unsupervised_learner_;
    
    bool components_initialized_;
    std::vector<std::string> loaded_components_;
    std::map<std::string, bool> component_status_;
    
    // Helper methods
    void initializeComponents();
    void loadComponentConfigurations();
    bool checkComponentDependencies();
    void setupIntegrationPipeline();
    
    // Language processing integration
    Handle parseAndIntegrateText(const std::string& text);
    std::vector<Handle> extractLanguageFeatures(const Handle& parse_result);
    
    // Learning integration
    void feedLearningAlgorithms(const std::vector<Handle>& data);
    std::vector<Handle> retrieveLearnedKnowledge();
    
    // System health monitoring
    bool checkSystemHealth();
    void updateSystemMetrics();
    
    // Configuration management
    void applySystemConfiguration();
    void validateConfiguration();
};

} // namespace main
} // namespace opencog

#endif // _OPENCOG_INTEGRATION_COORDINATOR_H