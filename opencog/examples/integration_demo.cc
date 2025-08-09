/*
 * integration_demo.cc - Demonstration of complete OpenCog integration
 */

#include <iostream>
#include <vector>
#include <opencog/atomspace/AtomSpace.h>
#include "../opencog/main/OpenCogConfig.h"
#include "../opencog/main/IntegrationCoordinator.h"

using namespace opencog;
using namespace opencog::main;

int main()
{
    std::cout << "=== OpenCog Complete Integration Demo ===" << std::endl;
    
    // Create AtomSpace
    AtomSpace atomspace;
    
    // Create configuration
    OpenCogConfig config;
    std::cout << "Configuration loaded:" << std::endl;
    std::cout << "  Log level: " << config.getLogLevel() << std::endl;
    std::cout << "  Server port: " << config.getServerPort() << std::endl;
    
    // Create integration coordinator
    IntegrationCoordinator coordinator(&atomspace, config);
    
    // Initialize system
    std::cout << "\nInitializing OpenCog unified system..." << std::endl;
    if (!coordinator.initializeSystem()) {
        std::cerr << "Failed to initialize system!" << std::endl;
        return 1;
    }
    std::cout << "✅ System initialized successfully" << std::endl;
    
    // Get system status
    auto status = coordinator.getSystemStatus();
    std::cout << "\nSystem Status:" << std::endl;
    for (const auto& status_pair : status) {
        std::cout << "  " << status_pair.first << ": " << status_pair.second << std::endl;
    }
    
    // Test language processing
    std::cout << "\nTesting language processing..." << std::endl;
    std::vector<std::string> test_sentences = {
        "The cat sits on the mat.",
        "OpenCog integrates multiple cognitive architectures.",
        "Machine learning discovers patterns in data."
    };
    
    for (const auto& sentence : test_sentences) {
        std::cout << "Processing: " << sentence << std::endl;
        auto results = coordinator.processLanguageInput(sentence);
        std::cout << "  Results: " << results.size() << " atoms created" << std::endl;
    }
    
    // Test unsupervised learning
    std::cout << "\nTesting unsupervised learning..." << std::endl;
    std::vector<Handle> learning_data;
    learning_data.push_back(atomspace.add_node(CONCEPT_NODE, "intelligence"));
    learning_data.push_back(atomspace.add_node(CONCEPT_NODE, "cognition"));
    learning_data.push_back(atomspace.add_node(CONCEPT_NODE, "reasoning"));
    learning_data.push_back(atomspace.add_node(CONCEPT_NODE, "learning"));
    
    auto learned_knowledge = coordinator.performUnsupervisedLearning(learning_data);
    std::cout << "Learned knowledge: " << learned_knowledge.size() << " items" << std::endl;
    
    // Test reasoning cycle
    std::cout << "\nTesting reasoning cycle..." << std::endl;
    auto inferences = coordinator.executeReasoningCycle();
    std::cout << "Inferences generated: " << inferences.size() << std::endl;
    
    // Get system metrics
    auto metrics = coordinator.getSystemMetrics();
    std::cout << "\nSystem Metrics:" << std::endl;
    for (const auto& metric : metrics) {
        std::cout << "  " << metric.first << ": " << metric.second << std::endl;
    }
    
    // Get integration statistics
    auto stats = coordinator.getIntegrationStats();
    std::cout << "\nIntegration Statistics:" << std::endl;
    std::cout << "  Components loaded: " << stats.components_loaded << std::endl;
    std::cout << "  Components active: " << stats.components_active << std::endl;
    std::cout << "  Processing cycles: " << stats.processing_cycles << std::endl;
    std::cout << "  Total processing time: " << stats.total_processing_time << " ms" << std::endl;
    std::cout << "  System ready: " << (stats.system_ready ? "Yes" : "No") << std::endl;
    
    // Final validation
    std::cout << "\nRunning final system validation..." << std::endl;
    if (coordinator.validateSystemIntegration()) {
        std::cout << "✅ System validation passed" << std::endl;
    } else {
        std::cout << "❌ System validation failed" << std::endl;
    }
    
    std::cout << "\nFinal AtomSpace size: " << atomspace.get_size() << " atoms" << std::endl;
    std::cout << "\n🎉 OpenCog Complete Integration Demo finished successfully!" << std::endl;
    std::cout << "Phase V (Language & Final Integration) implementation complete!" << std::endl;
    
    // Shutdown system
    coordinator.shutdownSystem();
    
    return 0;
}