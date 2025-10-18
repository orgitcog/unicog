#include <iostream>
#include <memory>
#include "../bootstrap/entropic-drift-detector.hpp"
#include "../bootstrap/self-healing-atomspace.hpp"
#include "../bootstrap/bootstrap-resource-manager.hpp"
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attention/AttentionBank.h>

using namespace opencog;
using namespace opencog::autonomous;

/**
 * Bootstrap Example: Demonstrates basic usage of autonomous agency bootstrap components
 * for overcoming entropic drift and maintaining system coherence.
 */
int main() {
    std::cout << "=== Autonomous Agency Bootstrap Example ===" << std::endl;
    std::cout << "This example demonstrates foundational mechanisms for autonomous agency" << std::endl;
    std::cout << "through entropic drift detection, self-healing, and resource management." << std::endl;
    std::cout << std::endl;
    
    // Step 1: Create cognitive infrastructure
    std::cout << "Step 1: Initializing cognitive infrastructure..." << std::endl;
    
    AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
    auto attention_bank = std::make_shared<AttentionBank>(atomspace.get());
    
    // Add some initial knowledge to the AtomSpace
    Handle cat = atomspace->add_node(CONCEPT_NODE, "Cat");
    Handle animal = atomspace->add_node(CONCEPT_NODE, "Animal");
    Handle mammal = atomspace->add_node(CONCEPT_NODE, "Mammal");
    
    Handle cat_is_animal = atomspace->add_link(INHERITANCE_LINK, cat, animal);
    Handle cat_is_mammal = atomspace->add_link(INHERITANCE_LINK, cat, mammal);
    Handle mammal_is_animal = atomspace->add_link(INHERITANCE_LINK, mammal, animal);
    
    std::cout << "  Created AtomSpace with " << atomspace->get_size() << " atoms" << std::endl;
    
    // Step 2: Initialize bootstrap systems
    std::cout << "\nStep 2: Initializing bootstrap systems..." << std::endl;
    
    EntropicDriftDetector drift_detector(atomspace, attention_bank);
    SelfHealingAtomSpace self_healing(atomspace);
    BootstrapResourceManager resource_manager;
    
    // Start background processes
    self_healing.startBackgroundMaintenance();
    resource_manager.startResourceManager();
    
    std::cout << "  Bootstrap systems initialized and active" << std::endl;
    
    // Step 3: Demonstrate drift detection
    std::cout << "\nStep 3: Monitoring for entropic drift..." << std::endl;
    
    // Perform initial baseline measurement
    drift_detector.updateBaselines();
    
    // Simulate some system activity that might cause drift
    for (int i = 0; i < 10; ++i) {
        Handle concept = atomspace->add_node(CONCEPT_NODE, "DynamicConcept" + std::to_string(i));
        if (i % 3 == 0) {
            atomspace->add_link(INHERITANCE_LINK, concept, animal);
        }
    }
    
    // Check for drift
    auto drift_analysis = drift_detector.performComprehensiveAnalysis();
    
    std::cout << "  Entropy Score: " << drift_analysis.entropy_score << std::endl;
    std::cout << "  Coherence Score: " << drift_analysis.coherence_score << std::endl;
    std::cout << "  Attention Distribution Score: " << drift_analysis.attention_distribution_score << std::endl;
    
    if (!drift_analysis.critical_issues.empty()) {
        std::cout << "  Critical Issues Detected:" << std::endl;
        for (const std::string& issue : drift_analysis.critical_issues) {
            std::cout << "    - " << issue << std::endl;
        }
    } else {
        std::cout << "  No critical issues detected" << std::endl;
    }
    
    // Step 4: Demonstrate self-healing capabilities
    std::cout << "\nStep 4: Testing self-healing capabilities..." << std::endl;
    
    // Create a backup before potential issues
    self_healing.createBackupSnapshot();
    std::cout << "  Backup snapshot created" << std::endl;
    
    // Perform integrity check
    auto integrity_report = self_healing.performIntegrityCheck();
    std::cout << "  Integrity Check Results:" << std::endl;
    std::cout << "    Overall Integrity: " << (integrity_report.overall_integrity ? "Good" : "Issues Found") << std::endl;
    std::cout << "    Structural Health Score: " << integrity_report.structural_health_score << std::endl;
    std::cout << "    Corrupted Atoms: " << integrity_report.corrupted_atoms_count << std::endl;
    std::cout << "    Orphaned Atoms: " << integrity_report.orphaned_atoms_count << std::endl;
    
    // Demonstrate garbage collection
    self_healing.performGarbageCollection();
    std::cout << "  Garbage collection completed" << std::endl;
    
    // Step 5: Demonstrate resource management
    std::cout << "\nStep 5: Testing resource management..." << std::endl;
    
    // Submit various tasks with different priorities
    std::vector<BootstrapResourceManager::Task> tasks;
    
    // Critical task
    BootstrapResourceManager::Task critical_task;
    critical_task.task_id = "critical_system_maintenance";
    critical_task.priority = BootstrapResourceManager::TaskPriority::CRITICAL;
    critical_task.required_resource = BootstrapResourceManager::ResourceType::COMPUTATIONAL;
    critical_task.required_amount = 200;
    critical_task.estimated_duration = std::chrono::milliseconds(500);
    critical_task.execution_function = []() {
        std::cout << "    Executing critical system maintenance..." << std::endl;
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    };
    critical_task.is_emergency = false;
    
    // Regular task
    BootstrapResourceManager::Task regular_task;
    regular_task.task_id = "regular_optimization";
    regular_task.priority = BootstrapResourceManager::TaskPriority::MEDIUM;
    regular_task.required_resource = BootstrapResourceManager::ResourceType::MEMORY;
    regular_task.required_amount = 512;
    regular_task.estimated_duration = std::chrono::milliseconds(1000);
    regular_task.execution_function = []() {
        std::cout << "    Executing regular optimization..." << std::endl;
        std::this_thread::sleep_for(std::chrono::milliseconds(200));
    };
    regular_task.is_emergency = false;
    
    // Submit tasks
    resource_manager.submitTask(critical_task);
    resource_manager.submitTask(regular_task);
    
    std::cout << "  Tasks submitted to resource manager" << std::endl;
    
    // Wait for task processing
    std::this_thread::sleep_for(std::chrono::seconds(2));
    
    // Check resource utilization
    std::cout << "  Resource Utilization:" << std::endl;
    std::cout << "    Computational: " << (resource_manager.getResourceUtilization(BootstrapResourceManager::ResourceType::COMPUTATIONAL) * 100) << "%" << std::endl;
    std::cout << "    Memory: " << (resource_manager.getResourceUtilization(BootstrapResourceManager::ResourceType::MEMORY) * 100) << "%" << std::endl;
    std::cout << "    Attention: " << (resource_manager.getResourceUtilization(BootstrapResourceManager::ResourceType::ATTENTION) * 100) << "%" << std::endl;
    
    // Step 6: Simulate emergency scenario
    std::cout << "\nStep 6: Simulating emergency scenario..." << std::endl;
    
    // Force emergency mode
    resource_manager.enterEmergencyMode();
    std::cout << "  Emergency mode activated" << std::endl;
    
    // Submit emergency task
    BootstrapResourceManager::Task emergency_task;
    emergency_task.task_id = "emergency_repair";
    emergency_task.priority = BootstrapResourceManager::TaskPriority::CRITICAL;
    emergency_task.required_resource = BootstrapResourceManager::ResourceType::COMPUTATIONAL;
    emergency_task.required_amount = 300;
    emergency_task.estimated_duration = std::chrono::milliseconds(200);
    emergency_task.execution_function = []() {
        std::cout << "    Executing emergency repair..." << std::endl;
    };
    emergency_task.is_emergency = true;
    
    resource_manager.submitEmergencyTask(emergency_task);
    
    // Trigger system reorganization if needed
    if (drift_analysis.fragmentation_detected || drift_analysis.memory_decay_detected || drift_analysis.reasoning_degradation_detected) {
        std::cout << "  Triggering system reorganization due to detected drift..." << std::endl;
        drift_detector.triggerSystemReorganization();
        drift_detector.activateRepairMechanisms();
    }
    
    // Wait for emergency processing
    std::this_thread::sleep_for(std::chrono::seconds(1));
    
    // Exit emergency mode
    resource_manager.exitEmergencyMode();
    std::cout << "  Emergency mode deactivated" << std::endl;
    
    // Step 7: Final system status
    std::cout << "\nStep 7: Final system status..." << std::endl;
    
    // Get final performance metrics
    auto performance_metrics = resource_manager.getPerformanceMetrics();
    std::cout << "  Performance Metrics:" << std::endl;
    std::cout << "    Tasks Completed: " << performance_metrics.tasks_completed << std::endl;
    std::cout << "    Tasks Failed: " << performance_metrics.tasks_failed << std::endl;
    std::cout << "    Emergency Interventions: " << performance_metrics.emergency_interventions << std::endl;
    std::cout << "    Resource Utilization Efficiency: " << (performance_metrics.resource_utilization_efficiency * 100) << "%" << std::endl;
    
    // Final integrity check
    auto final_integrity = self_healing.performIntegrityCheck();
    std::cout << "  Final System Integrity: " << (final_integrity.overall_integrity ? "Good" : "Issues Remain") << std::endl;
    std::cout << "  Final Health Score: " << final_integrity.structural_health_score << std::endl;
    
    // Clean shutdown
    std::cout << "\nShutting down bootstrap systems..." << std::endl;
    self_healing.stopBackgroundMaintenance();
    resource_manager.stopResourceManager();
    
    std::cout << "\n=== Bootstrap Example Completed Successfully ===" << std::endl;
    std::cout << "The system has demonstrated basic autonomous agency capabilities:" << std::endl;
    std::cout << "✓ Entropic drift detection and monitoring" << std::endl;
    std::cout << "✓ Self-healing and integrity maintenance" << std::endl;
    std::cout << "✓ Resource management and emergency response" << std::endl;
    std::cout << "✓ Integrated bootstrap mechanisms for system survival" << std::endl;
    
    return 0;
}