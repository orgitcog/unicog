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
 * Integration test for the autonomous agency bootstrap components.
 * This test demonstrates the interaction between all three bootstrap systems.
 */
int main() {
    std::cout << "=== Autonomous Agency Bootstrap Integration Test ===" << std::endl;
    
    try {
        // Create AtomSpace and AttentionBank
        AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
        auto attention_bank = std::make_shared<AttentionBank>(atomspace.get());
        
        std::cout << "\n1. Testing Entropic Drift Detection..." << std::endl;
        
        // Test Entropic Drift Detector
        EntropicDriftDetector drift_detector(atomspace, attention_bank);
        
        // Add some test atoms
        Handle concept1 = atomspace->add_node(CONCEPT_NODE, "TestConcept1");
        Handle concept2 = atomspace->add_node(CONCEPT_NODE, "TestConcept2");
        Handle inheritance = atomspace->add_link(INHERITANCE_LINK, concept1, concept2);
        
        // Perform drift analysis
        auto drift_analysis = drift_detector.performComprehensiveAnalysis();
        std::cout << "  Entropy Score: " << drift_analysis.entropy_score << std::endl;
        std::cout << "  Coherence Score: " << drift_analysis.coherence_score << std::endl;
        std::cout << "  Fragmentation Detected: " << drift_analysis.fragmentation_detected << std::endl;
        
        // Generate drift report
        auto drift_report = drift_detector.generateDriftReport();
        for (const std::string& line : drift_report) {
            std::cout << "  " << line << std::endl;
        }
        
        std::cout << "\n2. Testing Self-Healing AtomSpace..." << std::endl;
        
        // Test Self-Healing AtomSpace
        SelfHealingAtomSpace self_healing(atomspace);
        
        // Perform integrity check
        auto integrity_report = self_healing.performIntegrityCheck();
        std::cout << "  Overall Integrity: " << integrity_report.overall_integrity << std::endl;
        std::cout << "  Structural Health Score: " << integrity_report.structural_health_score << std::endl;
        std::cout << "  Corrupted Atoms: " << integrity_report.corrupted_atoms_count << std::endl;
        
        // Test backup and restoration
        self_healing.createBackupSnapshot();
        auto backups = self_healing.listAvailableBackups();
        std::cout << "  Available Backups: " << backups.size() << std::endl;
        
        // Test garbage collection
        self_healing.performGarbageCollection();
        
        // Get health status
        auto health_status = self_healing.getHealthStatus();
        for (const std::string& status : health_status) {
            std::cout << "  " << status << std::endl;
        }
        
        std::cout << "\n3. Testing Bootstrap Resource Manager..." << std::endl;
        
        // Test Bootstrap Resource Manager
        BootstrapResourceManager resource_manager;
        
        // Start the resource manager
        resource_manager.startResourceManager();
        
        // Test emergency resource allocation
        bool emergency_success = resource_manager.allocateEmergencyResources(
            "test_emergency_task", 
            BootstrapResourceManager::ResourceType::COMPUTATIONAL, 
            100
        );
        std::cout << "  Emergency Allocation Success: " << emergency_success << std::endl;
        
        // Test task submission
        BootstrapResourceManager::Task test_task;
        test_task.task_id = "test_task_1";
        test_task.priority = BootstrapResourceManager::TaskPriority::HIGH;
        test_task.required_resource = BootstrapResourceManager::ResourceType::MEMORY;
        test_task.required_amount = 256;
        test_task.estimated_duration = std::chrono::milliseconds(1000);
        test_task.execution_function = []() {
            std::cout << "    Executing test task..." << std::endl;
        };
        test_task.is_emergency = false;
        
        bool task_submitted = resource_manager.submitTask(test_task);
        std::cout << "  Task Submission Success: " << task_submitted << std::endl;
        
        // Wait a moment for task processing
        std::this_thread::sleep_for(std::chrono::seconds(2));
        
        // Get resource status
        auto resource_status = resource_manager.getResourceStatus();
        for (const std::string& status : resource_status) {
            std::cout << "  " << status << std::endl;
        }
        
        // Test performance metrics
        auto performance_metrics = resource_manager.getPerformanceMetrics();
        std::cout << "  Tasks Completed: " << performance_metrics.tasks_completed << std::endl;
        std::cout << "  Tasks Failed: " << performance_metrics.tasks_failed << std::endl;
        std::cout << "  Utilization Efficiency: " << performance_metrics.resource_utilization_efficiency << std::endl;
        
        // Stop the resource manager
        resource_manager.stopResourceManager();
        
        std::cout << "\n4. Testing Integrated Emergency Response..." << std::endl;
        
        // Simulate an emergency scenario
        resource_manager.enterEmergencyMode();
        
        // Trigger system reorganization due to drift
        if (drift_analysis.fragmentation_detected || drift_analysis.memory_decay_detected) {
            drift_detector.triggerSystemReorganization();
            drift_detector.activateRepairMechanisms();
        }
        
        // Perform emergency healing
        std::vector<Handle> simulated_corrupted = {concept1}; // Simulate corruption
        self_healing.repairCorruptedAtoms(simulated_corrupted);
        
        // Exit emergency mode
        resource_manager.exitEmergencyMode();
        
        std::cout << "\n=== Integration Test Completed Successfully ===" << std::endl;
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "Integration test failed with exception: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Integration test failed with unknown exception" << std::endl;
        return 1;
    }
}