/*
 * meta_cognitive_demo.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Demonstration of meta-cognitive monitoring with real OpenCog ECAN attention system
 */

#include "../include/MetaCognitiveMonitor.h"
#include <opencog/attention/ECANAgent.h>
#include <opencog/attention/AttentionBank.h>
#include <opencog/attention/ImportanceSpreadingAgent.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <iostream>
#include <thread>
#include <chrono>
#include <random>

using namespace opencog;

// Real cognitive component using ECAN attention allocation
class ECANCognitiveComponent {
private:
    std::string name_;
    AtomSpace* atomspace_;
    std::shared_ptr<AttentionBank> attention_bank_;
    std::shared_ptr<ECANAgent> ecan_agent_;
    std::shared_ptr<ImportanceSpreadingAgent> spreading_agent_;
    Handle self_concept_;
    std::mt19937 rng_;
    std::uniform_real_distribution<> dist_;
    
public:
    ECANCognitiveComponent(const std::string& name, AtomSpace* as)
        : name_(name), atomspace_(as), rng_(std::random_device{}()), dist_(0.0, 1.0)
    {
        // Create attention bank
        attention_bank_ = std::make_shared<AttentionBank>(as);
        
        // Create ECAN agent
        ecan_agent_ = std::make_shared<ECANAgent>(as, attention_bank_);
        
        // Create importance spreading agent
        spreading_agent_ = std::make_shared<ImportanceSpreadingAgent>(as, attention_bank_);
        
        // Create self-concept atom
        self_concept_ = atomspace_->add_node(CONCEPT_NODE, name_ + "_concept");
        self_concept_->setTruthValue(SimpleTruthValue::createTV(0.8, 0.9));
        
        // Initialize with some attention value
        attention_bank_->setSTI(self_concept_, 100);
        attention_bank_->setLTI(self_concept_, 50);
        
        // Add to attentional focus
        attention_bank_->addToAF(self_concept_);
    }
    
    double get_performance_metric() {
        // Performance based on attention value and connectivity
        double sti = attention_bank_->getSTI(self_concept_);
        double lti = attention_bank_->getLTI(self_concept_);
        
        // Get incoming set size as measure of connectivity
        HandleSeq incoming = atomspace_->get_incoming(self_concept_);
        double connectivity = incoming.size();
        
        // Combine metrics with some noise
        double base_performance = (sti / 200.0 + lti / 100.0 + connectivity / 10.0) / 3.0;
        double noise = (dist_(rng_) - 0.5) * 0.1;
        
        return std::max(0.0, std::min(1.0, base_performance + noise));
    }
    
    void improve_performance(double improvement) {
        // Improve by increasing attention values and creating new connections
        double current_sti = attention_bank_->getSTI(self_concept_);
        double current_lti = attention_bank_->getLTI(self_concept_);
        
        // Increase attention values
        attention_bank_->setSTI(self_concept_, current_sti + improvement * 50);
        attention_bank_->setLTI(self_concept_, current_lti + improvement * 25);
        
        // Create new conceptual connections to improve connectivity
        if (improvement > 0.01) {
            Handle improvement_node = atomspace_->add_node(CONCEPT_NODE, 
                name_ + "_improvement_" + std::to_string(dist_(rng_)));
            
            Handle improvement_link = atomspace_->add_link(SIMILARITY_LINK, 
                HandleSeq{self_concept_, improvement_node});
            improvement_link->setTruthValue(SimpleTruthValue::createTV(improvement, 0.8));
            
            // Give new nodes some attention
            attention_bank_->setSTI(improvement_node, improvement * 30);
        }
        
        std::cout << "[" << name_ << "] ECAN Performance improved: STI=" 
                  << attention_bank_->getSTI(self_concept_) 
                  << ", LTI=" << attention_bank_->getLTI(self_concept_) << std::endl;
    }
    
    void run_attention_cycle() {
        // Run ECAN attention allocation cycle
        ecan_agent_->runCycle();
        spreading_agent_->spreadImportance();
    }
    
    const std::string& get_name() const { return name_; }
    
    Handle get_concept() const { return self_concept_; }
    
    AttentionBank* get_attention_bank() const { return attention_bank_.get(); }
};

int main() {
    std::cout << "=== Phase 5: Meta-Cognitive Monitoring with Real ECAN ===" << std::endl;
    std::cout << "Initializing recursive meta-cognitive system with OpenCog ECAN..." << std::endl;
    
    // Create real AtomSpace for cognitive operations
    AtomSpace atomspace;
    
    // Create meta-cognitive monitor with deep reflection capability
    MetaCognitiveMonitor monitor(5, 0.05); // max depth 5, adaptation threshold 0.05
    
    // Create real ECAN cognitive components
    std::vector<ECANCognitiveComponent> components = {
        ECANCognitiveComponent("PatternExtractor", &atomspace),
        ECANCognitiveComponent("AttentionAllocator", &atomspace),
        ECANCognitiveComponent("ReasoningEngine", &atomspace),
        ECANCognitiveComponent("LearningSystem", &atomspace),
        ECANCognitiveComponent("MemoryManager", &atomspace)
    };
    
    // Create cross-connections between cognitive components
    for (size_t i = 0; i < components.size(); ++i) {
        for (size_t j = i + 1; j < components.size(); ++j) {
            Handle connection = atomspace.add_link(ASSOCIATION_LINK, 
                HandleSeq{components[i].get_concept(), components[j].get_concept()});
            connection->setTruthValue(SimpleTruthValue::createTV(0.6, 0.8));
        }
    }
    
    // Register observation functions for each component
    for (auto& component : components) {
        monitor.register_observation_function(
            component.get_name(),
            [&component]() { return component.get_performance_metric(); }
        );
    }
    
    std::cout << "\nStarting meta-cognitive observation cycle with ECAN..." << std::endl;
    
    // Simulation loop
    for (int cycle = 0; cycle < 20; ++cycle) {
        std::cout << "\n--- Cognitive Cycle " << (cycle + 1) << " ---" << std::endl;
        
        // Run ECAN attention cycles for all components
        for (auto& component : components) {
            component.run_attention_cycle();
        }
        
        // Observe cognitive state
        auto state = monitor.observe_cognitive_state();
        
        std::cout << "Meta-Cognitive Tensor State:" << std::endl;
        std::cout << "  Self-awareness level: " << state.self_awareness_level << std::endl;
        std::cout << "  Fitness score: " << state.fitness_score << std::endl;
        std::cout << "  Adaptation rate: " << state.adaptation_rate << std::endl;
        std::cout << "  Cognitive complexity: " << state.cognitive_complexity << std::endl;
        std::cout << "  Meta-level: " << state.meta_level << std::endl;
        std::cout << "  Reflection depth: " << state.reflection_depth << std::endl;
        
        // Update performance metrics with real ECAN data
        std::map<std::string, double> metrics;
        for (const auto& component : components) {
            metrics[component.get_name()] = component.get_performance_metric();
            
            // Add ECAN-specific metrics
            double sti = component.get_attention_bank()->getSTI(component.get_concept());
            double lti = component.get_attention_bank()->getLTI(component.get_concept());
            metrics[component.get_name() + "_STI"] = sti / 200.0; // Normalize
            metrics[component.get_name() + "_LTI"] = lti / 100.0; // Normalize
        }
        
        // Add global AtomSpace metrics
        metrics["atomspace_size"] = atomspace.get_size() / 1000.0; // Normalize
        metrics["attentional_focus_size"] = components[0].get_attention_bank()->getAFSize() / 100.0;
        
        metrics["accuracy"] = state.performance_metric[0];
        metrics["efficiency"] = state.performance_metric[1];
        metrics["adaptability"] = state.performance_metric[2];
        
        monitor.update_performance_metrics(metrics);
        
        // Analyze performance trends
        auto trends = monitor.analyze_performance_trends();
        if (!trends.empty()) {
            std::cout << "ECAN Performance Insights:" << std::endl;
            for (const auto& insight : trends) {
                std::cout << "  - " << insight << std::endl;
            }
        }
        
        // Detect anomalies
        auto anomalies = monitor.detect_performance_anomalies();
        if (!anomalies.empty()) {
            std::cout << "ECAN Performance Anomalies Detected:" << std::endl;
            for (const auto& anomaly : anomalies) {
                std::cout << "  ! " << anomaly << std::endl;
            }
        }
        
        // Generate improvement suggestions based on ECAN metrics
        auto suggestions = monitor.generate_improvement_suggestions();
        if (!suggestions.empty()) {
            std::cout << "ECAN Improvement Suggestions:" << std::endl;
            for (const auto& suggestion : suggestions) {
                std::cout << "  → " << suggestion << std::endl;
                
                // Apply ECAN-based improvements
                if (suggestion.find("attention") != std::string::npos ||
                    suggestion.find("learning parameters") != std::string::npos) {
                    for (auto& component : components) {
                        component.improve_performance(0.02);
                    }
                }
            }
        }
        
        // Perform recursive self-reflection every few cycles
        if (cycle % 5 == 4) {
            std::cout << "\nPerforming recursive self-reflection with ECAN..." << std::endl;
            monitor.recursive_self_reflection();
            
            // Generate meta-insights
            auto meta_insights = monitor.generate_meta_insights();
            if (!meta_insights.empty()) {
                std::cout << "Meta-Cognitive ECAN Insights:" << std::endl;
                for (const auto& insight : meta_insights) {
                    std::cout << "  ◈ " << insight << std::endl;
                }
            }
        }
        
        // Perform recursive monitoring cycle every 7 cycles
        if (cycle % 7 == 6) {
            std::cout << "\nExecuting recursive ECAN monitoring cycle..." << std::endl;
            monitor.recursive_monitoring_cycle();
        }
        
        // Adaptive parameter tuning
        monitor.adaptive_parameter_tuning();
        
        // Advance generation
        monitor.advance_generation();
        
        // Brief pause for demonstration
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
    
    std::cout << "\n=== Meta-Cognitive ECAN Demonstration Complete ===" << std::endl;
    
    // Final state summary
    auto final_state = monitor.get_current_state();
    std::cout << "\nFinal Meta-Cognitive ECAN State:" << std::endl;
    std::cout << "  Generation: " << monitor.get_current_generation() << std::endl;
    std::cout << "  Self-awareness: " << final_state.self_awareness_level << std::endl;
    std::cout << "  Overall fitness: " << final_state.calculate_overall_fitness() << std::endl;
    std::cout << "  Cognitive complexity: " << final_state.cognitive_complexity << std::endl;
    std::cout << "  Total atoms: " << atomspace.get_size() << std::endl;
    
    // Display final ECAN statistics
    std::cout << "\nFinal ECAN Statistics:" << std::endl;
    for (const auto& component : components) {
        std::cout << "  " << component.get_name() << ":" << std::endl;
        std::cout << "    STI: " << component.get_attention_bank()->getSTI(component.get_concept()) << std::endl;
        std::cout << "    LTI: " << component.get_attention_bank()->getLTI(component.get_concept()) << std::endl;
    }
    
    // Cognitive introspection
    std::cout << "\nPerforming final cognitive introspection..." << std::endl;
    auto introspection = monitor.cognitive_introspection();
    for (const auto& result : introspection) {
        std::cout << "  " << result.first << ": " << result.second << std::endl;
    }
    
    return 0;
}