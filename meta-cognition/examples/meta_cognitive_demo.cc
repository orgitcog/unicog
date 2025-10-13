/*
 * meta_cognitive_demo.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Demonstration of meta-cognitive monitoring and recursive self-reflection
 */

#include "../include/MetaCognitiveMonitor.h"
#include <iostream>
#include <thread>
#include <chrono>
#include <random>

using namespace opencog;

// Mock cognitive component that reports metrics
class MockCognitiveComponent {
private:
    std::string name_;
    std::mt19937 rng_;
    std::uniform_real_distribution<> dist_;
    double base_performance_;
    
public:
    MockCognitiveComponent(const std::string& name, double base_perf = 0.7)
        : name_(name), rng_(std::random_device{}()), 
          dist_(0.0, 1.0), base_performance_(base_perf) {}
    
    double get_performance_metric() {
        // Simulate varying performance with slight random fluctuation
        double noise = (dist_(rng_) - 0.5) * 0.2;
        return std::max(0.0, std::min(1.0, base_performance_ + noise));
    }
    
    void improve_performance(double improvement) {
        base_performance_ = std::min(1.0, base_performance_ + improvement);
        std::cout << "[" << name_ << "] Performance improved to: " << base_performance_ << std::endl;
    }
    
    const std::string& get_name() const { return name_; }
};

int main() {
    std::cout << "=== Phase 5: Meta-Cognitive Monitoring Demonstration ===" << std::endl;
    std::cout << "Initializing recursive meta-cognitive system..." << std::endl;
    
    // Create meta-cognitive monitor with deep reflection capability
    MetaCognitiveMonitor monitor(5, 0.05); // max depth 5, adaptation threshold 0.05
    
    // Create mock cognitive components
    std::vector<MockCognitiveComponent> components = {
        MockCognitiveComponent("PatternExtractor", 0.65),
        MockCognitiveComponent("AttentionAllocator", 0.72),
        MockCognitiveComponent("ReasoningEngine", 0.58),
        MockCognitiveComponent("LearningSystem", 0.80),
        MockCognitiveComponent("MemoryManager", 0.70)
    };
    
    // Register observation functions for each component
    for (auto& component : components) {
        monitor.register_observation_function(
            component.get_name(),
            [&component]() { return component.get_performance_metric(); }
        );
    }
    
    std::cout << "\nStarting meta-cognitive observation cycle..." << std::endl;
    
    // Simulation loop
    for (int cycle = 0; cycle < 20; ++cycle) {
        std::cout << "\n--- Cognitive Cycle " << (cycle + 1) << " ---" << std::endl;
        
        // Observe cognitive state
        auto state = monitor.observe_cognitive_state();
        
        std::cout << "Meta-Cognitive Tensor State:" << std::endl;
        std::cout << "  Self-awareness level: " << state.self_awareness_level << std::endl;
        std::cout << "  Fitness score: " << state.fitness_score << std::endl;
        std::cout << "  Adaptation rate: " << state.adaptation_rate << std::endl;
        std::cout << "  Cognitive complexity: " << state.cognitive_complexity << std::endl;
        std::cout << "  Meta-level: " << state.meta_level << std::endl;
        std::cout << "  Reflection depth: " << state.reflection_depth << std::endl;
        
        // Update performance metrics
        std::map<std::string, double> metrics;
        for (const auto& component : components) {
            metrics[component.get_name()] = component.get_performance_metric();
        }
        metrics["accuracy"] = state.performance_metric[0];
        metrics["efficiency"] = state.performance_metric[1];
        metrics["adaptability"] = state.performance_metric[2];
        
        monitor.update_performance_metrics(metrics);
        
        // Analyze performance trends
        auto trends = monitor.analyze_performance_trends();
        if (!trends.empty()) {
            std::cout << "Performance Insights:" << std::endl;
            for (const auto& insight : trends) {
                std::cout << "  - " << insight << std::endl;
            }
        }
        
        // Detect anomalies
        auto anomalies = monitor.detect_performance_anomalies();
        if (!anomalies.empty()) {
            std::cout << "Performance Anomalies Detected:" << std::endl;
            for (const auto& anomaly : anomalies) {
                std::cout << "  ! " << anomaly << std::endl;
            }
        }
        
        // Generate improvement suggestions
        auto suggestions = monitor.generate_improvement_suggestions();
        if (!suggestions.empty()) {
            std::cout << "Improvement Suggestions:" << std::endl;
            for (const auto& suggestion : suggestions) {
                std::cout << "  → " << suggestion << std::endl;
                
                // Simulate applying improvements
                if (suggestion.find("learning parameters") != std::string::npos) {
                    for (auto& component : components) {
                        component.improve_performance(0.02);
                    }
                }
            }
        }
        
        // Perform recursive self-reflection every few cycles
        if (cycle % 5 == 4) {
            std::cout << "\nPerforming recursive self-reflection..." << std::endl;
            monitor.recursive_self_reflection();
            
            // Generate meta-insights
            auto meta_insights = monitor.generate_meta_insights();
            if (!meta_insights.empty()) {
                std::cout << "Meta-Cognitive Insights:" << std::endl;
                for (const auto& insight : meta_insights) {
                    std::cout << "  ◈ " << insight << std::endl;
                }
            }
        }
        
        // Perform recursive monitoring cycle every 7 cycles
        if (cycle % 7 == 6) {
            std::cout << "\nExecuting recursive monitoring cycle..." << std::endl;
            monitor.recursive_monitoring_cycle();
        }
        
        // Adaptive parameter tuning
        monitor.adaptive_parameter_tuning();
        
        // Advance generation
        monitor.advance_generation();
        
        // Brief pause for demonstration
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
    
    std::cout << "\n=== Meta-Cognitive Demonstration Complete ===" << std::endl;
    
    // Final state summary
    auto final_state = monitor.get_current_state();
    std::cout << "\nFinal Meta-Cognitive State:" << std::endl;
    std::cout << "  Generation: " << monitor.get_current_generation() << std::endl;
    std::cout << "  Self-awareness: " << final_state.self_awareness_level << std::endl;
    std::cout << "  Overall fitness: " << final_state.calculate_overall_fitness() << std::endl;
    std::cout << "  Cognitive complexity: " << final_state.cognitive_complexity << std::endl;
    
    // Cognitive introspection
    std::cout << "\nPerforming final cognitive introspection..." << std::endl;
    auto introspection = monitor.cognitive_introspection();
    for (const auto& result : introspection) {
        std::cout << "  " << result.first << ": " << result.second << std::endl;
    }
    
    return 0;
}