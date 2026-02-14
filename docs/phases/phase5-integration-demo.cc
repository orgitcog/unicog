/*
 * phase5-integration-demo.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Comprehensive demonstration of integrated meta-cognitive and evolutionary systems
 */

#include "meta-cognition/include/MetaCognitiveMonitor.h"
#include "evolutionary-optimization/include/EvolutionaryOptimizer.h"
#include "cognitive-patterns/include/HypergraphPatternExtractor.h"
#include <iostream>
#include <thread>
#include <chrono>
#include <random>
#include <fstream>

using namespace opencog;

/**
 * Integrated Cognitive System demonstrating Phase 5 capabilities
 */
class RecursiveCognitiveSystem {
private:
    std::unique_ptr<MetaCognitiveMonitor> monitor_;
    std::unique_ptr<EvolutionaryOptimizer> optimizer_;
    std::unique_ptr<HypergraphPatternExtractor> pattern_extractor_;
    
    // System state
    int current_cycle_;
    std::vector<double> performance_history_;
    std::map<std::string, double> system_metrics_;
    
public:
    RecursiveCognitiveSystem() 
        : current_cycle_(0)
    {
        // Initialize components
        monitor_ = std::make_unique<MetaCognitiveMonitor>(5, 0.05);
        optimizer_ = std::make_unique<EvolutionaryOptimizer>(30, 50);
        pattern_extractor_ = std::make_unique<HypergraphPatternExtractor>(0.7, 20);
        
        // Configure optimizer with custom fitness
        std::map<std::string, double> objectives = {
            {"meta_awareness", 0.3},
            {"pattern_quality", 0.3},
            {"adaptation_speed", 0.2},
            {"system_stability", 0.2}
        };
        optimizer_->configure_fitness_objectives(objectives);
        
        std::cout << "[RecursiveCognitiveSystem] Initialized with deep integration" << std::endl;
    }
    
    void run_recursive_cognitive_cycle() {
        std::cout << "\n=== Recursive Cognitive Cycle " << (current_cycle_ + 1) << " ===" << std::endl;
        
        // 1. Meta-cognitive observation and reflection
        auto cognitive_state = monitor_->observe_cognitive_state();
        
        std::cout << "1. Meta-Cognitive State Analysis:" << std::endl;
        std::cout << "   Self-awareness: " << cognitive_state.self_awareness_level << std::endl;
        std::cout << "   Meta-level: " << cognitive_state.meta_level << std::endl;
        std::cout << "   Reflection depth: " << cognitive_state.reflection_depth << std::endl;
        
        // 2. Pattern extraction with self-reflexive adaptation
        std::vector<std::string> sample_nodes = {"concept_A", "concept_B", "concept_C", "concept_D"};
        std::vector<std::pair<std::string, std::vector<std::string>>> sample_edges = {
            {"relation_1", {"concept_A", "concept_B"}},
            {"relation_2", {"concept_B", "concept_C"}},
            {"relation_3", {"concept_A", "concept_C", "concept_D"}}
        };
        
        auto detected_patterns = pattern_extractor_->extract_emergent_patterns(sample_nodes, sample_edges);
        std::cout << "2. Pattern Detection: Found " << detected_patterns.size() << " emergent patterns" << std::endl;
        
        // 3. Calculate system performance metrics
        update_system_metrics(detected_patterns, cognitive_state);
        
        // 4. Evolutionary optimization if needed
        if (should_trigger_evolution()) {
            std::cout << "3. Triggering evolutionary optimization..." << std::endl;
            perform_evolutionary_adaptation(cognitive_state);
        }
        
        // 5. Recursive self-reflection every few cycles
        if (current_cycle_ % 3 == 2) {
            std::cout << "4. Performing recursive self-reflection..." << std::endl;
            monitor_->recursive_self_reflection();
            
            auto meta_insights = monitor_->generate_meta_insights();
            for (const auto& insight : meta_insights) {
                std::cout << "   Meta-insight: " << insight << std::endl;
            }
        }
        
        // 6. Apply meta-cognitive improvements
        auto suggestions = monitor_->generate_improvement_suggestions();
        apply_improvement_suggestions(suggestions);
        
        // 7. Update performance history
        double current_performance = calculate_overall_performance(cognitive_state);
        performance_history_.push_back(current_performance);
        
        std::cout << "5. Cycle Performance: " << current_performance << std::endl;
        
        current_cycle_++;
    }
    
    void update_system_metrics(const std::vector<std::string>& patterns, 
                              const MetaCognitiveTensor& state) {
        // Pattern quality metrics
        system_metrics_["pattern_count"] = static_cast<double>(patterns.size());
        system_metrics_["pattern_diversity"] = calculate_pattern_diversity(patterns);
        
        // Meta-cognitive metrics
        system_metrics_["self_awareness"] = state.self_awareness_level;
        system_metrics_["adaptation_rate"] = state.adaptation_rate;
        system_metrics_["meta_fitness"] = state.calculate_overall_fitness();
        
        // System integration metrics
        system_metrics_["integration_score"] = calculate_integration_score();
        
        // Update monitor with new metrics
        monitor_->update_performance_metrics(system_metrics_);
    }
    
    bool should_trigger_evolution() {
        // Trigger evolution based on performance trends
        if (performance_history_.size() < 5) return false;
        
        // Check if performance has stagnated
        double recent_avg = 0.0;
        for (int i = performance_history_.size() - 3; i < static_cast<int>(performance_history_.size()); ++i) {
            recent_avg += performance_history_[i];
        }
        recent_avg /= 3.0;
        
        double older_avg = 0.0;
        for (int i = performance_history_.size() - 6; i < static_cast<int>(performance_history_.size()) - 3; ++i) {
            older_avg += performance_history_[i];
        }
        older_avg /= 3.0;
        
        return (recent_avg - older_avg) < 0.02; // Minimal improvement
    }
    
    void perform_evolutionary_adaptation(const MetaCognitiveTensor& cognitive_state) {
        // Integrate meta-cognitive feedback into evolution
        optimizer_->integrate_metacognitive_feedback(cognitive_state);
        
        // Run evolutionary step
        optimizer_->evolutionary_step();
        
        // Get evolved parameters
        auto best_genome = optimizer_->get_best_genome();
        std::cout << "   Best evolved fitness: " << best_genome.fitness_score << std::endl;
        
        // Apply evolved parameters to pattern extractor
        auto it = best_genome.hyperparameters.find("pattern_threshold");
        if (it != best_genome.hyperparameters.end()) {
            pattern_extractor_->set_pattern_threshold(it->second);
            std::cout << "   Updated pattern threshold to: " << it->second << std::endl;
        }
        
        // Log evolutionary insights
        auto insights = optimizer_->generate_evolutionary_insights();
        for (const auto& insight : insights) {
            std::cout << "   Evolutionary insight - " << insight.first << ": " << insight.second << std::endl;
        }
    }
    
    void apply_improvement_suggestions(const std::vector<std::string>& suggestions) {
        for (const auto& suggestion : suggestions) {
            std::cout << "   Applying: " << suggestion << std::endl;
            
            if (suggestion.find("learning parameters") != std::string::npos) {
                // Simulate parameter improvement
                system_metrics_["learning_rate"] = std::min(1.0, system_metrics_["learning_rate"] + 0.05);
            } else if (suggestion.find("monitoring frequency") != std::string::npos) {
                // Increase self-monitoring
                monitor_->adaptive_parameter_tuning();
            }
        }
    }
    
    double calculate_pattern_diversity(const std::vector<std::string>& patterns) {
        if (patterns.empty()) return 0.0;
        
        // Simple diversity metric based on unique pattern characteristics
        std::set<char> unique_chars;
        for (const auto& pattern : patterns) {
            for (char c : pattern) {
                unique_chars.insert(c);
            }
        }
        
        return static_cast<double>(unique_chars.size()) / (patterns.size() * 10.0);
    }
    
    double calculate_integration_score() {
        // Integration score based on component interaction
        double score = 0.0;
        
        // Meta-cognitive integration
        score += system_metrics_["self_awareness"] * 0.3;
        
        // Pattern extraction integration  
        score += (system_metrics_["pattern_count"] / 10.0) * 0.3;
        
        // Evolutionary integration
        score += system_metrics_["adaptation_rate"] * 0.4;
        
        return std::min(1.0, score);
    }
    
    double calculate_overall_performance(const MetaCognitiveTensor& state) {
        return state.calculate_overall_fitness() * 0.5 + 
               system_metrics_["integration_score"] * 0.3 +
               system_metrics_["pattern_diversity"] * 0.2;
    }
    
    void generate_final_report() {
        std::cout << "\n=== Phase 5 Integration Report ===" << std::endl;
        std::cout << "Total cognitive cycles: " << current_cycle_ << std::endl;
        
        // Performance trajectory
        if (!performance_history_.empty()) {
            std::cout << "Performance trajectory:" << std::endl;
            std::cout << "  Initial: " << performance_history_.front() << std::endl;
            std::cout << "  Final: " << performance_history_.back() << std::endl;
            std::cout << "  Improvement: " << (performance_history_.back() - performance_history_.front()) << std::endl;
        }
        
        // Meta-cognitive state
        auto final_state = monitor_->get_current_state();
        std::cout << "\nFinal Meta-Cognitive Tensor:" << std::endl;
        std::cout << "  Self-awareness: " << final_state.self_awareness_level << std::endl;
        std::cout << "  Cognitive complexity: " << final_state.cognitive_complexity << std::endl;
        std::cout << "  Meta-level: " << final_state.meta_level << std::endl;
        std::cout << "  Reflection depth: " << final_state.reflection_depth << std::endl;
        
        // Evolutionary statistics
        auto evolution_stats = optimizer_->get_evolution_statistics();
        std::cout << "\nEvolutionary Optimization Results:" << std::endl;
        std::cout << "  Generations completed: " << evolution_stats["current_generation"] << std::endl;
        std::cout << "  Best fitness achieved: " << evolution_stats["best_fitness"] << std::endl;
        std::cout << "  Population diversity: " << evolution_stats["population_diversity"] << std::endl;
        
        // System metrics summary
        std::cout << "\nSystem Integration Metrics:" << std::endl;
        for (const auto& metric : system_metrics_) {
            std::cout << "  " << metric.first << ": " << metric.second << std::endl;
        }
        
        // Save results to file
        save_results_to_file();
    }
    
    void save_results_to_file() {
        std::ofstream report_file("phase5_integration_results.json");
        report_file << "{\n";
        report_file << "  \"phase\": \"Phase 5: Recursive Meta-Cognition & Evolutionary Optimization\",\n";
        report_file << "  \"cycles_completed\": " << current_cycle_ << ",\n";
        report_file << "  \"final_performance\": " << (performance_history_.empty() ? 0.0 : performance_history_.back()) << ",\n";
        
        auto final_state = monitor_->get_current_state();
        report_file << "  \"meta_cognitive_state\": {\n";
        report_file << "    \"self_awareness\": " << final_state.self_awareness_level << ",\n";
        report_file << "    \"cognitive_complexity\": \"" << final_state.cognitive_complexity << "\",\n";
        report_file << "    \"meta_level\": \"" << final_state.meta_level << "\",\n";
        report_file << "    \"reflection_depth\": " << final_state.reflection_depth << "\n";
        report_file << "  },\n";
        
        auto stats = optimizer_->get_evolution_statistics();
        report_file << "  \"evolutionary_stats\": {\n";
        report_file << "    \"generations\": " << stats["current_generation"] << ",\n";
        report_file << "    \"best_fitness\": " << stats["best_fitness"] << ",\n";
        report_file << "    \"diversity\": " << stats["population_diversity"] << "\n";
        report_file << "  }\n";
        report_file << "}\n";
        report_file.close();
        
        std::cout << "\nResults saved to phase5_integration_results.json" << std::endl;
    }
};

int main() {
    std::cout << "=================================" << std::endl;
    std::cout << "Phase 5: Recursive Meta-Cognition" << std::endl;
    std::cout << "& Evolutionary Optimization Demo" << std::endl;
    std::cout << "=================================" << std::endl;
    
    // Create integrated cognitive system
    RecursiveCognitiveSystem system;
    
    std::cout << "\nInitializing recursive cognitive architecture..." << std::endl;
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    
    // Run multiple cognitive cycles
    for (int cycle = 0; cycle < 15; ++cycle) {
        system.run_recursive_cognitive_cycle();
        
        // Brief pause between cycles
        std::this_thread::sleep_for(std::chrono::milliseconds(800));
    }
    
    // Generate comprehensive final report
    system.generate_final_report();
    
    std::cout << "\n=== Phase 5 Integration Demonstration Complete ===" << std::endl;
    std::cout << "The system has demonstrated:" << std::endl;
    std::cout << "  ✓ Recursive meta-cognitive monitoring" << std::endl;
    std::cout << "  ✓ Evolutionary optimization of cognitive parameters" << std::endl;
    std::cout << "  ✓ Self-reflexive pattern extraction" << std::endl;
    std::cout << "  ✓ Integrated meta-cognitive and evolutionary feedback loops" << std::endl;
    std::cout << "  ✓ Multi-level recursive self-improvement" << std::endl;
    
    return 0;
}