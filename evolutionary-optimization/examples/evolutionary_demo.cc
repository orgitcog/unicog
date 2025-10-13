/*
 * evolutionary_demo.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Demonstration of evolutionary optimization for cognitive architecture
 */

#include "../include/EvolutionaryOptimizer.h"
#include "../../meta-cognition/include/MetaCognitiveMonitor.h"
#include <iostream>
#include <thread>
#include <chrono>

using namespace opencog;

// Mock fitness evaluation functions
double evaluate_accuracy_fitness(const CognitiveGenome& genome) {
    auto it = genome.hyperparameters.find("learning_rate");
    if (it != genome.hyperparameters.end()) {
        // Optimal learning rate around 0.3
        return 1.0 - std::abs(0.3 - it->second) * 2.0;
    }
    return 0.5;
}

double evaluate_efficiency_fitness(const CognitiveGenome& genome) {
    auto it = genome.hyperparameters.find("attention_threshold");
    if (it != genome.hyperparameters.end()) {
        // Efficiency peaks at moderate threshold
        return 1.0 - std::abs(0.6 - it->second);
    }
    return 0.5;
}

double evaluate_adaptability_fitness(const CognitiveGenome& genome) {
    auto adapt_it = genome.hyperparameters.find("adaptation_rate");
    bool has_adaptive = genome.features.count("adaptive_learning") && 
                       genome.features.at("adaptive_learning");
    
    double base_fitness = 0.5;
    if (adapt_it != genome.hyperparameters.end()) {
        base_fitness = adapt_it->second;
    }
    
    return has_adaptive ? std::min(1.0, base_fitness + 0.2) : base_fitness;
}

int main() {
    std::cout << "=== Phase 5: Evolutionary Optimization Demonstration ===" << std::endl;
    std::cout << "Initializing evolutionary cognitive architecture optimizer..." << std::endl;
    
    // Create evolutionary optimizer
    EvolutionaryOptimizer optimizer(50, 100); // 50 individuals, 100 generations max
    
    // Configure multi-objective fitness evaluation
    std::map<std::string, double> objectives = {
        {"accuracy", 0.4},
        {"efficiency", 0.3},
        {"adaptability", 0.3}
    };
    optimizer.configure_fitness_objectives(objectives);
    
    // Add custom fitness functions
    auto fitness_evaluator = std::make_unique<MultiObjectiveFitness>();
    fitness_evaluator->add_objective("accuracy", 0.4, evaluate_accuracy_fitness);
    fitness_evaluator->add_objective("efficiency", 0.3, evaluate_efficiency_fitness);
    fitness_evaluator->add_objective("adaptability", 0.3, evaluate_adaptability_fitness);
    
    std::cout << "Configured multi-objective fitness with accuracy (40%), efficiency (30%), adaptability (30%)" << std::endl;
    
    // Enable adaptive evolution
    optimizer.set_adaptive_evolution(true, true);
    
    // Create meta-cognitive monitor for feedback
    MetaCognitiveMonitor monitor(3, 0.1);
    
    std::cout << "\nStarting evolutionary optimization..." << std::endl;
    
    // Initialize population
    optimizer.initialize_population();
    
    // Evolution loop with periodic reporting
    for (int generation = 0; generation < 50; ++generation) {
        // Perform evolutionary step
        optimizer.evolutionary_step();
        
        // Get current statistics
        auto stats = optimizer.get_evolution_statistics();
        
        if (generation % 5 == 0) {
            std::cout << "\n--- Generation " << generation << " ---" << std::endl;
            std::cout << "Best fitness: " << stats["best_fitness"] << std::endl;
            std::cout << "Average fitness: " << stats["average_fitness"] << std::endl;
            std::cout << "Population diversity: " << stats["population_diversity"] << std::endl;
            std::cout << "Mutation rate: " << stats["mutation_rate"] << std::endl;
            
            // Analyze fitness landscape
            auto landscape = optimizer.analyze_fitness_landscape();
            std::cout << "Convergence rate: " << landscape["convergence_rate"] << std::endl;
            if (landscape["predicted_convergence_time"] > 0) {
                std::cout << "Predicted convergence in " << landscape["predicted_convergence_time"] << " generations" << std::endl;
            }
        }
        
        // Get best genome and analyze
        auto best_genome = optimizer.get_best_genome();
        
        // Update meta-cognitive feedback every 10 generations
        if (generation % 10 == 9) {
            // Create meta-cognitive feedback
            MetaCognitiveTensor feedback;
            feedback.self_awareness_level = 0.8;
            feedback.performance_metric = {stats["best_fitness"], stats["population_diversity"], 0.7};
            feedback.evolutionary_generation = generation;
            feedback.fitness_score = stats["best_fitness"];
            feedback.adaptation_rate = stats["mutation_rate"];
            feedback.cognitive_complexity = "complex";
            feedback.meta_level = "meta";
            feedback.reflection_depth = 2;
            
            // Adjust optimization target based on performance
            if (stats["best_fitness"] < 0.6) {
                feedback.optimization_target = "accuracy";
            } else if (stats["population_diversity"] < 0.3) {
                feedback.optimization_target = "generalization";
            } else {
                feedback.optimization_target = "efficiency";
            }
            
            std::cout << "Integrating meta-cognitive feedback - target: " << feedback.optimization_target << std::endl;
            optimizer.integrate_metacognitive_feedback(feedback);
        }
        
        // Check for convergence
        if (optimizer.check_convergence()) {
            std::cout << "\nConvergence detected at generation " << generation << std::endl;
            break;
        }
        
        // Brief pause for demonstration
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
    
    std::cout << "\n=== Evolution Complete ===" << std::endl;
    
    // Final results
    auto final_stats = optimizer.get_evolution_statistics();
    auto best_genome = optimizer.get_best_genome();
    auto fitness_trajectory = optimizer.get_fitness_trajectory();
    
    std::cout << "\nFinal Results:" << std::endl;
    std::cout << "  Total generations: " << final_stats["current_generation"] << std::endl;
    std::cout << "  Best fitness achieved: " << final_stats["best_fitness"] << std::endl;
    std::cout << "  Final population diversity: " << final_stats["population_diversity"] << std::endl;
    
    std::cout << "\nBest Evolved Cognitive Genome:" << std::endl;
    std::cout << "  Generation: " << best_genome.generation << std::endl;
    std::cout << "  Fitness: " << best_genome.fitness_score << std::endl;
    std::cout << "  Genome ID: " << best_genome.genome_id << std::endl;
    
    std::cout << "\n  Hyperparameters:" << std::endl;
    for (const auto& param : best_genome.hyperparameters) {
        std::cout << "    " << param.first << ": " << param.second << std::endl;
    }
    
    std::cout << "\n  Features:" << std::endl;
    for (const auto& feature : best_genome.features) {
        std::cout << "    " << feature.first << ": " << (feature.second ? "enabled" : "disabled") << std::endl;
    }
    
    std::cout << "\n  Architecture:" << std::endl;
    for (const auto& arch : best_genome.architecture) {
        std::cout << "    " << arch.first << ": " << arch.second << std::endl;
    }
    
    // Fitness trajectory analysis
    if (fitness_trajectory.size() >= 2) {
        std::cout << "\nFitness Evolution:" << std::endl;
        std::cout << "  Initial fitness: " << fitness_trajectory.front() << std::endl;
        std::cout << "  Final fitness: " << fitness_trajectory.back() << std::endl;
        std::cout << "  Total improvement: " << (fitness_trajectory.back() - fitness_trajectory.front()) << std::endl;
        
        // Calculate average improvement rate
        double total_improvement = fitness_trajectory.back() - fitness_trajectory.front();
        double improvement_rate = total_improvement / fitness_trajectory.size();
        std::cout << "  Average improvement per generation: " << improvement_rate << std::endl;
    }
    
    // Generate evolutionary insights
    auto insights = optimizer.generate_evolutionary_insights();
    std::cout << "\nEvolutionary Insights:" << std::endl;
    for (const auto& insight : insights) {
        std::cout << "  " << insight.first << ": " << insight.second << std::endl;
    }
    
    // Demonstrate application of evolved parameters
    std::cout << "\nApplying evolved parameters to cognitive system..." << std::endl;
    optimizer.apply_evolved_parameters(best_genome);
    
    std::cout << "\n=== Evolutionary Optimization Demonstration Complete ===" << std::endl;
    
    return 0;
}