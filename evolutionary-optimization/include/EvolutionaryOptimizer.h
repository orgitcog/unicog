/*
 * EvolutionaryOptimizer.h
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Evolutionary optimization framework for cognitive architecture evolution
 */

#ifndef _OPENCOG_EVOLUTIONARY_OPTIMIZER_H
#define _OPENCOG_EVOLUTIONARY_OPTIMIZER_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <random>
#include <functional>

namespace opencog {

// Forward declarations
struct MetaCognitiveTensor;
struct CognitivePerformanceMetrics;

/**
 * Cognitive Genome Structure
 * Represents the evolutionary encoding of cognitive architecture parameters
 */
struct CognitiveGenome {
    std::map<std::string, double> hyperparameters;     // Numerical parameters
    std::map<std::string, std::string> architecture;   // Structural configurations
    std::map<std::string, bool> features;              // Feature toggles
    double fitness_score;
    int generation;
    std::string genome_id;
    
    // Genome operations
    CognitiveGenome crossover(const CognitiveGenome& other) const;
    void mutate(double mutation_rate);
    std::vector<double> to_vector() const;
    static CognitiveGenome from_vector(const std::vector<double>& vec);
};

/**
 * Fitness Landscape Analysis
 * Tracks evolutionary trajectory and fitness landscape characteristics
 */
struct FitnessLandscape {
    std::vector<double> fitness_history;
    std::vector<std::vector<double>> parameter_trajectories;
    std::map<std::string, double> landscape_metrics;
    double convergence_rate;
    bool has_converged;
    
    void update(const std::vector<CognitiveGenome>& population);
    double calculate_diversity();
    double predict_convergence_time();
};

/**
 * Multi-Objective Fitness Function
 * Evaluates cognitive performance across multiple dimensions
 */
class MultiObjectiveFitness {
private:
    std::map<std::string, double> objective_weights_;
    std::map<std::string, std::function<double(const CognitiveGenome&)>> fitness_functions_;
    
public:
    MultiObjectiveFitness();
    
    void add_objective(const std::string& name, double weight,
                      std::function<double(const CognitiveGenome&)> fitness_func);
    
    double evaluate_fitness(const CognitiveGenome& genome);
    std::vector<double> evaluate_objectives(const CognitiveGenome& genome);
    
    void update_objective_weights(const std::map<std::string, double>& weights);
    std::map<std::string, double> get_objective_weights() const { return objective_weights_; }
};

/**
 * Evolutionary Optimizer
 * 
 * Implements genetic algorithms for cognitive architecture evolution,
 * enabling recursive self-improvement of the cognitive system.
 */
class EvolutionaryOptimizer
{
private:
    // Population management
    std::vector<CognitiveGenome> population_;
    std::vector<CognitiveGenome> elite_archive_;
    
    // Evolutionary parameters
    int population_size_;
    int max_generations_;
    int current_generation_;
    double mutation_rate_;
    double crossover_rate_;
    double elite_ratio_;
    
    // Fitness evaluation
    std::unique_ptr<MultiObjectiveFitness> fitness_evaluator_;
    FitnessLandscape fitness_landscape_;
    
    // Random number generation
    std::mt19937 rng_;
    std::uniform_real_distribution<> uniform_dist_;
    std::normal_distribution<> normal_dist_;
    
    // Adaptive parameters
    bool adaptive_mutation_;
    bool adaptive_selection_;
    
public:
    EvolutionaryOptimizer(int population_size = 100, int max_generations = 1000);
    ~EvolutionaryOptimizer();

    /**
     * Core Evolutionary Operations
     */
    
    /**
     * Initialize population with random cognitive genomes
     */
    void initialize_population();
    
    /**
     * Run complete evolutionary optimization cycle
     */
    CognitiveGenome evolve_cognitive_architecture();
    
    /**
     * Single evolutionary generation step
     */
    void evolutionary_step();
    
    /**
     * Evaluate fitness for entire population
     */
    void evaluate_population_fitness();
    
    /**
     * Selection Operations
     */
    
    /**
     * Tournament selection for parent selection
     */
    std::vector<CognitiveGenome> tournament_selection(int tournament_size = 3);
    
    /**
     * Elite selection - preserve best genomes
     */
    std::vector<CognitiveGenome> elite_selection(double elite_ratio);
    
    /**
     * Roulette wheel selection based on fitness
     */
    CognitiveGenome roulette_selection();
    
    /**
     * Genetic Operations
     */
    
    /**
     * Crossover operation between two parent genomes
     */
    std::pair<CognitiveGenome, CognitiveGenome> crossover(
        const CognitiveGenome& parent1, const CognitiveGenome& parent2);
    
    /**
     * Mutation operation on a genome
     */
    void mutate(CognitiveGenome& genome);
    
    /**
     * Adaptive mutation that adjusts based on population diversity
     */
    void adaptive_mutation_step();
    
    /**
     * Fitness Landscape Analysis
     */
    
    /**
     * Analyze current fitness landscape characteristics
     */
    std::map<std::string, double> analyze_fitness_landscape();
    
    /**
     * Detect convergence and stagnation
     */
    bool check_convergence();
    
    /**
     * Calculate population diversity metrics
     */
    double calculate_population_diversity();
    
    /**
     * Performance Tracking
     */
    
    /**
     * Get best genome from current population
     */
    CognitiveGenome get_best_genome() const;
    
    /**
     * Get evolution statistics and metrics
     */
    std::map<std::string, double> get_evolution_statistics();
    
    /**
     * Track evolutionary trajectory over generations
     */
    std::vector<double> get_fitness_trajectory() const;
    
    /**
     * Configuration and Control
     */
    
    /**
     * Set evolutionary parameters
     */
    void set_parameters(int pop_size, int max_gen, double mutation, double crossover);
    
    /**
     * Configure fitness evaluation criteria
     */
    void configure_fitness_objectives(const std::map<std::string, double>& objectives);
    
    /**
     * Enable/disable adaptive mechanisms
     */
    void set_adaptive_evolution(bool adaptive_mutation, bool adaptive_selection);
    
    /**
     * Integration with Meta-Cognitive System
     */
    
    /**
     * Update genome based on meta-cognitive insights
     */
    void integrate_metacognitive_feedback(const MetaCognitiveTensor& feedback);
    
    /**
     * Apply evolved parameters to cognitive system
     */
    void apply_evolved_parameters(const CognitiveGenome& genome);
    
    /**
     * Generate evolutionary insights for meta-cognitive system
     */
    std::map<std::string, double> generate_evolutionary_insights();
    
    // Accessors
    int get_current_generation() const { return current_generation_; }
    int get_population_size() const { return population_size_; }
    FitnessLandscape get_fitness_landscape() const { return fitness_landscape_; }

private:
    /**
     * Helper methods for internal operations
     */
    
    CognitiveGenome create_random_genome();
    void update_fitness_landscape();
    void update_adaptive_parameters();
    double calculate_genome_similarity(const CognitiveGenome& g1, const CognitiveGenome& g2);
    void maintain_elite_archive();
};

} // namespace opencog

#endif // _OPENCOG_EVOLUTIONARY_OPTIMIZER_H