/*
 * EvolutionaryOptimizer.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Implementation of evolutionary optimization framework
 */

#include "../include/EvolutionaryOptimizer.h"
#include "../../meta-cognition/include/MetaCognitiveMonitor.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <cmath>
#include <iostream>
#include <random>

namespace opencog {

// CognitiveGenome implementation
CognitiveGenome CognitiveGenome::crossover(const CognitiveGenome& other) const {
    CognitiveGenome offspring;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);
    
    // Crossover hyperparameters
    for (const auto& param : hyperparameters) {
        if (dis(gen) < 0.5) {
            offspring.hyperparameters[param.first] = param.second;
        } else if (other.hyperparameters.count(param.first)) {
            offspring.hyperparameters[param.first] = other.hyperparameters.at(param.first);
        } else {
            offspring.hyperparameters[param.first] = param.second;
        }
    }
    
    // Crossover architecture components
    for (const auto& arch : architecture) {
        if (dis(gen) < 0.5) {
            offspring.architecture[arch.first] = arch.second;
        } else if (other.architecture.count(arch.first)) {
            offspring.architecture[arch.first] = other.architecture.at(arch.first);
        } else {
            offspring.architecture[arch.first] = arch.second;
        }
    }
    
    // Crossover features
    for (const auto& feature : features) {
        if (dis(gen) < 0.5) {
            offspring.features[feature.first] = feature.second;
        } else if (other.features.count(feature.first)) {
            offspring.features[feature.first] = other.features.at(feature.first);
        } else {
            offspring.features[feature.first] = feature.second;
        }
    }
    
    offspring.fitness_score = 0.0;
    offspring.generation = std::max(generation, other.generation) + 1;
    offspring.genome_id = "offspring_" + std::to_string(offspring.generation) + "_" + 
                          std::to_string(std::hash<std::string>{}(std::to_string(dis(gen))));
    
    return offspring;
}

void CognitiveGenome::mutate(double mutation_rate) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);
    std::normal_distribution<> normal(0.0, 0.1);
    
    // Mutate hyperparameters
    for (auto& param : hyperparameters) {
        if (dis(gen) < mutation_rate) {
            param.second += normal(gen);
            param.second = std::max(0.0, std::min(1.0, param.second)); // Clamp to [0,1]
        }
    }
    
    // Mutate features (random toggle)
    for (auto& feature : features) {
        if (dis(gen) < mutation_rate * 0.1) { // Lower rate for binary features
            feature.second = !feature.second;
        }
    }
    
    // Architecture mutations are more conservative
    // (In a full implementation, this would modify network topology)
}

std::vector<double> CognitiveGenome::to_vector() const {
    std::vector<double> vec;
    
    // Convert hyperparameters
    for (const auto& param : hyperparameters) {
        vec.push_back(param.second);
    }
    
    // Convert features as 0/1
    for (const auto& feature : features) {
        vec.push_back(feature.second ? 1.0 : 0.0);
    }
    
    vec.push_back(fitness_score);
    vec.push_back(static_cast<double>(generation));
    
    return vec;
}

CognitiveGenome CognitiveGenome::from_vector(const std::vector<double>& vec) {
    CognitiveGenome genome;
    
    if (vec.size() >= 8) {
        // Standard parameter set
        genome.hyperparameters["learning_rate"] = vec[0];
        genome.hyperparameters["attention_threshold"] = vec[1];
        genome.hyperparameters["pattern_threshold"] = vec[2];
        genome.hyperparameters["adaptation_rate"] = vec[3];
        
        genome.features["self_reflexivity"] = vec[4] > 0.5;
        genome.features["recursive_monitoring"] = vec[5] > 0.5;
        
        genome.fitness_score = vec[6];
        genome.generation = static_cast<int>(vec[7]);
        
        genome.architecture["cognitive_layers"] = "deep";
        genome.genome_id = "reconstructed_" + std::to_string(genome.generation);
    }
    
    return genome;
}

// FitnessLandscape implementation
void FitnessLandscape::update(const std::vector<CognitiveGenome>& population) {
    // Update fitness history with best fitness from population
    if (!population.empty()) {
        auto best_genome = std::max_element(population.begin(), population.end(),
            [](const CognitiveGenome& a, const CognitiveGenome& b) {
                return a.fitness_score < b.fitness_score;
            });
        fitness_history.push_back(best_genome->fitness_score);
    }
    
    // Update parameter trajectories
    std::vector<double> avg_params(6, 0.0);
    for (const auto& genome : population) {
        auto vec = genome.to_vector();
        for (size_t i = 0; i < std::min(avg_params.size(), vec.size()); ++i) {
            avg_params[i] += vec[i];
        }
    }
    
    if (!population.empty()) {
        for (auto& param : avg_params) {
            param /= population.size();
        }
        parameter_trajectories.push_back(avg_params);
    }
    
    // Update convergence metrics
    if (fitness_history.size() >= 10) {
        double recent_variance = 0.0;
        double recent_mean = 0.0;
        int window = std::min(10, static_cast<int>(fitness_history.size()));
        
        for (int i = fitness_history.size() - window; i < static_cast<int>(fitness_history.size()); ++i) {
            recent_mean += fitness_history[i];
        }
        recent_mean /= window;
        
        for (int i = fitness_history.size() - window; i < static_cast<int>(fitness_history.size()); ++i) {
            recent_variance += std::pow(fitness_history[i] - recent_mean, 2);
        }
        recent_variance /= window;
        
        convergence_rate = 1.0 / (1.0 + recent_variance);
        has_converged = recent_variance < 0.001;
    }
}

double FitnessLandscape::calculate_diversity() {
    if (parameter_trajectories.empty()) return 0.0;
    
    const auto& latest = parameter_trajectories.back();
    double diversity = 0.0;
    
    for (size_t i = 0; i < latest.size(); ++i) {
        double param_variance = 0.0;
        double param_mean = latest[i];
        
        // Calculate variance across recent trajectory
        int window = std::min(5, static_cast<int>(parameter_trajectories.size()));
        for (int j = parameter_trajectories.size() - window; j < static_cast<int>(parameter_trajectories.size()); ++j) {
            if (i < parameter_trajectories[j].size()) {
                param_variance += std::pow(parameter_trajectories[j][i] - param_mean, 2);
            }
        }
        param_variance /= window;
        diversity += param_variance;
    }
    
    return diversity / latest.size();
}

double FitnessLandscape::predict_convergence_time() {
    if (fitness_history.size() < 5) return -1.0; // Insufficient data
    
    // Simple linear regression on recent fitness improvements
    double sum_x = 0.0, sum_y = 0.0, sum_xy = 0.0, sum_x2 = 0.0;
    int n = std::min(10, static_cast<int>(fitness_history.size()));
    
    for (int i = 0; i < n; ++i) {
        double x = static_cast<double>(i);
        double y = fitness_history[fitness_history.size() - n + i];
        sum_x += x;
        sum_y += y;
        sum_xy += x * y;
        sum_x2 += x * x;
    }
    
    double slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x);
    
    if (slope <= 0.0) return -1.0; // No improvement trend
    
    double current_fitness = fitness_history.back();
    double target_fitness = 0.95; // Target 95% fitness
    
    return (target_fitness - current_fitness) / slope;
}

// MultiObjectiveFitness implementation
MultiObjectiveFitness::MultiObjectiveFitness() {
    // Initialize with default objectives
    objective_weights_["accuracy"] = 0.3;
    objective_weights_["efficiency"] = 0.25;
    objective_weights_["adaptability"] = 0.25;
    objective_weights_["stability"] = 0.2;
}

void MultiObjectiveFitness::add_objective(const std::string& name, double weight,
                                        std::function<double(const CognitiveGenome&)> fitness_func) {
    objective_weights_[name] = weight;
    fitness_functions_[name] = fitness_func;
}

double MultiObjectiveFitness::evaluate_fitness(const CognitiveGenome& genome) {
    double total_fitness = 0.0;
    double total_weight = 0.0;
    
    for (const auto& objective : objective_weights_) {
        double weight = objective.second;
        double fitness_value = 0.5; // Default value
        
        // Use custom fitness function if available
        if (fitness_functions_.count(objective.first)) {
            fitness_value = fitness_functions_[objective.first](genome);
        } else {
            // Default fitness calculations based on genome parameters
            if (objective.first == "accuracy") {
                auto it = genome.hyperparameters.find("learning_rate");
                if (it != genome.hyperparameters.end()) {
                    fitness_value = std::min(1.0, it->second * 2.0); // Favor higher learning rates
                }
            } else if (objective.first == "efficiency") {
                auto it = genome.hyperparameters.find("attention_threshold");
                if (it != genome.hyperparameters.end()) {
                    fitness_value = 1.0 - std::abs(0.7 - it->second); // Optimal around 0.7
                }
            } else if (objective.first == "adaptability") {
                auto it = genome.hyperparameters.find("adaptation_rate");
                if (it != genome.hyperparameters.end()) {
                    fitness_value = it->second;
                }
            } else if (objective.first == "stability") {
                // Prefer balanced parameter values
                double param_variance = 0.0;
                for (const auto& param : genome.hyperparameters) {
                    param_variance += std::pow(param.second - 0.5, 2);
                }
                fitness_value = 1.0 / (1.0 + param_variance);
            }
        }
        
        total_fitness += weight * fitness_value;
        total_weight += weight;
    }
    
    return total_weight > 0.0 ? total_fitness / total_weight : 0.0;
}

std::vector<double> MultiObjectiveFitness::evaluate_objectives(const CognitiveGenome& genome) {
    std::vector<double> objectives;
    
    for (const auto& objective : objective_weights_) {
        double fitness_value = 0.5; // Default
        
        if (fitness_functions_.count(objective.first)) {
            fitness_value = fitness_functions_[objective.first](genome);
        }
        
        objectives.push_back(fitness_value);
    }
    
    return objectives;
}

void MultiObjectiveFitness::update_objective_weights(const std::map<std::string, double>& weights) {
    for (const auto& weight : weights) {
        if (objective_weights_.count(weight.first)) {
            objective_weights_[weight.first] = weight.second;
        }
    }
}

// EvolutionaryOptimizer implementation
EvolutionaryOptimizer::EvolutionaryOptimizer(int population_size, int max_generations)
    : population_size_(population_size), max_generations_(max_generations),
      current_generation_(0), mutation_rate_(0.1), crossover_rate_(0.8),
      elite_ratio_(0.1), rng_(std::random_device{}()),
      uniform_dist_(0.0, 1.0), normal_dist_(0.0, 1.0),
      adaptive_mutation_(true), adaptive_selection_(true)
{
    fitness_evaluator_ = std::make_unique<MultiObjectiveFitness>();
    
    std::cout << "[EvolutionaryOptimizer] Initialized with population size: " 
              << population_size << ", max generations: " << max_generations << std::endl;
}

EvolutionaryOptimizer::~EvolutionaryOptimizer() {
    std::cout << "[EvolutionaryOptimizer] Completed evolution at generation: " 
              << current_generation_ << std::endl;
}

void EvolutionaryOptimizer::initialize_population() {
    population_.clear();
    population_.reserve(population_size_);
    
    for (int i = 0; i < population_size_; ++i) {
        population_.push_back(create_random_genome());
    }
    
    std::cout << "[EvolutionaryOptimizer] Initialized population of " 
              << population_.size() << " genomes" << std::endl;
}

CognitiveGenome EvolutionaryOptimizer::evolve_cognitive_architecture() {
    std::cout << "[EvolutionaryOptimizer] Starting evolutionary optimization" << std::endl;
    
    initialize_population();
    
    for (current_generation_ = 0; current_generation_ < max_generations_; ++current_generation_) {
        evolutionary_step();
        
        // Check for convergence
        if (check_convergence()) {
            std::cout << "[EvolutionaryOptimizer] Convergence detected at generation " 
                      << current_generation_ << std::endl;
            break;
        }
        
        // Periodic updates
        if (current_generation_ % 100 == 0) {
            auto stats = get_evolution_statistics();
            std::cout << "[EvolutionaryOptimizer] Generation " << current_generation_ 
                      << " - Best fitness: " << stats["best_fitness"] 
                      << ", Diversity: " << stats["population_diversity"] << std::endl;
        }
    }
    
    return get_best_genome();
}

void EvolutionaryOptimizer::evolutionary_step() {
    // Evaluate fitness for all genomes
    evaluate_population_fitness();
    
    // Update fitness landscape
    fitness_landscape_.update(population_);
    
    // Selection and reproduction
    std::vector<CognitiveGenome> new_population;
    
    // Elite preservation
    auto elites = elite_selection(elite_ratio_);
    new_population.insert(new_population.end(), elites.begin(), elites.end());
    
    // Generate offspring to fill remaining population
    while (new_population.size() < static_cast<size_t>(population_size_)) {
        auto parents = tournament_selection(3);
        
        if (parents.size() >= 2 && uniform_dist_(rng_) < crossover_rate_) {
            // Crossover
            auto offspring_pair = crossover(parents[0], parents[1]);
            
            mutate(offspring_pair.first);
            mutate(offspring_pair.second);
            
            new_population.push_back(offspring_pair.first);
            if (new_population.size() < static_cast<size_t>(population_size_)) {
                new_population.push_back(offspring_pair.second);
            }
        } else if (!parents.empty()) {
            // Mutation only
            CognitiveGenome mutant = parents[0];
            mutate(mutant);
            new_population.push_back(mutant);
        }
    }
    
    population_ = new_population;
    
    // Adaptive parameter updates
    if (adaptive_mutation_ || adaptive_selection_) {
        update_adaptive_parameters();
    }
    
    // Maintain elite archive
    maintain_elite_archive();
}

void EvolutionaryOptimizer::evaluate_population_fitness() {
    for (auto& genome : population_) {
        genome.fitness_score = fitness_evaluator_->evaluate_fitness(genome);
    }
}

std::vector<CognitiveGenome> EvolutionaryOptimizer::tournament_selection(int tournament_size) {
    std::vector<CognitiveGenome> selected;
    std::uniform_int_distribution<> index_dist(0, population_.size() - 1);
    
    for (int i = 0; i < 2; ++i) { // Select 2 parents
        CognitiveGenome best_candidate;
        double best_fitness = -1.0;
        
        for (int j = 0; j < tournament_size; ++j) {
            int idx = index_dist(rng_);
            if (population_[idx].fitness_score > best_fitness) {
                best_fitness = population_[idx].fitness_score;
                best_candidate = population_[idx];
            }
        }
        
        selected.push_back(best_candidate);
    }
    
    return selected;
}

std::vector<CognitiveGenome> EvolutionaryOptimizer::elite_selection(double elite_ratio) {
    int elite_count = static_cast<int>(population_size_ * elite_ratio);
    
    // Sort population by fitness
    auto sorted_population = population_;
    std::sort(sorted_population.begin(), sorted_population.end(),
        [](const CognitiveGenome& a, const CognitiveGenome& b) {
            return a.fitness_score > b.fitness_score;
        });
    
    std::vector<CognitiveGenome> elites;
    for (int i = 0; i < elite_count && i < static_cast<int>(sorted_population.size()); ++i) {
        elites.push_back(sorted_population[i]);
    }
    
    return elites;
}

CognitiveGenome EvolutionaryOptimizer::roulette_selection() {
    // Calculate total fitness
    double total_fitness = 0.0;
    for (const auto& genome : population_) {
        total_fitness += std::max(0.0, genome.fitness_score);
    }
    
    if (total_fitness == 0.0) {
        // Fallback to random selection
        std::uniform_int_distribution<> index_dist(0, population_.size() - 1);
        return population_[index_dist(rng_)];
    }
    
    // Roulette wheel selection
    double selector = uniform_dist_(rng_) * total_fitness;
    double cumulative = 0.0;
    
    for (const auto& genome : population_) {
        cumulative += std::max(0.0, genome.fitness_score);
        if (cumulative >= selector) {
            return genome;
        }
    }
    
    return population_.back(); // Fallback
}

std::pair<CognitiveGenome, CognitiveGenome> EvolutionaryOptimizer::crossover(
    const CognitiveGenome& parent1, const CognitiveGenome& parent2) {
    
    CognitiveGenome offspring1 = parent1.crossover(parent2);
    CognitiveGenome offspring2 = parent2.crossover(parent1);
    
    return {offspring1, offspring2};
}

void EvolutionaryOptimizer::mutate(CognitiveGenome& genome) {
    genome.mutate(mutation_rate_);
}

void EvolutionaryOptimizer::adaptive_mutation_step() {
    double diversity = calculate_population_diversity();
    
    if (diversity < 0.1) {
        // Low diversity - increase mutation
        mutation_rate_ = std::min(0.5, mutation_rate_ * 1.1);
    } else if (diversity > 0.5) {
        // High diversity - decrease mutation
        mutation_rate_ = std::max(0.01, mutation_rate_ * 0.95);
    }
}

std::map<std::string, double> EvolutionaryOptimizer::analyze_fitness_landscape() {
    std::map<std::string, double> analysis;
    
    analysis["diversity"] = fitness_landscape_.calculate_diversity();
    analysis["convergence_rate"] = fitness_landscape_.convergence_rate;
    analysis["predicted_convergence_time"] = fitness_landscape_.predict_convergence_time();
    analysis["has_converged"] = fitness_landscape_.has_converged ? 1.0 : 0.0;
    
    if (!fitness_landscape_.fitness_history.empty()) {
        analysis["current_best_fitness"] = fitness_landscape_.fitness_history.back();
        
        if (fitness_landscape_.fitness_history.size() >= 2) {
            analysis["fitness_improvement"] = 
                fitness_landscape_.fitness_history.back() - 
                fitness_landscape_.fitness_history[fitness_landscape_.fitness_history.size() - 2];
        }
    }
    
    return analysis;
}

bool EvolutionaryOptimizer::check_convergence() {
    if (fitness_landscape_.fitness_history.size() < 20) return false;
    
    // Check if fitness has plateaued
    double recent_improvement = 0.0;
    int window = 10;
    
    for (int i = fitness_landscape_.fitness_history.size() - window; 
         i < static_cast<int>(fitness_landscape_.fitness_history.size()) - 1; ++i) {
        recent_improvement += fitness_landscape_.fitness_history[i+1] - fitness_landscape_.fitness_history[i];
    }
    
    return std::abs(recent_improvement) < 0.001; // Very small improvement threshold
}

double EvolutionaryOptimizer::calculate_population_diversity() {
    if (population_.size() < 2) return 0.0;
    
    double total_distance = 0.0;
    int comparisons = 0;
    
    for (size_t i = 0; i < population_.size(); ++i) {
        for (size_t j = i + 1; j < population_.size(); ++j) {
            total_distance += calculate_genome_similarity(population_[i], population_[j]);
            comparisons++;
        }
    }
    
    return comparisons > 0 ? 1.0 - (total_distance / comparisons) : 0.0;
}

CognitiveGenome EvolutionaryOptimizer::get_best_genome() const {
    if (population_.empty()) {
        return CognitiveGenome{}; // Return empty genome
    }
    
    auto best_it = std::max_element(population_.begin(), population_.end(),
        [](const CognitiveGenome& a, const CognitiveGenome& b) {
            return a.fitness_score < b.fitness_score;
        });
    
    return *best_it;
}

std::map<std::string, double> EvolutionaryOptimizer::get_evolution_statistics() {
    std::map<std::string, double> stats;
    
    if (!population_.empty()) {
        auto best_genome = get_best_genome();
        stats["best_fitness"] = best_genome.fitness_score;
        
        double avg_fitness = 0.0;
        for (const auto& genome : population_) {
            avg_fitness += genome.fitness_score;
        }
        stats["average_fitness"] = avg_fitness / population_.size();
    }
    
    stats["current_generation"] = static_cast<double>(current_generation_);
    stats["population_diversity"] = calculate_population_diversity();
    stats["mutation_rate"] = mutation_rate_;
    stats["crossover_rate"] = crossover_rate_;
    
    return stats;
}

std::vector<double> EvolutionaryOptimizer::get_fitness_trajectory() const {
    return fitness_landscape_.fitness_history;
}

void EvolutionaryOptimizer::set_parameters(int pop_size, int max_gen, double mutation, double crossover) {
    population_size_ = pop_size;
    max_generations_ = max_gen;
    mutation_rate_ = mutation;
    crossover_rate_ = crossover;
    
    std::cout << "[EvolutionaryOptimizer] Parameters updated: pop=" << pop_size 
              << ", gen=" << max_gen << ", mut=" << mutation << ", cross=" << crossover << std::endl;
}

void EvolutionaryOptimizer::configure_fitness_objectives(const std::map<std::string, double>& objectives) {
    fitness_evaluator_->update_objective_weights(objectives);
}

void EvolutionaryOptimizer::set_adaptive_evolution(bool adaptive_mutation, bool adaptive_selection) {
    adaptive_mutation_ = adaptive_mutation;
    adaptive_selection_ = adaptive_selection;
}

void EvolutionaryOptimizer::integrate_metacognitive_feedback(const MetaCognitiveTensor& feedback) {
    // Update fitness objectives based on meta-cognitive insights
    std::map<std::string, double> new_weights;
    
    // Adjust weights based on optimization target
    if (feedback.optimization_target == "accuracy") {
        new_weights["accuracy"] = 0.5;
        new_weights["efficiency"] = 0.2;
        new_weights["adaptability"] = 0.2;
        new_weights["stability"] = 0.1;
    } else if (feedback.optimization_target == "efficiency") {
        new_weights["accuracy"] = 0.2;
        new_weights["efficiency"] = 0.5;
        new_weights["adaptability"] = 0.2;
        new_weights["stability"] = 0.1;
    } else if (feedback.optimization_target == "generalization") {
        new_weights["accuracy"] = 0.25;
        new_weights["efficiency"] = 0.25;
        new_weights["adaptability"] = 0.4;
        new_weights["stability"] = 0.1;
    }
    
    configure_fitness_objectives(new_weights);
    
    // Adjust evolutionary parameters based on meta-cognitive state
    if (feedback.adaptation_rate > 0.8) {
        mutation_rate_ = std::min(0.3, mutation_rate_ * 1.2); // Increase exploration
    } else if (feedback.adaptation_rate < 0.2) {
        mutation_rate_ = std::max(0.05, mutation_rate_ * 0.8); // Decrease exploration
    }
}

void EvolutionaryOptimizer::apply_evolved_parameters(const CognitiveGenome& genome) {
    std::cout << "[EvolutionaryOptimizer] Applying evolved parameters from generation " 
              << genome.generation << std::endl;
    
    // In a full implementation, this would update the actual cognitive system parameters
    for (const auto& param : genome.hyperparameters) {
        std::cout << "  " << param.first << ": " << param.second << std::endl;
    }
    
    for (const auto& feature : genome.features) {
        std::cout << "  " << feature.first << ": " << (feature.second ? "enabled" : "disabled") << std::endl;
    }
}

std::map<std::string, double> EvolutionaryOptimizer::generate_evolutionary_insights() {
    std::map<std::string, double> insights;
    
    auto landscape_analysis = analyze_fitness_landscape();
    auto stats = get_evolution_statistics();
    
    insights["evolutionary_progress"] = landscape_analysis["fitness_improvement"];
    insights["population_health"] = stats["population_diversity"];
    insights["convergence_status"] = landscape_analysis["convergence_rate"];
    insights["optimization_efficiency"] = stats["best_fitness"] / (current_generation_ + 1);
    
    return insights;
}

// Private helper methods
CognitiveGenome EvolutionaryOptimizer::create_random_genome() {
    CognitiveGenome genome;
    
    // Random hyperparameters
    genome.hyperparameters["learning_rate"] = uniform_dist_(rng_) * 0.5 + 0.01; // [0.01, 0.51]
    genome.hyperparameters["attention_threshold"] = uniform_dist_(rng_) * 0.8 + 0.1; // [0.1, 0.9]
    genome.hyperparameters["pattern_threshold"] = uniform_dist_(rng_) * 0.6 + 0.3; // [0.3, 0.9]
    genome.hyperparameters["adaptation_rate"] = uniform_dist_(rng_) * 0.3 + 0.05; // [0.05, 0.35]
    
    // Random features
    genome.features["self_reflexivity"] = uniform_dist_(rng_) > 0.5;
    genome.features["recursive_monitoring"] = uniform_dist_(rng_) > 0.4;
    genome.features["adaptive_learning"] = uniform_dist_(rng_) > 0.3;
    
    // Architecture (simplified)
    std::vector<std::string> architectures = {"shallow", "deep", "hierarchical", "distributed"};
    genome.architecture["cognitive_layers"] = architectures[static_cast<int>(uniform_dist_(rng_) * architectures.size())];
    genome.architecture["attention_mechanism"] = uniform_dist_(rng_) > 0.5 ? "global" : "local";
    
    genome.fitness_score = 0.0;
    genome.generation = 0;
    genome.genome_id = "random_" + std::to_string(std::hash<std::string>{}(std::to_string(uniform_dist_(rng_))));
    
    return genome;
}

void EvolutionaryOptimizer::update_fitness_landscape() {
    fitness_landscape_.update(population_);
}

void EvolutionaryOptimizer::update_adaptive_parameters() {
    if (adaptive_mutation_) {
        adaptive_mutation_step();
    }
    
    // Adaptive selection could adjust tournament size, etc.
    if (adaptive_selection_) {
        double diversity = calculate_population_diversity();
        if (diversity < 0.2) {
            // Increase selection pressure to maintain diversity
            elite_ratio_ = std::max(0.05, elite_ratio_ * 0.8);
        } else if (diversity > 0.6) {
            // Decrease selection pressure to focus search
            elite_ratio_ = std::min(0.2, elite_ratio_ * 1.1);
        }
    }
}

double EvolutionaryOptimizer::calculate_genome_similarity(const CognitiveGenome& g1, const CognitiveGenome& g2) {
    double similarity = 0.0;
    int comparisons = 0;
    
    // Compare hyperparameters
    for (const auto& param1 : g1.hyperparameters) {
        if (g2.hyperparameters.count(param1.first)) {
            similarity += 1.0 - std::abs(param1.second - g2.hyperparameters.at(param1.first));
            comparisons++;
        }
    }
    
    // Compare features
    for (const auto& feature1 : g1.features) {
        if (g2.features.count(feature1.first)) {
            similarity += (feature1.second == g2.features.at(feature1.first)) ? 1.0 : 0.0;
            comparisons++;
        }
    }
    
    return comparisons > 0 ? similarity / comparisons : 0.0;
}

void EvolutionaryOptimizer::maintain_elite_archive() {
    // Add current best to elite archive
    auto best = get_best_genome();
    
    // Check if this genome is already in archive (avoid duplicates)
    bool is_duplicate = false;
    for (const auto& elite : elite_archive_) {
        if (calculate_genome_similarity(best, elite) > 0.95) {
            is_duplicate = true;
            break;
        }
    }
    
    if (!is_duplicate) {
        elite_archive_.push_back(best);
        
        // Maintain archive size limit
        if (elite_archive_.size() > 50) {
            // Remove oldest elites
            elite_archive_.erase(elite_archive_.begin());
        }
    }
}

} // namespace opencog