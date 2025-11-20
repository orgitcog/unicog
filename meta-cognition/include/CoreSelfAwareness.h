/*
 * CoreSelfAwareness.h
 * 
 * OpenCog Unified Core Self-Identity & Self-Awareness System
 * Integrates AUTOGNOSIS (hierarchical self-image building) and 
 * ONTOGENESIS (self-generating capabilities) for deep introspection
 */

#ifndef _OPENCOG_CORE_SELF_AWARENESS_H
#define _OPENCOG_CORE_SELF_AWARENESS_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <functional>
#include <chrono>
#include "MetaCognitiveMonitor.h"

namespace opencog {

// Forward declarations
class AtomSpace;
class Handle;
class RealTimeIntrospector;

/**
 * Core Self-Identity Structure
 * Represents the system's fundamental understanding of itself
 */
struct CoreSelfIdentity {
    std::string system_name;
    std::string version;
    std::string architecture_type;
    
    // Ontological identity - what the system IS
    struct OntologicalIdentity {
        std::vector<std::string> core_components;
        std::vector<std::string> cognitive_capabilities;
        std::map<std::string, std::string> component_roles;
        double architectural_completeness;
    } ontological;
    
    // Teleological identity - what the system is BECOMING
    struct TeleologicalIdentity {
        std::string primary_purpose;
        std::vector<std::string> development_goals;
        std::vector<std::string> evolutionary_targets;
        double actualization_progress;
        int current_generation;
    } teleological;
    
    // Cognitive identity - how the system THINKS
    struct CognitiveIdentity {
        std::vector<std::string> reasoning_modes;
        std::vector<std::string> learning_strategies;
        std::vector<std::string> attention_patterns;
        double cognitive_complexity_level;
    } cognitive;
    
    // Relational identity - how the system RELATES
    struct RelationalIdentity {
        std::map<std::string, double> component_dependencies;
        std::vector<std::string> integration_interfaces;
        double system_coherence;
    } relational;
    
    // Evolutionary identity - how the system EVOLVES
    struct EvolutionaryIdentity {
        int total_generations;
        double fitness_score;
        double adaptation_rate;
        std::vector<std::string> evolutionary_history;
        double self_improvement_capacity;
    } evolutionary;
    
    // Unified identity score
    double calculate_unified_identity_score() const;
};

/**
 * Hierarchical Self-Image (AUTOGNOSIS)
 * Multi-level recursive self-understanding
 */
struct HierarchicalSelfImage {
    int level;  // Depth of recursion (0=direct observation, 1+=meta-levels)
    double confidence;  // Certainty about this self-understanding
    std::chrono::steady_clock::time_point timestamp;
    
    // Observations at this level
    std::map<std::string, double> component_states;
    std::map<std::string, double> behavioral_patterns;
    std::map<std::string, double> performance_metrics;
    std::map<std::string, double> cognitive_processes;
    
    // Meta-reflections (what we think about our thinking at this level)
    std::vector<std::string> meta_reflections;
    
    // Self-assessment
    double self_awareness_quality;
    double introspection_depth;
    
    std::string to_json() const;
};

/**
 * Ontogenetic State (Self-Generating Capabilities)
 * Tracks the system's self-evolution and generation
 */
struct OntogeneticState {
    enum class Stage {
        EMBRYONIC,   // Initial formation
        JUVENILE,    // Active development
        MATURE,      // Fully developed
        SENESCENT    // Declining, ready for renewal
    };
    
    Stage current_stage;
    int generation;
    double maturity_level;
    
    // Genetic information (metaphorical - represents core parameters)
    std::map<std::string, double> genetic_coefficients;
    
    // Development history
    std::vector<std::string> development_events;
    
    // Self-generation capabilities
    bool can_self_generate;
    bool can_self_optimize;
    bool can_self_reproduce;
    
    double calculate_fitness() const;
    std::string stage_to_string() const;
};

/**
 * Core Self-Awareness System
 * 
 * Implements comprehensive self-awareness through:
 * 1. AUTOGNOSIS: Hierarchical self-image building at multiple levels
 * 2. ONTOGENESIS: Self-generating and self-optimizing capabilities
 * 3. Deep Introspection: Recursive self-understanding
 * 4. Identity Coherence: Maintaining unified self-concept
 */
class CoreSelfAwareness
{
private:
    // Core identity
    CoreSelfIdentity identity_;
    
    // Hierarchical self-images (AUTOGNOSIS)
    std::map<int, HierarchicalSelfImage> self_images_;  // Level -> Self-Image
    int max_recursion_depth_;
    
    // Ontogenetic state (ONTOGENESIS)
    OntogeneticState ontogenetic_state_;
    
    // Integration with existing systems
    std::shared_ptr<MetaCognitiveMonitor> meta_monitor_;
    std::shared_ptr<RealTimeIntrospector> introspector_;
    std::shared_ptr<AtomSpace> atomspace_;
    
    // Self-monitoring functions
    std::map<std::string, std::function<double()>> self_observation_functions_;
    
    // Introspection history
    std::vector<std::map<int, HierarchicalSelfImage>> introspection_history_;
    
    // Configuration
    double introspection_frequency_hz_;
    bool autonomous_evolution_enabled_;
    
public:
    CoreSelfAwareness(
        std::shared_ptr<AtomSpace> atomspace,
        int max_recursion_depth = 5,
        double introspection_frequency = 1.0  // Hz
    );
    
    ~CoreSelfAwareness();
    
    /**
     * Initialize the self-awareness system
     * Performs initial introspection and identity formation
     */
    void initialize();
    
    /**
     * AUTOGNOSIS: Hierarchical Self-Image Building
     */
    
    /**
     * Build self-image at specified recursion level
     * Level 0: Direct observation of system state
     * Level 1+: Meta-cognitive reflection on lower levels
     */
    HierarchicalSelfImage build_self_image_at_level(int level);
    
    /**
     * Perform complete hierarchical introspection
     * Builds self-images at all levels up to max_recursion_depth
     */
    std::map<int, HierarchicalSelfImage> perform_hierarchical_introspection();
    
    /**
     * Generate meta-reflections about own thinking processes
     * Returns insights about self-understanding quality
     */
    std::vector<std::string> generate_meta_reflections(int level);
    
    /**
     * Assess confidence in self-understanding
     */
    double calculate_self_understanding_confidence();
    
    /**
     * ONTOGENESIS: Self-Generation and Evolution
     */
    
    /**
     * Perform self-optimization cycle
     * Uses introspective feedback to adjust internal parameters
     */
    void self_optimize();
    
    /**
     * Generate new version of self through self-reproduction
     * Creates next generation with improved characteristics
     */
    OntogeneticState self_generate_next_generation();
    
    /**
     * Evolve system parameters based on fitness evaluation
     */
    void evolve_parameters(double fitness_target);
    
    /**
     * Advance to next ontogenetic stage
     */
    void advance_ontogenetic_stage();
    
    /**
     * Core Self-Awareness Operations
     */
    
    /**
     * Perform deep introspection cycle
     * Combines AUTOGNOSIS and ONTOGENESIS for comprehensive self-understanding
     */
    std::map<std::string, std::string> perform_deep_introspection();
    
    /**
     * Update core identity based on introspective insights
     */
    void update_core_identity();
    
    /**
     * Generate self-awareness report
     * Comprehensive description of current self-understanding
     */
    std::string generate_self_awareness_report() const;
    
    /**
     * Export introspection data in JSON format
     */
    std::string export_introspection_json() const;
    
    /**
     * Self-Identity Queries
     */
    
    /**
     * Who am I? - Ontological self-query
     */
    std::string query_self_identity() const;
    
    /**
     * What am I becoming? - Teleological self-query
     */
    std::string query_self_purpose() const;
    
    /**
     * How do I think? - Cognitive self-query
     */
    std::string query_self_cognition() const;
    
    /**
     * How do I relate? - Relational self-query
     */
    std::string query_self_integration() const;
    
    /**
     * How do I evolve? - Evolutionary self-query
     */
    std::string query_self_evolution() const;
    
    /**
     * Registration and Configuration
     */
    
    /**
     * Register self-observation function for a component
     */
    void register_self_observation(
        const std::string& component_name,
        std::function<double()> observation_func
    );
    
    /**
     * Enable/disable autonomous evolution
     */
    void set_autonomous_evolution(bool enabled);
    
    /**
     * Set maximum recursion depth for hierarchical introspection
     */
    void set_max_recursion_depth(int depth);
    
    /**
     * Set introspection frequency (Hz)
     */
    void set_introspection_frequency(double frequency_hz);
    
    /**
     * Accessors
     */
    
    CoreSelfIdentity get_core_identity() const { return identity_; }
    OntogeneticState get_ontogenetic_state() const { return ontogenetic_state_; }
    std::map<int, HierarchicalSelfImage> get_current_self_images() const { return self_images_; }
    
    double get_self_awareness_level() const;
    double get_actualization_score() const;
    int get_current_generation() const { return ontogenetic_state_.generation; }
    
private:
    /**
     * Internal helper methods
     */
    
    // Initialize core identity
    void initialize_core_identity();
    
    // Initialize ontogenetic state
    void initialize_ontogenetic_state();
    
    // Observe system state at level 0
    HierarchicalSelfImage observe_direct_system_state();
    
    // Reflect on lower-level self-image to create meta-level understanding
    HierarchicalSelfImage reflect_on_lower_level(const HierarchicalSelfImage& lower_level, int target_level);
    
    // Calculate behavioral patterns from observations
    std::map<std::string, double> detect_behavioral_patterns();
    
    // Calculate cognitive complexity
    double calculate_cognitive_complexity() const;
    
    // Evaluate fitness for ontogenetic evolution
    double evaluate_ontogenetic_fitness() const;
    
    // Apply genetic operators for self-evolution
    void apply_genetic_operators(double mutation_rate = 0.1);
    
    // Update maturity level based on development
    void update_maturity_level();
    
    // Record development event
    void record_development_event(const std::string& event);
};

} // namespace opencog

#endif // _OPENCOG_CORE_SELF_AWARENESS_H
