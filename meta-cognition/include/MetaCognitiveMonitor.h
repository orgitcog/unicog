/*
 * MetaCognitiveMonitor.h
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Meta-cognitive monitoring system for self-observation and recursive improvement
 */

#ifndef _OPENCOG_METACOGNITIVE_MONITOR_H
#define _OPENCOG_METACOGNITIVE_MONITOR_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <functional>

namespace opencog {

// Forward declarations
class Handle;
class AtomSpace;

/**
 * Meta-Cognitive Tensor Signature Structure
 * Represents the 9-dimensional meta-cognitive state space
 */
struct MetaCognitiveTensor {
    double self_awareness_level;      // [0.0, 1.0]
    std::vector<double> performance_metric;  // [accuracy, efficiency, adaptability]  
    int evolutionary_generation;      // [0, max_generations]
    double fitness_score;            // [0.0, 1.0]
    double adaptation_rate;          // [0.0, 1.0]
    std::string cognitive_complexity; // [simple, moderate, complex]
    std::string meta_level;          // [object, meta, meta-meta]
    int reflection_depth;            // [1, max_depth]
    std::string optimization_target;  // [speed, accuracy, generalization]
    
    // Utility methods
    std::vector<double> to_vector() const;
    static MetaCognitiveTensor from_vector(const std::vector<double>& vec);
    double calculate_overall_fitness() const;
};

/**
 * Cognitive Performance Metrics
 * Tracks system performance across multiple dimensions
 */
struct CognitivePerformanceMetrics {
    double accuracy_score;
    double processing_efficiency;
    double adaptability_index;
    double learning_rate;
    double resource_utilization;
    std::chrono::steady_clock::time_point timestamp;
    std::map<std::string, double> component_metrics;
};

/**
 * Meta-Cognitive Monitor
 * 
 * Implements recursive meta-cognitive monitoring system that observes,
 * analyzes, and recursively improves the cognitive system's behavior.
 */
class MetaCognitiveMonitor
{
private:
    // Current meta-cognitive state
    MetaCognitiveTensor current_state_;
    
    // Performance history tracking
    std::vector<CognitivePerformanceMetrics> performance_history_;
    
    // Self-observation mechanisms
    std::map<std::string, std::function<double()>> observation_functions_;
    
    // Reflection depth limits and control
    int max_reflection_depth_;
    int current_reflection_depth_;
    
    // Evolutionary generation tracking
    int current_generation_;
    
    // Adaptive parameters
    double adaptation_threshold_;
    double improvement_target_threshold_;
    
public:
    MetaCognitiveMonitor(int max_depth = 5, double adaptation_threshold = 0.1);
    ~MetaCognitiveMonitor();

    /**
     * Core Meta-Cognitive Operations
     */
    
    /**
     * Observe current cognitive state and update meta-cognitive tensor
     * Implements recursive self-observation at multiple levels
     */
    MetaCognitiveTensor observe_cognitive_state();
    
    /**
     * Analyze cognitive performance trends and patterns
     * Returns insights about system behavior evolution
     */
    std::vector<std::string> analyze_performance_trends();
    
    /**
     * Reflect on own meta-cognitive processes (recursive reflection)
     * Implements meta-meta level awareness
     */
    void recursive_self_reflection();
    
    /**
     * Generate improvement suggestions based on meta-analysis
     */
    std::vector<std::string> generate_improvement_suggestions();
    
    /**
     * Performance Monitoring
     */
    
    /**
     * Update performance metrics from various cognitive components
     */
    void update_performance_metrics(const std::map<std::string, double>& metrics);
    
    /**
     * Calculate cognitive performance trends over time
     */
    std::vector<double> calculate_performance_trends(int window_size = 10);
    
    /**
     * Detect performance anomalies and adaptation needs
     */
    std::vector<std::string> detect_performance_anomalies();
    
    /**
     * Self-Monitoring and Adaptation
     */
    
    /**
     * Register observation function for specific cognitive component
     */
    void register_observation_function(const std::string& component_name,
                                     std::function<double()> observation_func);
    
    /**
     * Update self-awareness level based on meta-cognitive analysis
     */
    void update_self_awareness_level();
    
    /**
     * Adapt monitoring parameters based on observed effectiveness
     */
    void adaptive_parameter_tuning();
    
    /**
     * Recursive Meta-Cognition
     */
    
    /**
     * Implement recursive cognitive monitoring loops
     * Monitors the monitoring process itself
     */
    void recursive_monitoring_cycle();
    
    /**
     * Meta-cognitive introspection - thinking about thinking
     */
    std::map<std::string, double> cognitive_introspection();
    
    /**
     * Generate meta-insights about meta-cognitive processes
     */
    std::vector<std::string> generate_meta_insights();
    
    /**
     * Accessors and Status
     */
    
    MetaCognitiveTensor get_current_state() const { return current_state_; }
    std::vector<CognitivePerformanceMetrics> get_performance_history() const { return performance_history_; }
    int get_current_generation() const { return current_generation_; }
    int get_reflection_depth() const { return current_reflection_depth_; }
    
    /**
     * Set optimization target for evolutionary improvement
     */
    void set_optimization_target(const std::string& target);
    
    /**
     * Advance to next evolutionary generation
     */
    void advance_generation();

private:
    /**
     * Helper methods for internal processing
     */
    
    double calculate_adaptability_index();
    double calculate_learning_effectiveness();
    double calculate_cognitive_complexity_score();
    void update_evolutionary_metrics();
    std::string determine_cognitive_complexity_level();
    std::string determine_current_meta_level();
};

} // namespace opencog

#endif // _OPENCOG_METACOGNITIVE_MONITOR_H