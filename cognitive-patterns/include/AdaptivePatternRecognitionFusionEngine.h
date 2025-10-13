/*
 * AdaptivePatternRecognitionFusionEngine.h
 *
 * Adaptive Pattern-Recognition Fusion Engine (APRFE) Header
 * Dynamic fusion of symbolic reasoning, neural processing, and attention allocation
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ADAPTIVE_PATTERN_RECOGNITION_FUSION_ENGINE_H
#define _OPENCOG_ADAPTIVE_PATTERN_RECOGNITION_FUSION_ENGINE_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <functional>
#include <atomic>
#include <chrono>
#include <mutex>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

// Forward declarations for integration components
namespace opencog {
    class NeuralSymbolicBridge;
    class AttentionAllocator;
    class URE;
    struct ggml_tensor;
    struct ggml_context;
}

namespace opencog {

/**
 * Recognition Strategy Types
 */
enum class RecognitionStrategy {
    SYMBOLIC_ONLY,      // Pure URE-based symbolic reasoning
    NEURAL_ONLY,        // Pure neural pattern matching
    ATTENTION_GUIDED,   // ECAN-guided pattern recognition
    HYBRID_FUSION,      // Dynamic combination of methods
    ADAPTIVE_SELECT     // Adaptive selection based on context
};

/**
 * Pattern Recognition Result Structure
 */
struct PatternRecognitionResult {
    HandleSeq matched_patterns;
    std::vector<double> confidence_scores;
    RecognitionStrategy strategy_used;
    double processing_time_ms;
    double accuracy_estimate;
    double cognitive_load_estimate;
    std::map<std::string, double> performance_metrics;
    
    PatternRecognitionResult() 
        : strategy_used(RecognitionStrategy::ADAPTIVE_SELECT),
          processing_time_ms(0.0), accuracy_estimate(0.0),
          cognitive_load_estimate(0.0) {}
};

/**
 * Cross-Modal Integration Result
 */
struct CrossModalResult {
    double symbolic_contributions;
    double neural_contributions;
    double attention_contributions;
    HandleSeq integrated_patterns;
    std::vector<std::string> emergent_insights;
    
    CrossModalResult() 
        : symbolic_contributions(0.0), neural_contributions(0.0),
          attention_contributions(0.0) {}
};

/**
 * Strategy Performance Metrics
 */
struct StrategyPerformance {
    RecognitionStrategy strategy;
    double average_accuracy;
    double average_processing_time_ms;
    size_t usage_count;
    double success_rate;
    std::chrono::system_clock::time_point last_used;
    
    StrategyPerformance(RecognitionStrategy s = RecognitionStrategy::ADAPTIVE_SELECT)
        : strategy(s), average_accuracy(0.5), average_processing_time_ms(100.0),
          usage_count(0), success_rate(0.5), last_used(std::chrono::system_clock::now()) {}
};

/**
 * Adaptive Configuration Parameters
 */
struct AdaptiveConfig {
    double adaptation_rate;              // Rate of strategy adaptation (0.0-1.0)
    double cognitive_load_threshold;     // Threshold for switching to faster strategies
    double accuracy_threshold;           // Minimum acceptable accuracy
    size_t performance_history_size;     // Size of performance history to maintain
    double strategy_switch_penalty;      // Cost of switching strategies
    bool enable_emergent_learning;       // Enable emergent behavior detection
    
    AdaptiveConfig()
        : adaptation_rate(0.1), cognitive_load_threshold(0.8),
          accuracy_threshold(0.7), performance_history_size(100),
          strategy_switch_penalty(0.05), enable_emergent_learning(true) {}
};

/**
 * Adaptive Pattern-Recognition Fusion Engine (APRFE)
 * 
 * Core class implementing adaptive fusion of multiple pattern recognition modalities:
 * - Symbolic reasoning via URE
 * - Neural pattern matching via GGML tensors
 * - Attention-guided processing via ECAN
 * 
 * Key Features:
 * - Dynamic strategy selection based on context and performance
 * - Real-time adaptation based on feedback
 * - Cross-modal learning and knowledge transfer
 * - Emergent behavior detection and integration
 */
class AdaptivePatternRecognitionFusionEngine {
private:
    // Core OpenCog components
    AtomSpace* atomspace_;
    std::shared_ptr<NeuralSymbolicBridge> neural_bridge_;
    std::shared_ptr<AttentionAllocator> attention_allocator_;
    std::shared_ptr<URE> reasoning_engine_;
    ggml_context* tensor_context_;
    
    // Adaptive configuration
    AdaptiveConfig config_;
    
    // Strategy performance tracking
    std::map<RecognitionStrategy, StrategyPerformance> strategy_performance_;
    std::vector<PatternRecognitionResult> performance_history_;
    
    // Real-time adaptation state
    std::atomic<double> current_cognitive_load_;
    std::atomic<RecognitionStrategy> preferred_strategy_;
    std::mutex adaptation_mutex_;
    
    // Cross-modal learning state
    std::map<std::string, std::vector<double>> cross_modal_correlations_;
    std::vector<std::string> emergent_patterns_;
    
    // Performance monitoring
    std::chrono::steady_clock::time_point last_adaptation_time_;
    std::atomic<size_t> total_recognitions_;
    std::atomic<double> cumulative_accuracy_;

public:
    /**
     * Constructor
     * @param atomspace Pointer to AtomSpace for knowledge representation
     * @param neural_bridge Shared pointer to neural-symbolic bridge
     * @param attention_allocator Shared pointer to attention allocation system
     * @param reasoning_engine Shared pointer to URE reasoning engine
     * @param tensor_context GGML tensor context for neural operations
     */
    AdaptivePatternRecognitionFusionEngine(
        AtomSpace* atomspace,
        std::shared_ptr<NeuralSymbolicBridge> neural_bridge,
        std::shared_ptr<AttentionAllocator> attention_allocator,
        std::shared_ptr<URE> reasoning_engine,
        ggml_context* tensor_context = nullptr);
    
    ~AdaptivePatternRecognitionFusionEngine();
    
    // Core pattern recognition interface
    
    /**
     * Recognize patterns using adaptive strategy selection
     * @param input_atoms Set of atoms to search for patterns
     * @param target_pattern Specific pattern to search for (optional)
     * @param time_limit_seconds Maximum time to spend on recognition
     * @return Pattern recognition results with performance metrics
     */
    PatternRecognitionResult recognize_patterns(
        const HandleSeq& input_atoms,
        const Handle& target_pattern = Handle::UNDEFINED,
        double time_limit_seconds = 10.0);
    
    /**
     * Cross-modal pattern recognition integrating all modalities
     * @param task_description Natural language description of the recognition task
     * @return Cross-modal integration results
     */
    CrossModalResult cross_modal_recognition(const std::string& task_description);
    
    // Strategy-specific recognition methods
    
    /**
     * Symbolic pattern recognition using URE
     */
    PatternRecognitionResult symbolic_recognition(
        const HandleSeq& input_atoms,
        const Handle& target_pattern,
        double time_limit_seconds = 10.0);
    
    /**
     * Neural pattern recognition using GGML tensors
     */
    PatternRecognitionResult neural_recognition(
        const HandleSeq& input_atoms,
        const Handle& target_pattern,
        double time_limit_seconds = 10.0);
    
    /**
     * Attention-guided pattern recognition using ECAN
     */
    PatternRecognitionResult attention_guided_recognition(
        const HandleSeq& input_atoms,
        const Handle& target_pattern,
        double time_limit_seconds = 10.0);
    
    /**
     * Hybrid fusion recognition combining multiple strategies
     */
    PatternRecognitionResult hybrid_fusion_recognition(
        const HandleSeq& input_atoms,
        const Handle& target_pattern,
        double time_limit_seconds = 10.0);
    
    // Adaptive control methods
    
    /**
     * Adapt recognition strategies based on performance feedback
     * @param performance_metrics Recent performance data for adaptation
     */
    void adapt_strategies(const std::map<std::string, double>& performance_metrics);
    
    /**
     * Update cognitive load estimate
     * @param load_estimate Current cognitive load (0.0-1.0)
     */
    void update_cognitive_load(double load_estimate);
    
    /**
     * Select optimal strategy for current context
     * @param context_atoms Atoms providing context for strategy selection
     * @param urgency_factor Factor indicating how quickly result is needed (0.0-1.0)
     * @return Recommended recognition strategy
     */
    RecognitionStrategy select_optimal_strategy(
        const HandleSeq& context_atoms,
        double urgency_factor = 0.5) const;
    
    // Cross-modal learning methods
    
    /**
     * Learn correlations between different recognition modalities
     * @param symbolic_results Results from symbolic recognition
     * @param neural_results Results from neural recognition
     * @param attention_results Results from attention-guided recognition
     */
    void learn_cross_modal_correlations(
        const PatternRecognitionResult& symbolic_results,
        const PatternRecognitionResult& neural_results,
        const PatternRecognitionResult& attention_results);
    
    /**
     * Detect and integrate emergent patterns across modalities
     * @return Newly detected emergent patterns
     */
    std::vector<Handle> detect_emergent_patterns();
    
    /**
     * Transfer knowledge between recognition strategies
     * @param source_strategy Strategy to transfer knowledge from
     * @param target_strategy Strategy to transfer knowledge to
     * @param transfer_strength Strength of knowledge transfer (0.0-1.0)
     */
    void transfer_knowledge_between_strategies(
        RecognitionStrategy source_strategy,
        RecognitionStrategy target_strategy,
        double transfer_strength = 0.1);
    
    // Performance monitoring and statistics
    
    /**
     * Get current performance statistics for all strategies
     */
    std::map<RecognitionStrategy, StrategyPerformance> get_strategy_performance() const;
    
    /**
     * Get overall system performance metrics
     */
    std::map<std::string, double> get_system_performance_metrics() const;
    
    /**
     * Get current cognitive load estimate
     */
    double get_current_cognitive_load() const { return current_cognitive_load_.load(); }
    
    /**
     * Get preferred strategy based on current context
     */
    RecognitionStrategy get_preferred_strategy() const { return preferred_strategy_.load(); }
    
    // Configuration methods
    
    /**
     * Update adaptive configuration parameters
     */
    void update_config(const AdaptiveConfig& new_config);
    
    /**
     * Get current configuration
     */
    const AdaptiveConfig& get_config() const { return config_; }
    
    // Debugging and introspection methods
    
    /**
     * Print current system state for debugging
     */
    void print_system_state() const;
    
    /**
     * Export performance history for analysis
     * @param filename File to export performance data to
     */
    void export_performance_history(const std::string& filename) const;
    
    /**
     * Validate system consistency and integrity
     * @return True if system is in consistent state
     */
    bool validate_system_integrity() const;

private:
    // Internal adaptation methods
    
    /**
     * Update strategy performance based on recognition result
     */
    void update_strategy_performance(const PatternRecognitionResult& result);
    
    /**
     * Calculate strategy selection scores for current context
     */
    std::map<RecognitionStrategy, double> calculate_strategy_scores(
        const HandleSeq& context_atoms, double urgency_factor) const;
    
    /**
     * Perform periodic adaptation based on accumulated performance data
     */
    void perform_periodic_adaptation();
    
    /**
     * Detect performance anomalies and trigger emergency adaptation
     */
    bool detect_performance_anomalies() const;
    
    // Cross-modal integration helpers
    
    /**
     * Fuse results from multiple recognition strategies
     */
    PatternRecognitionResult fuse_recognition_results(
        const std::vector<PatternRecognitionResult>& results) const;
    
    /**
     * Calculate cross-modal similarity between recognition results
     */
    double calculate_cross_modal_similarity(
        const PatternRecognitionResult& result1,
        const PatternRecognitionResult& result2) const;
    
    /**
     * Update cross-modal correlation matrix
     */
    void update_cross_modal_correlations(
        const std::string& modality1, const std::string& modality2, double correlation);
    
    // Performance optimization helpers
    
    /**
     * Optimize memory usage by pruning old performance data
     */
    void optimize_memory_usage();
    
    /**
     * Pre-compute frequently used recognition patterns
     */
    void precompute_frequent_patterns();
    
    /**
     * Initialize default strategy performance parameters
     */
    void initialize_strategy_performance();
};

// Utility functions

/**
 * Convert recognition strategy to human-readable string
 */
std::string strategy_to_string(RecognitionStrategy strategy);

/**
 * Parse recognition strategy from string
 */
RecognitionStrategy string_to_strategy(const std::string& strategy_str);

/**
 * Calculate weighted average of recognition results
 */
PatternRecognitionResult calculate_weighted_average_result(
    const std::vector<std::pair<PatternRecognitionResult, double>>& weighted_results);

} // namespace opencog

#endif // _OPENCOG_ADAPTIVE_PATTERN_RECOGNITION_FUSION_ENGINE_H