/*
 * ConvergenceValidator.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Implementation of self-improvement convergence validation
 */

#include "../include/MetaCognitiveMonitor.h"
#include "../../evolutionary-optimization/include/EvolutionaryOptimizer.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <iostream>

namespace opencog {

/**
 * Convergence Validator for Self-Improvement Systems
 * 
 * Validates convergence of meta-cognitive and evolutionary self-improvement
 * processes using multi-level analysis and statistical validation.
 */
class ConvergenceValidator {
private:
    // Convergence thresholds
    double performance_convergence_threshold_;
    double meta_cognitive_convergence_threshold_;
    double integration_convergence_threshold_;
    
    // Historical tracking
    std::vector<double> performance_history_;
    std::vector<double> meta_cognitive_history_;
    std::vector<double> integration_history_;
    
    // Convergence state
    bool performance_converged_;
    bool meta_cognitive_converged_;
    bool integration_converged_;
    
    // Analysis parameters
    int convergence_window_size_;
    int stability_window_size_;
    
public:
    ConvergenceValidator(double perf_threshold = 0.001, 
                        double meta_threshold = 0.005,
                        double integration_threshold = 0.01)
        : performance_convergence_threshold_(perf_threshold),
          meta_cognitive_convergence_threshold_(meta_threshold),
          integration_convergence_threshold_(integration_threshold),
          performance_converged_(false),
          meta_cognitive_converged_(false),
          integration_converged_(false),
          convergence_window_size_(10),
          stability_window_size_(5)
    {
        std::cout << "[ConvergenceValidator] Initialized with thresholds - "
                  << "Performance: " << perf_threshold 
                  << ", Meta-Cognitive: " << meta_threshold
                  << ", Integration: " << integration_threshold << std::endl;
    }
    
    /**
     * Validate overall system convergence
     */
    bool validate_system_convergence(const MetaCognitiveMonitor& monitor,
                                   const EvolutionaryOptimizer& optimizer) {
        
        std::cout << "\n[ConvergenceValidator] Starting convergence validation..." << std::endl;
        
        // Update historical tracking
        update_performance_history(monitor, optimizer);
        
        // Validate each convergence level
        performance_converged_ = validate_performance_convergence();
        meta_cognitive_converged_ = validate_meta_cognitive_convergence(monitor);
        integration_converged_ = validate_integration_convergence();
        
        // Overall convergence requires all levels to converge
        bool overall_converged = performance_converged_ && 
                               meta_cognitive_converged_ && 
                               integration_converged_;
        
        generate_convergence_report();
        
        return overall_converged;
    }
    
    /**
     * Validate performance convergence
     */
    bool validate_performance_convergence() {
        if (performance_history_.size() < convergence_window_size_) {
            std::cout << "[ConvergenceValidator] Insufficient performance history for convergence analysis" << std::endl;
            return false;
        }
        
        // Calculate recent performance variance
        double recent_variance = calculate_variance(performance_history_, convergence_window_size_);
        
        // Check for performance plateau
        bool plateau_detected = recent_variance < performance_convergence_threshold_;
        
        // Validate improvement stagnation
        double improvement_rate = calculate_improvement_rate(performance_history_);
        bool improvement_stagnated = std::abs(improvement_rate) < performance_convergence_threshold_;
        
        // Check stability over multiple windows
        bool stable = is_stable_over_windows(performance_history_, stability_window_size_);
        
        bool converged = plateau_detected && improvement_stagnated && stable;
        
        std::cout << "[ConvergenceValidator] Performance convergence analysis:" << std::endl;
        std::cout << "  Variance: " << recent_variance << " (threshold: " << performance_convergence_threshold_ << ")" << std::endl;
        std::cout << "  Improvement rate: " << improvement_rate << std::endl;
        std::cout << "  Plateau detected: " << (plateau_detected ? "Yes" : "No") << std::endl;
        std::cout << "  Stable: " << (stable ? "Yes" : "No") << std::endl;
        std::cout << "  Performance converged: " << (converged ? "Yes" : "No") << std::endl;
        
        return converged;
    }
    
    /**
     * Validate meta-cognitive convergence
     */
    bool validate_meta_cognitive_convergence(const MetaCognitiveMonitor& monitor) {
        if (meta_cognitive_history_.size() < convergence_window_size_) {
            std::cout << "[ConvergenceValidator] Insufficient meta-cognitive history for convergence analysis" << std::endl;
            return false;
        }
        
        // Analyze self-awareness stabilization
        auto current_state = monitor.get_current_state();
        double self_awareness_variance = calculate_variance(meta_cognitive_history_, convergence_window_size_);
        
        // Check reflection depth optimization
        bool optimal_depth = current_state.reflection_depth >= 3; // Minimum depth for meta-meta cognition
        
        // Validate meta-insight quality consistency
        double insight_consistency = calculate_insight_consistency(monitor);
        
        // Check meta-cognitive effectiveness
        double meta_effectiveness = calculate_meta_cognitive_effectiveness(monitor);
        
        bool converged = (self_awareness_variance < meta_cognitive_convergence_threshold_) &&
                        optimal_depth &&
                        (insight_consistency > 0.7) &&
                        (meta_effectiveness > 0.8);
        
        std::cout << "[ConvergenceValidator] Meta-cognitive convergence analysis:" << std::endl;
        std::cout << "  Self-awareness variance: " << self_awareness_variance << std::endl;
        std::cout << "  Reflection depth: " << current_state.reflection_depth << " (optimal: " << optimal_depth << ")" << std::endl;
        std::cout << "  Insight consistency: " << insight_consistency << std::endl;
        std::cout << "  Meta-effectiveness: " << meta_effectiveness << std::endl;
        std::cout << "  Meta-cognitive converged: " << (converged ? "Yes" : "No") << std::endl;
        
        return converged;
    }
    
    /**
     * Validate system integration convergence
     */
    bool validate_integration_convergence() {
        if (integration_history_.size() < convergence_window_size_) {
            std::cout << "[ConvergenceValidator] Insufficient integration history for convergence analysis" << std::endl;
            return false;
        }
        
        // Analyze component synchronization
        double synchronization_variance = calculate_variance(integration_history_, convergence_window_size_);
        
        // Check feedback loop stabilization
        bool feedback_stable = is_feedback_loop_stable();
        
        // Validate emergent property convergence
        bool emergent_properties_stable = validate_emergent_properties_convergence();
        
        bool converged = (synchronization_variance < integration_convergence_threshold_) &&
                        feedback_stable &&
                        emergent_properties_stable;
        
        std::cout << "[ConvergenceValidator] Integration convergence analysis:" << std::endl;
        std::cout << "  Synchronization variance: " << synchronization_variance << std::endl;
        std::cout << "  Feedback loops stable: " << (feedback_stable ? "Yes" : "No") << std::endl;
        std::cout << "  Emergent properties stable: " << (emergent_properties_stable ? "Yes" : "No") << std::endl;
        std::cout << "  Integration converged: " << (converged ? "Yes" : "No") << std::endl;
        
        return converged;
    }
    
    /**
     * Generate comprehensive convergence report
     */
    void generate_convergence_report() {
        std::cout << "\n" << "="*60 << std::endl;
        std::cout << "CONVERGENCE VALIDATION REPORT" << std::endl;
        std::cout << "="*60 << std::endl;
        
        std::cout << "Performance Level:" << std::endl;
        std::cout << "  Status: " << (performance_converged_ ? "CONVERGED" : "NOT CONVERGED") << std::endl;
        if (!performance_history_.empty()) {
            std::cout << "  Current Performance: " << performance_history_.back() << std::endl;
            std::cout << "  Performance Variance: " << calculate_variance(performance_history_, convergence_window_size_) << std::endl;
        }
        
        std::cout << "\nMeta-Cognitive Level:" << std::endl;
        std::cout << "  Status: " << (meta_cognitive_converged_ ? "CONVERGED" : "NOT CONVERGED") << std::endl;
        if (!meta_cognitive_history_.empty()) {
            std::cout << "  Current Meta-Cognitive Score: " << meta_cognitive_history_.back() << std::endl;
        }
        
        std::cout << "\nIntegration Level:" << std::endl;
        std::cout << "  Status: " << (integration_converged_ ? "CONVERGED" : "NOT CONVERGED") << std::endl;
        if (!integration_history_.empty()) {
            std::cout << "  Current Integration Score: " << integration_history_.back() << std::endl;
        }
        
        std::cout << "\nOverall Convergence Status: ";
        if (performance_converged_ && meta_cognitive_converged_ && integration_converged_) {
            std::cout << "FULLY CONVERGED ✓" << std::endl;
        } else if (performance_converged_ || meta_cognitive_converged_ || integration_converged_) {
            std::cout << "PARTIALLY CONVERGED ◐" << std::endl;
        } else {
            std::cout << "NOT CONVERGED ✗" << std::endl;
        }
        
        // Convergence timeline prediction
        if (!is_fully_converged()) {
            auto prediction = predict_convergence_timeline();
            std::cout << "\nConvergence Prediction:" << std::endl;
            std::cout << "  Estimated time to full convergence: " << prediction.first << " cycles" << std::endl;
            std::cout << "  Confidence level: " << (prediction.second * 100) << "%" << std::endl;
        }
        
        std::cout << "="*60 << std::endl;
    }
    
    /**
     * Check if system has fully converged
     */
    bool is_fully_converged() const {
        return performance_converged_ && meta_cognitive_converged_ && integration_converged_;
    }
    
    /**
     * Get convergence status summary
     */
    std::map<std::string, bool> get_convergence_status() const {
        return {
            {"performance", performance_converged_},
            {"meta_cognitive", meta_cognitive_converged_},
            {"integration", integration_converged_},
            {"overall", is_fully_converged()}
        };
    }
    
private:
    void update_performance_history(const MetaCognitiveMonitor& monitor,
                                  const EvolutionaryOptimizer& optimizer) {
        // Performance metric from meta-cognitive tensor
        auto state = monitor.get_current_state();
        double performance_score = state.calculate_overall_fitness();
        performance_history_.push_back(performance_score);
        
        // Meta-cognitive score
        double meta_score = state.self_awareness_level * 0.4 + 
                           state.adaptation_rate * 0.3 +
                           (static_cast<double>(state.reflection_depth) / 5.0) * 0.3;
        meta_cognitive_history_.push_back(meta_score);
        
        // Integration score (combination of component effectiveness)
        auto evolution_stats = optimizer.get_evolution_statistics();
        double integration_score = (performance_score * 0.4) + 
                                  (meta_score * 0.3) +
                                  (evolution_stats.count("best_fitness") ? evolution_stats.at("best_fitness") * 0.3 : 0.0);
        integration_history_.push_back(integration_score);
        
        // Maintain history size limits
        const size_t max_history = 1000;
        if (performance_history_.size() > max_history) {
            performance_history_.erase(performance_history_.begin());
            meta_cognitive_history_.erase(meta_cognitive_history_.begin());
            integration_history_.erase(integration_history_.begin());
        }
    }
    
    double calculate_variance(const std::vector<double>& data, int window_size) {
        if (data.size() < static_cast<size_t>(window_size)) return 1.0; // High variance if insufficient data
        
        auto start = data.end() - window_size;
        auto end = data.end();
        
        double mean = std::accumulate(start, end, 0.0) / window_size;
        
        double variance = 0.0;
        for (auto it = start; it != end; ++it) {
            variance += std::pow(*it - mean, 2);
        }
        
        return variance / window_size;
    }
    
    double calculate_improvement_rate(const std::vector<double>& data) {
        if (data.size() < 2) return 0.0;
        
        // Calculate improvement rate over recent window
        int window = std::min(convergence_window_size_, static_cast<int>(data.size()));
        double recent_avg = 0.0;
        double older_avg = 0.0;
        
        // Recent window average
        for (int i = data.size() - window; i < static_cast<int>(data.size()); ++i) {
            recent_avg += data[i];
        }
        recent_avg /= window;
        
        // Older window average (if available)
        if (data.size() >= static_cast<size_t>(window * 2)) {
            for (int i = data.size() - (window * 2); i < static_cast<int>(data.size()) - window; ++i) {
                older_avg += data[i];
            }
            older_avg /= window;
        } else {
            older_avg = data.front();
        }
        
        return recent_avg - older_avg;
    }
    
    bool is_stable_over_windows(const std::vector<double>& data, int window_size) {
        if (data.size() < static_cast<size_t>(window_size * 2)) return false;
        
        // Compare variances across multiple windows
        std::vector<double> window_variances;
        
        for (int offset = 0; offset < stability_window_size_; ++offset) {
            if (data.size() >= static_cast<size_t>((offset + 1) * window_size)) {
                auto start = data.end() - ((offset + 1) * window_size);
                auto end = data.end() - (offset * window_size);
                
                double mean = std::accumulate(start, end, 0.0) / window_size;
                double variance = 0.0;
                
                for (auto it = start; it != end; ++it) {
                    variance += std::pow(*it - mean, 2);
                }
                variance /= window_size;
                
                window_variances.push_back(variance);
            }
        }
        
        if (window_variances.empty()) return false;
        
        // Check if all window variances are below threshold
        return std::all_of(window_variances.begin(), window_variances.end(),
                          [this](double var) { return var < performance_convergence_threshold_ * 2; });
    }
    
    double calculate_insight_consistency(const MetaCognitiveMonitor& monitor) {
        // Simulate insight quality measurement (in real implementation, would track actual insights)
        auto state = monitor.get_current_state();
        
        // Higher self-awareness and deeper reflection suggest more consistent insights
        double consistency = state.self_awareness_level * 0.6 + 
                           (static_cast<double>(state.reflection_depth) / 5.0) * 0.4;
        
        return std::min(1.0, consistency);
    }
    
    double calculate_meta_cognitive_effectiveness(const MetaCognitiveMonitor& monitor) {
        // Measure how effective the meta-cognitive processes are
        auto state = monitor.get_current_state();
        
        // Effectiveness based on adaptation rate and self-awareness
        double effectiveness = state.adaptation_rate * 0.5 + state.self_awareness_level * 0.5;
        
        return effectiveness;
    }
    
    bool is_feedback_loop_stable() {
        // Analyze feedback loop stability based on integration metrics
        if (integration_history_.size() < 5) return false;
        
        // Check for oscillations or instabilities
        double variance = calculate_variance(integration_history_, 5);
        return variance < integration_convergence_threshold_;
    }
    
    bool validate_emergent_properties_convergence() {
        // Check if emergent properties have stabilized
        // In a full implementation, this would analyze specific emergent behaviors
        
        // Simple heuristic: stable if integration scores are consistently high
        if (integration_history_.size() < convergence_window_size_) return false;
        
        auto recent_start = integration_history_.end() - convergence_window_size_;
        auto recent_end = integration_history_.end();
        
        // Check if all recent integration scores are above a threshold
        return std::all_of(recent_start, recent_end,
                          [](double score) { return score > 0.7; });
    }
    
    std::pair<int, double> predict_convergence_timeline() {
        // Predict when full convergence will be achieved
        
        std::vector<double> convergence_rates;
        
        // Analyze convergence rates for each level
        if (!performance_converged_) {
            double perf_rate = std::abs(calculate_improvement_rate(performance_history_));
            convergence_rates.push_back(perf_rate > 0 ? performance_convergence_threshold_ / perf_rate : 1000);
        }
        
        if (!meta_cognitive_converged_) {
            double meta_rate = std::abs(calculate_improvement_rate(meta_cognitive_history_));
            convergence_rates.push_back(meta_rate > 0 ? meta_cognitive_convergence_threshold_ / meta_rate : 1000);
        }
        
        if (!integration_converged_) {
            double integration_rate = std::abs(calculate_improvement_rate(integration_history_));
            convergence_rates.push_back(integration_rate > 0 ? integration_convergence_threshold_ / integration_rate : 1000);
        }
        
        if (convergence_rates.empty()) {
            return {0, 1.0}; // Already converged
        }
        
        // Estimate based on slowest converging component
        int estimated_cycles = static_cast<int>(*std::max_element(convergence_rates.begin(), convergence_rates.end()));
        
        // Confidence based on data availability and consistency
        double confidence = std::min(0.9, static_cast<double>(performance_history_.size()) / 50.0);
        
        return {estimated_cycles, confidence};
    }
};

} // namespace opencog