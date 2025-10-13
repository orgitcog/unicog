/*
 * SelfImprovementDashboard.h
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Self-improvement metrics dashboard for monitoring cognitive evolution
 */

#ifndef _OPENCOG_SELF_IMPROVEMENT_DASHBOARD_H
#define _OPENCOG_SELF_IMPROVEMENT_DASHBOARD_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <fstream>

namespace opencog {

// Forward declarations
struct MetaCognitiveTensor;
struct CognitiveGenome;
class MetaCognitiveMonitor;
class EvolutionaryOptimizer;

/**
 * Dashboard Metrics Structure
 * Comprehensive tracking of system improvement metrics
 */
struct DashboardMetrics {
    std::chrono::steady_clock::time_point timestamp;
    
    // Performance metrics
    double overall_performance_score;
    double cognitive_efficiency_index;
    double adaptation_effectiveness;
    double learning_velocity;
    
    // Meta-cognitive metrics
    double self_awareness_growth_rate;
    double reflection_depth_utilization;
    double meta_insight_generation_rate;
    double recursive_improvement_factor;
    
    // Evolutionary metrics
    double fitness_improvement_rate;
    double population_diversity_index;
    double convergence_progress;
    double genetic_novelty_score;
    
    // Integration metrics
    double component_synergy_score;
    double feedback_loop_effectiveness;
    double emergent_behavior_index;
    double system_coherence_measure;
    
    // Comparative metrics
    double improvement_vs_baseline;
    double performance_consistency;
    double adaptive_robustness;
    
    std::map<std::string, double> custom_metrics;
};

/**
 * Performance Trend Analysis
 * Statistical analysis of improvement trajectories
 */
struct TrendAnalysis {
    std::vector<double> performance_trajectory;
    std::vector<double> improvement_rates;
    std::vector<double> volatility_measures;
    
    double linear_trend_slope;
    double exponential_growth_factor;
    double improvement_acceleration;
    double performance_stability;
    
    // Prediction metrics
    double predicted_peak_performance;
    double estimated_convergence_time;
    double confidence_interval_width;
    
    // Anomaly detection
    std::vector<std::pair<int, std::string>> detected_anomalies;
    double anomaly_recovery_time;
};

/**
 * Self-Improvement Dashboard
 * 
 * Real-time monitoring and visualization of cognitive system self-improvement
 * with comprehensive metrics, trend analysis, and predictive insights.
 */
class SelfImprovementDashboard
{
private:
    // Component references
    std::shared_ptr<MetaCognitiveMonitor> monitor_;
    std::shared_ptr<EvolutionaryOptimizer> optimizer_;
    
    // Metrics history
    std::vector<DashboardMetrics> metrics_history_;
    TrendAnalysis current_trends_;
    
    // Dashboard configuration
    int max_history_size_;
    double update_frequency_hz_;
    bool auto_save_enabled_;
    std::string output_directory_;
    
    // Baseline measurements
    DashboardMetrics baseline_metrics_;
    bool baseline_established_;
    
    // Real-time tracking
    std::chrono::steady_clock::time_point last_update_;
    bool dashboard_active_;
    
public:
    SelfImprovementDashboard(std::shared_ptr<MetaCognitiveMonitor> monitor,
                           std::shared_ptr<EvolutionaryOptimizer> optimizer);
    ~SelfImprovementDashboard();

    /**
     * Core Dashboard Operations
     */
    
    /**
     * Start real-time dashboard monitoring
     */
    void start_monitoring();
    
    /**
     * Stop dashboard monitoring
     */
    void stop_monitoring();
    
    /**
     * Update dashboard with current system state
     */
    void update_metrics();
    
    /**
     * Establish performance baseline for comparison
     */
    void establish_baseline();
    
    /**
     * Metrics Collection and Analysis
     */
    
    /**
     * Collect comprehensive system metrics
     */
    DashboardMetrics collect_current_metrics();
    
    /**
     * Calculate performance improvement rates
     */
    std::map<std::string, double> calculate_improvement_rates();
    
    /**
     * Analyze performance trends and trajectories
     */
    TrendAnalysis analyze_performance_trends();
    
    /**
     * Detect performance anomalies and patterns
     */
    std::vector<std::string> detect_performance_patterns();
    
    /**
     * Visualization and Reporting
     */
    
    /**
     * Generate real-time dashboard display
     */
    void display_dashboard();
    
    /**
     * Generate comprehensive performance report
     */
    void generate_performance_report(const std::string& filename = "");
    
    /**
     * Export metrics to JSON format
     */
    void export_metrics_json(const std::string& filename);
    
    /**
     * Export metrics to CSV format
     */
    void export_metrics_csv(const std::string& filename);
    
    /**
     * Predictive Analysis
     */
    
    /**
     * Predict future performance trajectories
     */
    std::vector<double> predict_performance_trajectory(int forecast_steps);
    
    /**
     * Estimate time to reach target performance
     */
    double estimate_convergence_time(double target_performance);
    
    /**
     * Identify optimization opportunities
     */
    std::vector<std::string> identify_optimization_opportunities();
    
    /**
     * Recommend improvement strategies
     */
    std::vector<std::string> recommend_improvement_strategies();
    
    /**
     * Self-Improvement Feedback
     */
    
    /**
     * Generate self-improvement insights
     */
    std::vector<std::string> generate_self_improvement_insights();
    
    /**
     * Apply automated improvement recommendations
     */
    void apply_automated_improvements();
    
    /**
     * Track improvement intervention effectiveness
     */
    std::map<std::string, double> track_intervention_effectiveness();
    
    /**
     * Configuration and Control
     */
    
    /**
     * Configure dashboard parameters
     */
    void configure_dashboard(int history_size, double update_freq, bool auto_save);
    
    /**
     * Set custom metric tracking
     */
    void add_custom_metric(const std::string& name, std::function<double()> metric_function);
    
    /**
     * Enable/disable specific metric categories
     */
    void configure_metric_categories(const std::map<std::string, bool>& categories);
    
    /**
     * Data Access and Queries
     */
    
    /**
     * Get current dashboard state
     */
    DashboardMetrics get_current_metrics() const;
    
    /**
     * Get historical metrics within time range
     */
    std::vector<DashboardMetrics> get_metrics_history(
        std::chrono::steady_clock::time_point start_time,
        std::chrono::steady_clock::time_point end_time) const;
    
    /**
     * Get performance summary statistics
     */
    std::map<std::string, double> get_performance_summary() const;
    
    /**
     * Get improvement milestones
     */
    std::vector<std::pair<std::chrono::steady_clock::time_point, std::string>> get_improvement_milestones() const;
    
    // Status accessors
    bool is_monitoring_active() const { return dashboard_active_; }
    bool is_baseline_established() const { return baseline_established_; }
    size_t get_metrics_count() const { return metrics_history_.size(); }

private:
    /**
     * Helper methods for internal operations
     */
    
    double calculate_performance_score();
    double calculate_meta_cognitive_score();
    double calculate_evolutionary_score();
    double calculate_integration_score();
    
    void update_trend_analysis();
    void detect_anomalies();
    void save_metrics_to_disk();
    void load_metrics_from_disk();
    
    std::string format_performance_display();
    std::string format_trends_display();
    std::string format_predictions_display();
    
    // Custom metric functions
    std::map<std::string, std::function<double()>> custom_metric_functions_;
    
    // Metric category enablement
    std::map<std::string, bool> metric_categories_enabled_;
};

} // namespace opencog

#endif // _OPENCOG_SELF_IMPROVEMENT_DASHBOARD_H