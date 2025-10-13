/*
 * MetaCognitiveMonitor.cc
 * 
 * Phase 5: Recursive Meta-Cognition & Evolutionary Optimization
 * Implementation of meta-cognitive monitoring system
 */

#include "../include/MetaCognitiveMonitor.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <cmath>
#include <iostream>

namespace opencog {

// MetaCognitiveTensor implementation
std::vector<double> MetaCognitiveTensor::to_vector() const {
    std::vector<double> vec;
    vec.push_back(self_awareness_level);
    vec.insert(vec.end(), performance_metric.begin(), performance_metric.end());
    vec.push_back(static_cast<double>(evolutionary_generation));
    vec.push_back(fitness_score);
    vec.push_back(adaptation_rate);
    vec.push_back(static_cast<double>(reflection_depth));
    return vec;
}

MetaCognitiveTensor MetaCognitiveTensor::from_vector(const std::vector<double>& vec) {
    MetaCognitiveTensor tensor;
    if (vec.size() >= 7) {
        tensor.self_awareness_level = vec[0];
        tensor.performance_metric = {vec[1], vec[2], vec[3]};
        tensor.evolutionary_generation = static_cast<int>(vec[4]);
        tensor.fitness_score = vec[5];
        tensor.adaptation_rate = vec[6];
        tensor.reflection_depth = static_cast<int>(vec[7]);
        tensor.cognitive_complexity = "moderate";
        tensor.meta_level = "meta";
        tensor.optimization_target = "accuracy";
    }
    return tensor;
}

double MetaCognitiveTensor::calculate_overall_fitness() const {
    if (performance_metric.empty()) return fitness_score;
    
    double avg_performance = std::accumulate(performance_metric.begin(), performance_metric.end(), 0.0) 
                           / performance_metric.size();
    
    return (fitness_score * 0.4) + (avg_performance * 0.3) + 
           (self_awareness_level * 0.2) + (adaptation_rate * 0.1);
}

// MetaCognitiveMonitor implementation
MetaCognitiveMonitor::MetaCognitiveMonitor(int max_depth, double adaptation_threshold)
    : max_reflection_depth_(max_depth), current_reflection_depth_(1),
      current_generation_(0), adaptation_threshold_(adaptation_threshold),
      improvement_target_threshold_(0.05)
{
    // Initialize meta-cognitive tensor with default values
    current_state_.self_awareness_level = 0.5;
    current_state_.performance_metric = {0.7, 0.6, 0.8}; // accuracy, efficiency, adaptability
    current_state_.evolutionary_generation = 0;
    current_state_.fitness_score = 0.5;
    current_state_.adaptation_rate = 0.1;
    current_state_.cognitive_complexity = "moderate";
    current_state_.meta_level = "meta";
    current_state_.reflection_depth = 1;
    current_state_.optimization_target = "accuracy";
    
    std::cout << "[MetaCognitiveMonitor] Initialized with max reflection depth: " 
              << max_depth << std::endl;
}

MetaCognitiveMonitor::~MetaCognitiveMonitor() {
    std::cout << "[MetaCognitiveMonitor] Shutting down - Final generation: " 
              << current_generation_ << std::endl;
}

MetaCognitiveTensor MetaCognitiveMonitor::observe_cognitive_state() {
    std::cout << "[MetaCognitiveMonitor] Observing cognitive state at generation " 
              << current_generation_ << std::endl;
    
    // Update performance metrics from observation functions
    for (const auto& obs_func : observation_functions_) {
        double metric_value = obs_func.second();
        current_state_.performance_metric[0] += metric_value * 0.1; // Update accuracy
        
        std::cout << "[MetaCognitiveMonitor] Component '" << obs_func.first 
                  << "' metric: " << metric_value << std::endl;
    }
    
    // Normalize performance metrics
    for (auto& metric : current_state_.performance_metric) {
        metric = std::min(1.0, std::max(0.0, metric));
    }
    
    // Update self-awareness based on reflection depth
    update_self_awareness_level();
    
    // Update cognitive complexity assessment
    current_state_.cognitive_complexity = determine_cognitive_complexity_level();
    current_state_.meta_level = determine_current_meta_level();
    
    return current_state_;
}

std::vector<std::string> MetaCognitiveMonitor::analyze_performance_trends() {
    std::vector<std::string> insights;
    
    if (performance_history_.size() < 2) {
        insights.push_back("Insufficient data for trend analysis");
        return insights;
    }
    
    // Calculate recent performance trends
    auto trends = calculate_performance_trends(5);
    
    if (trends[0] > 0.05) {
        insights.push_back("Improving accuracy trend detected");
    } else if (trends[0] < -0.05) {
        insights.push_back("Declining accuracy trend requires attention");
    }
    
    if (trends[1] > 0.05) {
        insights.push_back("Processing efficiency is improving");
    } else if (trends[1] < -0.05) {
        insights.push_back("Processing efficiency declining - optimization needed");
    }
    
    if (trends[2] > 0.05) {
        insights.push_back("Adaptability index shows positive trend");
    }
    
    std::cout << "[MetaCognitiveMonitor] Generated " << insights.size() 
              << " performance insights" << std::endl;
    
    return insights;
}

void MetaCognitiveMonitor::recursive_self_reflection() {
    if (current_reflection_depth_ >= max_reflection_depth_) {
        std::cout << "[MetaCognitiveMonitor] Maximum reflection depth reached: " 
                  << max_reflection_depth_ << std::endl;
        return;
    }
    
    current_reflection_depth_++;
    std::cout << "[MetaCognitiveMonitor] Recursive self-reflection at depth " 
              << current_reflection_depth_ << std::endl;
    
    // Meta-level reflection: observing the observation process
    auto meta_insights = cognitive_introspection();
    
    // Update meta-level awareness
    if (current_reflection_depth_ > 2) {
        current_state_.meta_level = "meta-meta";
        current_state_.self_awareness_level = std::min(1.0, 
            current_state_.self_awareness_level + 0.1);
    }
    
    // Recursive call - reflect on the reflection process itself
    if (current_reflection_depth_ < max_reflection_depth_) {
        recursive_self_reflection();
    }
    
    current_reflection_depth_--;
}

std::vector<std::string> MetaCognitiveMonitor::generate_improvement_suggestions() {
    std::vector<std::string> suggestions;
    
    // Analyze current performance state
    double overall_fitness = current_state_.calculate_overall_fitness();
    
    if (overall_fitness < 0.6) {
        suggestions.push_back("Overall performance below threshold - activate adaptive optimization");
    }
    
    if (current_state_.adaptation_rate < 0.05) {
        suggestions.push_back("Adaptation rate too low - increase learning parameters");
    }
    
    if (current_state_.performance_metric[1] < 0.5) {
        suggestions.push_back("Processing efficiency critical - optimize computational pathways");
    }
    
    // Meta-cognitive improvement suggestions
    if (current_state_.self_awareness_level < 0.7) {
        suggestions.push_back("Increase self-monitoring frequency for better awareness");
    }
    
    std::cout << "[MetaCognitiveMonitor] Generated " << suggestions.size() 
              << " improvement suggestions" << std::endl;
    
    return suggestions;
}

void MetaCognitiveMonitor::update_performance_metrics(const std::map<std::string, double>& metrics) {
    CognitivePerformanceMetrics perf_metrics;
    perf_metrics.timestamp = std::chrono::steady_clock::now();
    perf_metrics.component_metrics = metrics;
    
    // Extract standard metrics
    auto it = metrics.find("accuracy");
    perf_metrics.accuracy_score = (it != metrics.end()) ? it->second : 0.5;
    
    it = metrics.find("efficiency");
    perf_metrics.processing_efficiency = (it != metrics.end()) ? it->second : 0.5;
    
    it = metrics.find("adaptability");
    perf_metrics.adaptability_index = (it != metrics.end()) ? it->second : 0.5;
    
    it = metrics.find("learning_rate");
    perf_metrics.learning_rate = (it != metrics.end()) ? it->second : 0.1;
    
    it = metrics.find("resource_utilization");
    perf_metrics.resource_utilization = (it != metrics.end()) ? it->second : 0.6;
    
    performance_history_.push_back(perf_metrics);
    
    // Maintain history size limit
    if (performance_history_.size() > 1000) {
        performance_history_.erase(performance_history_.begin());
    }
    
    std::cout << "[MetaCognitiveMonitor] Updated performance metrics - history size: " 
              << performance_history_.size() << std::endl;
}

std::vector<double> MetaCognitiveMonitor::calculate_performance_trends(int window_size) {
    if (performance_history_.size() < static_cast<size_t>(window_size)) {
        return {0.0, 0.0, 0.0}; // No trend data available
    }
    
    auto recent_start = performance_history_.end() - window_size;
    auto old_start = performance_history_.end() - (window_size * 2);
    
    // Calculate average for recent window
    double recent_accuracy = 0.0, recent_efficiency = 0.0, recent_adaptability = 0.0;
    for (auto it = recent_start; it != performance_history_.end(); ++it) {
        recent_accuracy += it->accuracy_score;
        recent_efficiency += it->processing_efficiency;
        recent_adaptability += it->adaptability_index;
    }
    recent_accuracy /= window_size;
    recent_efficiency /= window_size;
    recent_adaptability /= window_size;
    
    // Calculate average for older window if available
    double old_accuracy = recent_accuracy, old_efficiency = recent_efficiency, 
           old_adaptability = recent_adaptability;
    
    if (performance_history_.size() >= static_cast<size_t>(window_size * 2)) {
        old_accuracy = 0.0; old_efficiency = 0.0; old_adaptability = 0.0;
        for (auto it = old_start; it != recent_start; ++it) {
            old_accuracy += it->accuracy_score;
            old_efficiency += it->processing_efficiency;
            old_adaptability += it->adaptability_index;
        }
        old_accuracy /= window_size;
        old_efficiency /= window_size;
        old_adaptability /= window_size;
    }
    
    return {
        recent_accuracy - old_accuracy,
        recent_efficiency - old_efficiency,
        recent_adaptability - old_adaptability
    };
}

std::vector<std::string> MetaCognitiveMonitor::detect_performance_anomalies() {
    std::vector<std::string> anomalies;
    
    if (performance_history_.empty()) return anomalies;
    
    const auto& latest = performance_history_.back();
    
    // Detect anomalous performance values
    if (latest.accuracy_score < 0.3) {
        anomalies.push_back("Critical accuracy anomaly detected");
    }
    
    if (latest.processing_efficiency < 0.2) {
        anomalies.push_back("Severe efficiency degradation detected");
    }
    
    if (latest.resource_utilization > 0.9) {
        anomalies.push_back("Resource utilization critically high");
    }
    
    // Check for sudden changes
    if (performance_history_.size() >= 2) {
        const auto& previous = performance_history_[performance_history_.size() - 2];
        
        if (std::abs(latest.accuracy_score - previous.accuracy_score) > 0.3) {
            anomalies.push_back("Sudden accuracy change detected");
        }
    }
    
    return anomalies;
}

void MetaCognitiveMonitor::register_observation_function(const std::string& component_name,
                                                       std::function<double()> observation_func) {
    observation_functions_[component_name] = observation_func;
    std::cout << "[MetaCognitiveMonitor] Registered observation function for: " 
              << component_name << std::endl;
}

void MetaCognitiveMonitor::update_self_awareness_level() {
    // Self-awareness increases with reflection depth and performance monitoring
    double awareness_factor = 0.1 * current_reflection_depth_ / max_reflection_depth_;
    
    // Increase awareness based on observation function diversity
    awareness_factor += 0.05 * observation_functions_.size() / 10.0;
    
    // Increase awareness based on performance history richness
    if (!performance_history_.empty()) {
        awareness_factor += 0.05 * std::min(10.0, static_cast<double>(performance_history_.size())) / 10.0;
    }
    
    current_state_.self_awareness_level = std::min(1.0, 
        current_state_.self_awareness_level + awareness_factor);
}

void MetaCognitiveMonitor::adaptive_parameter_tuning() {
    // Adapt monitoring parameters based on observed effectiveness
    if (!performance_history_.empty()) {
        auto trends = calculate_performance_trends(5);
        double avg_trend = (trends[0] + trends[1] + trends[2]) / 3.0;
        
        if (avg_trend > 0.1) {
            // Performance improving - can relax monitoring
            adaptation_threshold_ *= 1.1;
        } else if (avg_trend < -0.1) {
            // Performance declining - increase monitoring sensitivity
            adaptation_threshold_ *= 0.9;
        }
        
        // Bound the threshold
        adaptation_threshold_ = std::max(0.01, std::min(0.5, adaptation_threshold_));
    }
    
    std::cout << "[MetaCognitiveMonitor] Adapted threshold to: " 
              << adaptation_threshold_ << std::endl;
}

void MetaCognitiveMonitor::recursive_monitoring_cycle() {
    std::cout << "[MetaCognitiveMonitor] Starting recursive monitoring cycle" << std::endl;
    
    // Monitor the monitoring process itself
    auto pre_cycle_state = current_state_;
    
    // Perform standard observation
    observe_cognitive_state();
    
    // Analyze the change in state due to monitoring
    auto post_cycle_state = current_state_;
    
    // Meta-level analysis of monitoring effectiveness
    double monitoring_impact = std::abs(post_cycle_state.self_awareness_level - 
                                      pre_cycle_state.self_awareness_level);
    
    if (monitoring_impact < 0.01) {
        std::cout << "[MetaCognitiveMonitor] Low monitoring impact - increasing sensitivity" << std::endl;
        adaptation_threshold_ *= 0.95;
    }
    
    // Recursive reflection on the monitoring cycle
    recursive_self_reflection();
}

std::map<std::string, double> MetaCognitiveMonitor::cognitive_introspection() {
    std::map<std::string, double> introspection_results;
    
    // Analyze own cognitive processes
    introspection_results["monitoring_effectiveness"] = calculate_learning_effectiveness();
    introspection_results["reflection_depth_utilization"] = 
        static_cast<double>(current_reflection_depth_) / max_reflection_depth_;
    introspection_results["adaptation_responsiveness"] = current_state_.adaptation_rate;
    introspection_results["self_awareness_growth"] = current_state_.self_awareness_level;
    introspection_results["cognitive_complexity"] = calculate_cognitive_complexity_score();
    
    std::cout << "[MetaCognitiveMonitor] Cognitive introspection completed" << std::endl;
    
    return introspection_results;
}

std::vector<std::string> MetaCognitiveMonitor::generate_meta_insights() {
    std::vector<std::string> meta_insights;
    
    auto introspection = cognitive_introspection();
    
    if (introspection["monitoring_effectiveness"] > 0.8) {
        meta_insights.push_back("Meta-cognitive monitoring system operating effectively");
    }
    
    if (introspection["reflection_depth_utilization"] > 0.7) {
        meta_insights.push_back("Deep recursive reflection engaged - high meta-awareness achieved");
    }
    
    if (current_state_.self_awareness_level > 0.8) {
        meta_insights.push_back("System demonstrates high self-awareness - recursive cognition active");
    }
    
    return meta_insights;
}

void MetaCognitiveMonitor::set_optimization_target(const std::string& target) {
    current_state_.optimization_target = target;
    std::cout << "[MetaCognitiveMonitor] Optimization target set to: " << target << std::endl;
}

void MetaCognitiveMonitor::advance_generation() {
    current_generation_++;
    current_state_.evolutionary_generation = current_generation_;
    std::cout << "[MetaCognitiveMonitor] Advanced to generation: " << current_generation_ << std::endl;
}

// Private helper methods
double MetaCognitiveMonitor::calculate_adaptability_index() {
    if (performance_history_.size() < 2) return 0.5;
    
    // Calculate variance in performance as adaptability measure
    auto recent_metrics = calculate_performance_trends(3);
    double variance = (recent_metrics[0]*recent_metrics[0] + 
                      recent_metrics[1]*recent_metrics[1] + 
                      recent_metrics[2]*recent_metrics[2]) / 3.0;
    
    return std::min(1.0, variance * 10.0); // Normalize to [0,1]
}

double MetaCognitiveMonitor::calculate_learning_effectiveness() {
    if (performance_history_.size() < 5) return 0.5;
    
    // Measure improvement rate over time
    auto long_trend = calculate_performance_trends(performance_history_.size() / 2);
    double improvement = (long_trend[0] + long_trend[1] + long_trend[2]) / 3.0;
    
    return std::max(0.0, std::min(1.0, improvement * 5.0 + 0.5));
}

double MetaCognitiveMonitor::calculate_cognitive_complexity_score() {
    double complexity = 0.0;
    
    // Complexity based on number of observation functions
    complexity += observation_functions_.size() / 20.0;
    
    // Complexity based on reflection depth utilization
    complexity += static_cast<double>(current_reflection_depth_) / max_reflection_depth_ / 2.0;
    
    // Complexity based on performance history richness
    complexity += std::min(1.0, static_cast<double>(performance_history_.size()) / 100.0) / 2.0;
    
    return std::min(1.0, complexity);
}

void MetaCognitiveMonitor::update_evolutionary_metrics() {
    // Update adaptation rate based on recent performance changes
    auto trends = calculate_performance_trends(3);
    double avg_change = (std::abs(trends[0]) + std::abs(trends[1]) + std::abs(trends[2])) / 3.0;
    
    current_state_.adaptation_rate = std::min(1.0, avg_change * 10.0);
    
    // Update fitness score based on overall performance
    current_state_.fitness_score = current_state_.calculate_overall_fitness();
}

std::string MetaCognitiveMonitor::determine_cognitive_complexity_level() {
    double complexity_score = calculate_cognitive_complexity_score();
    
    if (complexity_score < 0.3) return "simple";
    else if (complexity_score < 0.7) return "moderate";
    else return "complex";
}

std::string MetaCognitiveMonitor::determine_current_meta_level() {
    if (current_reflection_depth_ >= 3) return "meta-meta";
    else if (current_reflection_depth_ >= 2) return "meta";
    else return "object";
}

} // namespace opencog