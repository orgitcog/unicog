#include "entropic-drift-detector.hpp"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>

namespace opencog {
namespace autonomous {

EntropicDriftDetector::EntropicDriftDetector(AtomSpacePtr atomspace, 
                                           std::shared_ptr<AttentionBank> attention_bank)
    : atomspace_(atomspace), attention_bank_(attention_bank) {
    
    // Initialize baseline measurements
    updateBaselines();
    
    // Setup default metrics
    addCustomMetric("system_entropy", 
                   [](AtomSpacePtr as) { return static_cast<double>(as->get_size()) / 1000.0; },
                   0.10);
    
    addCustomMetric("coherence_measure",
                   [](AtomSpacePtr as) { 
                       // Simplified coherence based on link density
                       HandleSeq all_atoms = as->get_handles_by_type(ATOM, true);
                       size_t link_count = 0;
                       for (const Handle& h : all_atoms) {
                           if (h->is_link()) link_count++;
                       }
                       return link_count > 0 ? static_cast<double>(link_count) / all_atoms.size() : 0.0;
                   },
                   0.15);
}

double EntropicDriftDetector::measureSystemEntropy() {
    return calculateInformationEntropy();
}

double EntropicDriftDetector::measureAtomSpaceCoherence() {
    return calculateStructuralCoherence();
}

double EntropicDriftDetector::measureAttentionDistribution() {
    return calculateAttentionEntropy();
}

bool EntropicDriftDetector::detectCognitiveFragmentation() {
    double current_coherence = measureAtomSpaceCoherence();
    return isSignificantDrift(current_coherence, baseline_coherence_, FRAGMENTATION_THRESHOLD);
}

bool EntropicDriftDetector::detectMemoryDecay() {
    // Check for significant reduction in meaningful connections
    HandleSeq all_atoms = atomspace_->get_handles_by_type(ATOM, true);
    double connectivity_ratio = 0.0;
    
    if (!all_atoms.empty()) {
        size_t connected_atoms = 0;
        for (const Handle& h : all_atoms) {
            if (h->is_link() && h->get_arity() > 1) {
                connected_atoms++;
            }
        }
        connectivity_ratio = static_cast<double>(connected_atoms) / all_atoms.size();
    }
    
    // Compare with expected baseline (simplified)
    static double expected_connectivity = 0.3; // 30% of atoms should be meaningful links
    return connectivity_ratio < (expected_connectivity * (1.0 - DECAY_THRESHOLD));
}

bool EntropicDriftDetector::detectReasoningDegradation() {
    double reasoning_efficiency = calculateReasoningEfficiency();
    return reasoning_efficiency < (1.0 - REASONING_THRESHOLD);
}

EntropicDriftDetector::DriftAnalysis EntropicDriftDetector::performComprehensiveAnalysis() {
    DriftAnalysis analysis;
    
    analysis.entropy_score = measureSystemEntropy();
    analysis.coherence_score = measureAtomSpaceCoherence();
    analysis.attention_distribution_score = measureAttentionDistribution();
    
    analysis.fragmentation_detected = detectCognitiveFragmentation();
    analysis.memory_decay_detected = detectMemoryDecay();
    analysis.reasoning_degradation_detected = detectReasoningDegradation();
    
    // Compile critical issues
    if (analysis.fragmentation_detected) {
        analysis.critical_issues.push_back("Cognitive fragmentation detected");
    }
    if (analysis.memory_decay_detected) {
        analysis.critical_issues.push_back("Memory decay patterns identified");
    }
    if (analysis.reasoning_degradation_detected) {
        analysis.critical_issues.push_back("Reasoning capability degradation");
    }
    
    return analysis;
}

void EntropicDriftDetector::triggerSystemReorganization() {
    // Implementation would trigger reorganization of AtomSpace structure
    // This would involve calling reorganization protocols when implemented
    
    // For now, log the trigger event
    std::cout << "BOOTSTRAP: System reorganization triggered due to entropic drift" << std::endl;
    
    // Update baseline after reorganization
    updateBaselines();
}

void EntropicDriftDetector::activateRepairMechanisms() {
    // Implementation would activate various repair mechanisms
    // This would integrate with self-healing components when implemented
    
    std::cout << "BOOTSTRAP: Repair mechanisms activated" << std::endl;
    
    // Force garbage collection if available
    if (atomspace_) {
        // Trigger any available cleanup mechanisms
        atomspace_->clear_transient();
    }
}

void EntropicDriftDetector::addCustomMetric(const std::string& name, 
                                          double (*measure_func)(AtomSpacePtr),
                                          double threshold) {
    MetricCollector metric;
    metric.name = name;
    metric.measure_func = measure_func;
    metric.drift_threshold = threshold;
    metric.baseline_value = measure_func(atomspace_);
    metric.current_value = metric.baseline_value;
    metric.last_measurement = std::chrono::steady_clock::now();
    
    entropy_metrics_.push_back(metric);
}

void EntropicDriftDetector::updateBaselines() {
    baseline_entropy_ = measureSystemEntropy();
    baseline_coherence_ = measureAtomSpaceCoherence();
    baseline_attention_distribution_ = measureAttentionDistribution();
    
    // Update custom metrics baselines
    for (auto& metric : entropy_metrics_) {
        metric.baseline_value = metric.measure_func(atomspace_);
    }
}

double EntropicDriftDetector::calculateInformationEntropy() {
    HandleSeq all_atoms = atomspace_->get_handles_by_type(ATOM, true);
    if (all_atoms.empty()) return 0.0;
    
    // Calculate type distribution entropy
    std::map<Type, size_t> type_counts;
    for (const Handle& h : all_atoms) {
        type_counts[h->get_type()]++;
    }
    
    double entropy = 0.0;
    double total = static_cast<double>(all_atoms.size());
    
    for (const auto& pair : type_counts) {
        double probability = static_cast<double>(pair.second) / total;
        if (probability > 0.0) {
            entropy -= probability * std::log2(probability);
        }
    }
    
    return entropy;
}

double EntropicDriftDetector::calculateStructuralCoherence() {
    HandleSeq all_atoms = atomspace_->get_handles_by_type(ATOM, true);
    if (all_atoms.empty()) return 0.0;
    
    size_t total_possible_connections = all_atoms.size() * (all_atoms.size() - 1) / 2;
    size_t actual_connections = 0;
    
    // Count meaningful structural connections
    for (const Handle& h : all_atoms) {
        if (h->is_link()) {
            actual_connections += h->get_arity() > 1 ? h->get_arity() : 0;
        }
    }
    
    return total_possible_connections > 0 ? 
           static_cast<double>(actual_connections) / total_possible_connections : 0.0;
}

double EntropicDriftDetector::calculateAttentionEntropy() {
    if (!attention_bank_) return 0.0;
    
    // Calculate attention value distribution entropy
    HandleSeq attended_atoms = attention_bank_->get_handles_by_AV(0, 1000); // Get all with some attention
    if (attended_atoms.empty()) return 0.0;
    
    std::vector<double> attention_values;
    for (const Handle& h : attended_atoms) {
        AttentionValuePtr av = h->getAttentionValue();
        if (av) {
            attention_values.push_back(static_cast<double>(av->getSTI()));
        }
    }
    
    if (attention_values.empty()) return 0.0;
    
    // Normalize and calculate entropy
    double sum = std::accumulate(attention_values.begin(), attention_values.end(), 0.0);
    if (sum == 0.0) return 0.0;
    
    double entropy = 0.0;
    for (double value : attention_values) {
        double probability = value / sum;
        if (probability > 0.0) {
            entropy -= probability * std::log2(probability);
        }
    }
    
    return entropy;
}

double EntropicDriftDetector::calculateReasoningEfficiency() {
    // Simplified reasoning efficiency based on successful link formations
    // In a full implementation, this would measure actual reasoning performance
    
    HandleSeq links = atomspace_->get_handles_by_type(LINK, true);
    HandleSeq nodes = atomspace_->get_handles_by_type(NODE, true);
    
    if (nodes.empty()) return 0.0;
    
    // Calculate ratio of meaningful links to nodes as efficiency proxy
    size_t meaningful_links = 0;
    for (const Handle& link : links) {
        if (link->get_arity() >= 2) { // Links connecting at least 2 nodes
            meaningful_links++;
        }
    }
    
    return nodes.size() > 0 ? 
           static_cast<double>(meaningful_links) / nodes.size() : 0.0;
}

bool EntropicDriftDetector::isSignificantDrift(double current, double baseline, double threshold) {
    if (baseline == 0.0) return false;
    double drift_ratio = std::abs(current - baseline) / baseline;
    return drift_ratio > threshold;
}

std::vector<std::string> EntropicDriftDetector::generateDriftReport() {
    std::vector<std::string> report;
    
    DriftAnalysis analysis = performComprehensiveAnalysis();
    
    report.push_back("=== Entropic Drift Analysis Report ===");
    report.push_back("Entropy Score: " + std::to_string(analysis.entropy_score));
    report.push_back("Coherence Score: " + std::to_string(analysis.coherence_score));
    report.push_back("Attention Distribution Score: " + std::to_string(analysis.attention_distribution_score));
    report.push_back("");
    
    report.push_back("Drift Detection Results:");
    report.push_back("Fragmentation: " + std::string(analysis.fragmentation_detected ? "DETECTED" : "Normal"));
    report.push_back("Memory Decay: " + std::string(analysis.memory_decay_detected ? "DETECTED" : "Normal"));
    report.push_back("Reasoning Degradation: " + std::string(analysis.reasoning_degradation_detected ? "DETECTED" : "Normal"));
    report.push_back("");
    
    if (!analysis.critical_issues.empty()) {
        report.push_back("Critical Issues:");
        for (const std::string& issue : analysis.critical_issues) {
            report.push_back("- " + issue);
        }
    } else {
        report.push_back("No critical issues detected.");
    }
    
    return report;
}

} // namespace autonomous
} // namespace opencog