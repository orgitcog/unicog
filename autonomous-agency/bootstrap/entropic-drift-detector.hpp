#ifndef ENTROPIC_DRIFT_DETECTOR_HPP
#define ENTROPIC_DRIFT_DETECTOR_HPP

#include <vector>
#include <memory>
#include <chrono>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attention/AttentionBank.h>

namespace opencog {
namespace autonomous {

/**
 * Measures and detects various forms of entropic drift in the cognitive system.
 * This is the foundation for maintaining coherent operation despite perturbations.
 */
class EntropicDriftDetector {
public:
    struct MetricCollector {
        std::string name;
        double (*measure_func)(AtomSpacePtr);
        double baseline_value;
        double current_value;
        double drift_threshold;
        std::chrono::time_point<std::chrono::steady_clock> last_measurement;
    };

    struct DriftAnalysis {
        double entropy_score;
        double coherence_score;
        double attention_distribution_score;
        bool fragmentation_detected;
        bool memory_decay_detected;
        bool reasoning_degradation_detected;
        std::vector<std::string> critical_issues;
    };

private:
    AtomSpacePtr atomspace_;
    std::shared_ptr<AttentionBank> attention_bank_;
    std::vector<MetricCollector> entropy_metrics_;
    
    // Baseline measurements for comparison
    double baseline_entropy_;
    double baseline_coherence_;
    double baseline_attention_distribution_;
    
    // Thresholds for drift detection
    static constexpr double FRAGMENTATION_THRESHOLD = 0.15;
    static constexpr double DECAY_THRESHOLD = 0.20;
    static constexpr double REASONING_THRESHOLD = 0.10;

public:
    EntropicDriftDetector(AtomSpacePtr atomspace, 
                         std::shared_ptr<AttentionBank> attention_bank);
    
    // Core entropy measurement functions
    double measureSystemEntropy();
    double measureAtomSpaceCoherence();
    double measureAttentionDistribution();
    
    // Specific drift detection algorithms
    bool detectCognitiveFragmentation();
    bool detectMemoryDecay();
    bool detectReasoningDegradation();
    
    // Comprehensive analysis
    DriftAnalysis performComprehensiveAnalysis();
    
    // Bootstrap intervention triggers
    void triggerSystemReorganization();
    void activateRepairMechanisms();
    
    // Metric management
    void addCustomMetric(const std::string& name, 
                        double (*measure_func)(AtomSpacePtr),
                        double threshold);
    void updateBaselines();
    void calibrateThresholds();
    
    // Monitoring and reporting
    void startContinuousMonitoring();
    void stopContinuousMonitoring();
    std::vector<std::string> generateDriftReport();

private:
    // Internal measurement helpers
    double calculateInformationEntropy();
    double calculateStructuralCoherence();
    double calculateAttentionEntropy();
    double calculateReasoningEfficiency();
    
    // Pattern detection helpers
    bool detectFragmentationPatterns();
    bool detectDecayPatterns();
    bool detectReasoningAnomalies();
    
    // Statistical analysis helpers
    double calculateMovingAverage(const std::vector<double>& values, size_t window_size);
    double calculateStandardDeviation(const std::vector<double>& values);
    bool isSignificantDrift(double current, double baseline, double threshold);
};

} // namespace autonomous
} // namespace opencog

#endif // ENTROPIC_DRIFT_DETECTOR_HPP