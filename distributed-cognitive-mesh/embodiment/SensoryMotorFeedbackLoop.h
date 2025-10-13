/*
 * SensoryMotorFeedbackLoop.h
 *
 * Sensory-Motor Cognitive Feedback Loop Implementation
 * Implements closed-loop sensory-motor integration for embodied cognition
 */

#ifndef _OPENCOG_SENSORY_MOTOR_FEEDBACK_LOOP_H
#define _OPENCOG_SENSORY_MOTOR_FEEDBACK_LOOP_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <thread>
#include <mutex>
#include <atomic>
#include <chrono>
#include <queue>
#include <condition_variable>

#include "../api/EmbodimentTensorSignature.h"

namespace opencog {
namespace embodiment {

/**
 * Sensory Input Data Structure
 */
struct SensoryInput {
    std::string input_id;
    EmbodimentTensorSignature::SensoryModality modality;
    std::vector<double> raw_data;
    std::map<std::string, double> processed_features;
    double intensity;
    double confidence;
    std::chrono::steady_clock::time_point timestamp;
    std::string source_agent_id;
    
    // Spatial information
    double position_x, position_y, position_z;
    double orientation_x, orientation_y, orientation_z, orientation_w;
    
    // Temporal information
    double duration_ms;
    bool is_continuous;
    
    // Quality metrics
    double noise_level;
    double signal_quality;
};

/**
 * Motor Output Data Structure
 */
struct MotorOutput {
    std::string output_id;
    EmbodimentTensorSignature::MotorCommand command_type;
    std::vector<double> command_values;
    std::map<std::string, double> command_parameters;
    double magnitude;
    double confidence;
    std::chrono::steady_clock::time_point timestamp;
    std::string target_agent_id;
    
    // Execution information
    double execution_time_ms;
    bool requires_feedback;
    double expected_duration_ms;
    
    // Safety constraints
    std::vector<double> safety_limits;
    double max_force;
    double max_velocity;
};

/**
 * Feedback Loop State
 */
enum class FeedbackLoopState {
    INACTIVE,
    INITIALIZING,
    RUNNING,
    PAUSED,
    ERROR,
    SHUTDOWN
};

/**
 * Feedback Performance Metrics
 */
struct FeedbackMetrics {
    double loop_frequency_hz;
    double average_latency_ms;
    double sensory_processing_time_ms;
    double cognitive_processing_time_ms;
    double motor_processing_time_ms;
    double total_cycle_time_ms;
    
    uint64_t successful_cycles;
    uint64_t failed_cycles;
    double success_rate;
    
    double prediction_accuracy;
    double adaptation_rate;
    double learning_progress;
};

/**
 * Sensory-Motor Feedback Loop
 * 
 * Implements real-time closed-loop sensory-motor integration for embodied agents
 */
class SensoryMotorFeedbackLoop {
private:
    // Configuration
    std::string agent_id_;
    double target_frequency_hz_;
    bool is_adaptive_;
    bool enable_learning_;
    bool enable_prediction_;
    
    // State management
    FeedbackLoopState current_state_;
    std::atomic<bool> is_running_;
    std::atomic<bool> should_stop_;
    
    // Data buffers
    std::queue<SensoryInput> sensory_input_buffer_;
    std::queue<MotorOutput> motor_output_buffer_;
    std::vector<SensoryInput> sensory_history_;
    std::vector<MotorOutput> motor_history_;
    
    // Current embodiment state
    EmbodimentTensorSignature current_tensor_;
    EmbodimentTensorSignature predicted_tensor_;
    
    // Callback functions
    std::function<std::vector<SensoryInput>()> sensory_input_provider_;
    std::function<void(const std::vector<MotorOutput>&)> motor_output_executor_;
    std::function<EmbodimentTensorSignature(const std::vector<SensoryInput>&, 
                                          const EmbodimentTensorSignature&)> cognitive_processor_;
    
    // Threading
    std::thread feedback_loop_thread_;
    std::thread sensory_processing_thread_;
    std::thread motor_processing_thread_;
    mutable std::mutex loop_mutex_;
    std::condition_variable loop_condition_;
    
    // Performance tracking
    FeedbackMetrics performance_metrics_;
    std::chrono::steady_clock::time_point last_cycle_time_;
    std::vector<double> cycle_times_;
    
    // Learning and adaptation
    std::map<std::string, std::vector<double>> learned_associations_;
    std::map<std::string, double> adaptation_weights_;
    double learning_rate_;
    double adaptation_threshold_;

public:
    /**
     * Constructor
     */
    SensoryMotorFeedbackLoop(const std::string& agent_id, double target_frequency_hz = 10.0);
    
    /**
     * Destructor
     */
    ~SensoryMotorFeedbackLoop();

    // ===== Lifecycle Management =====
    
    /**
     * Initialize feedback loop
     */
    bool initialize();
    
    /**
     * Start feedback loop execution
     */
    bool start();
    
    /**
     * Pause feedback loop
     */
    void pause();
    
    /**
     * Resume feedback loop
     */
    void resume();
    
    /**
     * Stop feedback loop
     */
    void stop();
    
    /**
     * Get current state
     */
    FeedbackLoopState get_state() const;

    // ===== Configuration =====
    
    /**
     * Set target loop frequency
     */
    void set_target_frequency(double frequency_hz);
    
    /**
     * Enable/disable adaptive behavior
     */
    void set_adaptive_mode(bool enable);
    
    /**
     * Enable/disable learning
     */
    void set_learning_mode(bool enable);
    
    /**
     * Enable/disable prediction
     */
    void set_prediction_mode(bool enable);
    
    /**
     * Set learning rate
     */
    void set_learning_rate(double rate);

    // ===== Callback Registration =====
    
    /**
     * Register sensory input provider callback
     */
    void register_sensory_input_provider(std::function<std::vector<SensoryInput>()> provider);
    
    /**
     * Register motor output executor callback
     */
    void register_motor_output_executor(std::function<void(const std::vector<MotorOutput>&)> executor);
    
    /**
     * Register cognitive processor callback
     */
    void register_cognitive_processor(
        std::function<EmbodimentTensorSignature(const std::vector<SensoryInput>&, 
                                               const EmbodimentTensorSignature&)> processor);

    // ===== Manual Data Injection =====
    
    /**
     * Add sensory input to processing queue
     */
    void add_sensory_input(const SensoryInput& input);
    
    /**
     * Add multiple sensory inputs
     */
    void add_sensory_inputs(const std::vector<SensoryInput>& inputs);
    
    /**
     * Get pending motor outputs
     */
    std::vector<MotorOutput> get_pending_motor_outputs();
    
    /**
     * Clear motor output buffer
     */
    void clear_motor_output_buffer();

    // ===== State Access =====
    
    /**
     * Get current embodiment tensor
     */
    EmbodimentTensorSignature get_current_tensor() const;
    
    /**
     * Get predicted embodiment tensor
     */
    EmbodimentTensorSignature get_predicted_tensor() const;
    
    /**
     * Set current embodiment tensor
     */
    void set_current_tensor(const EmbodimentTensorSignature& tensor);
    
    /**
     * Get sensory input history
     */
    std::vector<SensoryInput> get_sensory_history(size_t max_items = 100) const;
    
    /**
     * Get motor output history
     */
    std::vector<MotorOutput> get_motor_history(size_t max_items = 100) const;

    // ===== Performance Monitoring =====
    
    /**
     * Get performance metrics
     */
    FeedbackMetrics get_performance_metrics() const;
    
    /**
     * Reset performance metrics
     */
    void reset_performance_metrics();
    
    /**
     * Get loop timing statistics
     */
    struct TimingStats {
        double min_cycle_time_ms;
        double max_cycle_time_ms;
        double average_cycle_time_ms;
        double cycle_time_std_dev_ms;
        double actual_frequency_hz;
    };
    TimingStats get_timing_statistics() const;

    // ===== Learning and Adaptation =====
    
    /**
     * Train sensory-motor association
     */
    void train_association(const SensoryInput& sensory, const MotorOutput& motor);
    
    /**
     * Get learned association strength
     */
    double get_association_strength(const std::string& sensory_pattern, 
                                  const std::string& motor_pattern) const;
    
    /**
     * Adapt feedback loop parameters
     */
    void adapt_parameters();
    
    /**
     * Get adaptation weights
     */
    std::map<std::string, double> get_adaptation_weights() const;
    
    /**
     * Reset learning data
     */
    void reset_learning_data();

    // ===== Prediction and Anticipation =====
    
    /**
     * Predict next sensory input
     */
    std::vector<SensoryInput> predict_next_sensory_input() const;
    
    /**
     * Predict motor output for given sensory input
     */
    std::vector<MotorOutput> predict_motor_output(const std::vector<SensoryInput>& sensory) const;
    
    /**
     * Update prediction models
     */
    void update_prediction_models();
    
    /**
     * Evaluate prediction accuracy
     */
    double evaluate_prediction_accuracy() const;

    // ===== Diagnostic and Debugging =====
    
    /**
     * Get detailed loop status
     */
    struct LoopStatus {
        FeedbackLoopState state;
        double current_frequency_hz;
        size_t sensory_buffer_size;
        size_t motor_buffer_size;
        bool is_learning_active;
        bool is_prediction_active;
        std::string last_error;
    };
    LoopStatus get_loop_status() const;
    
    /**
     * Enable debug logging
     */
    void set_debug_mode(bool enable);
    
    /**
     * Get debug information
     */
    std::string get_debug_info() const;
    
    /**
     * Export loop data for analysis
     */
    std::string export_loop_data() const;
    
    /**
     * Import loop configuration
     */
    bool import_loop_configuration(const std::string& config_data);

private:
    // ===== Internal Processing Methods =====
    
    /**
     * Main feedback loop thread function
     */
    void feedback_loop_worker();
    
    /**
     * Sensory processing thread function
     */
    void sensory_processing_worker();
    
    /**
     * Motor processing thread function
     */
    void motor_processing_worker();
    
    /**
     * Execute one feedback loop cycle
     */
    void execute_feedback_cycle();
    
    /**
     * Process sensory inputs
     */
    void process_sensory_inputs();
    
    /**
     * Update cognitive state
     */
    void update_cognitive_state();
    
    /**
     * Generate motor outputs
     */
    void generate_motor_outputs();
    
    /**
     * Execute motor commands
     */
    void execute_motor_commands();
    
    /**
     * Update performance metrics
     */
    void update_performance_metrics();
    
    /**
     * Check for adaptation triggers
     */
    void check_adaptation_triggers();
    
    /**
     * Validate loop configuration
     */
    bool validate_configuration() const;
    
    /**
     * Initialize threading components
     */
    void initialize_threads();
    
    /**
     * Cleanup resources
     */
    void cleanup_resources();
    
    /**
     * Handle loop errors
     */
    void handle_loop_error(const std::string& error_message);
};

} // namespace embodiment
} // namespace opencog

#endif // _OPENCOG_SENSORY_MOTOR_FEEDBACK_LOOP_H