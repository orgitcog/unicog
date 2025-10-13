/*
 * EmbodimentTensorSignature.h
 *
 * Phase 4: Embodiment Tensor Signature Implementation
 * Implements the 8-dimensional embodiment tensor as specified in the issue
 */

#ifndef _OPENCOG_EMBODIMENT_TENSOR_SIGNATURE_H
#define _OPENCOG_EMBODIMENT_TENSOR_SIGNATURE_H

#include <vector>
#include <string>
#include <map>
#include <array>
#include <chrono>
#include <mutex>

namespace opencog {
namespace embodiment {

/**
 * Embodiment Tensor Signature
 * 8-dimensional tensor representing embodied cognitive state:
 * [sensory_modality, motor_command, spatial_coordinates, temporal_context,
 *  action_confidence, embodiment_state, interaction_mode, feedback_loop]
 */
struct EmbodimentTensorSignature {
    // Dimension 1: Sensory Modality [4 elements]
    enum SensoryModality { VISUAL = 0, AUDITORY = 1, TACTILE = 2, PROPRIOCEPTIVE = 3 };
    std::array<double, 4> sensory_modality;

    // Dimension 2: Motor Command [3 elements]
    enum MotorCommand { POSITION = 0, VELOCITY = 1, FORCE = 2 };
    std::array<double, 3> motor_command;

    // Dimension 3: Spatial Coordinates [4 elements]
    struct SpatialCoordinates {
        double x, y, z, orientation;
    } spatial_coordinates;

    // Dimension 4: Temporal Context [3 elements]
    enum TemporalContext { PAST = 0, PRESENT = 1, FUTURE = 2 };
    std::array<double, 3> temporal_context;

    // Dimension 5: Action Confidence [1 element, range 0.0-1.0]
    double action_confidence;

    // Dimension 6: Embodiment State [3 elements]
    enum EmbodimentState { VIRTUAL = 0, PHYSICAL = 1, HYBRID = 2 };
    std::array<double, 3> embodiment_state;

    // Dimension 7: Interaction Mode [3 elements]
    enum InteractionMode { PASSIVE = 0, ACTIVE = 1, ADAPTIVE = 2 };
    std::array<double, 3> interaction_mode;

    // Dimension 8: Feedback Loop [3 elements]
    enum FeedbackLoop { OPEN = 0, CLOSED = 1, PREDICTIVE = 2 };
    std::array<double, 3> feedback_loop;

    // Metadata
    std::string agent_id;
    std::chrono::steady_clock::time_point timestamp;
    std::string environment_context;
    double tensor_validity;  // Confidence in tensor accuracy

    // Constructor
    EmbodimentTensorSignature();
    EmbodimentTensorSignature(const std::string& agent_id);

    /**
     * Convert tensor to flat vector representation
     */
    std::vector<double> to_vector() const;

    /**
     * Load tensor from flat vector representation
     */
    void from_vector(const std::vector<double>& data);

    /**
     * Validate tensor consistency and ranges
     */
    bool validate() const;

    /**
     * Calculate tensor magnitude/norm
     */
    double magnitude() const;

    /**
     * Normalize tensor values to unit range
     */
    void normalize();

    /**
     * Calculate similarity with another tensor
     */
    double similarity(const EmbodimentTensorSignature& other) const;

    /**
     * Get tensor dimension count (should be 8)
     */
    static size_t dimension_count() { return 8; }

    /**
     * Get total tensor element count (should be 27)
     */
    static size_t element_count() { return 27; }

    /**
     * Convert to JSON representation
     */
    std::string to_json() const;

    /**
     * Load from JSON representation
     */
    bool from_json(const std::string& json);

    /**
     * Update sensory input
     */
    void update_sensory_input(SensoryModality modality, double intensity);

    /**
     * Update motor output
     */
    void update_motor_command(MotorCommand command, double value);

    /**
     * Update spatial position
     */
    void update_spatial_position(double x, double y, double z, double orientation);

    /**
     * Update temporal context focus
     */
    void update_temporal_focus(TemporalContext context, double weight);

    /**
     * Update embodiment state
     */
    void update_embodiment_state(EmbodimentState state, double activation);

    /**
     * Update interaction mode
     */
    void update_interaction_mode(InteractionMode mode, double activation);

    /**
     * Update feedback loop configuration
     */
    void update_feedback_loop(FeedbackLoop loop, double activation);

    /**
     * Calculate cognitive load based on tensor state
     */
    double calculate_cognitive_load() const;

    /**
     * Predict next tensor state based on current dynamics
     */
    EmbodimentTensorSignature predict_next_state(double time_delta) const;

    /**
     * Apply tensor transformation matrix
     */
    void apply_transformation(const std::vector<std::vector<double>>& transform_matrix);

    /**
     * Get tensor summary string
     */
    std::string get_summary() const;

    /**
     * Check if tensor represents active embodiment
     */
    bool is_actively_embodied() const;

    /**
     * Get dominant sensory modality
     */
    SensoryModality get_dominant_sensory_modality() const;

    /**
     * Get dominant motor command
     */
    MotorCommand get_dominant_motor_command() const;

    /**
     * Get current embodiment state
     */
    EmbodimentState get_current_embodiment_state() const;

    /**
     * Get active interaction mode
     */
    InteractionMode get_active_interaction_mode() const;

    /**
     * Get feedback loop configuration
     */
    FeedbackLoop get_feedback_loop_configuration() const;
};

/**
 * Embodiment Tensor Manager
 * Manages collections of embodiment tensors for multi-agent systems
 */
class EmbodimentTensorManager {
private:
    std::map<std::string, EmbodimentTensorSignature> agent_tensors_;
    std::map<std::string, std::vector<EmbodimentTensorSignature>> tensor_history_;
    size_t max_history_size_;
    mutable std::mutex manager_mutex_;

public:
    EmbodimentTensorManager(size_t max_history = 1000);

    /**
     * Register agent tensor
     */
    void register_agent(const std::string& agent_id, const EmbodimentTensorSignature& tensor);

    /**
     * Update agent tensor
     */
    void update_agent_tensor(const std::string& agent_id, const EmbodimentTensorSignature& tensor);

    /**
     * Get current agent tensor
     */
    EmbodimentTensorSignature get_agent_tensor(const std::string& agent_id) const;

    /**
     * Get tensor history for agent
     */
    std::vector<EmbodimentTensorSignature> get_tensor_history(const std::string& agent_id) const;

    /**
     * Get all active agents
     */
    std::vector<std::string> get_active_agents() const;

    /**
     * Calculate tensor correlation between agents
     */
    double calculate_tensor_correlation(const std::string& agent1, const std::string& agent2) const;

    /**
     * Get global tensor statistics
     */
    struct TensorStats {
        size_t active_agents;
        double average_cognitive_load;
        double tensor_diversity;
        size_t total_tensor_updates;
    };
    TensorStats get_tensor_statistics() const;

    /**
     * Find similar agents based on tensor signatures
     */
    std::vector<std::string> find_similar_agents(const std::string& reference_agent, 
                                                double similarity_threshold = 0.8) const;

    /**
     * Synchronize tensors across agents
     */
    void synchronize_tensors();

    /**
     * Clear tensor history
     */
    void clear_history();

    /**
     * Export tensor data for analysis
     */
    std::string export_tensor_data() const;

    /**
     * Import tensor data from export
     */
    bool import_tensor_data(const std::string& data);
};

} // namespace embodiment
} // namespace opencog

#endif // _OPENCOG_EMBODIMENT_TENSOR_SIGNATURE_H