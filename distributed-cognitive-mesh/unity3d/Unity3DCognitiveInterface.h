/*
 * Unity3DCognitiveInterface.h
 *
 * Unity3D Cognitive Integration Interface
 * Provides bidirectional communication between Unity3D and OpenCog cognitive systems
 */

#ifndef _OPENCOG_UNITY3D_COGNITIVE_INTERFACE_H
#define _OPENCOG_UNITY3D_COGNITIVE_INTERFACE_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <atomic>
#include <thread>
#include <mutex>

#include "../api/EmbodimentTensorSignature.h"
#include "../api/CognitiveMeshAPI.h"

namespace opencog {
namespace unity3d {

/**
 * Unity3D GameObject representation in cognitive space
 */
struct Unity3DGameObject {
    std::string object_id;
    std::string name;
    std::string tag;
    
    // Transform data
    struct Transform {
        double pos_x, pos_y, pos_z;
        double rot_x, rot_y, rot_z, rot_w;
        double scale_x, scale_y, scale_z;
    } transform;
    
    // Physics data
    struct Physics {
        bool has_rigidbody;
        double velocity_x, velocity_y, velocity_z;
        double angular_velocity_x, angular_velocity_y, angular_velocity_z;
        double mass;
    } physics;
    
    // Cognitive properties
    std::map<std::string, double> cognitive_properties;
    bool is_cognitive_agent;
    std::string agent_id;
};

/**
 * Unity3D Scene cognitive representation
 */
struct Unity3DScene {
    std::string scene_name;
    std::vector<Unity3DGameObject> objects;
    std::map<std::string, std::string> scene_properties;
    
    // Environmental factors
    struct Environment {
        double lighting_intensity;
        std::string weather_condition;
        double gravity_x, gravity_y, gravity_z;
        double time_of_day; // 0-24 hours
    } environment;
    
    // Camera/viewpoint data
    struct Camera {
        std::string camera_id;
        double pos_x, pos_y, pos_z;
        double rot_x, rot_y, rot_z, rot_w;
        double field_of_view;
        bool is_active;
    } main_camera;
};

/**
 * Sensory data from Unity3D
 */
struct Unity3DSensoryData {
    std::string agent_id;
    std::chrono::steady_clock::time_point timestamp;
    
    // Visual data
    struct Visual {
        std::vector<std::string> visible_objects;
        std::map<std::string, double> object_distances;
        std::map<std::string, std::string> object_colors;
        double light_level;
        std::string dominant_color;
    } visual;
    
    // Audio data  
    struct Audio {
        std::vector<std::string> audio_sources;
        std::map<std::string, double> audio_levels;
        double ambient_noise_level;
        std::string dominant_sound;
    } audio;
    
    // Physics/collision data
    struct Physics {
        std::vector<std::string> collision_objects;
        std::map<std::string, double> contact_forces;
        bool is_grounded;
        double ground_normal_x, ground_normal_y, ground_normal_z;
    } physics;
    
    // UI/interaction data
    struct UI {
        std::vector<std::string> ui_elements_visible;
        std::map<std::string, bool> button_states;
        std::string current_ui_context;
    } ui;
};

/**
 * Motor commands to Unity3D
 */
struct Unity3DMotorCommand {
    std::string agent_id;
    std::string command_type; // move, rotate, animate, interact
    
    // Movement commands
    struct Movement {
        double target_pos_x, target_pos_y, target_pos_z;
        double movement_speed;
        bool use_pathfinding;
        std::string movement_type; // walk, run, fly, teleport
    } movement;
    
    // Rotation commands
    struct Rotation {
        double target_rot_x, target_rot_y, target_rot_z, target_rot_w;
        double rotation_speed;
        bool smooth_rotation;
    } rotation;
    
    // Animation commands
    struct Animation {
        std::string animation_name;
        double animation_speed;
        bool loop_animation;
        std::map<std::string, double> animation_parameters;
    } animation;
    
    // Interaction commands
    struct Interaction {
        std::string target_object_id;
        std::string interaction_type; // grab, push, use, examine
        std::map<std::string, double> interaction_parameters;
    } interaction;
    
    // Force/impulse commands
    struct Force {
        double force_x, force_y, force_z;
        double impulse_x, impulse_y, impulse_z;
        std::string force_mode; // impulse, continuous, velocity_change
    } force;
};

/**
 * Unity3D Cognitive Interface
 * 
 * Main interface class for Unity3D integration with OpenCog cognitive systems
 */
class Unity3DCognitiveInterface {
private:
    // Core components
    std::unique_ptr<cognitive_mesh::CognitiveMeshAPI> cognitive_api_;
    std::unique_ptr<embodiment::EmbodimentTensorManager> tensor_manager_;
    
    // Interface configuration
    std::string unity_host_;
    int unity_port_;
    bool is_connected_;
    bool is_running_;
    
    // Data management
    std::map<std::string, Unity3DGameObject> tracked_objects_;
    std::map<std::string, Unity3DSensoryData> agent_sensory_data_;
    std::map<std::string, Unity3DMotorCommand> pending_motor_commands_;
    Unity3DScene current_scene_;
    
    // Threading
    std::thread unity_communication_thread_;
    std::thread cognitive_processing_thread_;
    mutable std::mutex interface_mutex_;
    
    // Performance metrics
    std::atomic<uint64_t> messages_sent_;
    std::atomic<uint64_t> messages_received_;
    std::atomic<double> average_latency_;

public:
    /**
     * Constructor
     */
    Unity3DCognitiveInterface(const std::string& unity_host = "localhost", 
                            int unity_port = 9090);
    
    /**
     * Destructor
     */
    ~Unity3DCognitiveInterface();

    // ===== Connection Management =====
    
    /**
     * Connect to Unity3D application
     */
    bool connect_to_unity();
    
    /**
     * Disconnect from Unity3D
     */
    void disconnect_from_unity();
    
    /**
     * Check connection status
     */
    bool is_connected() const;
    
    /**
     * Start cognitive processing
     */
    bool start_cognitive_processing();
    
    /**
     * Stop cognitive processing
     */
    void stop_cognitive_processing();

    // ===== Sensory Input Processing =====
    
    /**
     * Process visual input from Unity3D
     */
    void process_visual_input(const std::string& agent_id, const Unity3DSensoryData::Visual& visual_data);
    
    /**
     * Process audio input from Unity3D
     */
    void process_audio_input(const std::string& agent_id, const Unity3DSensoryData::Audio& audio_data);
    
    /**
     * Process physics/collision input
     */
    void process_physics_input(const std::string& agent_id, const Unity3DSensoryData::Physics& physics_data);
    
    /**
     * Process UI interaction input
     */
    void process_ui_input(const std::string& agent_id, const Unity3DSensoryData::UI& ui_data);
    
    /**
     * Update complete sensory data for agent
     */
    void update_agent_sensory_data(const std::string& agent_id, const Unity3DSensoryData& sensory_data);

    // ===== Motor Output Generation =====
    
    /**
     * Send movement command to Unity3D
     */
    bool send_movement_command(const std::string& agent_id, const Unity3DMotorCommand::Movement& movement);
    
    /**
     * Send rotation command to Unity3D
     */
    bool send_rotation_command(const std::string& agent_id, const Unity3DMotorCommand::Rotation& rotation);
    
    /**
     * Send animation command to Unity3D
     */
    bool send_animation_command(const std::string& agent_id, const Unity3DMotorCommand::Animation& animation);
    
    /**
     * Send interaction command to Unity3D
     */
    bool send_interaction_command(const std::string& agent_id, const Unity3DMotorCommand::Interaction& interaction);
    
    /**
     * Send complete motor command to Unity3D
     */
    bool send_motor_command(const std::string& agent_id, const Unity3DMotorCommand& command);

    // ===== Scene Management =====
    
    /**
     * Update scene information from Unity3D
     */
    void update_scene_data(const Unity3DScene& scene);
    
    /**
     * Get current scene representation
     */
    Unity3DScene get_current_scene() const;
    
    /**
     * Track new GameObject in cognitive space
     */
    void track_game_object(const Unity3DGameObject& object);
    
    /**
     * Update tracked GameObject
     */
    void update_game_object(const Unity3DGameObject& object);
    
    /**
     * Remove GameObject from tracking
     */
    void untrack_game_object(const std::string& object_id);
    
    /**
     * Get all tracked objects
     */
    std::vector<Unity3DGameObject> get_tracked_objects() const;

    // ===== Cognitive Integration =====
    
    /**
     * Generate embodiment tensor from Unity3D data
     */
    embodiment::EmbodimentTensorSignature generate_embodiment_tensor(const std::string& agent_id) const;
    
    /**
     * Update cognitive state based on Unity3D input
     */
    void update_cognitive_state_from_unity(const std::string& agent_id);
    
    /**
     * Generate Unity3D commands from cognitive state
     */
    Unity3DMotorCommand generate_motor_commands_from_cognitive_state(const std::string& agent_id) const;
    
    /**
     * Synchronize cognitive and Unity3D agent states
     */
    void synchronize_agent_state(const std::string& agent_id);

    // ===== Real-time Feedback Loops =====
    
    /**
     * Start real-time sensory-motor feedback loop
     */
    void start_feedback_loop(const std::string& agent_id);
    
    /**
     * Stop feedback loop for agent
     */
    void stop_feedback_loop(const std::string& agent_id);
    
    /**
     * Process feedback loop iteration
     */
    void process_feedback_iteration(const std::string& agent_id);

    // ===== Configuration and Calibration =====
    
    /**
     * Configure Unity3D interface parameters
     */
    void configure_interface(const std::map<std::string, std::string>& config);
    
    /**
     * Calibrate sensory input mappings
     */
    void calibrate_sensory_mappings();
    
    /**
     * Calibrate motor output mappings  
     */
    void calibrate_motor_mappings();

    // ===== Performance and Monitoring =====
    
    /**
     * Get interface performance metrics
     */
    struct InterfaceMetrics {
        uint64_t messages_sent;
        uint64_t messages_received;
        double average_latency_ms;
        double sensory_processing_rate;
        double motor_command_rate;
        size_t active_agents;
    };
    InterfaceMetrics get_performance_metrics() const;
    
    /**
     * Get agent-specific metrics
     */
    struct AgentMetrics {
        double sensory_data_rate;
        double motor_command_rate;
        double cognitive_processing_time;
        double feedback_loop_frequency;
    };
    AgentMetrics get_agent_metrics(const std::string& agent_id) const;

    // ===== Utility Methods =====
    
    /**
     * Convert Unity3D coordinates to cognitive space
     */
    std::vector<double> unity_to_cognitive_coordinates(double x, double y, double z) const;
    
    /**
     * Convert cognitive coordinates to Unity3D space
     */
    std::vector<double> cognitive_to_unity_coordinates(double x, double y, double z) const;
    
    /**
     * Serialize Unity3D data to JSON
     */
    std::string serialize_unity_data(const Unity3DSensoryData& data) const;
    
    /**
     * Deserialize Unity3D data from JSON
     */
    Unity3DSensoryData deserialize_unity_data(const std::string& json) const;

private:
    // ===== Internal Methods =====
    
    /**
     * Unity3D communication thread function
     */
    void unity_communication_worker();
    
    /**
     * Cognitive processing thread function
     */
    void cognitive_processing_worker();
    
    /**
     * Process incoming Unity3D messages
     */
    void process_unity_message(const std::string& message);
    
    /**
     * Send message to Unity3D
     */
    bool send_unity_message(const std::string& message);
    
    /**
     * Update performance metrics
     */
    void update_performance_metrics();
    
    /**
     * Initialize interface components
     */
    void initialize_interface();
    
    /**
     * Cleanup interface resources
     */
    void cleanup_interface();
};

} // namespace unity3d
} // namespace opencog

#endif // _OPENCOG_UNITY3D_COGNITIVE_INTERFACE_H