/*
 * ROSCognitiveNode.h
 *
 * ROS Cognitive Node Architecture
 * Integrates OpenCog cognitive systems with ROS-based robotic platforms
 */

#ifndef _OPENCOG_ROS_COGNITIVE_NODE_H
#define _OPENCOG_ROS_COGNITIVE_NODE_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <thread>
#include <mutex>
#include <atomic>

#include "../api/EmbodimentTensorSignature.h"
#include "../api/CognitiveMeshAPI.h"

namespace opencog {
namespace ros_integration {

/**
 * ROS Message Types for Cognitive Integration
 */
struct ROSCognitiveState {
    std::string node_name;
    std::string robot_id;
    embodiment::EmbodimentTensorSignature tensor_state;
    std::map<std::string, double> sensor_readings;
    std::map<std::string, double> actuator_states;
    std::chrono::steady_clock::time_point timestamp;
    double cognitive_load;
    std::string current_behavior;
};

struct ROSSensorData {
    std::string sensor_type; // laser, camera, imu, gps, etc.
    std::string sensor_id;
    std::string frame_id;
    
    // Generic sensor data
    std::vector<double> data_values;
    std::map<std::string, std::string> metadata;
    
    // Common sensor-specific data
    struct LaserScan {
        double angle_min, angle_max, angle_increment;
        double time_increment, scan_time;
        double range_min, range_max;
        std::vector<double> ranges;
        std::vector<double> intensities;
    } laser_scan;
    
    struct CameraImage {
        int width, height;
        std::string encoding;
        std::vector<uint8_t> data;
        double timestamp;
    } camera_image;
    
    struct IMUData {
        double orientation_x, orientation_y, orientation_z, orientation_w;
        double angular_velocity_x, angular_velocity_y, angular_velocity_z;
        double linear_acceleration_x, linear_acceleration_y, linear_acceleration_z;
    } imu_data;
    
    struct GPSData {
        double latitude, longitude, altitude;
        double position_covariance[9];
        std::string status;
    } gps_data;
    
    std::chrono::steady_clock::time_point timestamp;
};

struct ROSActuatorCommand {
    std::string actuator_type; // joint, gripper, mobile_base, etc.
    std::string actuator_id;
    
    // Generic command data
    std::map<std::string, double> command_values;
    std::map<std::string, std::string> command_parameters;
    
    // Specific actuator commands
    struct JointCommand {
        std::vector<std::string> joint_names;
        std::vector<double> positions;
        std::vector<double> velocities;
        std::vector<double> efforts;
    } joint_command;
    
    struct MobileBaseCommand {
        double linear_x, linear_y, angular_z;
        double max_linear_velocity, max_angular_velocity;
    } mobile_base_command;
    
    struct GripperCommand {
        double position;
        double effort;
        bool enable;
    } gripper_command;
    
    std::chrono::steady_clock::time_point timestamp;
    double execution_timeout;
};

/**
 * ROS Topic Configuration
 */
struct ROSTopicConfig {
    std::string topic_name;
    std::string message_type;
    int queue_size;
    bool is_subscriber;
    bool is_publisher;
    double publication_rate; // Hz for publishers
    std::function<void(const std::string&)> callback;
};

/**
 * ROS Service Configuration
 */
struct ROSServiceConfig {
    std::string service_name;
    std::string service_type;
    std::function<std::string(const std::string&)> callback;
};

/**
 * ROS Cognitive Node
 * 
 * Main ROS node class that integrates cognitive processing with ROS ecosystem
 */
class ROSCognitiveNode {
private:
    // Core components
    std::unique_ptr<cognitive_mesh::CognitiveMeshAPI> cognitive_api_;
    std::unique_ptr<embodiment::EmbodimentTensorManager> tensor_manager_;
    
    // ROS configuration
    std::string node_name_;
    std::string robot_namespace_;
    std::string robot_id_;
    bool is_initialized_;
    bool is_running_;
    
    // Topic management
    std::map<std::string, ROSTopicConfig> subscribed_topics_;
    std::map<std::string, ROSTopicConfig> published_topics_;
    std::map<std::string, ROSServiceConfig> provided_services_;
    
    // Data management
    std::map<std::string, ROSSensorData> latest_sensor_data_;
    std::map<std::string, ROSActuatorCommand> pending_actuator_commands_;
    ROSCognitiveState current_cognitive_state_;
    
    // Threading
    std::thread ros_spin_thread_;
    std::thread cognitive_processing_thread_;
    std::thread sensor_processing_thread_;
    std::thread actuator_control_thread_;
    mutable std::mutex node_mutex_;
    
    // Performance metrics
    std::atomic<uint64_t> messages_processed_;
    std::atomic<uint64_t> commands_sent_;
    std::atomic<double> average_processing_time_;

public:
    /**
     * Constructor
     */
    ROSCognitiveNode(const std::string& node_name = "opencog_cognitive_node",
                    const std::string& robot_namespace = "");
    
    /**
     * Destructor
     */
    ~ROSCognitiveNode();

    // ===== Node Lifecycle =====
    
    /**
     * Initialize ROS node
     */
    bool initialize(int argc, char** argv);
    
    /**
     * Start cognitive processing
     */
    bool start();
    
    /**
     * Stop cognitive processing
     */
    void stop();
    
    /**
     * Shutdown ROS node
     */
    void shutdown();
    
    /**
     * Check if node is running
     */
    bool is_running() const;

    // ===== Sensor Integration =====
    
    /**
     * Register sensor topic subscriber
     */
    bool subscribe_to_sensor(const std::string& topic_name, 
                           const std::string& sensor_type,
                           const std::string& message_type);
    
    /**
     * Process laser scan data
     */
    void process_laser_scan(const ROSSensorData::LaserScan& laser_data);
    
    /**
     * Process camera image data
     */
    void process_camera_image(const ROSSensorData::CameraImage& image_data);
    
    /**
     * Process IMU data
     */
    void process_imu_data(const ROSSensorData::IMUData& imu_data);
    
    /**
     * Process GPS data
     */
    void process_gps_data(const ROSSensorData::GPSData& gps_data);
    
    /**
     * Process generic sensor data
     */
    void process_sensor_data(const std::string& sensor_id, const ROSSensorData& sensor_data);

    // ===== Actuator Control =====
    
    /**
     * Register actuator command publisher
     */
    bool register_actuator_publisher(const std::string& topic_name,
                                   const std::string& actuator_type,
                                   const std::string& message_type);
    
    /**
     * Send joint position commands
     */
    bool send_joint_commands(const ROSActuatorCommand::JointCommand& joint_cmd);
    
    /**
     * Send mobile base velocity commands
     */
    bool send_mobile_base_command(const ROSActuatorCommand::MobileBaseCommand& base_cmd);
    
    /**
     * Send gripper commands
     */
    bool send_gripper_command(const ROSActuatorCommand::GripperCommand& gripper_cmd);
    
    /**
     * Send generic actuator command
     */
    bool send_actuator_command(const std::string& actuator_id, const ROSActuatorCommand& command);

    // ===== Cognitive Integration =====
    
    /**
     * Generate embodiment tensor from ROS sensor data
     */
    embodiment::EmbodimentTensorSignature generate_embodiment_tensor_from_sensors() const;
    
    /**
     * Update cognitive state from ROS data
     */
    void update_cognitive_state_from_ros();
    
    /**
     * Generate ROS commands from cognitive state
     */
    std::vector<ROSActuatorCommand> generate_ros_commands_from_cognitive_state() const;
    
    /**
     * Synchronize ROS and cognitive states
     */
    void synchronize_ros_cognitive_state();

    // ===== Behavior Integration =====
    
    /**
     * Load behavior from cognitive system
     */
    bool load_cognitive_behavior(const std::string& behavior_name);
    
    /**
     * Execute cognitive behavior step
     */
    void execute_behavior_step();
    
    /**
     * Switch to new behavior
     */
    bool switch_behavior(const std::string& new_behavior);
    
    /**
     * Get current active behavior
     */
    std::string get_current_behavior() const;

    // ===== Service Interfaces =====
    
    /**
     * Register ROS service for cognitive queries
     */
    bool register_cognitive_service(const std::string& service_name,
                                  const std::string& service_type);
    
    /**
     * Handle cognitive state query service
     */
    std::string handle_cognitive_state_query(const std::string& request);
    
    /**
     * Handle behavior control service
     */
    std::string handle_behavior_control(const std::string& request);
    
    /**
     * Handle tensor data service
     */
    std::string handle_tensor_data_request(const std::string& request);

    // ===== Real-time Processing =====
    
    /**
     * Start real-time sensor-cognition-actuator loop
     */
    void start_realtime_loop();
    
    /**
     * Stop real-time processing loop
     */
    void stop_realtime_loop();
    
    /**
     * Process one iteration of the real-time loop
     */
    void process_realtime_iteration();
    
    /**
     * Set real-time loop frequency
     */
    void set_realtime_frequency(double frequency_hz);

    // ===== Configuration and Calibration =====
    
    /**
     * Load ROS node configuration
     */
    bool load_configuration(const std::string& config_file);
    
    /**
     * Configure cognitive parameters
     */
    void configure_cognitive_parameters(const std::map<std::string, std::string>& params);
    
    /**
     * Calibrate sensor-cognitive mappings
     */
    void calibrate_sensor_mappings();
    
    /**
     * Calibrate actuator-cognitive mappings
     */
    void calibrate_actuator_mappings();

    // ===== Monitoring and Diagnostics =====
    
    /**
     * Get node performance metrics
     */
    struct NodeMetrics {
        uint64_t messages_processed;
        uint64_t commands_sent;
        double average_processing_time_ms;
        double sensor_data_rate;
        double actuator_command_rate;
        double cognitive_processing_rate;
        size_t active_topics;
        size_t active_services;
    };
    NodeMetrics get_performance_metrics() const;
    
    /**
     * Get cognitive processing statistics
     */
    struct CognitiveStats {
        double current_cognitive_load;
        double tensor_update_rate;
        std::string active_behavior;
        size_t processed_sensor_inputs;
        size_t generated_motor_commands;
    };
    CognitiveStats get_cognitive_statistics() const;
    
    /**
     * Publish diagnostic information
     */
    void publish_diagnostics();

    // ===== Topic and Message Utilities =====
    
    /**
     * Convert ROS message to cognitive format
     */
    std::string ros_message_to_cognitive_format(const std::string& ros_message,
                                               const std::string& message_type) const;
    
    /**
     * Convert cognitive format to ROS message
     */
    std::string cognitive_format_to_ros_message(const std::string& cognitive_data,
                                               const std::string& message_type) const;
    
    /**
     * Get available ROS topics
     */
    std::vector<std::string> get_available_topics() const;
    
    /**
     * Get available ROS services
     */
    std::vector<std::string> get_available_services() const;

    // ===== Utility Methods =====
    
    /**
     * Convert ROS coordinates to cognitive space
     */
    std::vector<double> ros_to_cognitive_coordinates(double x, double y, double z,
                                                   const std::string& frame_id) const;
    
    /**
     * Convert cognitive coordinates to ROS space
     */
    std::vector<double> cognitive_to_ros_coordinates(double x, double y, double z,
                                                   const std::string& target_frame) const;
    
    /**
     * Get current ROS time
     */
    std::chrono::steady_clock::time_point get_ros_time() const;
    
    /**
     * Log message to ROS logger
     */
    void log_message(const std::string& level, const std::string& message) const;

private:
    // ===== Internal Processing Methods =====
    
    /**
     * ROS spin thread function
     */
    void ros_spin_worker();
    
    /**
     * Cognitive processing thread function
     */
    void cognitive_processing_worker();
    
    /**
     * Sensor processing thread function
     */
    void sensor_processing_worker();
    
    /**
     * Actuator control thread function
     */
    void actuator_control_worker();
    
    /**
     * Process incoming ROS message
     */
    void process_ros_message(const std::string& topic, const std::string& message);
    
    /**
     * Handle ROS service call
     */
    std::string handle_ros_service_call(const std::string& service, const std::string& request);
    
    /**
     * Update performance metrics
     */
    void update_performance_metrics();
    
    /**
     * Initialize topic subscribers and publishers
     */
    void initialize_topics();
    
    /**
     * Initialize service providers
     */
    void initialize_services();
    
    /**
     * Cleanup node resources
     */
    void cleanup_node();
};

} // namespace ros_integration  
} // namespace opencog

#endif // _OPENCOG_ROS_COGNITIVE_NODE_H