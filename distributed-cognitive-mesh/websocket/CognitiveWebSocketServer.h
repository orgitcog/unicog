/*
 * CognitiveWebSocketServer.h
 *
 * WebSocket Real-time Cognitive Streams
 * Provides WebSocket-based real-time communication for cognitive state streaming
 */

#ifndef _OPENCOG_COGNITIVE_WEBSOCKET_SERVER_H
#define _OPENCOG_COGNITIVE_WEBSOCKET_SERVER_H

#include <string>
#include <vector>
#include <map>
#include <set>
#include <memory>
#include <functional>
#include <thread>
#include <mutex>
#include <atomic>
#include <queue>

#include "../api/EmbodimentTensorSignature.h"
#include "../api/CognitiveMeshAPI.h"

namespace opencog {
namespace websocket {

/**
 * WebSocket Message Types
 */
enum class MessageType {
    COGNITIVE_STATE_UPDATE,
    EMBODIMENT_TENSOR_UPDATE,
    TASK_STATUS_UPDATE,
    ATTENTION_UPDATE,
    SENSOR_DATA_STREAM,
    MOTOR_COMMAND_STREAM,
    REAL_TIME_METRICS,
    SYSTEM_EVENT,
    CLIENT_COMMAND,
    SERVER_RESPONSE
};

/**
 * WebSocket Message Structure
 */
struct WebSocketMessage {
    std::string message_id;
    MessageType type;
    std::string source_id;
    std::vector<std::string> target_clients;
    std::string payload;
    std::map<std::string, std::string> metadata;
    std::chrono::steady_clock::time_point timestamp;
    bool is_broadcast;
    int priority; // 0 = low, 10 = critical
};

/**
 * Client Connection Information
 */
struct ClientConnection {
    std::string client_id;
    std::string connection_id;
    std::string user_agent;
    std::string remote_address;
    std::chrono::steady_clock::time_point connected_at;
    
    // Subscription preferences
    std::set<MessageType> subscribed_message_types;
    std::set<std::string> subscribed_agents;
    std::map<std::string, std::string> filters;
    
    // Connection state
    bool is_authenticated;
    bool is_active;
    std::string authentication_token;
    
    // Performance metrics
    uint64_t messages_sent;
    uint64_t messages_received;
    double average_latency_ms;
    
    // Real-time stream configuration
    double stream_frequency_hz;
    bool enable_compression;
    std::string preferred_format; // json, binary, msgpack
};

/**
 * Real-time Stream Configuration
 */
struct StreamConfig {
    std::string stream_id;
    MessageType stream_type;
    std::string source_agent_id;
    double frequency_hz;
    bool enable_buffering;
    size_t buffer_size;
    bool enable_compression;
    std::vector<std::string> subscribed_clients;
    std::function<std::string()> data_generator;
};

/**
 * Cognitive WebSocket Server
 * 
 * High-performance WebSocket server for real-time cognitive data streaming
 */
class CognitiveWebSocketServer {
private:
    // Server configuration
    std::string server_address_;
    int server_port_;
    bool is_running_;
    bool enable_ssl_;
    std::string ssl_cert_file_;
    std::string ssl_key_file_;
    
    // Core components
    std::unique_ptr<cognitive_mesh::CognitiveMeshAPI> cognitive_api_;
    std::unique_ptr<embodiment::EmbodimentTensorManager> tensor_manager_;
    
    // Client management
    std::map<std::string, ClientConnection> active_clients_;
    std::map<std::string, std::string> connection_id_to_client_id_;
    std::set<std::string> authenticated_clients_;
    
    // Message handling
    std::queue<WebSocketMessage> message_queue_;
    std::map<MessageType, std::vector<std::string>> message_type_subscribers_;
    std::map<std::string, std::vector<std::string>> agent_subscribers_;
    
    // Real-time streams
    std::map<std::string, StreamConfig> active_streams_;
    std::map<std::string, std::thread> stream_threads_;
    
    // Threading and synchronization
    std::thread server_thread_;
    std::thread message_processing_thread_;
    std::vector<std::thread> stream_processing_threads_;
    mutable std::shared_mutex server_mutex_;
    std::mutex message_queue_mutex_;
    std::condition_variable message_condition_;
    
    // Performance metrics
    std::atomic<uint64_t> total_connections_;
    std::atomic<uint64_t> messages_processed_;
    std::atomic<uint64_t> bytes_transferred_;
    std::atomic<double> average_message_latency_;

public:
    /**
     * Constructor
     */
    CognitiveWebSocketServer(const std::string& address = "0.0.0.0", 
                           int port = 8081);
    
    /**
     * Destructor
     */
    ~CognitiveWebSocketServer();

    // ===== Server Lifecycle =====
    
    /**
     * Start WebSocket server
     */
    bool start();
    
    /**
     * Stop WebSocket server
     */
    void stop();
    
    /**
     * Check if server is running
     */
    bool is_running() const;
    
    /**
     * Configure SSL/TLS encryption
     */
    void configure_ssl(const std::string& cert_file, const std::string& key_file);

    // ===== Client Management =====
    
    /**
     * Handle new client connection
     */
    void on_client_connected(const std::string& connection_id, 
                           const std::map<std::string, std::string>& headers);
    
    /**
     * Handle client disconnection
     */
    void on_client_disconnected(const std::string& connection_id);
    
    /**
     * Handle client message
     */
    void on_client_message(const std::string& connection_id, const std::string& message);
    
    /**
     * Authenticate client
     */
    bool authenticate_client(const std::string& client_id, const std::string& token);
    
    /**
     * Get connected clients
     */
    std::vector<ClientConnection> get_connected_clients() const;
    
    /**
     * Disconnect specific client
     */
    void disconnect_client(const std::string& client_id);

    // ===== Message Broadcasting =====
    
    /**
     * Broadcast cognitive state update
     */
    void broadcast_cognitive_state_update(const std::string& agent_id, 
                                        const cognitive_mesh::CognitiveStateSnapshot& state);
    
    /**
     * Broadcast embodiment tensor update
     */
    void broadcast_embodiment_tensor_update(const std::string& agent_id,
                                          const embodiment::EmbodimentTensorSignature& tensor);
    
    /**
     * Broadcast task status update
     */
    void broadcast_task_status_update(const std::string& task_id, 
                                    const std::string& status, 
                                    const std::map<std::string, std::string>& metadata);
    
    /**
     * Broadcast attention update
     */
    void broadcast_attention_update(const std::string& agent_id, 
                                  const std::map<std::string, double>& attention_values);
    
    /**
     * Broadcast system event
     */
    void broadcast_system_event(const std::string& event_type, 
                               const std::string& event_data);
    
    /**
     * Send message to specific clients
     */
    void send_message_to_clients(const std::vector<std::string>& client_ids,
                               const WebSocketMessage& message);
    
    /**
     * Send message to all authenticated clients
     */
    void broadcast_message(const WebSocketMessage& message);

    // ===== Real-time Streams =====
    
    /**
     * Create real-time cognitive state stream
     */
    std::string create_cognitive_state_stream(const std::string& agent_id, 
                                            double frequency_hz = 10.0);
    
    /**
     * Create real-time embodiment tensor stream
     */
    std::string create_embodiment_tensor_stream(const std::string& agent_id,
                                              double frequency_hz = 20.0);
    
    /**
     * Create real-time sensor data stream
     */
    std::string create_sensor_data_stream(const std::string& agent_id,
                                        const std::string& sensor_type,
                                        double frequency_hz = 30.0);
    
    /**
     * Create real-time motor command stream
     */
    std::string create_motor_command_stream(const std::string& agent_id,
                                          double frequency_hz = 20.0);
    
    /**
     * Create custom data stream
     */
    std::string create_custom_stream(const StreamConfig& config);
    
    /**
     * Stop real-time stream
     */
    void stop_stream(const std::string& stream_id);
    
    /**
     * Get active streams
     */
    std::vector<StreamConfig> get_active_streams() const;

    // ===== Subscription Management =====
    
    /**
     * Subscribe client to message type
     */
    void subscribe_client_to_message_type(const std::string& client_id, MessageType type);
    
    /**
     * Unsubscribe client from message type
     */
    void unsubscribe_client_from_message_type(const std::string& client_id, MessageType type);
    
    /**
     * Subscribe client to agent updates
     */
    void subscribe_client_to_agent(const std::string& client_id, const std::string& agent_id);
    
    /**
     * Unsubscribe client from agent updates
     */
    void unsubscribe_client_from_agent(const std::string& client_id, const std::string& agent_id);
    
    /**
     * Set client filter
     */
    void set_client_filter(const std::string& client_id, 
                         const std::string& filter_name, 
                         const std::string& filter_value);
    
    /**
     * Remove client filter
     */
    void remove_client_filter(const std::string& client_id, const std::string& filter_name);

    // ===== Performance and Monitoring =====
    
    /**
     * Get server performance metrics
     */
    struct ServerMetrics {
        uint64_t total_connections;
        size_t active_connections;
        uint64_t messages_processed;
        uint64_t bytes_transferred;
        double average_message_latency_ms;
        double messages_per_second;
        size_t active_streams;
        double cpu_usage;
        double memory_usage_mb;
    };
    ServerMetrics get_server_metrics() const;
    
    /**
     * Get client-specific metrics
     */
    struct ClientMetrics {
        uint64_t messages_sent;
        uint64_t messages_received;
        double average_latency_ms;
        double connection_uptime_hours;
        size_t subscriptions;
        double data_rate_kbps;
    };
    ClientMetrics get_client_metrics(const std::string& client_id) const;
    
    /**
     * Get stream performance metrics
     */
    struct StreamMetrics {
        double actual_frequency_hz;
        uint64_t messages_sent;
        double average_message_size_bytes;
        double compression_ratio;
        size_t active_subscribers;
    };
    StreamMetrics get_stream_metrics(const std::string& stream_id) const;

    // ===== Configuration and Administration =====
    
    /**
     * Configure server parameters
     */
    void configure_server(const std::map<std::string, std::string>& config);
    
    /**
     * Set maximum concurrent connections
     */
    void set_max_connections(size_t max_connections);
    
    /**
     * Set message rate limits
     */
    void set_rate_limits(size_t messages_per_second, size_t bytes_per_second);
    
    /**
     * Enable/disable message compression
     */
    void configure_compression(bool enable, const std::string& algorithm = "gzip");
    
    /**
     * Configure authentication
     */
    void configure_authentication(bool require_auth, 
                                const std::string& auth_method = "token");

    // ===== Utility Methods =====
    
    /**
     * Convert message type to string
     */
    std::string message_type_to_string(MessageType type) const;
    
    /**
     * Convert string to message type
     */
    MessageType string_to_message_type(const std::string& type_str) const;
    
    /**
     * Serialize WebSocket message to JSON
     */
    std::string serialize_message(const WebSocketMessage& message) const;
    
    /**
     * Deserialize JSON to WebSocket message
     */
    WebSocketMessage deserialize_message(const std::string& json) const;
    
    /**
     * Generate unique message ID
     */
    std::string generate_message_id() const;
    
    /**
     * Validate message format
     */
    bool validate_message(const WebSocketMessage& message) const;

private:
    // ===== Internal Processing Methods =====
    
    /**
     * Server thread function
     */
    void server_worker();
    
    /**
     * Message processing thread function
     */
    void message_processing_worker();
    
    /**
     * Real-time stream thread function
     */
    void stream_worker(const std::string& stream_id);
    
    /**
     * Process incoming message from client
     */
    void process_client_message(const std::string& client_id, const WebSocketMessage& message);
    
    /**
     * Route message to appropriate handlers
     */
    void route_message(const WebSocketMessage& message);
    
    /**
     * Check if client is subscribed to message
     */
    bool is_client_subscribed(const std::string& client_id, const WebSocketMessage& message) const;
    
    /**
     * Apply client filters to message
     */
    bool apply_client_filters(const std::string& client_id, const WebSocketMessage& message) const;
    
    /**
     * Update performance metrics
     */
    void update_performance_metrics();
    
    /**
     * Initialize server components
     */
    void initialize_server();
    
    /**
     * Cleanup server resources
     */
    void cleanup_server();
    
    /**
     * Handle client authentication request
     */
    void handle_authentication_request(const std::string& client_id, const std::string& token);
    
    /**
     * Handle subscription request
     */
    void handle_subscription_request(const std::string& client_id, 
                                   const std::map<std::string, std::string>& subscription_data);
};

} // namespace websocket
} // namespace opencog

#endif // _OPENCOG_COGNITIVE_WEBSOCKET_SERVER_H