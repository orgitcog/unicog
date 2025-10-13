/*
 * CognitiveMeshAPI.h
 *
 * Phase 4: Distributed Cognitive Mesh API
 * Main API interface for distributed cognitive mesh operations
 */

#ifndef _OPENCOG_COGNITIVE_MESH_API_H
#define _OPENCOG_COGNITIVE_MESH_API_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <future>
#include <chrono>

#include "EmbodimentTensorSignature.h"
// Forward declaration for TensorHypergraphProtocol
namespace opencog {
    class TensorHypergraphProtocol;
}

namespace opencog {
namespace cognitive_mesh {

/**
 * API Request/Response Structures
 */
struct APIRequest {
    std::string request_id;
    std::string endpoint;
    std::string method; // GET, POST, PUT, DELETE, WEBSOCKET
    std::map<std::string, std::string> headers;
    std::map<std::string, std::string> parameters;
    std::string body;
    std::chrono::steady_clock::time_point timestamp;
    std::string client_id;
    std::string api_version;
};

struct APIResponse {
    std::string request_id;
    int status_code;
    std::string status_message;
    std::map<std::string, std::string> headers;
    std::string body;
    std::chrono::steady_clock::time_point timestamp;
    double processing_time_ms;
    bool success;
};

/**
 * Cognitive State Information
 */
struct CognitiveStateSnapshot {
    std::string agent_id;
    embodiment::EmbodimentTensorSignature tensor_signature;
    std::map<std::string, double> attention_values;
    std::vector<std::string> active_atoms;
    std::map<std::string, std::vector<double>> neural_activations;
    double cognitive_load;
    std::string state_description;
    std::chrono::steady_clock::time_point timestamp;
};

/**
 * Task Orchestration Structures
 */
struct CognitiveTask {
    std::string task_id;
    std::string task_type;
    std::string description;
    std::map<std::string, std::string> parameters;
    std::vector<std::string> required_agents;
    std::vector<std::string> dependencies;
    double priority;
    std::chrono::steady_clock::time_point created_at;
    std::chrono::steady_clock::time_point deadline;
    std::string status; // pending, running, completed, failed
};

struct TaskResult {
    std::string task_id;
    std::string result_data;
    bool success;
    std::string error_message;
    std::map<std::string, std::string> metrics;
    std::chrono::steady_clock::time_point completed_at;
    double execution_time_ms;
};

/**
 * API Callback Types
 */
using StateUpdateCallback = std::function<void(const CognitiveStateSnapshot&)>;
using TaskCompletionCallback = std::function<void(const TaskResult&)>;
using APIEventCallback = std::function<void(const std::string& event, const std::string& data)>;

/**
 * Main Cognitive Mesh API Class
 * 
 * Provides REST and WebSocket interfaces for distributed cognitive operations
 */
class CognitiveMeshAPI {
private:
    // Core components
    std::unique_ptr<TensorHypergraphProtocol> tensor_protocol_;
    std::unique_ptr<embodiment::EmbodimentTensorManager> tensor_manager_;
    
    // API configuration
    std::string api_version_;
    int http_port_;
    int websocket_port_;
    bool is_running_;
    
    // State management
    std::map<std::string, CognitiveStateSnapshot> agent_states_;
    std::map<std::string, CognitiveTask> active_tasks_;
    std::map<std::string, TaskResult> task_results_;
    
    // Callbacks
    std::vector<StateUpdateCallback> state_callbacks_;
    std::vector<TaskCompletionCallback> task_callbacks_;
    std::vector<APIEventCallback> event_callbacks_;
    
    // Threading and synchronization
    mutable std::shared_mutex api_mutex_;
    std::vector<std::thread> worker_threads_;
    
    // Performance metrics
    std::atomic<uint64_t> total_requests_;
    std::atomic<uint64_t> successful_requests_;
    std::atomic<double> average_response_time_;

public:
    /**
     * Constructor
     */
    CognitiveMeshAPI(int http_port = 8080, int websocket_port = 8081);
    
    /**
     * Destructor
     */
    ~CognitiveMeshAPI();

    // ===== API Lifecycle =====
    
    /**
     * Start the API server
     */
    bool start();
    
    /**
     * Stop the API server
     */
    void stop();
    
    /**
     * Check if API is running
     */
    bool is_running() const;

    // ===== State Propagation API =====
    
    /**
     * Get current cognitive state for agent
     * GET /api/v1/agents/{agent_id}/state
     */
    APIResponse get_agent_state(const std::string& agent_id);
    
    /**
     * Update cognitive state for agent
     * PUT /api/v1/agents/{agent_id}/state
     */
    APIResponse update_agent_state(const std::string& agent_id, const CognitiveStateSnapshot& state);
    
    /**
     * Get all active agents
     * GET /api/v1/agents
     */
    APIResponse get_active_agents();
    
    /**
     * Synchronize states across distributed agents
     * POST /api/v1/sync/states
     */
    APIResponse synchronize_agent_states();

    // ===== Task Orchestration API =====
    
    /**
     * Submit new cognitive task
     * POST /api/v1/tasks
     */
    APIResponse submit_task(const CognitiveTask& task);
    
    /**
     * Get task status
     * GET /api/v1/tasks/{task_id}
     */
    APIResponse get_task_status(const std::string& task_id);
    
    /**
     * Get task results
     * GET /api/v1/tasks/{task_id}/results
     */
    APIResponse get_task_results(const std::string& task_id);
    
    /**
     * Cancel running task
     * DELETE /api/v1/tasks/{task_id}
     */
    APIResponse cancel_task(const std::string& task_id);
    
    /**
     * Get all active tasks
     * GET /api/v1/tasks
     */
    APIResponse get_active_tasks();

    // ===== Embodiment Tensor API =====
    
    /**
     * Get embodiment tensor for agent
     * GET /api/v1/agents/{agent_id}/embodiment
     */
    APIResponse get_embodiment_tensor(const std::string& agent_id);
    
    /**
     * Update embodiment tensor
     * PUT /api/v1/agents/{agent_id}/embodiment
     */
    APIResponse update_embodiment_tensor(const std::string& agent_id, 
                                       const embodiment::EmbodimentTensorSignature& tensor);
    
    /**
     * Get embodiment statistics
     * GET /api/v1/embodiment/stats
     */
    APIResponse get_embodiment_statistics();

    // ===== Real-time WebSocket Streams =====
    
    /**
     * Start WebSocket server for real-time updates
     */
    bool start_websocket_server();
    
    /**
     * Broadcast state update to WebSocket clients
     */
    void broadcast_state_update(const CognitiveStateSnapshot& state);
    
    /**
     * Broadcast task completion to WebSocket clients
     */
    void broadcast_task_completion(const TaskResult& result);
    
    /**
     * Send custom event to WebSocket clients
     */
    void broadcast_event(const std::string& event_type, const std::string& data);

    // ===== API Versioning & Compatibility =====
    
    /**
     * Get API version information
     * GET /api/version
     */
    APIResponse get_api_version();
    
    /**
     * Check API compatibility
     * GET /api/v1/compatibility
     */
    APIResponse check_compatibility(const std::string& client_version);
    
    /**
     * Get API schema/documentation
     * GET /api/v1/schema
     */
    APIResponse get_api_schema();

    // ===== Performance & Monitoring =====
    
    /**
     * Get API performance metrics
     * GET /api/v1/metrics
     */
    APIResponse get_performance_metrics();
    
    /**
     * Get system health status
     * GET /api/v1/health
     */
    APIResponse get_health_status();

    // ===== Callback Management =====
    
    /**
     * Register callback for state updates
     */
    void register_state_callback(const StateUpdateCallback& callback);
    
    /**
     * Register callback for task completions
     */
    void register_task_callback(const TaskCompletionCallback& callback);
    
    /**
     * Register callback for API events
     */
    void register_event_callback(const APIEventCallback& callback);

    // ===== Configuration =====
    
    /**
     * Set API configuration parameters
     */
    void configure_api(const std::map<std::string, std::string>& config);
    
    /**
     * Get current configuration
     */
    std::map<std::string, std::string> get_configuration() const;

    // ===== Utility Methods =====
    
    /**
     * Validate API request format
     */
    bool validate_request(const APIRequest& request) const;
    
    /**
     * Create standardized error response
     */
    APIResponse create_error_response(const std::string& request_id, 
                                    int status_code, 
                                    const std::string& message) const;
    
    /**
     * Create successful response
     */
    APIResponse create_success_response(const std::string& request_id, 
                                      const std::string& body) const;
    
    /**
     * Generate unique request ID
     */
    std::string generate_request_id() const;

private:
    // ===== Internal Methods =====
    
    /**
     * Handle HTTP requests
     */
    void handle_http_request(const APIRequest& request, APIResponse& response);
    
    /**
     * Handle WebSocket connections
     */
    void handle_websocket_connection(const std::string& connection_id);
    
    /**
     * Process cognitive task asynchronously
     */
    std::future<TaskResult> process_task_async(const CognitiveTask& task);
    
    /**
     * Update performance metrics
     */
    void update_performance_metrics(double response_time_ms, bool success);
    
    /**
     * Initialize API endpoints
     */
    void initialize_endpoints();
    
    /**
     * Cleanup resources
     */
    void cleanup_resources();
};

} // namespace cognitive_mesh
} // namespace opencog

#endif // _OPENCOG_COGNITIVE_MESH_API_H