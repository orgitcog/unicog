/*
 * CognitiveMeshAPI.cc
 *
 * Basic implementation of Cognitive Mesh API
 */

#include "CognitiveMeshAPI.h"
#include <sstream>
#include <iomanip>
#include <random>

namespace opencog {
namespace cognitive_mesh {

CognitiveMeshAPI::CognitiveMeshAPI(int http_port, int websocket_port)
    : api_version_("1.0"), http_port_(http_port), websocket_port_(websocket_port),
      is_running_(false), total_requests_(0), successful_requests_(0), 
      average_response_time_(0.0) {
    
    // Initialize tensor protocol
    tensor_protocol_ = std::make_unique<TensorHypergraphProtocol>();
    tensor_manager_ = std::make_unique<embodiment::EmbodimentTensorManager>();
}

CognitiveMeshAPI::~CognitiveMeshAPI() {
    if (is_running_) {
        stop();
    }
}

bool CognitiveMeshAPI::start() {
    std::unique_lock<std::shared_mutex> lock(api_mutex_);
    
    if (is_running_) {
        return false; // Already running
    }
    
    // Initialize components
    initialize_endpoints();
    
    // Start HTTP server (simulated for now)
    is_running_ = true;
    
    // Start WebSocket server
    start_websocket_server();
    
    return true;
}

void CognitiveMeshAPI::stop() {
    std::unique_lock<std::shared_mutex> lock(api_mutex_);
    
    if (!is_running_) {
        return;
    }
    
    is_running_ = false;
    
    // Stop worker threads
    for (auto& thread : worker_threads_) {
        if (thread.joinable()) {
            thread.join();
        }
    }
    worker_threads_.clear();
    
    cleanup_resources();
}

bool CognitiveMeshAPI::is_running() const {
    std::shared_lock<std::shared_mutex> lock(api_mutex_);
    return is_running_;
}

APIResponse CognitiveMeshAPI::get_agent_state(const std::string& agent_id) {
    std::string request_id = generate_request_id();
    
    std::shared_lock<std::shared_mutex> lock(api_mutex_);
    
    auto it = agent_states_.find(agent_id);
    if (it == agent_states_.end()) {
        return create_error_response(request_id, 404, "Agent not found");
    }
    
    // Serialize agent state to JSON (simplified)
    std::ostringstream json;
    json << "{";
    json << "\"agent_id\":\"" << agent_id << "\",";
    json << "\"cognitive_load\":" << it->second.cognitive_load << ",";
    json << "\"active_atoms\":" << it->second.active_atoms.size() << ",";
    json << "\"tensor_signature\":{";
    json << "\"magnitude\":" << it->second.tensor_signature.magnitude();
    json << "}";
    json << "}";
    
    return create_success_response(request_id, json.str());
}

APIResponse CognitiveMeshAPI::update_agent_state(const std::string& agent_id, 
                                               const CognitiveStateSnapshot& state) {
    std::string request_id = generate_request_id();
    
    std::unique_lock<std::shared_mutex> lock(api_mutex_);
    
    // Update agent state
    agent_states_[agent_id] = state;
    
    // Update tensor manager
    tensor_manager_->update_agent_tensor(agent_id, state.tensor_signature);
    
    // Trigger callbacks
    for (const auto& callback : state_callbacks_) {
        callback(state);
    }
    
    return create_success_response(request_id, "{\"status\":\"updated\"}");
}

APIResponse CognitiveMeshAPI::get_embodiment_tensor(const std::string& agent_id) {
    std::string request_id = generate_request_id();
    
    try {
        auto tensor = tensor_manager_->get_agent_tensor(agent_id);
        
        std::ostringstream json;
        json << "{";
        json << "\"agent_id\":\"" << agent_id << "\",";
        json << "\"tensor_magnitude\":" << tensor.magnitude() << ",";
        json << "\"cognitive_load\":" << tensor.calculate_cognitive_load() << ",";
        json << "\"summary\":\"" << tensor.get_summary() << "\"";
        json << "}";
        
        return create_success_response(request_id, json.str());
    } catch (const std::exception& e) {
        return create_error_response(request_id, 500, "Internal server error");
    }
}

APIResponse CognitiveMeshAPI::get_api_version() {
    std::string request_id = generate_request_id();
    
    std::ostringstream json;
    json << "{";
    json << "\"api_version\":\"" << api_version_ << "\",";
    json << "\"server_status\":\"" << (is_running_ ? "running" : "stopped") << "\",";
    json << "\"total_requests\":" << total_requests_.load() << ",";
    json << "\"successful_requests\":" << successful_requests_.load();
    json << "}";
    
    return create_success_response(request_id, json.str());
}

bool CognitiveMeshAPI::start_websocket_server() {
    // Simplified WebSocket server simulation
    // In real implementation, this would start actual WebSocket server
    return true;
}

void CognitiveMeshAPI::broadcast_state_update(const CognitiveStateSnapshot& state) {
    // Simulate broadcasting to WebSocket clients
    // In real implementation, this would send actual WebSocket messages
}

std::string CognitiveMeshAPI::generate_request_id() const {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dis(1000, 9999);
    
    auto now = std::chrono::steady_clock::now();
    auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count();
    
    std::ostringstream oss;
    oss << "req_" << timestamp << "_" << dis(gen);
    return oss.str();
}

APIResponse CognitiveMeshAPI::create_success_response(const std::string& request_id, 
                                                    const std::string& body) const {
    APIResponse response;
    response.request_id = request_id;
    response.status_code = 200;
    response.status_message = "OK";
    response.body = body;
    response.success = true;
    response.timestamp = std::chrono::steady_clock::now();
    response.processing_time_ms = 1.0; // Simplified
    
    return response;
}

APIResponse CognitiveMeshAPI::create_error_response(const std::string& request_id, 
                                                  int status_code, 
                                                  const std::string& message) const {
    APIResponse response;
    response.request_id = request_id;
    response.status_code = status_code;
    response.status_message = message;
    response.body = "{\"error\":\"" + message + "\"}";
    response.success = false;
    response.timestamp = std::chrono::steady_clock::now();
    response.processing_time_ms = 0.5; // Simplified
    
    return response;
}

void CognitiveMeshAPI::initialize_endpoints() {
    // Initialize API endpoints (simplified implementation)
    // In real implementation, this would set up HTTP/WebSocket routes
}

void CognitiveMeshAPI::cleanup_resources() {
    // Cleanup API resources
    agent_states_.clear();
    active_tasks_.clear();
    task_results_.clear();
}

} // namespace cognitive_mesh
} // namespace opencog