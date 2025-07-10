/*
 * TensorHypergraphProtocol.cc
 *
 * Implementation of tensor-based hypergraph message protocol
 */

#include "../include/TensorHypergraphProtocol.h"
#include <algorithm>
#include <numeric>
#include <random>
#include <sstream>
#include <iomanip>
#include <iostream>

namespace opencog {

// MessageBuffer implementation
void MessageBuffer::add_message(const TensorMessage& message) {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    if (buffer_.size() < max_buffer_size_) {
        buffer_.push_back(message);
    }
}

std::vector<TensorMessage> MessageBuffer::flush_buffer() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    std::vector<TensorMessage> result = buffer_;
    buffer_.clear();
    return result;
}

size_t MessageBuffer::size() const {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return buffer_.size();
}

bool MessageBuffer::is_full() const {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return buffer_.size() >= max_buffer_size_;
}

// TensorHypergraphProtocol implementation
TensorHypergraphProtocol::TensorHypergraphProtocol(size_t max_dimension,
                                                 double compression_threshold,
                                                 bool enable_batching,
                                                 size_t batch_size)
    : max_tensor_dimension_(max_dimension)
    , compression_threshold_(compression_threshold)
    , enable_batching_(enable_batching)
    , batch_size_(batch_size)
    , messages_sent_(0)
    , messages_received_(0)
    , bytes_transmitted_(0)
    , average_latency_(0.0)
{
    std::cout << "TensorHypergraphProtocol initialized with max_dimension=" 
              << max_dimension << ", batching=" << (enable_batching ? "enabled" : "disabled")
              << std::endl;
}

TensorHypergraphProtocol::~TensorHypergraphProtocol() = default;

void TensorHypergraphProtocol::register_agent(const std::string& agent_id, double bandwidth_limit) {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    agent_message_buffers_[agent_id] = MessageBuffer(batch_size_);
    pending_messages_[agent_id] = std::vector<TensorMessage>();
    agent_bandwidth_[agent_id] = bandwidth_limit;
    agent_topology_[agent_id] = std::vector<std::string>();
    
    std::cout << "Registered agent " << agent_id 
              << " with bandwidth limit " << bandwidth_limit << std::endl;
}

TensorMessage TensorHypergraphProtocol::create_tensor_message(
    const std::string& sender_id,
    const std::vector<std::string>& recipients,
    const std::vector<std::vector<double>>& hypergraph_nodes,
    const std::vector<std::vector<size_t>>& hypergraph_edges,
    const std::vector<double>& edge_weights,
    const std::string& message_type,
    double priority) {
    
    TensorMessage message;
    message.message_id = generate_message_id();
    message.sender_id = sender_id;
    message.recipient_ids = recipients;
    message.message_type = message_type;
    message.priority = priority;
    message.timestamp = std::chrono::steady_clock::now();
    message.is_compressed = false;
    message.compression_ratio = 1.0;
    
    // Convert hypergraph to tensor representation
    message.tensor_data = hypergraph_to_tensor(hypergraph_nodes, hypergraph_edges, edge_weights);
    
    // Set tensor shape based on hypergraph structure
    if (!hypergraph_nodes.empty()) {
        message.tensor_shape.push_back(hypergraph_nodes.size());
        if (!hypergraph_nodes[0].empty()) {
            message.tensor_shape.push_back(hypergraph_nodes[0].size());
        }
    }
    
    // Store hypergraph structure metadata
    for (size_t i = 0; i < hypergraph_nodes.size(); ++i) {
        message.node_ids.push_back("node_" + std::to_string(i));
    }
    message.edge_connections = hypergraph_edges;
    message.edge_weights = edge_weights;
    
    // Apply compression if tensor is large enough
    if (message.tensor_data.size() > max_tensor_dimension_ * compression_threshold_) {
        message.tensor_data = compress_tensor_data(message.tensor_data, compression_threshold_);
        message.is_compressed = true;
        message.compression_ratio = compression_threshold_;
    }
    
    return message;
}

bool TensorHypergraphProtocol::send_message(const TensorMessage& message) {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    auto start_time = std::chrono::steady_clock::now();
    
    // Validate message
    if (!validate_tensor_message(message)) {
        std::cerr << "Invalid tensor message from " << message.sender_id << std::endl;
        return false;
    }
    
    // Route message to recipients
    std::vector<std::string> routed_recipients = route_message(message.sender_id, message.recipient_ids);
    
    bool all_sent = true;
    for (const std::string& recipient : routed_recipients) {
        auto recipient_it = pending_messages_.find(recipient);
        if (recipient_it != pending_messages_.end()) {
            recipient_it->second.push_back(message);
            
            // Add to buffer for batching if enabled
            if (enable_batching_) {
                agent_message_buffers_[recipient].add_message(message);
            }
        } else {
            all_sent = false;
        }
    }
    
    // Update performance metrics
    auto end_time = std::chrono::steady_clock::now();
    double latency_ms = std::chrono::duration<double, std::milli>(end_time - start_time).count();
    size_t message_size = calculate_message_size(message);
    
    update_performance_metrics(message_size, latency_ms);
    
    messages_sent_++;
    
    return all_sent;
}

std::vector<TensorMessage> TensorHypergraphProtocol::receive_messages(const std::string& agent_id) {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    auto it = pending_messages_.find(agent_id);
    if (it == pending_messages_.end()) {
        return std::vector<TensorMessage>();
    }
    
    std::vector<TensorMessage> messages = it->second;
    it->second.clear();
    
    messages_received_ += messages.size();
    
    // Decompress messages if needed
    for (auto& message : messages) {
        if (message.is_compressed) {
            message.tensor_data = decompress_tensor_data(message.tensor_data, message.tensor_shape);
            message.is_compressed = false;
        }
    }
    
    return messages;
}

void TensorHypergraphProtocol::broadcast_message(
    const std::string& sender_id,
    const std::vector<std::vector<double>>& hypergraph_nodes,
    const std::vector<std::vector<size_t>>& hypergraph_edges,
    const std::vector<double>& edge_weights,
    const std::string& message_type,
    double priority) {
    
    std::shared_lock<std::shared_mutex> lock(protocol_mutex_);
    
    // Get all connected agents
    std::vector<std::string> all_recipients;
    auto topology_it = agent_topology_.find(sender_id);
    if (topology_it != agent_topology_.end()) {
        all_recipients = topology_it->second;
    }
    
    lock.unlock();
    
    if (!all_recipients.empty()) {
        TensorMessage broadcast_message = create_tensor_message(
            sender_id, all_recipients, hypergraph_nodes, hypergraph_edges,
            edge_weights, message_type, priority
        );
        
        send_message(broadcast_message);
    }
}

void TensorHypergraphProtocol::process_tensor_batch() {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    if (!enable_batching_) return;
    
    // Process batched operations for each agent
    for (auto& [agent_id, buffer] : agent_message_buffers_) {
        if (buffer.is_full() || buffer.size() > batch_size_ / 2) {
            std::vector<TensorMessage> batch_messages = buffer.flush_buffer();
            
            if (!batch_messages.empty()) {
                // Perform vectorized tensor operations on batch
                std::vector<TensorMessage> optimized_batch = batch_tensor_operations(batch_messages);
                
                // Update pending messages with optimized batch
                pending_messages_[agent_id].insert(
                    pending_messages_[agent_id].end(),
                    optimized_batch.begin(),
                    optimized_batch.end()
                );
            }
        }
    }
}

void TensorHypergraphProtocol::connect_agents(const std::string& agent1_id,
                                            const std::string& agent2_id,
                                            bool bidirectional) {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    // Add agent2 to agent1's topology
    auto& agent1_connections = agent_topology_[agent1_id];
    if (std::find(agent1_connections.begin(), agent1_connections.end(), agent2_id) 
        == agent1_connections.end()) {
        agent1_connections.push_back(agent2_id);
    }
    
    if (bidirectional) {
        // Add agent1 to agent2's topology
        auto& agent2_connections = agent_topology_[agent2_id];
        if (std::find(agent2_connections.begin(), agent2_connections.end(), agent1_id) 
            == agent2_connections.end()) {
            agent2_connections.push_back(agent1_id);
        }
    }
    
    std::cout << "Connected agents " << agent1_id << " <-> " << agent2_id 
              << (bidirectional ? " (bidirectional)" : " (unidirectional)") << std::endl;
}

TensorHypergraphProtocol::ProtocolStats TensorHypergraphProtocol::get_protocol_statistics() const {
    std::shared_lock<std::shared_mutex> lock(protocol_mutex_);
    
    ProtocolStats stats;
    stats.messages_sent = messages_sent_.load();
    stats.messages_received = messages_received_.load();
    stats.bytes_transmitted = bytes_transmitted_.load();
    stats.average_latency_ms = average_latency_.load();
    stats.active_agents = agent_topology_.size();
    
    // Calculate total pending messages
    stats.pending_messages = 0;
    for (const auto& [agent_id, messages] : pending_messages_) {
        stats.pending_messages += messages.size();
    }
    
    // Calculate throughput and compression efficiency
    if (stats.messages_sent > 0) {
        stats.message_throughput = static_cast<double>(stats.messages_sent) / 
                                 (std::chrono::duration<double>(
                                     std::chrono::steady_clock::now().time_since_epoch()
                                 ).count());
        stats.compression_efficiency = compression_threshold_;
    } else {
        stats.message_throughput = 0.0;
        stats.compression_efficiency = 1.0;
    }
    
    return stats;
}

TensorMessage TensorHypergraphProtocol::optimize_tensor_message(const TensorMessage& message) {
    TensorMessage optimized = message;
    
    // Apply tensor optimization techniques
    if (optimized.tensor_data.size() > max_tensor_dimension_) {
        // Dimensionality reduction using simple sampling
        std::vector<double> reduced_data;
        size_t reduction_factor = optimized.tensor_data.size() / max_tensor_dimension_;
        
        for (size_t i = 0; i < optimized.tensor_data.size(); i += reduction_factor) {
            reduced_data.push_back(optimized.tensor_data[i]);
        }
        
        optimized.tensor_data = reduced_data;
        
        // Update tensor shape
        if (!optimized.tensor_shape.empty()) {
            optimized.tensor_shape[0] = reduced_data.size();
        }
    }
    
    // Quantize tensor values for efficiency
    for (auto& value : optimized.tensor_data) {
        value = std::round(value * 1000.0) / 1000.0; // 3 decimal precision
    }
    
    return optimized;
}

std::vector<double> TensorHypergraphProtocol::compress_tensor_data(
    const std::vector<double>& tensor_data, double compression_ratio) {
    
    if (compression_ratio >= 1.0) return tensor_data;
    
    // Simple compression: keep top values and zero out small ones
    std::vector<std::pair<double, size_t>> value_index_pairs;
    for (size_t i = 0; i < tensor_data.size(); ++i) {
        value_index_pairs.emplace_back(std::abs(tensor_data[i]), i);
    }
    
    // Sort by magnitude
    std::sort(value_index_pairs.begin(), value_index_pairs.end(),
              [](const auto& a, const auto& b) { return a.first > b.first; });
    
    // Keep only top percentage of values
    size_t keep_count = static_cast<size_t>(tensor_data.size() * compression_ratio);
    std::vector<double> compressed_data(tensor_data.size(), 0.0);
    
    for (size_t i = 0; i < keep_count && i < value_index_pairs.size(); ++i) {
        size_t original_index = value_index_pairs[i].second;
        compressed_data[original_index] = tensor_data[original_index];
    }
    
    return compressed_data;
}

std::vector<double> TensorHypergraphProtocol::decompress_tensor_data(
    const std::vector<double>& compressed_data, const std::vector<size_t>& original_shape) {
    
    // For this simple compression, decompression is just returning the data
    // In a more sophisticated implementation, this would reverse the compression algorithm
    return compressed_data;
}

bool TensorHypergraphProtocol::validate_tensor_message(const TensorMessage& message) {
    // Basic validation checks
    if (message.sender_id.empty() || message.recipient_ids.empty()) {
        return false;
    }
    
    if (message.tensor_data.empty() || message.tensor_shape.empty()) {
        return false;
    }
    
    // Check tensor shape consistency
    size_t expected_size = 1;
    for (size_t dim : message.tensor_shape) {
        expected_size *= dim;
    }
    
    if (message.tensor_data.size() != expected_size && !message.is_compressed) {
        return false;
    }
    
    // Check priority bounds
    if (message.priority < 0.0 || message.priority > 1.0) {
        return false;
    }
    
    return true;
}

std::map<std::string, std::vector<std::string>> TensorHypergraphProtocol::get_agent_topology() const {
    std::shared_lock<std::shared_mutex> lock(protocol_mutex_);
    return agent_topology_;
}

void TensorHypergraphProtocol::set_protocol_parameters(size_t max_dimension,
                                                      double compression_threshold,
                                                      bool enable_batching,
                                                      size_t batch_size) {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    max_tensor_dimension_ = max_dimension;
    compression_threshold_ = compression_threshold;
    enable_batching_ = enable_batching;
    batch_size_ = batch_size;
    
    std::cout << "Updated protocol parameters: max_dimension=" << max_dimension
              << ", compression_threshold=" << compression_threshold
              << ", batching=" << (enable_batching ? "enabled" : "disabled")
              << ", batch_size=" << batch_size << std::endl;
}

void TensorHypergraphProtocol::clear_pending_messages() {
    std::unique_lock<std::shared_mutex> lock(protocol_mutex_);
    
    for (auto& [agent_id, messages] : pending_messages_) {
        messages.clear();
    }
    
    for (auto& [agent_id, buffer] : agent_message_buffers_) {
        buffer.flush_buffer();
    }
    
    std::cout << "Cleared all pending messages" << std::endl;
}

size_t TensorHypergraphProtocol::get_message_buffer_size(const std::string& agent_id) const {
    std::shared_lock<std::shared_mutex> lock(protocol_mutex_);
    
    auto it = agent_message_buffers_.find(agent_id);
    return (it != agent_message_buffers_.end()) ? it->second.size() : 0;
}

// Private methods implementation

std::vector<double> TensorHypergraphProtocol::hypergraph_to_tensor(
    const std::vector<std::vector<double>>& nodes,
    const std::vector<std::vector<size_t>>& edges,
    const std::vector<double>& weights) {
    
    std::vector<double> tensor_data;
    
    // Flatten node data
    for (const auto& node : nodes) {
        tensor_data.insert(tensor_data.end(), node.begin(), node.end());
    }
    
    // Encode edge structure as tensor
    for (size_t i = 0; i < edges.size() && i < weights.size(); ++i) {
        for (size_t node_idx : edges[i]) {
            tensor_data.push_back(static_cast<double>(node_idx));
        }
        tensor_data.push_back(weights[i]);
    }
    
    return tensor_data;
}

void TensorHypergraphProtocol::tensor_to_hypergraph(
    const std::vector<double>& tensor_data,
    const std::vector<size_t>& shape,
    std::vector<std::vector<double>>& nodes,
    std::vector<std::vector<size_t>>& edges,
    std::vector<double>& weights) {
    
    // This is a simplified conversion - in practice would need more sophisticated parsing
    nodes.clear();
    edges.clear();
    weights.clear();
    
    if (shape.size() >= 2) {
        size_t num_nodes = shape[0];
        size_t node_dim = shape[1];
        
        // Extract node data
        for (size_t i = 0; i < num_nodes && i * node_dim < tensor_data.size(); ++i) {
            std::vector<double> node_data;
            for (size_t j = 0; j < node_dim; ++j) {
                if (i * node_dim + j < tensor_data.size()) {
                    node_data.push_back(tensor_data[i * node_dim + j]);
                }
            }
            nodes.push_back(node_data);
        }
    }
}

size_t TensorHypergraphProtocol::calculate_message_size(const TensorMessage& message) {
    size_t size = 0;
    
    // String sizes
    size += message.message_id.size();
    size += message.sender_id.size();
    size += message.message_type.size();
    
    for (const auto& recipient : message.recipient_ids) {
        size += recipient.size();
    }
    
    for (const auto& node_id : message.node_ids) {
        size += node_id.size();
    }
    
    // Tensor data
    size += message.tensor_data.size() * sizeof(double);
    size += message.tensor_shape.size() * sizeof(size_t);
    size += message.edge_weights.size() * sizeof(double);
    
    // Edge connections
    for (const auto& edge : message.edge_connections) {
        size += edge.size() * sizeof(size_t);
    }
    
    return size;
}

void TensorHypergraphProtocol::update_performance_metrics(size_t message_size, double latency_ms) {
    bytes_transmitted_ += message_size;
    
    // Update average latency with exponential moving average
    double current_avg = average_latency_.load();
    double new_avg = 0.9 * current_avg + 0.1 * latency_ms;
    average_latency_.store(new_avg);
}

std::string TensorHypergraphProtocol::generate_message_id() {
    static std::atomic<uint64_t> counter(0);
    std::stringstream ss;
    ss << "msg_" << std::hex << std::chrono::steady_clock::now().time_since_epoch().count()
       << "_" << counter++;
    return ss.str();
}

std::vector<std::string> TensorHypergraphProtocol::route_message(
    const std::string& sender, const std::vector<std::string>& recipients) {
    
    // Simple routing - return all recipients that are connected to sender
    std::vector<std::string> routed;
    
    auto topology_it = agent_topology_.find(sender);
    if (topology_it == agent_topology_.end()) {
        return recipients; // No topology info, send to all requested
    }
    
    const auto& connected_agents = topology_it->second;
    
    for (const std::string& recipient : recipients) {
        if (std::find(connected_agents.begin(), connected_agents.end(), recipient) 
            != connected_agents.end()) {
            routed.push_back(recipient);
        }
    }
    
    return routed;
}

std::vector<double> TensorHypergraphProtocol::apply_tensor_compression(
    const std::vector<double>& data, double ratio) {
    
    return compress_tensor_data(data, ratio);
}

std::vector<TensorMessage> TensorHypergraphProtocol::batch_tensor_operations(
    const std::vector<TensorMessage>& messages) {
    
    std::vector<TensorMessage> optimized_messages;
    
    for (const auto& message : messages) {
        TensorMessage optimized = optimize_tensor_message(message);
        optimized_messages.push_back(optimized);
    }
    
    return optimized_messages;
}

} // namespace opencog