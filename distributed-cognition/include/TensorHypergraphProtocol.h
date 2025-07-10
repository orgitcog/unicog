/*
 * TensorHypergraphProtocol.h
 *
 * Tensor-based hypergraph message protocol for multi-agent communication
 * Implements hypergraph messages as tensors for efficient distributed processing
 */

#ifndef _OPENCOG_TENSOR_HYPERGRAPH_PROTOCOL_H
#define _OPENCOG_TENSOR_HYPERGRAPH_PROTOCOL_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <mutex>
#include <atomic>
#include <chrono>

namespace opencog {

/**
 * Tensor Representation of Hypergraph Messages
 * Enables efficient vectorized operations for multi-agent communication
 */
struct TensorMessage {
    std::string message_id;
    std::string sender_id;
    std::vector<std::string> recipient_ids;
    
    // Tensor dimensions and data
    std::vector<size_t> tensor_shape;
    std::vector<double> tensor_data;
    
    // Message metadata
    std::string message_type;
    double priority;
    std::chrono::steady_clock::time_point timestamp;
    
    // Hypergraph structure
    std::vector<std::string> node_ids;
    std::vector<std::vector<size_t>> edge_connections;
    std::vector<double> edge_weights;
    
    // Compression and efficiency
    bool is_compressed;
    double compression_ratio;
};

/**
 * Message Buffer for Efficient Batching
 * Buffers messages for batched tensor operations
 */
class MessageBuffer {
private:
    std::vector<TensorMessage> buffer_;
    size_t max_buffer_size_;
    std::mutex buffer_mutex_;
    
public:
    MessageBuffer(size_t max_size = 1000) : max_buffer_size_(max_size) {}
    
    void add_message(const TensorMessage& message);
    std::vector<TensorMessage> flush_buffer();
    size_t size() const;
    bool is_full() const;
};

/**
 * Tensor Hypergraph Protocol
 * 
 * Implements efficient multi-agent communication using tensor-based
 * hypergraph messages. Enables vectorized operations and batch processing
 * for high-performance distributed cognition.
 */
class TensorHypergraphProtocol
{
private:
    // Message management
    std::map<std::string, MessageBuffer> agent_message_buffers_;
    std::map<std::string, std::vector<TensorMessage>> pending_messages_;
    
    // Protocol configuration
    size_t max_tensor_dimension_;
    double compression_threshold_;
    bool enable_batching_;
    size_t batch_size_;
    
    // Performance metrics
    std::atomic<uint64_t> messages_sent_;
    std::atomic<uint64_t> messages_received_;
    std::atomic<uint64_t> bytes_transmitted_;
    std::atomic<double> average_latency_;
    
    // Synchronization
    mutable std::shared_mutex protocol_mutex_;
    
    // Message routing and topology
    std::map<std::string, std::vector<std::string>> agent_topology_;
    std::map<std::string, double> agent_bandwidth_;

public:
    TensorHypergraphProtocol(size_t max_dimension = 1024, 
                           double compression_threshold = 0.7,
                           bool enable_batching = true,
                           size_t batch_size = 32);
    ~TensorHypergraphProtocol();

    /**
     * Register agent in the communication protocol
     * 
     * @param agent_id Unique agent identifier
     * @param bandwidth_limit Communication bandwidth limit for agent
     */
    void register_agent(const std::string& agent_id, double bandwidth_limit = 1000.0);

    /**
     * Create tensor message from hypergraph structure
     * 
     * @param sender_id Sending agent
     * @param recipients List of recipient agents
     * @param hypergraph_nodes Vector of node representations
     * @param hypergraph_edges Hypergraph edge connections
     * @param edge_weights Weights for hypergraph edges
     * @param message_type Type of message (e.g., "cognitive_update", "resource_request")
     * @param priority Message priority (0.0 - 1.0)
     * @return Tensor message structure
     */
    TensorMessage create_tensor_message(
        const std::string& sender_id,
        const std::vector<std::string>& recipients,
        const std::vector<std::vector<double>>& hypergraph_nodes,
        const std::vector<std::vector<size_t>>& hypergraph_edges,
        const std::vector<double>& edge_weights,
        const std::string& message_type,
        double priority = 0.5
    );

    /**
     * Send tensor message through the protocol
     * 
     * @param message Tensor message to send
     * @return Success status
     */
    bool send_message(const TensorMessage& message);

    /**
     * Receive pending messages for an agent
     * 
     * @param agent_id Agent identifier
     * @return Vector of received tensor messages
     */
    std::vector<TensorMessage> receive_messages(const std::string& agent_id);

    /**
     * Broadcast message to all connected agents
     * 
     * @param sender_id Sending agent
     * @param hypergraph_nodes Hypergraph node data
     * @param hypergraph_edges Hypergraph edge structure
     * @param edge_weights Edge weights
     * @param message_type Message type
     * @param priority Message priority
     */
    void broadcast_message(
        const std::string& sender_id,
        const std::vector<std::vector<double>>& hypergraph_nodes,
        const std::vector<std::vector<size_t>>& hypergraph_edges,
        const std::vector<double>& edge_weights,
        const std::string& message_type,
        double priority = 0.5
    );

    /**
     * Process batched tensor operations
     * Performs vectorized operations on batched messages for efficiency
     */
    void process_tensor_batch();

    /**
     * Establish communication topology between agents
     * 
     * @param agent1_id First agent
     * @param agent2_id Second agent
     * @param bidirectional Whether connection is bidirectional
     */
    void connect_agents(const std::string& agent1_id, 
                       const std::string& agent2_id,
                       bool bidirectional = true);

    /**
     * Get protocol performance statistics
     */
    struct ProtocolStats {
        uint64_t messages_sent;
        uint64_t messages_received;
        uint64_t bytes_transmitted;
        double average_latency_ms;
        double message_throughput;
        double compression_efficiency;
        size_t active_agents;
        size_t pending_messages;
    };
    ProtocolStats get_protocol_statistics() const;

    /**
     * Optimize tensor representation for efficiency
     * 
     * @param message Message to optimize
     * @return Optimized tensor message
     */
    TensorMessage optimize_tensor_message(const TensorMessage& message);

    /**
     * Compress tensor data using various algorithms
     * 
     * @param tensor_data Raw tensor data
     * @param compression_ratio Target compression ratio
     * @return Compressed tensor data
     */
    std::vector<double> compress_tensor_data(const std::vector<double>& tensor_data,
                                           double compression_ratio = 0.5);

    /**
     * Decompress tensor data
     * 
     * @param compressed_data Compressed tensor data
     * @param original_shape Original tensor shape
     * @return Decompressed tensor data
     */
    std::vector<double> decompress_tensor_data(const std::vector<double>& compressed_data,
                                             const std::vector<size_t>& original_shape);

    /**
     * Validate tensor message integrity
     * 
     * @param message Message to validate
     * @return Validation result
     */
    bool validate_tensor_message(const TensorMessage& message);

    /**
     * Get agent topology information
     */
    std::map<std::string, std::vector<std::string>> get_agent_topology() const;

    /**
     * Set protocol parameters
     */
    void set_protocol_parameters(size_t max_dimension, 
                               double compression_threshold,
                               bool enable_batching,
                               size_t batch_size);

    /**
     * Clear all pending messages (useful for reset)
     */
    void clear_pending_messages();

    /**
     * Get message buffer status for an agent
     */
    size_t get_message_buffer_size(const std::string& agent_id) const;

private:
    /**
     * Convert hypergraph structure to tensor representation
     */
    std::vector<double> hypergraph_to_tensor(
        const std::vector<std::vector<double>>& nodes,
        const std::vector<std::vector<size_t>>& edges,
        const std::vector<double>& weights
    );

    /**
     * Convert tensor back to hypergraph structure
     */
    void tensor_to_hypergraph(
        const std::vector<double>& tensor_data,
        const std::vector<size_t>& shape,
        std::vector<std::vector<double>>& nodes,
        std::vector<std::vector<size_t>>& edges,
        std::vector<double>& weights
    );

    /**
     * Calculate tensor message size in bytes
     */
    size_t calculate_message_size(const TensorMessage& message);

    /**
     * Update performance metrics
     */
    void update_performance_metrics(size_t message_size, double latency_ms);

    /**
     * Generate unique message ID
     */
    std::string generate_message_id();

    /**
     * Route message based on topology
     */
    std::vector<std::string> route_message(const std::string& sender,
                                         const std::vector<std::string>& recipients);

    /**
     * Apply tensor compression algorithms
     */
    std::vector<double> apply_tensor_compression(const std::vector<double>& data,
                                               double ratio);

    /**
     * Perform tensor batching operations
     */
    std::vector<TensorMessage> batch_tensor_operations(
        const std::vector<TensorMessage>& messages
    );
};

} // namespace opencog

#endif // _OPENCOG_TENSOR_HYPERGRAPH_PROTOCOL_H