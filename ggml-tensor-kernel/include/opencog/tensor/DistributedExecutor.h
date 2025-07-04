/*
 * DistributedExecutor.h
 *
 * Distributed execution and sharding for GGML tensor operations
 */

#ifndef _OPENCOG_DISTRIBUTED_EXECUTOR_H
#define _OPENCOG_DISTRIBUTED_EXECUTOR_H

#include <memory>
#include <vector>
#include <string>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include <atomic>

#include "ggml.h"

namespace opencog {

/**
 * ExecutionNode represents a node in the distributed execution network
 */
struct ExecutionNode {
    std::string node_id;
    std::string address;
    size_t compute_capacity;
    size_t memory_capacity;
    std::atomic<bool> is_available;
    std::atomic<size_t> current_load;
    
    ExecutionNode(const std::string& id, const std::string& addr, 
                 size_t compute_cap, size_t memory_cap)
        : node_id(id), address(addr), compute_capacity(compute_cap), 
          memory_capacity(memory_cap), is_available(true), current_load(0) {}
};

/**
 * TensorShard represents a portion of a tensor distributed across nodes
 */
struct TensorShard {
    std::string shard_id;
    std::string node_id;
    std::vector<size_t> dimensions;
    std::vector<size_t> offset;
    ggml_tensor* tensor_data;
    
    TensorShard(const std::string& id, const std::string& node, 
               const std::vector<size_t>& dims, const std::vector<size_t>& off)
        : shard_id(id), node_id(node), dimensions(dims), offset(off), tensor_data(nullptr) {}
};

/**
 * DistributedTask represents a task to be executed across the network
 */
struct DistributedTask {
    std::string task_id;
    std::string operation_name;
    std::vector<std::string> input_shards;
    std::vector<std::string> output_shards;
    std::string assigned_node;
    std::atomic<bool> is_completed;
    
    DistributedTask(const std::string& id, const std::string& op)
        : task_id(id), operation_name(op), is_completed(false) {}
};

/**
 * DistributedExecutor manages distributed tensor computation across multiple nodes
 */
class DistributedExecutor {
private:
    std::vector<std::unique_ptr<ExecutionNode>> nodes_;
    std::unordered_map<std::string, std::vector<TensorShard>> tensor_shards_;
    
    // Task management
    std::queue<std::unique_ptr<DistributedTask>> task_queue_;
    std::mutex task_mutex_;
    std::condition_variable task_cv_;
    std::atomic<bool> executor_running_;
    
    // Worker threads
    std::vector<std::thread> worker_threads_;
    size_t num_workers_;
    
    // Load balancing
    std::mutex load_balancer_mutex_;
    
    // Fault tolerance
    std::atomic<size_t> failed_nodes_;
    std::vector<std::string> backup_nodes_;
    
public:
    DistributedExecutor(size_t num_worker_threads = 4);
    ~DistributedExecutor();
    
    // Node management
    void register_node(const std::string& node_id, const std::string& address,
                      size_t compute_capacity, size_t memory_capacity);
    void remove_node(const std::string& node_id);
    std::vector<std::string> get_available_nodes() const;
    bool is_node_available(const std::string& node_id) const;
    
    // Tensor sharding
    void shard_tensor(const std::string& tensor_id, ggml_tensor* tensor,
                     const std::vector<std::string>& target_nodes);
    std::vector<TensorShard> get_tensor_shards(const std::string& tensor_id) const;
    ggml_tensor* reconstruct_tensor(const std::string& tensor_id);
    
    // Distributed execution
    void execute_distributed_operation(const std::string& operation_name,
                                     const std::vector<std::string>& input_tensors,
                                     const std::vector<std::string>& output_tensors);
    
    // Task scheduling
    void submit_task(std::unique_ptr<DistributedTask> task);
    void execute_task_queue();
    void wait_for_completion();
    
    // Load balancing
    std::string select_best_node_for_task(const DistributedTask& task) const;
    void rebalance_load();
    
    // Fault tolerance
    void enable_fault_tolerance(const std::vector<std::string>& backup_node_addresses);
    void handle_node_failure(const std::string& failed_node_id);
    void migrate_shards_from_failed_node(const std::string& failed_node_id);
    
    // Parallel execution coordination
    void start_executor();
    void stop_executor();
    bool is_running() const { return executor_running_; }
    
    // Communication (simplified interface)
    bool send_tensor_to_node(const std::string& node_id, const TensorShard& shard);
    bool receive_tensor_from_node(const std::string& node_id, TensorShard& shard);
    
    // Statistics and monitoring
    size_t get_total_compute_capacity() const;
    size_t get_total_memory_capacity() const;
    size_t get_current_load() const;
    double get_network_utilization() const;
    
    // Configuration
    void set_worker_count(size_t count);
    void set_fault_tolerance_level(size_t level);
    
    // Debugging
    void print_node_status() const;
    void print_shard_distribution() const;
    void print_task_queue_status() const;
    
private:
    // Worker thread function
    void worker_thread_function();
    
    // Internal task execution
    void execute_task(DistributedTask& task);
    
    // Shard management
    void distribute_shards_evenly();
    void optimize_shard_placement();
    
    // Network communication helpers
    bool establish_connection(const std::string& node_address);
    void cleanup_connections();
};

} // namespace opencog

#endif // _OPENCOG_DISTRIBUTED_EXECUTOR_H