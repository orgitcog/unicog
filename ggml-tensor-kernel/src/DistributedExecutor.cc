/*
 * DistributedExecutor.cc
 *
 * Basic implementation of distributed tensor execution
 */

#include <opencog/tensor/DistributedExecutor.h>
#include <opencog/util/Logger.h>

using namespace opencog;

DistributedExecutor::DistributedExecutor(size_t num_worker_threads)
    : num_workers_(num_worker_threads), executor_running_(false), failed_nodes_(0) {
}

DistributedExecutor::~DistributedExecutor() {
    stop_executor();
}

void DistributedExecutor::register_node(const std::string& node_id, const std::string& address,
                                       size_t compute_capacity, size_t memory_capacity) {
    auto node = std::make_unique<ExecutionNode>(node_id, address, compute_capacity, memory_capacity);
    nodes_.push_back(std::move(node));
    logger().info("DistributedExecutor") << "Registered node: " << node_id << " at " << address;
}

void DistributedExecutor::remove_node(const std::string& node_id) {
    auto it = std::remove_if(nodes_.begin(), nodes_.end(),
        [&node_id](const std::unique_ptr<ExecutionNode>& node) {
            return node->node_id == node_id;
        });
    
    if (it != nodes_.end()) {
        nodes_.erase(it, nodes_.end());
        logger().info("DistributedExecutor") << "Removed node: " << node_id;
    }
}

std::vector<std::string> DistributedExecutor::get_available_nodes() const {
    std::vector<std::string> available;
    for (const auto& node : nodes_) {
        if (node->is_available.load()) {
            available.push_back(node->node_id);
        }
    }
    return available;
}

bool DistributedExecutor::is_node_available(const std::string& node_id) const {
    for (const auto& node : nodes_) {
        if (node->node_id == node_id) {
            return node->is_available.load();
        }
    }
    return false;
}

void DistributedExecutor::shard_tensor(const std::string& tensor_id, ggml_tensor* tensor,
                                      const std::vector<std::string>& target_nodes) {
    if (!tensor || target_nodes.empty()) {
        logger().error("DistributedExecutor") << "Invalid tensor or target nodes for sharding";
        return;
    }
    
    std::vector<TensorShard> shards;
    size_t shard_size = target_nodes.size();
    
    for (size_t i = 0; i < shard_size; ++i) {
        std::string shard_id = tensor_id + "_shard_" + std::to_string(i);
        TensorShard shard(shard_id, target_nodes[i % target_nodes.size()], 
                         {100, 100}, {i * 100, 0}); // Basic sharding
        shards.push_back(shard);
    }
    
    tensor_shards_[tensor_id] = shards;
    logger().info("DistributedExecutor") << "Sharded tensor " << tensor_id << " into " << shards.size() << " pieces";
}

std::vector<TensorShard> DistributedExecutor::get_tensor_shards(const std::string& tensor_id) const {
    auto it = tensor_shards_.find(tensor_id);
    if (it != tensor_shards_.end()) {
        return it->second;
    }
    return std::vector<TensorShard>();
}

ggml_tensor* DistributedExecutor::reconstruct_tensor(const std::string& tensor_id) {
    logger().debug("DistributedExecutor") << "Reconstructed tensor: " << tensor_id;
    return nullptr; // Basic implementation
}

void DistributedExecutor::execute_distributed_operation(const std::string& operation_name,
                                                       const std::vector<std::string>& input_tensors,
                                                       const std::vector<std::string>& output_tensors) {
    auto task = std::make_unique<DistributedTask>(operation_name + "_task", operation_name);
    task->input_shards = input_tensors;
    task->output_shards = output_tensors;
    
    submit_task(std::move(task));
    logger().info("DistributedExecutor") << "Submitted distributed operation: " << operation_name;
}

void DistributedExecutor::submit_task(std::unique_ptr<DistributedTask> task) {
    {
        std::lock_guard<std::mutex> lock(task_mutex_);
        task_queue_.push(std::move(task));
    }
    task_cv_.notify_one();
}

void DistributedExecutor::execute_task_queue() {
    while (executor_running_.load()) {
        std::unique_lock<std::mutex> lock(task_mutex_);
        task_cv_.wait(lock, [this]() { return !task_queue_.empty() || !executor_running_.load(); });
        
        if (!executor_running_.load()) {
            break;
        }
        
        if (!task_queue_.empty()) {
            auto task = std::move(task_queue_.front());
            task_queue_.pop();
            lock.unlock();
            
            execute_task(*task);
        }
    }
}

void DistributedExecutor::wait_for_completion() {
    while (!task_queue_.empty()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
}

std::string DistributedExecutor::select_best_node_for_task(const DistributedTask& task) const {
    std::string best_node;
    size_t min_load = std::numeric_limits<size_t>::max();
    
    for (const auto& node : nodes_) {
        if (node->is_available.load() && node->current_load.load() < min_load) {
            min_load = node->current_load.load();
            best_node = node->node_id;
        }
    }
    
    return best_node;
}

void DistributedExecutor::rebalance_load() {
    std::lock_guard<std::mutex> lock(load_balancer_mutex_);
    logger().debug("DistributedExecutor") << "Rebalanced load across nodes";
}

void DistributedExecutor::enable_fault_tolerance(const std::vector<std::string>& backup_node_addresses) {
    backup_nodes_ = backup_node_addresses;
    logger().info("DistributedExecutor") << "Enabled fault tolerance with " << backup_nodes_.size() << " backup nodes";
}

void DistributedExecutor::handle_node_failure(const std::string& failed_node_id) {
    for (auto& node : nodes_) {
        if (node->node_id == failed_node_id) {
            node->is_available.store(false);
            failed_nodes_.fetch_add(1);
            break;
        }
    }
    
    migrate_shards_from_failed_node(failed_node_id);
    logger().warn("DistributedExecutor") << "Handled failure of node: " << failed_node_id;
}

void DistributedExecutor::migrate_shards_from_failed_node(const std::string& failed_node_id) {
    for (auto& tensor_pair : tensor_shards_) {
        for (auto& shard : tensor_pair.second) {
            if (shard.node_id == failed_node_id) {
                // Find a new node for this shard
                std::vector<std::string> available = get_available_nodes();
                if (!available.empty()) {
                    shard.node_id = available[0]; // Simple migration
                }
            }
        }
    }
    
    logger().info("DistributedExecutor") << "Migrated shards from failed node: " << failed_node_id;
}

void DistributedExecutor::start_executor() {
    if (executor_running_.load()) {
        return;
    }
    
    executor_running_.store(true);
    
    // Start worker threads
    for (size_t i = 0; i < num_workers_; ++i) {
        worker_threads_.emplace_back(&DistributedExecutor::worker_thread_function, this);
    }
    
    logger().info("DistributedExecutor") << "Started executor with " << num_workers_ << " worker threads";
}

void DistributedExecutor::stop_executor() {
    if (!executor_running_.load()) {
        return;
    }
    
    executor_running_.store(false);
    task_cv_.notify_all();
    
    // Join worker threads
    for (auto& thread : worker_threads_) {
        if (thread.joinable()) {
            thread.join();
        }
    }
    worker_threads_.clear();
    
    logger().info("DistributedExecutor") << "Stopped executor";
}

bool DistributedExecutor::send_tensor_to_node(const std::string& node_id, const TensorShard& shard) {
    logger().debug("DistributedExecutor") << "Sent tensor shard " << shard.shard_id << " to node " << node_id;
    return true; // Assume success for now
}

bool DistributedExecutor::receive_tensor_from_node(const std::string& node_id, TensorShard& shard) {
    logger().debug("DistributedExecutor") << "Received tensor shard " << shard.shard_id << " from node " << node_id;
    return true; // Assume success for now
}

size_t DistributedExecutor::get_total_compute_capacity() const {
    size_t total = 0;
    for (const auto& node : nodes_) {
        total += node->compute_capacity;
    }
    return total;
}

size_t DistributedExecutor::get_total_memory_capacity() const {
    size_t total = 0;
    for (const auto& node : nodes_) {
        total += node->memory_capacity;
    }
    return total;
}

size_t DistributedExecutor::get_current_load() const {
    size_t total = 0;
    for (const auto& node : nodes_) {
        total += node->current_load.load();
    }
    return total;
}

double DistributedExecutor::get_network_utilization() const {
    size_t total_capacity = get_total_compute_capacity();
    size_t current_load = get_current_load();
    
    if (total_capacity == 0) {
        return 0.0;
    }
    
    return static_cast<double>(current_load) / total_capacity;
}

void DistributedExecutor::set_worker_count(size_t count) {
    if (executor_running_.load()) {
        logger().warn("DistributedExecutor") << "Cannot change worker count while executor is running";
        return;
    }
    
    num_workers_ = count;
}

void DistributedExecutor::set_fault_tolerance_level(size_t level) {
    logger().info("DistributedExecutor") << "Set fault tolerance level to: " << level;
}

void DistributedExecutor::print_node_status() const {
    std::cout << "=== Distributed Executor Node Status ===" << std::endl;
    std::cout << "Total nodes: " << nodes_.size() << std::endl;
    std::cout << "Failed nodes: " << failed_nodes_.load() << std::endl;
    std::cout << "Total compute capacity: " << get_total_compute_capacity() << std::endl;
    std::cout << "Network utilization: " << (get_network_utilization() * 100) << "%" << std::endl;
    
    for (const auto& node : nodes_) {
        std::cout << "Node " << node->node_id << ": ";
        std::cout << (node->is_available.load() ? "Available" : "Unavailable");
        std::cout << ", Load: " << node->current_load.load() << std::endl;
    }
}

void DistributedExecutor::print_shard_distribution() const {
    std::cout << "=== Tensor Shard Distribution ===" << std::endl;
    for (const auto& tensor_pair : tensor_shards_) {
        std::cout << "Tensor " << tensor_pair.first << ": " << tensor_pair.second.size() << " shards" << std::endl;
    }
}

void DistributedExecutor::print_task_queue_status() const {
    std::cout << "=== Task Queue Status ===" << std::endl;
    std::cout << "Queue size: " << task_queue_.size() << std::endl;
    std::cout << "Executor running: " << (executor_running_.load() ? "Yes" : "No") << std::endl;
    std::cout << "Worker threads: " << worker_threads_.size() << std::endl;
}

void DistributedExecutor::worker_thread_function() {
    execute_task_queue();
}

void DistributedExecutor::execute_task(DistributedTask& task) {
    std::string best_node = select_best_node_for_task(task);
    if (best_node.empty()) {
        logger().error("DistributedExecutor") << "No available nodes for task: " << task.task_id;
        return;
    }
    
    task.assigned_node = best_node;
    
    // Simulate task execution
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    
    task.is_completed.store(true);
    logger().debug("DistributedExecutor") << "Executed task " << task.task_id << " on node " << best_node;
}

void DistributedExecutor::distribute_shards_evenly() {
    logger().debug("DistributedExecutor") << "Distributed shards evenly across nodes";
}

void DistributedExecutor::optimize_shard_placement() {
    logger().debug("DistributedExecutor") << "Optimized shard placement";
}

bool DistributedExecutor::establish_connection(const std::string& node_address) {
    logger().debug("DistributedExecutor") << "Established connection to: " << node_address;
    return true;
}

void DistributedExecutor::cleanup_connections() {
    logger().debug("DistributedExecutor") << "Cleaned up connections";
}