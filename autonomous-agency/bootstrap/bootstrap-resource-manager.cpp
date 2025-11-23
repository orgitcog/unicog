#include "bootstrap-resource-manager.hpp"
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iomanip>
#include <sstream>

namespace opencog {
namespace autonomous {

BootstrapResourceManager::BootstrapResourceManager()
    : manager_active_(false), emergency_mode_(false) {
    
    // Initialize default resource pools
    initializeResourcePool(ResourceType::COMPUTATIONAL, 1000);
    initializeResourcePool(ResourceType::MEMORY, 2048); // MB
    initializeResourcePool(ResourceType::ATTENTION, 100);
    initializeResourcePool(ResourceType::STORAGE, 10240); // MB
    initializeResourcePool(ResourceType::NETWORK, 100);
    
    // Initialize performance metrics
    performance_metrics_.resource_utilization_efficiency = 0.0;
    performance_metrics_.task_completion_rate = 0.0;
    performance_metrics_.emergency_response_time = 0.0;
    performance_metrics_.tasks_completed = 0;
    performance_metrics_.tasks_failed = 0;
    performance_metrics_.emergency_interventions = 0;
}

BootstrapResourceManager::~BootstrapResourceManager() {
    stopResourceManager();
}

bool BootstrapResourceManager::allocateEmergencyResources(const std::string& task_id, 
                                                         ResourceType resource_type, 
                                                         size_t amount) {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    // Emergency allocation bypasses normal queue and takes priority
    if (!canAllocateResource(resource_type, amount)) {
        // Try to free up resources by suspending low priority tasks
        suspendLowPriorityTasks();
        
        if (!canAllocateResource(resource_type, amount)) {
            std::cout << "BOOTSTRAP RESOURCE: Emergency allocation failed - insufficient resources" << std::endl;
            return false;
        }
    }
    
    std::string allocation_id = allocateResource(task_id, resource_type, amount, 
                                               std::chrono::milliseconds(5000)); // 5 second emergency allocation
    
    if (!allocation_id.empty()) {
        std::cout << "BOOTSTRAP RESOURCE: Emergency allocation successful - " 
                  << allocation_id << std::endl;
        recordEmergencyIntervention();
        return true;
    }
    
    return false;
}

void BootstrapResourceManager::prioritizeCriticalSystems() {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    std::cout << "BOOTSTRAP RESOURCE: Prioritizing critical systems..." << std::endl;
    
    // Reserve resources for critical operations
    for (auto& pool_pair : resource_pools_) {
        ResourcePool& pool = pool_pair.second;
        
        // Reserve 50% of resources for critical tasks during emergency
        if (emergency_mode_) {
            pool.reserved_capacity = pool.total_capacity / 2;
        } else {
            pool.reserved_capacity = pool.total_capacity / 4; // 25% normal reservation
        }
        
        // Ensure available capacity reflects reservations
        if (pool.available_capacity + pool.reserved_capacity > pool.total_capacity) {
            pool.available_capacity = pool.total_capacity - pool.reserved_capacity;
        }
    }
    
    std::cout << "BOOTSTRAP RESOURCE: Critical system prioritization completed." << std::endl;
}

void BootstrapResourceManager::throttleNonEssentialProcesses() {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    std::cout << "BOOTSTRAP RESOURCE: Throttling non-essential processes..." << std::endl;
    
    // Release resources from low priority allocations
    std::vector<std::string> to_release;
    
    for (const auto& allocation : active_allocations_) {
        // In a full implementation, we would check task priority
        // For now, we'll release allocations that have been running for a while
        auto allocation_duration = std::chrono::steady_clock::now() - allocation.allocation_time;
        if (allocation_duration > std::chrono::minutes(5)) {
            to_release.push_back(allocation.allocation_id);
        }
    }
    
    for (const std::string& allocation_id : to_release) {
        releaseResource(allocation_id);
    }
    
    std::cout << "BOOTSTRAP RESOURCE: Throttled " << to_release.size() 
              << " non-essential processes." << std::endl;
}

void BootstrapResourceManager::redistributeResources() {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    std::cout << "BOOTSTRAP RESOURCE: Redistributing resources..." << std::endl;
    
    // Calculate total utilization across all resource types
    std::map<ResourceType, double> utilization_rates;
    
    for (const auto& pool_pair : resource_pools_) {
        ResourceType type = pool_pair.first;
        const ResourcePool& pool = pool_pair.second;
        
        if (pool.total_capacity > 0) {
            utilization_rates[type] = 1.0 - (static_cast<double>(pool.available_capacity) / pool.total_capacity);
        } else {
            utilization_rates[type] = 0.0;
        }
    }
    
    // Identify over-utilized and under-utilized resources
    double avg_utilization = 0.0;
    for (const auto& rate_pair : utilization_rates) {
        avg_utilization += rate_pair.second;
    }
    avg_utilization /= utilization_rates.size();
    
    // Redistribute from under-utilized to over-utilized pools
    for (auto& pool_pair : resource_pools_) {
        ResourceType type = pool_pair.first;
        ResourcePool& pool = pool_pair.second;
        
        if (utilization_rates[type] < avg_utilization * 0.5) {
            // Under-utilized - reduce capacity slightly
            size_t reduction = pool.total_capacity * 0.1;
            if (pool.total_capacity > reduction + pool.reserved_capacity) {
                pool.total_capacity -= reduction;
                pool.available_capacity = std::min(pool.available_capacity, 
                                                 pool.total_capacity - pool.reserved_capacity);
            }
        } else if (utilization_rates[type] > avg_utilization * 1.5) {
            // Over-utilized - increase capacity
            size_t increase = pool.total_capacity * 0.1;
            pool.total_capacity += increase;
            pool.available_capacity += increase;
        }
    }
    
    std::cout << "BOOTSTRAP RESOURCE: Resource redistribution completed." << std::endl;
}

void BootstrapResourceManager::scaleResourcePools(ResourceType type, double scale_factor) {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    auto pool_it = resource_pools_.find(type);
    if (pool_it != resource_pools_.end()) {
        ResourcePool& pool = pool_it->second;
        
        size_t new_capacity = static_cast<size_t>(pool.total_capacity * scale_factor);
        
        if (scale_factor > 1.0) {
            // Scaling up - increase available capacity
            size_t increase = new_capacity - pool.total_capacity;
            pool.available_capacity += increase;
        } else {
            // Scaling down - ensure we don't go below current usage
            size_t current_usage = pool.total_capacity - pool.available_capacity;
            if (new_capacity < current_usage) {
                new_capacity = current_usage;
            }
            pool.available_capacity = new_capacity - current_usage;
        }
        
        pool.total_capacity = new_capacity;
        pool.last_update = std::chrono::steady_clock::now();
        
        std::cout << "BOOTSTRAP RESOURCE: Scaled resource pool " 
                  << static_cast<int>(type) << " by factor " << scale_factor << std::endl;
    }
}

void BootstrapResourceManager::optimizeResourceUtilization() {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    std::cout << "BOOTSTRAP RESOURCE: Optimizing resource utilization..." << std::endl;
    
    // Release expired allocations
    releaseExpiredAllocations();
    
    // Defragment resource pools
    defragmentResourcePools();
    
    // Balance resource distribution
    balanceResourceDistribution();
    
    // Update performance metrics
    updatePerformanceMetrics();
    
    std::cout << "BOOTSTRAP RESOURCE: Resource optimization completed." << std::endl;
}

bool BootstrapResourceManager::submitTask(const Task& task) {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    critical_tasks_.push(task);
    resource_cv_.notify_one();
    
    std::cout << "BOOTSTRAP RESOURCE: Task submitted - " << task.task_id 
              << " (Priority: " << static_cast<int>(task.priority) << ")" << std::endl;
    
    return true;
}

bool BootstrapResourceManager::submitEmergencyTask(const Task& task) {
    std::lock_guard<std::mutex> lock(resource_mutex_);
    
    Task emergency_task = task;
    emergency_task.is_emergency = true;
    emergency_task.priority = TaskPriority::CRITICAL;
    
    critical_tasks_.push(emergency_task);
    resource_cv_.notify_one();
    
    std::cout << "BOOTSTRAP RESOURCE: Emergency task submitted - " << task.task_id << std::endl;
    
    return true;
}

void BootstrapResourceManager::startResourceManager() {
    if (manager_active_) return;
    
    manager_active_ = true;
    resource_manager_thread_ = std::make_unique<std::thread>(&BootstrapResourceManager::resourceManagerLoop, this);
    
    std::cout << "BOOTSTRAP RESOURCE: Resource manager started." << std::endl;
}

void BootstrapResourceManager::stopResourceManager() {
    if (!manager_active_) return;
    
    manager_active_ = false;
    resource_cv_.notify_all();
    
    if (resource_manager_thread_ && resource_manager_thread_->joinable()) {
        resource_manager_thread_->join();
    }
    
    std::cout << "BOOTSTRAP RESOURCE: Resource manager stopped." << std::endl;
}

void BootstrapResourceManager::enterEmergencyMode() {
    emergency_mode_ = true;
    emergency_start_time_ = std::chrono::steady_clock::now();
    
    std::cout << "BOOTSTRAP RESOURCE: Entering emergency mode." << std::endl;
    
    // Execute emergency protocols
    executeEmergencyProtocols();
}

void BootstrapResourceManager::exitEmergencyMode() {
    if (emergency_mode_) {
        auto emergency_duration = std::chrono::steady_clock::now() - emergency_start_time_;
        performance_metrics_.emergency_response_time = 
            std::chrono::duration<double, std::milli>(emergency_duration).count();
        
        emergency_mode_ = false;
        
        std::cout << "BOOTSTRAP RESOURCE: Exiting emergency mode. Duration: " 
                  << performance_metrics_.emergency_response_time << "ms" << std::endl;
    }
}

void BootstrapResourceManager::initializeResourcePool(ResourceType type, size_t capacity) {
    ResourcePool pool;
    pool.type = type;
    pool.total_capacity = capacity;
    pool.available_capacity = capacity;
    pool.reserved_capacity = capacity / 10; // Reserve 10% by default
    pool.last_update = std::chrono::steady_clock::now();
    
    resource_pools_[type] = pool;
    
    std::cout << "BOOTSTRAP RESOURCE: Initialized resource pool " 
              << static_cast<int>(type) << " with capacity " << capacity << std::endl;
}

BootstrapResourceManager::PerformanceMetrics BootstrapResourceManager::getPerformanceMetrics() const {
    return performance_metrics_;
}

std::vector<std::string> BootstrapResourceManager::getResourceStatus() {
    std::vector<std::string> status;
    
    status.push_back("=== Bootstrap Resource Manager Status ===");
    status.push_back("Emergency Mode: " + std::string(emergency_mode_ ? "ACTIVE" : "Inactive"));
    status.push_back("Active Allocations: " + std::to_string(active_allocations_.size()));
    status.push_back("Pending Tasks: " + std::to_string(critical_tasks_.size()));
    status.push_back("");
    
    status.push_back("Resource Pool Status:");
    for (const auto& pool_pair : resource_pools_) {
        const ResourcePool& pool = pool_pair.second;
        double utilization = pool.total_capacity > 0 ? 
            (1.0 - static_cast<double>(pool.available_capacity) / pool.total_capacity) * 100.0 : 0.0;
        
        std::ostringstream oss;
        oss << "  Type " << static_cast<int>(pool.type) << ": " 
            << pool.available_capacity << "/" << pool.total_capacity 
            << " (" << std::fixed << std::setprecision(1) << utilization << "% used)";
        status.push_back(oss.str());
    }
    
    status.push_back("");
    status.push_back("Performance Metrics:");
    status.push_back("  Utilization Efficiency: " + std::to_string(performance_metrics_.resource_utilization_efficiency));
    status.push_back("  Task Completion Rate: " + std::to_string(performance_metrics_.task_completion_rate));
    status.push_back("  Tasks Completed: " + std::to_string(performance_metrics_.tasks_completed));
    status.push_back("  Tasks Failed: " + std::to_string(performance_metrics_.tasks_failed));
    status.push_back("  Emergency Interventions: " + std::to_string(performance_metrics_.emergency_interventions));
    
    return status;
}

double BootstrapResourceManager::getResourceUtilization(ResourceType type) const {
    auto pool_it = resource_pools_.find(type);
    if (pool_it != resource_pools_.end()) {
        const ResourcePool& pool = pool_it->second;
        if (pool.total_capacity > 0) {
            return 1.0 - (static_cast<double>(pool.available_capacity) / pool.total_capacity);
        }
    }
    return 0.0;
}

// Private implementation methods

void BootstrapResourceManager::resourceManagerLoop() {
    while (manager_active_) {
        std::unique_lock<std::mutex> lock(resource_mutex_);
        
        // Wait for tasks or timeout for periodic maintenance
        resource_cv_.wait_for(lock, RESOURCE_CHECK_INTERVAL, [this] {
            return !manager_active_ || !critical_tasks_.empty();
        });
        
        if (!manager_active_) break;
        
        // Process critical tasks
        processCriticalTasks();
        
        // Update resource pools
        updateResourcePools();
        
        // Check emergency conditions
        checkEmergencyConditions();
        
        lock.unlock();
        
        // Brief sleep to prevent excessive CPU usage
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
}

void BootstrapResourceManager::processCriticalTasks() {
    while (!critical_tasks_.empty()) {
        Task task = critical_tasks_.top();
        critical_tasks_.pop();
        
        // Check if resources are available
        if (canAllocateResource(task.required_resource, task.required_amount)) {
            std::string allocation_id = allocateResource(task.task_id, 
                                                       task.required_resource, 
                                                       task.required_amount, 
                                                       task.estimated_duration);
            
            if (!allocation_id.empty()) {
                // Execute the task (in a real implementation, this would be done asynchronously)
                try {
                    if (task.execution_function) {
                        task.execution_function();
                    }
                    recordTaskCompletion(true);
                } catch (...) {
                    recordTaskCompletion(false);
                }
                
                // Release resources after execution
                releaseResource(allocation_id);
            }
        } else {
            // Task failed due to insufficient resources
            recordTaskCompletion(false);
            
            if (task.is_emergency) {
                // Try emergency protocols
                executeEmergencyProtocols();
            }
        }
    }
}

void BootstrapResourceManager::updateResourcePools() {
    for (auto& pool_pair : resource_pools_) {
        ResourcePool& pool = pool_pair.second;
        pool.last_update = std::chrono::steady_clock::now();
    }
    
    // Release expired allocations
    releaseExpiredAllocations();
}

void BootstrapResourceManager::checkEmergencyConditions() {
    bool should_enter_emergency = false;
    
    for (const auto& pool_pair : resource_pools_) {
        const ResourcePool& pool = pool_pair.second;
        double available_ratio = static_cast<double>(pool.available_capacity) / pool.total_capacity;
        
        if (available_ratio < EMERGENCY_THRESHOLD) {
            should_enter_emergency = true;
            break;
        }
    }
    
    if (should_enter_emergency && !emergency_mode_) {
        enterEmergencyMode();
    } else if (!should_enter_emergency && emergency_mode_) {
        exitEmergencyMode();
    }
}

bool BootstrapResourceManager::canAllocateResource(ResourceType type, size_t amount) {
    auto pool_it = resource_pools_.find(type);
    if (pool_it != resource_pools_.end()) {
        const ResourcePool& pool = pool_it->second;
        return pool.available_capacity >= amount;
    }
    return false;
}

std::string BootstrapResourceManager::allocateResource(const std::string& task_id, 
                                                      ResourceType type, 
                                                      size_t amount, 
                                                      std::chrono::milliseconds duration) {
    auto pool_it = resource_pools_.find(type);
    if (pool_it == resource_pools_.end() || pool_it->second.available_capacity < amount) {
        return "";
    }
    
    // Create allocation
    ResourceAllocation allocation;
    allocation.allocation_id = "alloc_" + task_id + "_" + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
    allocation.task_id = task_id;
    allocation.resource_type = type;
    allocation.allocated_amount = amount;
    allocation.allocation_time = std::chrono::steady_clock::now();
    allocation.expected_release_time = allocation.allocation_time + duration;
    
    // Update resource pool
    pool_it->second.available_capacity -= amount;
    pool_it->second.active_allocations.push_back(allocation.allocation_id);
    
    // Store allocation
    active_allocations_.push_back(allocation);
    
    return allocation.allocation_id;
}

void BootstrapResourceManager::releaseResource(const std::string& allocation_id) {
    auto alloc_it = std::find_if(active_allocations_.begin(), active_allocations_.end(),
                                [&allocation_id](const ResourceAllocation& alloc) {
                                    return alloc.allocation_id == allocation_id;
                                });
    
    if (alloc_it != active_allocations_.end()) {
        // Return resources to pool
        auto pool_it = resource_pools_.find(alloc_it->resource_type);
        if (pool_it != resource_pools_.end()) {
            pool_it->second.available_capacity += alloc_it->allocated_amount;
            
            // Remove from active allocations list
            auto& alloc_list = pool_it->second.active_allocations;
            alloc_list.erase(std::remove(alloc_list.begin(), alloc_list.end(), allocation_id), alloc_list.end());
        }
        
        // Remove from active allocations
        active_allocations_.erase(alloc_it);
    }
}

void BootstrapResourceManager::releaseExpiredAllocations() {
    auto now = std::chrono::steady_clock::now();
    std::vector<std::string> expired_allocations;
    
    for (const auto& allocation : active_allocations_) {
        if (now >= allocation.expected_release_time) {
            expired_allocations.push_back(allocation.allocation_id);
        }
    }
    
    for (const std::string& allocation_id : expired_allocations) {
        releaseResource(allocation_id);
    }
}

void BootstrapResourceManager::executeEmergencyProtocols() {
    std::cout << "BOOTSTRAP RESOURCE: Executing emergency protocols..." << std::endl;
    
    // Prioritize critical systems
    prioritizeCriticalSystems();
    
    // Throttle non-essential processes
    throttleNonEssentialProcesses();
    
    // Suspend low priority tasks
    suspendLowPriorityTasks();
    
    recordEmergencyIntervention();
}

void BootstrapResourceManager::suspendLowPriorityTasks() {
    // Release resources from low priority allocations
    std::vector<std::string> to_suspend;
    
    for (const auto& allocation : active_allocations_) {
        // In a full implementation, we would track task priorities
        // For now, suspend long-running allocations
        auto duration = std::chrono::steady_clock::now() - allocation.allocation_time;
        if (duration > std::chrono::minutes(2)) {
            to_suspend.push_back(allocation.allocation_id);
        }
    }
    
    for (const std::string& allocation_id : to_suspend) {
        releaseResource(allocation_id);
    }
    
    std::cout << "BOOTSTRAP RESOURCE: Suspended " << to_suspend.size() << " low priority tasks." << std::endl;
}

void BootstrapResourceManager::defragmentResourcePools() {
    std::cout << "BOOTSTRAP RESOURCE: Defragmenting resource pools..." << std::endl;
    
    // Consolidate fragmented resource allocations
    size_t defragmented_count = 0;
    
    for (auto& [resource_type, pool] : resource_pools_) {
        // Find small fragmented allocations
        std::vector<std::string> small_allocations;
        for (const auto& [alloc_id, allocation] : active_allocations_) {
            if (allocation.resource_type == resource_type && 
                allocation.amount < pool.total_capacity * 0.05) { // Less than 5% of pool
                small_allocations.push_back(alloc_id);
            }
        }
        
        // Consolidate small allocations if beneficial
        if (small_allocations.size() > 3) {
            double total_small = 0.0;
            for (const std::string& alloc_id : small_allocations) {
                total_small += active_allocations_[alloc_id].amount;
            }
            
            // If small allocations together are significant, they're worth consolidating
            if (total_small > pool.total_capacity * 0.1) {
                defragmented_count += small_allocations.size();
            }
        }
        
        // Update pool fragmentation metric
        double fragmentation_ratio = static_cast<double>(small_allocations.size()) / 
                                    std::max(1.0, static_cast<double>(active_allocations_.size()));
        pool.current_usage = pool.allocated_amount; // Refresh usage stats
    }
    
    std::cout << "BOOTSTRAP RESOURCE: Defragmentation complete. Analyzed " 
              << defragmented_count << " fragmented allocations." << std::endl;
}

void BootstrapResourceManager::balanceResourceDistribution() {
    // Redistribute resources based on demand patterns
    redistributeResources();
}

void BootstrapResourceManager::updatePerformanceMetrics() {
    // Calculate resource utilization efficiency
    double total_utilization = 0.0;
    size_t pool_count = 0;
    
    for (const auto& pool_pair : resource_pools_) {
        const ResourcePool& pool = pool_pair.second;
        if (pool.total_capacity > 0) {
            total_utilization += 1.0 - (static_cast<double>(pool.available_capacity) / pool.total_capacity);
            pool_count++;
        }
    }
    
    if (pool_count > 0) {
        performance_metrics_.resource_utilization_efficiency = total_utilization / pool_count;
    }
    
    // Calculate task completion rate
    size_t total_tasks = performance_metrics_.tasks_completed + performance_metrics_.tasks_failed;
    if (total_tasks > 0) {
        performance_metrics_.task_completion_rate = 
            static_cast<double>(performance_metrics_.tasks_completed) / total_tasks;
    }
}

void BootstrapResourceManager::recordTaskCompletion(bool success) {
    if (success) {
        performance_metrics_.tasks_completed++;
    } else {
        performance_metrics_.tasks_failed++;
    }
}

void BootstrapResourceManager::recordEmergencyIntervention() {
    performance_metrics_.emergency_interventions++;
}

} // namespace autonomous
} // namespace opencog