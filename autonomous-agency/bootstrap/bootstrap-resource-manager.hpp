#ifndef BOOTSTRAP_RESOURCE_MANAGER_HPP
#define BOOTSTRAP_RESOURCE_MANAGER_HPP

#include <vector>
#include <queue>
#include <memory>
#include <mutex>
#include <atomic>
#include <chrono>
#include <thread>
#include <functional>

namespace opencog {
namespace autonomous {

/**
 * Manages computational and memory resources for bootstrap operations,
 * ensuring critical systems have priority access during emergency scenarios.
 */
class BootstrapResourceManager {
public:
    enum class ResourceType {
        COMPUTATIONAL,
        MEMORY,
        ATTENTION,
        STORAGE,
        NETWORK
    };

    enum class TaskPriority {
        CRITICAL,     // System survival tasks
        HIGH,         // Important maintenance
        MEDIUM,       // Regular operations
        LOW,          // Background tasks
        DEFERRED      // Can be postponed indefinitely
    };

    struct ResourcePool {
        ResourceType type;
        size_t total_capacity;
        size_t available_capacity;
        size_t reserved_capacity;
        std::vector<std::string> active_allocations;
        std::chrono::time_point<std::chrono::steady_clock> last_update;
    };

    struct Task {
        std::string task_id;
        TaskPriority priority;
        ResourceType required_resource;
        size_t required_amount;
        std::chrono::milliseconds estimated_duration;
        std::function<void()> execution_function;
        std::chrono::time_point<std::chrono::steady_clock> submission_time;
        bool is_emergency;
        
        // Comparison operator for priority queue
        bool operator<(const Task& other) const {
            if (is_emergency != other.is_emergency) {
                return !is_emergency; // Emergency tasks have higher priority
            }
            return priority > other.priority; // Lower enum value = higher priority
        }
    };

    struct ResourceAllocation {
        std::string allocation_id;
        std::string task_id;
        ResourceType resource_type;
        size_t allocated_amount;
        std::chrono::time_point<std::chrono::steady_clock> allocation_time;
        std::chrono::time_point<std::chrono::steady_clock> expected_release_time;
    };

    struct PerformanceMetrics {
        double resource_utilization_efficiency;
        double task_completion_rate;
        double emergency_response_time;
        size_t tasks_completed;
        size_t tasks_failed;
        size_t emergency_interventions;
    };

private:
    std::map<ResourceType, ResourcePool> resource_pools_;
    std::priority_queue<Task> critical_tasks_;
    std::vector<ResourceAllocation> active_allocations_;
    
    // Thread management
    std::unique_ptr<std::thread> resource_manager_thread_;
    std::atomic<bool> manager_active_;
    std::mutex resource_mutex_;
    std::condition_variable resource_cv_;
    
    // Performance tracking
    PerformanceMetrics performance_metrics_;
    
    // Emergency state management
    std::atomic<bool> emergency_mode_;
    std::chrono::time_point<std::chrono::steady_clock> emergency_start_time_;
    
    // Configuration
    static constexpr double EMERGENCY_THRESHOLD = 0.1; // 10% available resources triggers emergency
    static constexpr std::chrono::seconds RESOURCE_CHECK_INTERVAL{5};

public:
    BootstrapResourceManager();
    ~BootstrapResourceManager();
    
    // Resource allocation for survival operations
    bool allocateEmergencyResources(const std::string& task_id, 
                                   ResourceType resource_type, 
                                   size_t amount);
    void prioritizeCriticalSystems();
    void throttleNonEssentialProcesses();
    
    // Dynamic reallocation operations
    void redistributeResources();
    void scaleResourcePools(ResourceType type, double scale_factor);
    void optimizeResourceUtilization();
    
    // Task management
    bool submitTask(const Task& task);
    bool submitEmergencyTask(const Task& task);
    void cancelTask(const std::string& task_id);
    std::vector<std::string> getRunningTasks();
    
    // Resource monitoring and control
    void startResourceManager();
    void stopResourceManager();
    void enterEmergencyMode();
    void exitEmergencyMode();
    bool isInEmergencyMode() const { return emergency_mode_; }
    
    // Resource pool management
    void initializeResourcePool(ResourceType type, size_t capacity);
    void expandResourcePool(ResourceType type, size_t additional_capacity);
    void setResourceReservation(ResourceType type, size_t reserved_amount);
    
    // Monitoring and reporting
    PerformanceMetrics getPerformanceMetrics() const;
    std::vector<std::string> getResourceStatus();
    double getResourceUtilization(ResourceType type) const;
    
    // Configuration
    void setEmergencyThreshold(double threshold);
    void setResourceCheckInterval(std::chrono::seconds interval);

private:
    // Internal resource management
    void resourceManagerLoop();
    void processCriticalTasks();
    void updateResourcePools();
    void checkEmergencyConditions();
    
    // Allocation helpers
    bool canAllocateResource(ResourceType type, size_t amount);
    std::string allocateResource(const std::string& task_id, 
                               ResourceType type, 
                               size_t amount, 
                               std::chrono::milliseconds duration);
    void releaseResource(const std::string& allocation_id);
    void releaseExpiredAllocations();
    
    // Emergency response helpers
    void executeEmergencyProtocols();
    void suspendLowPriorityTasks();
    void reallocateResourcesForEmergency();
    
    // Optimization helpers
    void defragmentResourcePools();
    void balanceResourceDistribution();
    void predictResourceNeeds();
    
    // Performance tracking helpers
    void updatePerformanceMetrics();
    void recordTaskCompletion(bool success);
    void recordEmergencyIntervention();
};

} // namespace autonomous
} // namespace opencog

#endif // BOOTSTRAP_RESOURCE_MANAGER_HPP