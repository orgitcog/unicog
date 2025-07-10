/*
 * ECANResourceManager.h
 *
 * Economic Attention Networks (ECAN) for distributed multi-agent resource allocation
 * Implements adaptive, economic resource distribution among cognitive agents
 */

#ifndef _OPENCOG_ECAN_RESOURCE_MANAGER_H
#define _OPENCOG_ECAN_RESOURCE_MANAGER_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <mutex>
#include <atomic>
#include <chrono>

namespace opencog {

/**
 * Economic Resource Unit
 * Represents quantized cognitive resources that can be allocated among agents
 */
struct EconomicResource {
    std::string resource_type;
    double quantity;
    double unit_cost;
    double allocation_priority;
    std::chrono::steady_clock::time_point last_update;
};

/**
 * Agent Economic Profile
 * Tracks economic metrics and resource usage for each cognitive agent
 */
struct AgentEconomicProfile {
    std::string agent_id;
    double economic_fitness;
    double resource_utilization_efficiency;
    double contribution_to_system;
    std::map<std::string, double> resource_allocations;
    std::map<std::string, double> resource_demands;
    std::vector<double> performance_history;
    double economic_reputation;
};

/**
 * ECAN Resource Manager
 * 
 * Implements Economic Attention Networks for dynamic resource allocation
 * among distributed cognitive agents. Uses economic principles to optimize
 * resource distribution based on agent performance and system needs.
 */
class ECANResourceManager
{
private:
    // Economic system state
    std::map<std::string, EconomicResource> available_resources_;
    std::map<std::string, AgentEconomicProfile> agent_profiles_;
    
    // Economic parameters
    double total_system_resources_;
    double resource_inflation_rate_;
    double economic_fairness_threshold_;
    double performance_decay_factor_;
    
    // Synchronization
    mutable std::shared_mutex econ_mutex_;
    std::atomic<uint64_t> economic_cycle_count_;
    
    // Resource allocation strategies
    enum AllocationStrategy {
        PERFORMANCE_BASED,
        FAIRNESS_BALANCED,
        DEMAND_DRIVEN,
        ADAPTIVE_HYBRID
    };
    AllocationStrategy current_strategy_;

public:
    ECANResourceManager(double total_resources = 1000.0);
    ~ECANResourceManager();

    /**
     * Register a new cognitive agent in the economic system
     * 
     * @param agent_id Unique identifier for the agent
     * @param initial_allocation Initial resource allocation
     */
    void register_agent(const std::string& agent_id, double initial_allocation = 10.0);

    /**
     * Update agent performance metrics for economic evaluation
     * 
     * @param agent_id Agent identifier
     * @param performance_score Current performance metric (0.0 - 1.0)
     * @param resource_efficiency How efficiently agent uses resources
     * @param system_contribution Agent's contribution to overall system performance
     */
    void update_agent_performance(const std::string& agent_id,
                                 double performance_score,
                                 double resource_efficiency,
                                 double system_contribution);

    /**
     * Request resources for a specific agent
     * 
     * @param agent_id Agent requesting resources
     * @param resource_type Type of resource needed
     * @param requested_amount Amount of resource requested
     * @return Amount of resource actually allocated
     */
    double request_resources(const std::string& agent_id,
                           const std::string& resource_type,
                           double requested_amount);

    /**
     * Perform economic cycle - redistribute resources based on performance
     * This is the core ECAN algorithm that dynamically allocates resources
     */
    void perform_economic_cycle();

    /**
     * Get current resource allocation for an agent
     * 
     * @param agent_id Agent identifier
     * @param resource_type Type of resource to query
     * @return Current allocation amount
     */
    double get_agent_resource_allocation(const std::string& agent_id,
                                       const std::string& resource_type) const;

    /**
     * Get economic fitness score for an agent
     * 
     * @param agent_id Agent identifier
     * @return Current economic fitness (0.0 - 1.0)
     */
    double get_agent_economic_fitness(const std::string& agent_id) const;

    /**
     * Set resource allocation strategy
     */
    void set_allocation_strategy(AllocationStrategy strategy) {
        current_strategy_ = strategy;
    }

    /**
     * Get system-wide economic statistics
     */
    struct EconomicStats {
        double total_allocated_resources;
        double average_agent_fitness;
        double resource_utilization_rate;
        double economic_inequality_index;
        uint64_t total_cycles;
        std::vector<std::string> top_performing_agents;
    };
    EconomicStats get_economic_statistics() const;

    /**
     * Adjust economic parameters for system tuning
     */
    void set_economic_parameters(double inflation_rate = 0.01,
                               double fairness_threshold = 0.8,
                               double decay_factor = 0.95);

    /**
     * Force resource redistribution based on fairness criteria
     */
    void enforce_economic_fairness();

    /**
     * Get list of all registered agents
     */
    std::vector<std::string> get_registered_agents() const;

    /**
     * Remove agent from economic system
     */
    void unregister_agent(const std::string& agent_id);

private:
    /**
     * Calculate economic fitness based on multiple factors
     */
    double calculate_economic_fitness(const AgentEconomicProfile& profile);

    /**
     * Implement performance-based allocation strategy
     */
    void allocate_by_performance();

    /**
     * Implement fairness-balanced allocation strategy
     */
    void allocate_by_fairness();

    /**
     * Implement demand-driven allocation strategy
     */
    void allocate_by_demand();

    /**
     * Implement adaptive hybrid allocation strategy
     */
    void allocate_adaptive_hybrid();

    /**
     * Calculate Gini coefficient for resource inequality
     */
    double calculate_resource_inequality() const;

    /**
     * Update resource demands based on agent behavior
     */
    void update_resource_demands();
};

} // namespace opencog

#endif // _OPENCOG_ECAN_RESOURCE_MANAGER_H