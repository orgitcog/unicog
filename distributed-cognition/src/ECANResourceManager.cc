/*
 * ECANResourceManager.cc
 *
 * Implementation of Economic Attention Networks (ECAN) for distributed
 * multi-agent resource allocation
 */

#include "../include/ECANResourceManager.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <iostream>
#include <iomanip>

namespace opencog {

ECANResourceManager::ECANResourceManager(double total_resources)
    : total_system_resources_(total_resources)
    , resource_inflation_rate_(0.01)
    , economic_fairness_threshold_(0.8)
    , performance_decay_factor_(0.95)
    , economic_cycle_count_(0)
    , current_strategy_(ADAPTIVE_HYBRID)
{
    // Initialize default resource types
    available_resources_["processing"] = {
        "processing", total_resources * 0.4, 1.0, 1.0, std::chrono::steady_clock::now()
    };
    available_resources_["memory"] = {
        "memory", total_resources * 0.3, 1.0, 0.8, std::chrono::steady_clock::now()
    };
    available_resources_["communication"] = {
        "communication", total_resources * 0.2, 1.0, 0.6, std::chrono::steady_clock::now()
    };
    available_resources_["attention"] = {
        "attention", total_resources * 0.1, 1.0, 1.2, std::chrono::steady_clock::now()
    };
}

ECANResourceManager::~ECANResourceManager() = default;

void ECANResourceManager::register_agent(const std::string& agent_id, double initial_allocation) {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    
    if (agent_profiles_.find(agent_id) != agent_profiles_.end()) {
        return; // Agent already registered
    }
    
    AgentEconomicProfile profile;
    profile.agent_id = agent_id;
    profile.economic_fitness = 0.5; // Neutral starting fitness
    profile.resource_utilization_efficiency = 0.5;
    profile.contribution_to_system = 0.0;
    profile.economic_reputation = 0.5;
    
    // Distribute initial allocation across resource types
    double per_resource_allocation = initial_allocation / available_resources_.size();
    for (const auto& [resource_type, resource] : available_resources_) {
        profile.resource_allocations[resource_type] = per_resource_allocation;
        profile.resource_demands[resource_type] = per_resource_allocation;
    }
    
    agent_profiles_[agent_id] = profile;
    
    std::cout << "ECAN: Registered agent " << agent_id 
              << " with initial allocation " << initial_allocation << std::endl;
}

void ECANResourceManager::update_agent_performance(const std::string& agent_id,
                                                  double performance_score,
                                                  double resource_efficiency,
                                                  double system_contribution) {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    
    auto it = agent_profiles_.find(agent_id);
    if (it == agent_profiles_.end()) {
        return; // Agent not registered
    }
    
    AgentEconomicProfile& profile = it->second;
    
    // Update performance history with decay
    profile.performance_history.push_back(performance_score);
    if (profile.performance_history.size() > 10) {
        profile.performance_history.erase(profile.performance_history.begin());
    }
    
    // Update efficiency and contribution
    profile.resource_utilization_efficiency = 
        0.7 * profile.resource_utilization_efficiency + 0.3 * resource_efficiency;
    profile.contribution_to_system = 
        0.8 * profile.contribution_to_system + 0.2 * system_contribution;
    
    // Recalculate economic fitness
    profile.economic_fitness = calculate_economic_fitness(profile);
    
    // Update reputation based on sustained performance
    double avg_performance = std::accumulate(profile.performance_history.begin(),
                                           profile.performance_history.end(), 0.0) 
                           / profile.performance_history.size();
    profile.economic_reputation = 0.9 * profile.economic_reputation + 0.1 * avg_performance;
}

double ECANResourceManager::request_resources(const std::string& agent_id,
                                            const std::string& resource_type,
                                            double requested_amount) {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    
    auto agent_it = agent_profiles_.find(agent_id);
    auto resource_it = available_resources_.find(resource_type);
    
    if (agent_it == agent_profiles_.end() || resource_it == available_resources_.end()) {
        return 0.0; // Agent or resource type not found
    }
    
    AgentEconomicProfile& profile = agent_it->second;
    EconomicResource& resource = resource_it->second;
    
    // Calculate allocation based on economic fitness and current allocation
    double current_allocation = profile.resource_allocations[resource_type];
    double allocation_multiplier = std::min(profile.economic_fitness * 1.5, 2.0);
    double max_allocation = current_allocation * allocation_multiplier;
    
    double granted_amount = std::min(requested_amount, max_allocation);
    granted_amount = std::min(granted_amount, resource.quantity * 0.1); // Max 10% of total resource
    
    // Update resource demand tracking
    profile.resource_demands[resource_type] = 
        0.8 * profile.resource_demands[resource_type] + 0.2 * requested_amount;
    
    // Deduct from available resources temporarily (will be restored in next cycle)
    resource.quantity -= granted_amount;
    resource.last_update = std::chrono::steady_clock::now();
    
    return granted_amount;
}

void ECANResourceManager::perform_economic_cycle() {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    
    economic_cycle_count_++;
    
    // Restore resource quantities for this cycle
    for (auto& [type, resource] : available_resources_) {
        double base_quantity = total_system_resources_ * 
                              (type == "processing" ? 0.4 : 
                               type == "memory" ? 0.3 :
                               type == "communication" ? 0.2 : 0.1);
        resource.quantity = base_quantity * (1.0 + resource_inflation_rate_ * economic_cycle_count_);
    }
    
    // Apply performance decay to all agents
    for (auto& [agent_id, profile] : agent_profiles_) {
        profile.economic_fitness *= performance_decay_factor_;
        profile.economic_reputation *= performance_decay_factor_;
    }
    
    // Update resource demands
    update_resource_demands();
    
    // Phase 2 enhancement: Dynamic strategy selection based on system state
    double inequality = calculate_resource_inequality();
    if (inequality > economic_fairness_threshold_) {
        current_strategy_ = FAIRNESS_BALANCED;
    } else if (economic_cycle_count_ % 10 == 0) {
        current_strategy_ = ADAPTIVE_HYBRID; // Periodic rebalancing
    }
    
    // Apply selected allocation strategy
    switch (current_strategy_) {
        case PERFORMANCE_BASED:
            allocate_by_performance();
            break;
        case FAIRNESS_BALANCED:
            allocate_by_fairness();
            break;
        case DEMAND_DRIVEN:
            allocate_by_demand();
            break;
        case ADAPTIVE_HYBRID:
            allocate_adaptive_hybrid();
            break;
    }
    
    std::cout << "ECAN: Completed economic cycle " << economic_cycle_count_ 
              << " with " << agent_profiles_.size() << " agents"
              << ", inequality: " << std::fixed << std::setprecision(3) << inequality
              << ", strategy: " << static_cast<int>(current_strategy_) << std::endl;
}

double ECANResourceManager::get_agent_resource_allocation(const std::string& agent_id,
                                                        const std::string& resource_type) const {
    std::shared_lock<std::shared_mutex> lock(econ_mutex_);
    
    auto agent_it = agent_profiles_.find(agent_id);
    if (agent_it == agent_profiles_.end()) {
        return 0.0;
    }
    
    auto alloc_it = agent_it->second.resource_allocations.find(resource_type);
    return (alloc_it != agent_it->second.resource_allocations.end()) ? alloc_it->second : 0.0;
}

double ECANResourceManager::get_agent_economic_fitness(const std::string& agent_id) const {
    std::shared_lock<std::shared_mutex> lock(econ_mutex_);
    
    auto it = agent_profiles_.find(agent_id);
    return (it != agent_profiles_.end()) ? it->second.economic_fitness : 0.0;
}

ECANResourceManager::EconomicStats ECANResourceManager::get_economic_statistics() const {
    std::shared_lock<std::shared_mutex> lock(econ_mutex_);
    
    EconomicStats stats;
    stats.total_cycles = economic_cycle_count_;
    
    if (agent_profiles_.empty()) {
        stats.total_allocated_resources = 0.0;
        stats.average_agent_fitness = 0.0;
        stats.resource_utilization_rate = 0.0;
        stats.economic_inequality_index = 0.0;
        return stats;
    }
    
    double total_allocated = 0.0;
    double total_fitness = 0.0;
    std::vector<std::pair<std::string, double>> agent_fitness_pairs;
    
    for (const auto& [agent_id, profile] : agent_profiles_) {
        double agent_total_allocation = std::accumulate(
            profile.resource_allocations.begin(),
            profile.resource_allocations.end(),
            0.0,
            [](double sum, const auto& pair) { return sum + pair.second; }
        );
        
        total_allocated += agent_total_allocation;
        total_fitness += profile.economic_fitness;
        agent_fitness_pairs.emplace_back(agent_id, profile.economic_fitness);
    }
    
    stats.total_allocated_resources = total_allocated;
    stats.average_agent_fitness = total_fitness / agent_profiles_.size();
    stats.resource_utilization_rate = total_allocated / total_system_resources_;
    stats.economic_inequality_index = calculate_resource_inequality();
    
    // Sort agents by fitness and get top performers
    std::sort(agent_fitness_pairs.begin(), agent_fitness_pairs.end(),
              [](const auto& a, const auto& b) { return a.second > b.second; });
    
    for (size_t i = 0; i < std::min(size_t(5), agent_fitness_pairs.size()); ++i) {
        stats.top_performing_agents.push_back(agent_fitness_pairs[i].first);
    }
    
    return stats;
}

void ECANResourceManager::set_economic_parameters(double inflation_rate,
                                                double fairness_threshold,
                                                double decay_factor) {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    resource_inflation_rate_ = inflation_rate;
    economic_fairness_threshold_ = fairness_threshold;
    performance_decay_factor_ = decay_factor;
}

void ECANResourceManager::enforce_economic_fairness() {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    
    // Calculate average allocations
    std::map<std::string, double> avg_allocations;
    for (const auto& [resource_type, resource] : available_resources_) {
        avg_allocations[resource_type] = 0.0;
    }
    
    for (const auto& [agent_id, profile] : agent_profiles_) {
        for (const auto& [resource_type, allocation] : profile.resource_allocations) {
            avg_allocations[resource_type] += allocation;
        }
    }
    
    for (auto& [resource_type, total] : avg_allocations) {
        total /= agent_profiles_.size();
    }
    
    // Redistribute to enforce fairness
    for (auto& [agent_id, profile] : agent_profiles_) {
        for (auto& [resource_type, allocation] : profile.resource_allocations) {
            double fair_allocation = avg_allocations[resource_type];
            allocation = 0.8 * allocation + 0.2 * fair_allocation;
        }
    }
    
    std::cout << "ECAN: Enforced economic fairness redistribution" << std::endl;
}

std::vector<std::string> ECANResourceManager::get_registered_agents() const {
    std::shared_lock<std::shared_mutex> lock(econ_mutex_);
    
    std::vector<std::string> agents;
    for (const auto& [agent_id, profile] : agent_profiles_) {
        agents.push_back(agent_id);
    }
    return agents;
}

void ECANResourceManager::unregister_agent(const std::string& agent_id) {
    std::unique_lock<std::shared_mutex> lock(econ_mutex_);
    agent_profiles_.erase(agent_id);
    std::cout << "ECAN: Unregistered agent " << agent_id << std::endl;
}

// Private methods implementation

double ECANResourceManager::calculate_economic_fitness(const AgentEconomicProfile& profile) {
    if (profile.performance_history.empty()) {
        return 0.5; // Neutral fitness for new agents
    }
    
    // Weighted combination of multiple factors
    double avg_performance = std::accumulate(profile.performance_history.begin(),
                                           profile.performance_history.end(), 0.0) 
                           / profile.performance_history.size();
    
    double fitness = 0.4 * avg_performance +
                    0.3 * profile.resource_utilization_efficiency +
                    0.2 * profile.contribution_to_system +
                    0.1 * profile.economic_reputation;
    
    return std::max(0.0, std::min(1.0, fitness));
}

void ECANResourceManager::allocate_by_performance() {
    // Calculate total fitness for normalization
    double total_fitness = 0.0;
    for (const auto& [agent_id, profile] : agent_profiles_) {
        total_fitness += profile.economic_fitness;
    }
    
    if (total_fitness == 0.0) {
        allocate_by_fairness(); // Fallback to fair allocation
        return;
    }
    
    // Allocate proportionally to fitness
    for (auto& [agent_id, profile] : agent_profiles_) {
        double fitness_ratio = profile.economic_fitness / total_fitness;
        
        for (auto& [resource_type, allocation] : profile.resource_allocations) {
            double base_allocation = available_resources_[resource_type].quantity * fitness_ratio;
            allocation = 0.7 * allocation + 0.3 * base_allocation;
        }
    }
}

void ECANResourceManager::allocate_by_fairness() {
    if (agent_profiles_.empty()) return;
    
    // Equal allocation among all agents
    for (const auto& [resource_type, resource] : available_resources_) {
        double per_agent_allocation = resource.quantity / agent_profiles_.size();
        
        for (auto& [agent_id, profile] : agent_profiles_) {
            profile.resource_allocations[resource_type] = 
                0.8 * profile.resource_allocations[resource_type] + 0.2 * per_agent_allocation;
        }
    }
}

void ECANResourceManager::allocate_by_demand() {
    // Calculate total demand for each resource type
    std::map<std::string, double> total_demands;
    for (const auto& [resource_type, resource] : available_resources_) {
        total_demands[resource_type] = 0.0;
    }
    
    for (const auto& [agent_id, profile] : agent_profiles_) {
        for (const auto& [resource_type, demand] : profile.resource_demands) {
            total_demands[resource_type] += demand;
        }
    }
    
    // Allocate proportionally to demand
    for (auto& [agent_id, profile] : agent_profiles_) {
        for (auto& [resource_type, allocation] : profile.resource_allocations) {
            if (total_demands[resource_type] > 0.0) {
                double demand_ratio = profile.resource_demands[resource_type] / total_demands[resource_type];
                double demand_allocation = available_resources_[resource_type].quantity * demand_ratio;
                allocation = 0.6 * allocation + 0.4 * demand_allocation;
            }
        }
    }
}

void ECANResourceManager::allocate_adaptive_hybrid() {
    // Adaptive strategy that balances performance, fairness, and demand
    double inequality_index = calculate_resource_inequality();
    
    if (inequality_index > economic_fairness_threshold_) {
        // High inequality - prioritize fairness
        allocate_by_fairness();
    } else {
        // Low inequality - use hybrid performance-demand allocation
        allocate_by_performance();
        
        // Adjust based on demand
        for (auto& [agent_id, profile] : agent_profiles_) {
            for (auto& [resource_type, allocation] : profile.resource_allocations) {
                double demand_factor = profile.resource_demands[resource_type] / 
                                     (allocation + 1e-6); // Avoid division by zero
                demand_factor = std::min(demand_factor, 2.0); // Cap at 2x
                allocation *= (0.8 + 0.2 * demand_factor);
            }
        }
    }
}

double ECANResourceManager::calculate_resource_inequality() const {
    if (agent_profiles_.size() < 2) return 0.0;
    
    // Calculate Gini coefficient for resource distribution
    std::vector<double> total_allocations;
    for (const auto& [agent_id, profile] : agent_profiles_) {
        double total = std::accumulate(
            profile.resource_allocations.begin(),
            profile.resource_allocations.end(),
            0.0,
            [](double sum, const auto& pair) { return sum + pair.second; }
        );
        total_allocations.push_back(total);
    }
    
    std::sort(total_allocations.begin(), total_allocations.end());
    
    double sum = 0.0;
    double weighted_sum = 0.0;
    for (size_t i = 0; i < total_allocations.size(); ++i) {
        sum += total_allocations[i];
        weighted_sum += (i + 1) * total_allocations[i];
    }
    
    if (sum == 0.0) return 0.0;
    
    size_t n = total_allocations.size();
    return (2.0 * weighted_sum) / (n * sum) - (n + 1.0) / n;
}

void ECANResourceManager::update_resource_demands() {
    // Update resource demands based on recent allocation patterns and usage
    for (auto& [agent_id, profile] : agent_profiles_) {
        for (auto& [resource_type, demand] : profile.resource_demands) {
            double current_allocation = profile.resource_allocations[resource_type];
            double utilization = profile.resource_utilization_efficiency;
            
            // Increase demand if highly utilizing current allocation
            if (utilization > 0.8) {
                demand *= 1.1;
            } else if (utilization < 0.3) {
                demand *= 0.9;
            }
            
            // Keep demand within reasonable bounds
            demand = std::max(0.1, std::min(demand, current_allocation * 3.0));
        }
    }
}

} // namespace opencog