/**
 * ECANAgent.h
 *
 * Economic Attention Networks Agent implementation
 * Implements the core ECAN algorithms for attention allocation
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ECAN_AGENT_H
#define _OPENCOG_ECAN_AGENT_H

#include <memory>
#include <queue>
#include <vector>
#include <mutex>
#include <chrono>
#include <unordered_map>
#include <opencog/atoms/base/Atom.h>
#include <opencog/attention/AttentionBank.h>

namespace opencog
{

class AtomSpace;

/**
 * Priority-based resource request for attention allocation
 */
struct AttentionRequest {
    Handle atom;
    double priority;
    double requested_amount;
    std::chrono::steady_clock::time_point timestamp;
    std::string request_type;
    
    AttentionRequest(Handle h, double prio, double amount, const std::string& type)
        : atom(h), priority(prio), requested_amount(amount), 
          timestamp(std::chrono::steady_clock::now()), request_type(type) {}
          
    bool operator<(const AttentionRequest& other) const {
        return priority < other.priority; // For priority queue (max-heap)
    }
};

/**
 * ECANAgent implements the Economic Attention Networks framework
 * for dynamic attention allocation based on economic principles
 * Enhanced with Phase 2 requirements: priority queues, resource scheduling
 */
class ECANAgent  
{
private:
    AtomSpace* atomSpace;
    std::shared_ptr<AttentionBank> attentionBank;
    
    // ECAN parameters
    double rentRate = 0.01;           // Rent charged per cycle
    double wageRate = 0.05;           // Wage paid for cognitive contribution
    double taxRate = 0.02;            // Tax for global attention management
    double spreadingRate = 0.1;       // Rate of attention spreading
    
    // Economic cycles
    int cycleCount = 0;
    int rentCycle = 10;               // Rent every N cycles
    int wageCycle = 5;                // Wage every N cycles  
    int taxCycle = 20;                // Tax every N cycles
    
    // Phase 2 enhancements: Priority queue-based resource scheduling
    std::priority_queue<AttentionRequest> attention_request_queue;
    std::mutex request_queue_mutex;
    
    // Attention decay and refresh mechanisms
    double attention_decay_factor = 0.95;
    double refresh_threshold = 0.1;
    std::unordered_map<Handle, double> attention_refresh_rates;
    
    // Cross-agent synchronization for distributed mesh
    std::vector<std::string> connected_agents;
    std::mutex sync_mutex;
    std::chrono::steady_clock::time_point last_sync_time;
    
public:
    // Constructor
    ECANAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab);
    
    // Destructor
    ~ECANAgent() = default;
    
    // Main ECAN cycle
    void runCycle();
    
    // Economic operations
    void collectRent();
    void payWages();
    void collectTaxes();
    
    // Attention spreading algorithms
    void spreadAttention();
    void spreadAttentionBetween(Handle source, Handle target, double amount);
    
    // Phase 2 enhancements: Priority-based resource scheduling
    void processAttentionRequests();
    void scheduleAttentionRequest(Handle atom, double priority, double amount, 
                                const std::string& type = "standard");
    void executeHighPriorityRequest(const AttentionRequest& request);
    
    // Attention decay and refresh mechanisms  
    void applyAttentionDecay();
    void refreshAttentionValues();
    void setRefreshRate(Handle atom, double rate);
    
    // Cross-agent attention synchronization
    void synchronizeWithAgents();
    void sendAttentionUpdate(const std::string& agent_id, Handle atom, double value);
    void receiveAttentionUpdate(Handle atom, double value, const std::string& from_agent);
    void registerConnectedAgent(const std::string& agent_id);
    
    // Attention conflict resolution
    void resolveAttentionConflict(Handle atom, const std::vector<double>& conflicting_values);
    double computeConflictResolution(const std::vector<double>& values);
    
    // Parameter management
    void setRentRate(double rate) { rentRate = rate; }
    void setWageRate(double rate) { wageRate = rate; }
    void setTaxRate(double rate) { taxRate = rate; }
    void setSpreadingRate(double rate) { spreadingRate = rate; }
    
    double getRentRate() const { return rentRate; }
    double getWageRate() const { return wageRate; }
    double getTaxRate() const { return taxRate; }
    double getSpreadingRate() const { return spreadingRate; }
    
    // Cycle management
    void setRentCycle(int cycle) { rentCycle = cycle; }
    void setWageCycle(int cycle) { wageCycle = cycle; }
    void setTaxCycle(int cycle) { taxCycle = cycle; }
    
    int getRentCycle() const { return rentCycle; }
    int getWageCycle() const { return wageCycle; }
    int getTaxCycle() const { return taxCycle; }
    
    // Statistics
    int getCycleCount() const { return cycleCount; }
    void resetCycleCount() { cycleCount = 0; }
    
    // Phase 2 enhancements: Additional parameters and statistics
    void setAttentionDecayFactor(double factor) { attention_decay_factor = factor; }
    double getAttentionDecayFactor() const { return attention_decay_factor; }
    void setRefreshThreshold(double threshold) { refresh_threshold = threshold; }
    double getRefreshThreshold() const { return refresh_threshold; }
    
    size_t getPendingRequestCount() const { return attention_request_queue.size(); }
    size_t getConnectedAgentCount() const { return connected_agents.size(); }
    std::vector<std::string> getConnectedAgents() const { return connected_agents; }
    
    // Performance and fairness metrics
    struct AttentionMetrics {
        double total_attention_allocated;
        double average_request_processing_time;
        double attention_distribution_fairness;  // Gini coefficient
        double conflict_resolution_rate;
        size_t total_requests_processed;
        size_t total_conflicts_resolved;
    };
    AttentionMetrics computePerformanceMetrics() const;
};

} // namespace opencog

#endif // _OPENCOG_ECAN_AGENT_H