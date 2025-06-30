/*
 * SharedHypergraphContext.h
 * 
 * Phase II.3: Shared hypergraph context for agent synchronization
 */

#ifndef _OPENCOG_SHARED_HYPERGRAPH_CONTEXT_H
#define _OPENCOG_SHARED_HYPERGRAPH_CONTEXT_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <mutex>
#include <shared_mutex>

namespace opencog {

/**
 * Shared Hypergraph Context
 * 
 * Synchronized shared state for distributed cognitive agents.
 * Enables agent state synchronization via shared hypergraph context.
 */
class SharedHypergraphContext
{
private:
    // Shared hypergraph state
    std::map<std::string, std::vector<double>> hypergraph_nodes_;
    std::map<std::string, std::pair<std::vector<std::string>, std::vector<double>>> hypergraph_edges_;
    
    // Agent state tracking
    std::map<std::string, std::vector<double>> agent_states_;
    std::map<std::string, std::chrono::steady_clock::time_point> last_update_times_;
    
    // Thread synchronization
    mutable std::shared_mutex context_mutex_;
    
    // Context versioning for consistency
    std::atomic<uint64_t> context_version_;

public:
    SharedHypergraphContext();
    ~SharedHypergraphContext();

    /**
     * Update hypergraph node state
     * Thread-safe update of shared hypergraph structure
     * 
     * @param node_id Identifier for the hypergraph node
     * @param node_state New state vector for the node
     */
    void update_hypergraph_node(const std::string& node_id, 
                               const std::vector<double>& node_state);

    /**
     * Update hypergraph edge connections and weights
     * 
     * @param edge_id Identifier for the hypergraph edge
     * @param connected_nodes Vector of node IDs connected by this edge
     * @param edge_weights Weight values for the connections
     */
    void update_hypergraph_edge(const std::string& edge_id,
                               const std::vector<std::string>& connected_nodes,
                               const std::vector<double>& edge_weights);

    /**
     * Update agent state in shared context
     * 
     * @param agent_id Identifier of the agent
     * @param agent_state Current state vector of the agent
     */
    void update_agent_state(const std::string& agent_id,
                           const std::vector<double>& agent_state);

    /**
     * Get current hypergraph node state (thread-safe read)
     * 
     * @param node_id Identifier for the requested node
     * @return Node state vector (empty if node doesn't exist)
     */
    std::vector<double> get_hypergraph_node(const std::string& node_id) const;

    /**
     * Get current hypergraph edge information
     * 
     * @param edge_id Identifier for the requested edge
     * @return Pair of (connected_nodes, edge_weights)
     */
    std::pair<std::vector<std::string>, std::vector<double>> 
    get_hypergraph_edge(const std::string& edge_id) const;

    /**
     * Get current agent state from shared context
     * 
     * @param agent_id Identifier of the requested agent
     * @return Agent state vector (empty if agent not found)
     */
    std::vector<double> get_agent_state(const std::string& agent_id) const;

    /**
     * Get all agent states for distributed processing
     * 
     * @return Map of agent_id -> state_vector for all agents
     */
    std::map<std::string, std::vector<double>> get_all_agent_states() const;

    /**
     * Get aggregated context state for cognitive processing
     * Combines hypergraph and agent states into unified context
     * 
     * @return Aggregated context vector for agent processing
     */
    std::vector<double> get_aggregated_context() const;

    /**
     * Synchronize context with external updates
     * Handles consistency and version management
     * 
     * @param external_updates Map of external state updates
     * @return Success flag
     */
    bool synchronize_context(const std::map<std::string, std::vector<double>>& external_updates);

    /**
     * Get current context version for consistency checking
     */
    uint64_t get_context_version() const { return context_version_.load(); }

    /**
     * Clear stale agent states (agents that haven't updated recently)
     * 
     * @param timeout_seconds Age threshold for considering states stale
     */
    void cleanup_stale_states(double timeout_seconds = 30.0);

    /**
     * Get context statistics for monitoring
     */
    struct ContextStats {
        size_t num_nodes;
        size_t num_edges;
        size_t num_agents;
        uint64_t version;
        double avg_update_frequency;
    };
    ContextStats get_context_statistics() const;

private:
    /**
     * Internal helper to compute aggregated state
     */
    std::vector<double> compute_aggregated_state() const;
};

} // namespace opencog

#endif // _OPENCOG_SHARED_HYPERGRAPH_CONTEXT_H