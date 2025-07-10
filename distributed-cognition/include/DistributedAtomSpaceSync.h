/*
 * DistributedAtomSpaceSync.h
 *
 * Distributed AtomSpace synchronization primitives for multi-agent cognition
 * Enables consistent, efficient synchronization of knowledge across agents
 */

#ifndef _OPENCOG_DISTRIBUTED_ATOMSPACE_SYNC_H
#define _OPENCOG_DISTRIBUTED_ATOMSPACE_SYNC_H

#include <memory>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <mutex>
#include <shared_mutex>
#include <atomic>
#include <chrono>
#include <functional>

namespace opencog {

/**
 * Atom Synchronization Record
 * Tracks synchronization state for individual atoms
 */
struct AtomSyncRecord {
    std::string atom_id;
    std::string atom_type;
    std::vector<double> atom_data;
    uint64_t version;
    std::chrono::steady_clock::time_point last_modified;
    std::set<std::string> synchronized_agents;
    bool requires_sync;
    double sync_priority;
};

/**
 * Synchronization Conflict Resolution
 * Handles conflicts when multiple agents modify the same atom
 */
enum ConflictResolution {
    LAST_WRITER_WINS,
    HIGHEST_PRIORITY,
    CONSENSUS_BASED,
    CUSTOM_RESOLVER
};

/**
 * Synchronization Strategy
 * Different approaches for maintaining consistency
 */
enum SyncStrategy {
    IMMEDIATE_SYNC,     // Synchronize immediately on changes
    BATCHED_SYNC,       // Batch synchronization for efficiency
    LAZY_SYNC,          // Synchronize only when accessed
    PERIODIC_SYNC       // Synchronize at regular intervals
};

/**
 * Distributed AtomSpace Synchronization Manager
 * 
 * Manages synchronization of AtomSpace knowledge across multiple
 * distributed cognitive agents. Ensures consistency while maintaining
 * performance and handling network partitions.
 */
class DistributedAtomSpaceSync
{
private:
    // Synchronization state
    std::map<std::string, AtomSyncRecord> atom_sync_records_;
    std::map<std::string, std::set<std::string>> agent_atom_subscriptions_;
    std::map<std::string, uint64_t> agent_sync_versions_;
    
    // Synchronization configuration
    SyncStrategy sync_strategy_;
    ConflictResolution conflict_resolution_;
    double sync_batch_interval_ms_;
    size_t max_batch_size_;
    
    // Performance tracking
    std::atomic<uint64_t> sync_operations_count_;
    std::atomic<uint64_t> conflicts_resolved_;
    std::atomic<double> average_sync_latency_;
    
    // Thread synchronization
    mutable std::shared_mutex sync_mutex_;
    std::atomic<bool> sync_active_;
    
    // Network topology
    std::map<std::string, std::vector<std::string>> sync_topology_;
    std::map<std::string, double> agent_reliability_scores_;
    
    // Custom conflict resolver
    std::function<AtomSyncRecord(const std::vector<AtomSyncRecord>&)> custom_resolver_;

public:
    DistributedAtomSpaceSync(SyncStrategy strategy = BATCHED_SYNC,
                           ConflictResolution resolution = HIGHEST_PRIORITY,
                           double batch_interval_ms = 100.0,
                           size_t max_batch_size = 1000);
    ~DistributedAtomSpaceSync();

    /**
     * Register agent in the synchronization network
     * 
     * @param agent_id Unique agent identifier
     * @param reliability_score Initial reliability score (0.0 - 1.0)
     */
    void register_agent(const std::string& agent_id, double reliability_score = 0.8);

    /**
     * Subscribe agent to specific atom updates
     * 
     * @param agent_id Agent identifier
     * @param atom_id Atom to subscribe to
     */
    void subscribe_to_atom(const std::string& agent_id, const std::string& atom_id);

    /**
     * Unsubscribe agent from atom updates
     * 
     * @param agent_id Agent identifier
     * @param atom_id Atom to unsubscribe from
     */
    void unsubscribe_from_atom(const std::string& agent_id, const std::string& atom_id);

    /**
     * Update atom data and trigger synchronization
     * 
     * @param agent_id Agent making the update
     * @param atom_id Atom identifier
     * @param atom_type Type of atom
     * @param atom_data New atom data
     * @param priority Update priority (affects conflict resolution)
     * @return Success status
     */
    bool update_atom(const std::string& agent_id,
                    const std::string& atom_id,
                    const std::string& atom_type,
                    const std::vector<double>& atom_data,
                    double priority = 0.5);

    /**
     * Get current atom data for an agent
     * 
     * @param agent_id Requesting agent
     * @param atom_id Atom identifier
     * @return Atom synchronization record
     */
    AtomSyncRecord get_atom(const std::string& agent_id, const std::string& atom_id);

    /**
     * Force synchronization of specific atom across all subscribed agents
     * 
     * @param atom_id Atom to synchronize
     * @return Number of agents synchronized
     */
    size_t force_sync_atom(const std::string& atom_id);

    /**
     * Perform batch synchronization for all pending changes
     * 
     * @return Number of atoms synchronized
     */
    size_t perform_batch_sync();

    /**
     * Handle synchronization conflicts for an atom
     * 
     * @param conflicting_records Multiple versions of the same atom
     * @return Resolved atom record
     */
    AtomSyncRecord resolve_conflict(const std::vector<AtomSyncRecord>& conflicting_records);

    /**
     * Establish synchronization topology between agents
     * 
     * @param agent1_id First agent
     * @param agent2_id Second agent
     * @param bidirectional Whether sync is bidirectional
     */
    void connect_agents_for_sync(const std::string& agent1_id,
                                const std::string& agent2_id,
                                bool bidirectional = true);

    /**
     * Get synchronization statistics
     */
    struct SyncStats {
        uint64_t total_sync_operations;
        uint64_t conflicts_resolved;
        double average_sync_latency_ms;
        size_t total_atoms_tracked;
        size_t agents_connected;
        double sync_efficiency;
        std::map<std::string, size_t> agent_sync_counts;
    };
    SyncStats get_sync_statistics() const;

    /**
     * Set custom conflict resolution function
     * 
     * @param resolver Custom conflict resolution function
     */
    void set_custom_conflict_resolver(
        std::function<AtomSyncRecord(const std::vector<AtomSyncRecord>&)> resolver);

    /**
     * Update agent reliability score based on performance
     * 
     * @param agent_id Agent identifier
     * @param new_score New reliability score (0.0 - 1.0)
     */
    void update_agent_reliability(const std::string& agent_id, double new_score);

    /**
     * Get list of atoms requiring synchronization
     * 
     * @param agent_id Agent identifier
     * @return List of atom IDs requiring sync
     */
    std::vector<std::string> get_pending_sync_atoms(const std::string& agent_id);

    /**
     * Mark atom as synchronized for specific agent
     * 
     * @param agent_id Agent identifier
     * @param atom_id Atom identifier
     */
    void mark_atom_synchronized(const std::string& agent_id, const std::string& atom_id);

    /**
     * Check if atom is synchronized across all subscribed agents
     * 
     * @param atom_id Atom identifier
     * @return True if fully synchronized
     */
    bool is_atom_fully_synchronized(const std::string& atom_id);

    /**
     * Get synchronization topology
     */
    std::map<std::string, std::vector<std::string>> get_sync_topology() const;

    /**
     * Set synchronization strategy
     */
    void set_sync_strategy(SyncStrategy strategy);

    /**
     * Set conflict resolution method
     */
    void set_conflict_resolution(ConflictResolution resolution);

    /**
     * Enable/disable synchronization
     */
    void set_sync_active(bool active);

    /**
     * Clear all synchronization records (useful for reset)
     */
    void clear_sync_records();

    /**
     * Get agent reliability scores
     */
    std::map<std::string, double> get_agent_reliability_scores() const;

    /**
     * Validate synchronization consistency across network
     * 
     * @return Consistency report
     */
    struct ConsistencyReport {
        size_t total_atoms_checked;
        size_t consistent_atoms;
        size_t inconsistent_atoms;
        std::vector<std::string> problematic_atoms;
        double overall_consistency_score;
    };
    ConsistencyReport validate_network_consistency();

private:
    /**
     * Propagate atom update to subscribed agents
     */
    void propagate_atom_update(const AtomSyncRecord& record);

    /**
     * Apply conflict resolution strategy
     */
    AtomSyncRecord apply_conflict_resolution(const std::vector<AtomSyncRecord>& records);

    /**
     * Update synchronization metrics
     */
    void update_sync_metrics(double latency_ms);

    /**
     * Generate version number for atom
     */
    uint64_t generate_version_number();

    /**
     * Check if agents are connected in sync topology
     */
    bool are_agents_connected(const std::string& agent1, const std::string& agent2);

    /**
     * Get agents subscribed to specific atom
     */
    std::vector<std::string> get_subscribed_agents(const std::string& atom_id);

    /**
     * Validate atom data integrity
     */
    bool validate_atom_data(const AtomSyncRecord& record);

    /**
     * Calculate sync priority based on various factors
     */
    double calculate_sync_priority(const AtomSyncRecord& record,
                                 const std::string& requesting_agent);

    /**
     * Handle network partition scenarios
     */
    void handle_network_partition(const std::vector<std::string>& disconnected_agents);

    /**
     * Merge atom records using vector clocks
     */
    AtomSyncRecord merge_with_vector_clocks(const std::vector<AtomSyncRecord>& records);
};

} // namespace opencog

#endif // _OPENCOG_DISTRIBUTED_ATOMSPACE_SYNC_H