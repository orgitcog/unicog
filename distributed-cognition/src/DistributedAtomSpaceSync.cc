/*
 * DistributedAtomSpaceSync.cc
 *
 * Implementation of distributed AtomSpace synchronization primitives
 */

#include "../include/DistributedAtomSpaceSync.h"
#include <algorithm>
#include <numeric>
#include <iostream>

namespace opencog {

DistributedAtomSpaceSync::DistributedAtomSpaceSync(SyncStrategy strategy,
                                                 ConflictResolution resolution,
                                                 double batch_interval_ms,
                                                 size_t max_batch_size)
    : sync_strategy_(strategy)
    , conflict_resolution_(resolution)
    , sync_batch_interval_ms_(batch_interval_ms)
    , max_batch_size_(max_batch_size)
    , sync_operations_count_(0)
    , conflicts_resolved_(0)
    , average_sync_latency_(0.0)
    , sync_active_(true)
{
    std::cout << "DistributedAtomSpaceSync initialized with strategy=" << static_cast<int>(strategy)
              << ", resolution=" << static_cast<int>(resolution) << std::endl;
}

DistributedAtomSpaceSync::~DistributedAtomSpaceSync() = default;

void DistributedAtomSpaceSync::register_agent(const std::string& agent_id, double reliability_score) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    agent_sync_versions_[agent_id] = 0;
    agent_reliability_scores_[agent_id] = reliability_score;
    sync_topology_[agent_id] = std::vector<std::string>();
    
    std::cout << "Registered agent " << agent_id 
              << " with reliability " << reliability_score << std::endl;
}

bool DistributedAtomSpaceSync::update_atom(const std::string& agent_id,
                                         const std::string& atom_id,
                                         const std::string& atom_type,
                                         const std::vector<double>& atom_data,
                                         double priority) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    if (!sync_active_) return false;
    
    auto start_time = std::chrono::steady_clock::now();
    
    // Create or update atom record
    AtomSyncRecord record;
    record.atom_id = atom_id;
    record.atom_type = atom_type;
    record.atom_data = atom_data;
    record.version = generate_version_number();
    record.last_modified = std::chrono::steady_clock::now();
    record.requires_sync = true;
    record.sync_priority = priority;
    
    atom_sync_records_[atom_id] = record;
    
    // Update agent version
    agent_sync_versions_[agent_id]++;
    
    auto end_time = std::chrono::steady_clock::now();
    double latency_ms = std::chrono::duration<double, std::milli>(end_time - start_time).count();
    update_sync_metrics(latency_ms);
    
    sync_operations_count_++;
    
    // Trigger synchronization based on strategy
    if (sync_strategy_ == IMMEDIATE_SYNC) {
        lock.unlock();
        force_sync_atom(atom_id);
    }
    
    return true;
}

AtomSyncRecord DistributedAtomSpaceSync::get_atom(const std::string& agent_id, const std::string& atom_id) {
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    auto it = atom_sync_records_.find(atom_id);
    if (it != atom_sync_records_.end()) {
        return it->second;
    }
    
    return AtomSyncRecord(); // Return empty record if not found
}

size_t DistributedAtomSpaceSync::perform_batch_sync() {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    size_t synced_count = 0;
    
    for (auto& [atom_id, record] : atom_sync_records_) {
        if (record.requires_sync) {
            propagate_atom_update(record);
            record.requires_sync = false;
            synced_count++;
        }
    }
    
    std::cout << "Batch sync completed: " << synced_count << " atoms synchronized" << std::endl;
    return synced_count;
}

DistributedAtomSpaceSync::SyncStats DistributedAtomSpaceSync::get_sync_statistics() const {
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    SyncStats stats;
    stats.total_sync_operations = sync_operations_count_.load();
    stats.conflicts_resolved = conflicts_resolved_.load();
    stats.average_sync_latency_ms = average_sync_latency_.load();
    stats.total_atoms_tracked = atom_sync_records_.size();
    stats.agents_connected = agent_sync_versions_.size();
    
    // Calculate sync efficiency
    if (stats.total_sync_operations > 0) {
        stats.sync_efficiency = 1.0 - (static_cast<double>(stats.conflicts_resolved) / stats.total_sync_operations);
    } else {
        stats.sync_efficiency = 1.0;
    }
    
    // Count syncs per agent
    for (const auto& [agent_id, version] : agent_sync_versions_) {
        stats.agent_sync_counts[agent_id] = version;
    }
    
    return stats;
}

// Simplified implementations for other methods
void DistributedAtomSpaceSync::subscribe_to_atom(const std::string& agent_id, const std::string& atom_id) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    agent_atom_subscriptions_[agent_id].insert(atom_id);
}

size_t DistributedAtomSpaceSync::force_sync_atom(const std::string& atom_id) {
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    auto it = atom_sync_records_.find(atom_id);
    if (it != atom_sync_records_.end()) {
        propagate_atom_update(it->second);
        return get_subscribed_agents(atom_id).size();
    }
    return 0;
}

void DistributedAtomSpaceSync::connect_agents_for_sync(const std::string& agent1_id,
                                                      const std::string& agent2_id,
                                                      bool bidirectional) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    sync_topology_[agent1_id].push_back(agent2_id);
    if (bidirectional) {
        sync_topology_[agent2_id].push_back(agent1_id);
    }
}

// Private method implementations
void DistributedAtomSpaceSync::propagate_atom_update(const AtomSyncRecord& record) {
    // Simplified propagation - in practice would send actual network messages
    std::vector<std::string> subscribers = get_subscribed_agents(record.atom_id);
    for (const std::string& agent_id : subscribers) {
        // Update agent's sync version
        agent_sync_versions_[agent_id]++;
    }
}

uint64_t DistributedAtomSpaceSync::generate_version_number() {
    static std::atomic<uint64_t> version_counter(1);
    return version_counter++;
}

std::vector<std::string> DistributedAtomSpaceSync::get_subscribed_agents(const std::string& atom_id) {
    std::vector<std::string> subscribers;
    for (const auto& [agent_id, subscriptions] : agent_atom_subscriptions_) {
        if (subscriptions.find(atom_id) != subscriptions.end()) {
            subscribers.push_back(agent_id);
        }
    }
    return subscribers;
}

void DistributedAtomSpaceSync::update_sync_metrics(double latency_ms) {
    double current_avg = average_sync_latency_.load();
    double new_avg = 0.9 * current_avg + 0.1 * latency_ms;
    average_sync_latency_.store(new_avg);
}

//<<<<<<< copilot/fix-17
// Implementation of remaining synchronization methods
//void DistributedAtomSpaceSync::unsubscribe_from_atom(const std::string& agent_id, const std::string& atom_id) {}
//AtomSyncRecord DistributedAtomSpaceSync::resolve_conflict(const std::vector<AtomSyncRecord>& conflicting_records) { return AtomSyncRecord(); }
//void DistributedAtomSpaceSync::set_custom_conflict_resolver(std::function<AtomSyncRecord(const std::vector<AtomSyncRecord>&)> resolver) {}
//void DistributedAtomSpaceSync::update_agent_reliability(const std::string& agent_id, double new_score) {}
//std::vector<std::string> DistributedAtomSpaceSync::get_pending_sync_atoms(const std::string& agent_id) { return {}; }
//void DistributedAtomSpaceSync::mark_atom_synchronized(const std::string& agent_id, const std::string& atom_id) {}
//bool DistributedAtomSpaceSync::is_atom_fully_synchronized(const std::string& atom_id) { return true; }
//=======
// Real implementations for straightforward methods
void DistributedAtomSpaceSync::unsubscribe_from_atom(const std::string& agent_id, const std::string& atom_id) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    auto it = agent_atom_subscriptions_.find(agent_id);
    if (it != agent_atom_subscriptions_.end()) {
        it->second.erase(atom_id);
        std::cout << "Agent " << agent_id << " unsubscribed from atom " << atom_id << std::endl;
    }
}

void DistributedAtomSpaceSync::set_custom_conflict_resolver(std::function<AtomSyncRecord(const std::vector<AtomSyncRecord>&)> resolver) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    custom_resolver_ = resolver;
    conflict_resolution_ = CUSTOM_RESOLVER;
    std::cout << "Custom conflict resolver set and strategy updated" << std::endl;
}

void DistributedAtomSpaceSync::update_agent_reliability(const std::string& agent_id, double new_score) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    // Clamp score to valid range [0.0, 1.0]
    new_score = std::max(0.0, std::min(1.0, new_score));
    
    auto it = agent_reliability_scores_.find(agent_id);
    if (it != agent_reliability_scores_.end()) {
        double old_score = it->second;
        it->second = new_score;
        std::cout << "Agent " << agent_id << " reliability updated from " 
                  << old_score << " to " << new_score << std::endl;
    } else {
        std::cout << "Warning: Agent " << agent_id << " not found for reliability update" << std::endl;
    }
}

std::vector<std::string> DistributedAtomSpaceSync::get_pending_sync_atoms(const std::string& agent_id) {
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    std::vector<std::string> pending_atoms;
    
    // Get agent's subscriptions
    auto subscription_it = agent_atom_subscriptions_.find(agent_id);
    if (subscription_it == agent_atom_subscriptions_.end()) {
        return pending_atoms; // No subscriptions
    }
    
    // Check each subscribed atom for pending sync
    for (const std::string& atom_id : subscription_it->second) {
        auto record_it = atom_sync_records_.find(atom_id);
        if (record_it != atom_sync_records_.end() && record_it->second.requires_sync) {
            // Check if this agent hasn't been synchronized yet
            if (record_it->second.synchronized_agents.find(agent_id) == 
                record_it->second.synchronized_agents.end()) {
                pending_atoms.push_back(atom_id);
            }
        }
    }
    
    return pending_atoms;
}

void DistributedAtomSpaceSync::mark_atom_synchronized(const std::string& agent_id, const std::string& atom_id) {
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    auto record_it = atom_sync_records_.find(atom_id);
    if (record_it != atom_sync_records_.end()) {
        record_it->second.synchronized_agents.insert(agent_id);
        std::cout << "Marked atom " << atom_id << " as synchronized for agent " << agent_id << std::endl;
        
        // Check if atom is now fully synchronized
        std::vector<std::string> subscribed_agents = get_subscribed_agents(atom_id);
        if (record_it->second.synchronized_agents.size() == subscribed_agents.size()) {
            record_it->second.requires_sync = false;
            std::cout << "Atom " << atom_id << " is now fully synchronized across all agents" << std::endl;
        }
    }
}

bool DistributedAtomSpaceSync::is_atom_fully_synchronized(const std::string& atom_id) {
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    auto record_it = atom_sync_records_.find(atom_id);
    if (record_it == atom_sync_records_.end()) {
        return true; // No record means nothing to sync
    }
    
    // Get list of agents subscribed to this atom
    std::vector<std::string> subscribed_agents = get_subscribed_agents(atom_id);
    
    // Check if all subscribed agents have this atom synchronized
    const std::set<std::string>& synchronized_agents = record_it->second.synchronized_agents;
    
    for (const std::string& agent_id : subscribed_agents) {
        if (synchronized_agents.find(agent_id) == synchronized_agents.end()) {
            return false; // Found an agent that hasn't synchronized this atom
        }
    }
    
    return true; // All subscribed agents have synchronized this atom
}

// Complex methods that require detailed implementation - marked as TODO
AtomSyncRecord DistributedAtomSpaceSync::resolve_conflict(const std::vector<AtomSyncRecord>& conflicting_records) { 
    // TODO: Implement comprehensive conflict resolution algorithm
    // This method should:
    // 1. Apply the selected conflict resolution strategy (LAST_WRITER_WINS, HIGHEST_PRIORITY, CONSENSUS_BASED, CUSTOM_RESOLVER)
    // 2. Consider agent reliability scores when resolving conflicts
    // 3. Maintain audit trail of conflict resolution decisions
    // 4. Handle edge cases like simultaneous updates with identical timestamps
    // 5. Apply vector clock logic for distributed consistency
    // 6. Generate conflict resolution metrics
    
    conflicts_resolved_++;
    std::cout << "TODO: Complex conflict resolution not yet implemented. Using simple last-writer-wins." << std::endl;
    
    if (conflicting_records.empty()) return AtomSyncRecord();
    
    // Simple fallback: return the record with highest version number
    const AtomSyncRecord* latest = &conflicting_records[0];
    for (const auto& record : conflicting_records) {
        if (record.version > latest->version) {
            latest = &record;
        }
    }
    return *latest;
}
//>>>>>>> main
std::map<std::string, std::vector<std::string>> DistributedAtomSpaceSync::get_sync_topology() const { return sync_topology_; }
void DistributedAtomSpaceSync::set_sync_strategy(SyncStrategy strategy) { sync_strategy_ = strategy; }
void DistributedAtomSpaceSync::set_conflict_resolution(ConflictResolution resolution) { conflict_resolution_ = resolution; }
void DistributedAtomSpaceSync::set_sync_active(bool active) { sync_active_ = active; }
void DistributedAtomSpaceSync::clear_sync_records() { atom_sync_records_.clear(); }
std::map<std::string, double> DistributedAtomSpaceSync::get_agent_reliability_scores() const { return agent_reliability_scores_; }

DistributedAtomSpaceSync::ConsistencyReport DistributedAtomSpaceSync::validate_network_consistency() {
    // TODO: Implement comprehensive distributed consistency validation
    // This complex algorithm should:
    // 1. Check consistency across all agents in the network topology
    // 2. Validate atom version consistency using vector clocks
    // 3. Detect and report split-brain scenarios
    // 4. Verify synchronization invariants are maintained
    // 5. Perform Byzantine fault detection for unreliable agents
    // 6. Check network partition effects on consistency
    // 7. Generate detailed inconsistency reports with remediation suggestions
    // 8. Validate causal ordering of updates across the distributed system
    
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    ConsistencyReport report;
    report.total_atoms_checked = atom_sync_records_.size();
    
    // Simple consistency check implementation for now
    size_t consistent_count = 0;
    std::vector<std::string> problematic_atoms;
    
    for (const auto& [atom_id, record] : atom_sync_records_) {
        // Simple check: atom is consistent if it doesn't require sync or is fully synchronized
        if (!record.requires_sync || is_atom_fully_synchronized(atom_id)) {
            consistent_count++;
        } else {
            problematic_atoms.push_back(atom_id);
        }
    }
    
    report.consistent_atoms = consistent_count;
    report.inconsistent_atoms = report.total_atoms_checked - consistent_count;
    report.problematic_atoms = problematic_atoms;
    
    if (report.total_atoms_checked > 0) {
        report.overall_consistency_score = static_cast<double>(consistent_count) / report.total_atoms_checked;
    } else {
        report.overall_consistency_score = 1.0;
    }
    
    std::cout << "Network consistency validation: " << consistent_count << "/" 
              << report.total_atoms_checked << " atoms consistent ("
              << (report.overall_consistency_score * 100.0) << "%)" << std::endl;
    
    if (!problematic_atoms.empty()) {
        std::cout << "TODO: Complex consistency validation would provide detailed analysis of " 
                  << problematic_atoms.size() << " problematic atoms" << std::endl;
    }
    
    return report;
}

//<<<<<<< copilot/fix-17
// Additional private method implementations
//AtomSyncRecord DistributedAtomSpaceSync::apply_conflict_resolution(const std::vector<AtomSyncRecord>& records) { return AtomSyncRecord(); }
//bool DistributedAtomSpaceSync::are_agents_connected(const std::string& agent1, const std::string& agent2) { return true; }
//bool DistributedAtomSpaceSync::validate_atom_data(const AtomSyncRecord& record) { return true; }
//double DistributedAtomSpaceSync::calculate_sync_priority(const AtomSyncRecord& record, const std::string& requesting_agent) { return 0.5; }
//void DistributedAtomSpaceSync::handle_network_partition(const std::vector<std::string>& disconnected_agents) {}
//AtomSyncRecord DistributedAtomSpaceSync::merge_with_vector_clocks(const std::vector<AtomSyncRecord>& records) { return AtomSyncRecord(); }
//=======
// Additional private method implementations and TODOs
AtomSyncRecord DistributedAtomSpaceSync::apply_conflict_resolution(const std::vector<AtomSyncRecord>& records) { 
    // TODO: Implement sophisticated conflict resolution algorithm
    // This should handle different resolution strategies:
    // - LAST_WRITER_WINS: Use timestamps and version numbers
    // - HIGHEST_PRIORITY: Use sync_priority field and agent reliability
    // - CONSENSUS_BASED: Implement Byzantine fault tolerance algorithm
    // - CUSTOM_RESOLVER: Use the custom_resolver_ function if set
    // Should also update metrics and maintain consistency guarantees
    
    std::cout << "TODO: Complex conflict resolution algorithm not implemented. Using fallback." << std::endl;
    return records.empty() ? AtomSyncRecord() : records[0]; 
}

bool DistributedAtomSpaceSync::are_agents_connected(const std::string& agent1, const std::string& agent2) { 
    // Simple topology check - already functional
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    auto it = sync_topology_.find(agent1);
    if (it != sync_topology_.end()) {
        const std::vector<std::string>& connections = it->second;
        return std::find(connections.begin(), connections.end(), agent2) != connections.end();
    }
    return false; 
}

bool DistributedAtomSpaceSync::validate_atom_data(const AtomSyncRecord& record) { 
    // Basic validation - can be enhanced later
    return !record.atom_id.empty() && !record.atom_type.empty() && !record.atom_data.empty();
}

double DistributedAtomSpaceSync::calculate_sync_priority(const AtomSyncRecord& record, const std::string& requesting_agent) { 
    // Simple priority calculation - can be enhanced
    double base_priority = record.sync_priority;
    
    // Boost priority based on agent reliability
    auto reliability_it = agent_reliability_scores_.find(requesting_agent);
    if (reliability_it != agent_reliability_scores_.end()) {
        base_priority *= reliability_it->second;
    }
    
    return std::min(1.0, base_priority);
}

void DistributedAtomSpaceSync::handle_network_partition(const std::vector<std::string>& disconnected_agents) {
    // TODO: Implement sophisticated network partition handling
    // This complex algorithm should:
    // 1. Detect network partitions using timeout and heartbeat mechanisms
    // 2. Implement partition-tolerant consensus algorithms (e.g., Raft, PBFT)
    // 3. Handle split-brain scenarios with quorum-based decisions
    // 4. Maintain partial availability during partitions
    // 5. Implement partition recovery and state reconciliation
    // 6. Update agent reliability scores based on partition behavior
    // 7. Trigger appropriate fallback synchronization strategies
    
    std::cout << "TODO: Network partition handling not implemented. Agents marked as disconnected: ";
    for (const auto& agent : disconnected_agents) {
        std::cout << agent << " ";
    }
    std::cout << std::endl;
}

AtomSyncRecord DistributedAtomSpaceSync::merge_with_vector_clocks(const std::vector<AtomSyncRecord>& records) { 
    // TODO: Implement vector clock-based record merging
    // This sophisticated distributed algorithm should:
    // 1. Implement proper vector clock comparison and merging
    // 2. Detect concurrent updates vs. causally ordered updates
    // 3. Handle clock drift and timestamp synchronization issues
    // 4. Merge atom data using semantic merge rules (not just overwrite)
    // 5. Maintain causal consistency across distributed updates
    // 6. Generate proper conflict resolution audit trails
    // 7. Update vector clocks for merged records
    
    std::cout << "TODO: Vector clock merging algorithm not implemented. Using simple merge." << std::endl;
    return records.empty() ? AtomSyncRecord() : records[0]; 
}
//>>>>>>> main

} // namespace opencog