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

// Stub implementations for remaining methods
void DistributedAtomSpaceSync::unsubscribe_from_atom(const std::string& agent_id, const std::string& atom_id) {}
AtomSyncRecord DistributedAtomSpaceSync::resolve_conflict(const std::vector<AtomSyncRecord>& conflicting_records) { return AtomSyncRecord(); }
void DistributedAtomSpaceSync::set_custom_conflict_resolver(std::function<AtomSyncRecord(const std::vector<AtomSyncRecord>&)> resolver) {}
void DistributedAtomSpaceSync::update_agent_reliability(const std::string& agent_id, double new_score) {}
std::vector<std::string> DistributedAtomSpaceSync::get_pending_sync_atoms(const std::string& agent_id) { return {}; }
void DistributedAtomSpaceSync::mark_atom_synchronized(const std::string& agent_id, const std::string& atom_id) {}
bool DistributedAtomSpaceSync::is_atom_fully_synchronized(const std::string& atom_id) { return true; }
std::map<std::string, std::vector<std::string>> DistributedAtomSpaceSync::get_sync_topology() const { return sync_topology_; }
void DistributedAtomSpaceSync::set_sync_strategy(SyncStrategy strategy) { sync_strategy_ = strategy; }
void DistributedAtomSpaceSync::set_conflict_resolution(ConflictResolution resolution) { conflict_resolution_ = resolution; }
void DistributedAtomSpaceSync::set_sync_active(bool active) { sync_active_ = active; }
void DistributedAtomSpaceSync::clear_sync_records() { atom_sync_records_.clear(); }
std::map<std::string, double> DistributedAtomSpaceSync::get_agent_reliability_scores() const { return agent_reliability_scores_; }

DistributedAtomSpaceSync::ConsistencyReport DistributedAtomSpaceSync::validate_network_consistency() {
    ConsistencyReport report;
    report.total_atoms_checked = atom_sync_records_.size();
    report.consistent_atoms = report.total_atoms_checked; // Simplified
    report.inconsistent_atoms = 0;
    report.overall_consistency_score = 1.0;
    return report;
}

// Additional private method stubs
AtomSyncRecord DistributedAtomSpaceSync::apply_conflict_resolution(const std::vector<AtomSyncRecord>& records) { return AtomSyncRecord(); }
bool DistributedAtomSpaceSync::are_agents_connected(const std::string& agent1, const std::string& agent2) { return true; }
bool DistributedAtomSpaceSync::validate_atom_data(const AtomSyncRecord& record) { return true; }
double DistributedAtomSpaceSync::calculate_sync_priority(const AtomSyncRecord& record, const std::string& requesting_agent) { return 0.5; }
void DistributedAtomSpaceSync::handle_network_partition(const std::vector<std::string>& disconnected_agents) {}
AtomSyncRecord DistributedAtomSpaceSync::merge_with_vector_clocks(const std::vector<AtomSyncRecord>& records) { return AtomSyncRecord(); }

} // namespace opencog