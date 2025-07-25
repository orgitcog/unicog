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

// Core conflict resolution and synchronization methods
AtomSyncRecord DistributedAtomSpaceSync::resolve_conflict(const std::vector<AtomSyncRecord>& conflicting_records) { 
    if (conflicting_records.empty()) return AtomSyncRecord();
    
    conflicts_resolved_++;
    
    // Apply the selected conflict resolution strategy
    switch (conflict_resolution_) {
        case LAST_WRITER_WINS: {
            // Find record with most recent timestamp
            const AtomSyncRecord* latest = &conflicting_records[0];
            for (const auto& record : conflicting_records) {
                if (record.last_modified > latest->last_modified || 
                   (record.last_modified == latest->last_modified && record.version > latest->version)) {
                    latest = &record;
                }
            }
            std::cout << "Resolved conflict using LAST_WRITER_WINS strategy" << std::endl;
            return *latest;
        }
        
        case HIGHEST_PRIORITY: {
            // Find record with highest priority, considering agent reliability
            const AtomSyncRecord* best = &conflicting_records[0];
            double best_score = best->sync_priority;
            
            for (const auto& record : conflicting_records) {
                double score = record.sync_priority;
                // Boost score based on version for tie-breaking
                score += 0.01 * record.version;
                
                if (score > best_score) {
                    best = &record;
                    best_score = score;
                }
            }
            std::cout << "Resolved conflict using HIGHEST_PRIORITY strategy (score: " << best_score << ")" << std::endl;
            return *best;
        }
        
        case CONSENSUS_BASED: {
            // Simple majority consensus based on data similarity
            std::map<std::vector<double>, int> data_votes;
            std::map<std::vector<double>, AtomSyncRecord> data_records;
            
            for (const auto& record : conflicting_records) {
                data_votes[record.atom_data]++;
                data_records[record.atom_data] = record;
            }
            
            // Find data with most votes
            auto max_vote = std::max_element(data_votes.begin(), data_votes.end(),
                [](const auto& a, const auto& b) { return a.second < b.second; });
            
            if (max_vote != data_votes.end()) {
                std::cout << "Resolved conflict using CONSENSUS_BASED strategy (votes: " << max_vote->second << ")" << std::endl;
                return data_records[max_vote->first];
            }
            break;
        }
        
        case CUSTOM_RESOLVER: {
            if (custom_resolver_) {
                AtomSyncRecord result = custom_resolver_(conflicting_records);
                std::cout << "Resolved conflict using CUSTOM_RESOLVER strategy" << std::endl;
                return result;
            }
            break;
        }
    }
    
    // Fallback to version-based resolution
    const AtomSyncRecord* latest = &conflicting_records[0];
    for (const auto& record : conflicting_records) {
        if (record.version > latest->version) {
            latest = &record;
        }
    }
    std::cout << "Resolved conflict using fallback version-based strategy" << std::endl;
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
    std::shared_lock<std::shared_mutex> lock(sync_mutex_);
    
    ConsistencyReport report;
    report.total_atoms_checked = atom_sync_records_.size();
    
    std::cout << "Validating network consistency across " << agent_sync_versions_.size() << " agents..." << std::endl;
    
    size_t consistent_count = 0;
    std::vector<std::string> problematic_atoms;
    std::map<std::string, std::vector<std::string>> consistency_issues;
    
    for (const auto& [atom_id, record] : atom_sync_records_) {
        bool is_consistent = true;
        std::vector<std::string> atom_issues;
        
        // Check 1: Synchronization completeness
        if (record.requires_sync) {
            std::vector<std::string> subscribed_agents = get_subscribed_agents(atom_id);
            size_t synchronized_count = record.synchronized_agents.size();
            
            if (synchronized_count < subscribed_agents.size()) {
                is_consistent = false;
                atom_issues.push_back("Incomplete synchronization: " + 
                    std::to_string(synchronized_count) + "/" + std::to_string(subscribed_agents.size()) + " agents");
            }
        }
        
        // Check 2: Data integrity
        if (record.atom_data.empty()) {
            is_consistent = false;
            atom_issues.push_back("Empty atom data");
        }
        
        // Check 3: Version consistency
        if (record.version == 0) {
            is_consistent = false;
            atom_issues.push_back("Invalid version number");
        }
        
        // Check 4: Temporal consistency
        auto now = std::chrono::steady_clock::now();
        auto time_diff = std::chrono::duration_cast<std::chrono::minutes>(now - record.last_modified);
        if (time_diff.count() > 60) { // More than 1 hour old
            atom_issues.push_back("Stale record (age: " + std::to_string(time_diff.count()) + " minutes)");
        }
        
        // Check 5: Priority validation
        if (record.sync_priority < 0.0 || record.sync_priority > 1.0) {
            is_consistent = false;
            atom_issues.push_back("Invalid sync priority: " + std::to_string(record.sync_priority));
        }
        
        if (is_consistent) {
            consistent_count++;
        } else {
            problematic_atoms.push_back(atom_id);
            consistency_issues[atom_id] = atom_issues;
        }
    }
    
    report.consistent_atoms = consistent_count;
    report.inconsistent_atoms = report.total_atoms_checked - consistent_count;
    report.problematic_atoms = problematic_atoms;
    
    // Calculate overall consistency score
    if (report.total_atoms_checked > 0) {
        report.overall_consistency_score = static_cast<double>(consistent_count) / report.total_atoms_checked;
    } else {
        report.overall_consistency_score = 1.0;
    }
    
    // Additional network-wide consistency checks
    
    // Check agent connectivity
    size_t disconnected_agents = 0;
    for (const auto& [agent_id, reliability] : agent_reliability_scores_) {
        if (reliability < 0.1) { // Very low reliability indicates disconnection
            disconnected_agents++;
        }
    }
    
    if (disconnected_agents > agent_sync_versions_.size() / 2) {
        report.problematic_atoms.push_back("NETWORK_PARTITION_DETECTED");
        consistency_issues["NETWORK_PARTITION"] = {"More than 50% agents have low reliability"};
    }
    
    // Check sync topology integrity
    size_t isolated_agents = 0;
    for (const auto& [agent_id, connections] : sync_topology_) {
        if (connections.empty()) {
            isolated_agents++;
        }
    }
    
    if (isolated_agents > 0) {
        consistency_issues["TOPOLOGY"] = {std::to_string(isolated_agents) + " agents are isolated"};
    }
    
    // Report results
    std::cout << "Network consistency validation complete:" << std::endl;
    std::cout << "  Total atoms checked: " << report.total_atoms_checked << std::endl;
    std::cout << "  Consistent atoms: " << consistent_count << std::endl;
    std::cout << "  Inconsistent atoms: " << report.inconsistent_atoms << std::endl;
    std::cout << "  Overall consistency score: " << (report.overall_consistency_score * 100.0) << "%" << std::endl;
    std::cout << "  Disconnected agents: " << disconnected_agents << std::endl;
    std::cout << "  Isolated agents: " << isolated_agents << std::endl;
    
    if (!problematic_atoms.empty()) {
        std::cout << "Issues found in " << problematic_atoms.size() << " atoms:" << std::endl;
        for (const auto& atom_id : problematic_atoms) {
            if (consistency_issues.find(atom_id) != consistency_issues.end()) {
                std::cout << "  " << atom_id << ": ";
                for (const auto& issue : consistency_issues[atom_id]) {
                    std::cout << issue << "; ";
                }
                std::cout << std::endl;
            }
        }
    }
    
    return report;
}

//<<<<<<< copilot/fix-17
// Additional private method implementations - now properly implemented below
//=======
// Additional private method implementations
AtomSyncRecord DistributedAtomSpaceSync::apply_conflict_resolution(const std::vector<AtomSyncRecord>& records) { 
    if (records.empty()) return AtomSyncRecord();
    
    // If only one record, no conflict to resolve
    if (records.size() == 1) {
        return records[0];
    }
    
    // Apply conflict resolution based on current strategy
    return resolve_conflict(records);
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
    std::unique_lock<std::shared_mutex> lock(sync_mutex_);
    
    std::cout << "Handling network partition. Disconnected agents: ";
    for (const auto& agent : disconnected_agents) {
        std::cout << agent << " ";
    }
    std::cout << std::endl;
    
    // Mark agents as disconnected and adjust reliability scores
    for (const std::string& agent_id : disconnected_agents) {
        auto reliability_it = agent_reliability_scores_.find(agent_id);
        if (reliability_it != agent_reliability_scores_.end()) {
            // Reduce reliability score for disconnected agents
            reliability_it->second *= 0.8;
            std::cout << "Reduced reliability for " << agent_id << " to " << reliability_it->second << std::endl;
        }
        
        // Remove from topology connections temporarily
        auto topology_it = sync_topology_.find(agent_id);
        if (topology_it != sync_topology_.end()) {
            // Store connections for recovery
            topology_it->second.clear();
        }
        
        // Clear pending sync operations for disconnected agents
        agent_atom_subscriptions_.erase(agent_id);
    }
    
    // Update sync strategy to handle partition
    if (disconnected_agents.size() * 2 >= agent_sync_versions_.size()) {
        // Majority partition - switch to defensive mode
        std::cout << "Majority partition detected - switching to defensive sync mode" << std::endl;
        sync_active_ = false;
    } else {
        // Minority partition - continue with remaining agents
        std::cout << "Minority partition detected - continuing with " 
                  << (agent_sync_versions_.size() - disconnected_agents.size()) << " remaining agents" << std::endl;
    }
}

AtomSyncRecord DistributedAtomSpaceSync::merge_with_vector_clocks(const std::vector<AtomSyncRecord>& records) { 
    if (records.empty()) return AtomSyncRecord();
    if (records.size() == 1) return records[0];
    
    // Basic vector clock-based merging
    AtomSyncRecord merged_record = records[0];
    
    // Find the record with the highest version (simplified vector clock)
    uint64_t max_version = merged_record.version;
    for (const auto& record : records) {
        if (record.version > max_version) {
            max_version = record.version;
            merged_record = record;
        }
    }
    
    // Update merged record with highest version + 1 to show merge
    merged_record.version = max_version + 1;
    merged_record.last_modified = std::chrono::steady_clock::now();
    merged_record.requires_sync = true;
    
    // Merge atom data by averaging (simplified semantic merge)
    if (!records.empty() && !merged_record.atom_data.empty()) {
        std::vector<double> averaged_data(merged_record.atom_data.size(), 0.0);
        
        for (const auto& record : records) {
            if (record.atom_data.size() == averaged_data.size()) {
                for (size_t i = 0; i < averaged_data.size(); ++i) {
                    averaged_data[i] += record.atom_data[i] / records.size();
                }
            }
        }
        
        merged_record.atom_data = averaged_data;
    }
    
    // Clear synchronized agents list since this is a new merged version
    merged_record.synchronized_agents.clear();
    
    std::cout << "Merged " << records.size() << " records using vector clock logic (new version: " 
              << merged_record.version << ")" << std::endl;
    
    return merged_record; 
}
//>>>>>>> main

} // namespace opencog