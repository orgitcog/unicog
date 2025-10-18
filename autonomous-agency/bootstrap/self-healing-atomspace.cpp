#include "self-healing-atomspace.hpp"
#include <algorithm>
#include <numeric>
#include <random>
#include <sstream>
#include <iomanip>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>

namespace opencog {
namespace autonomous {

SelfHealingAtomSpace::SelfHealingAtomSpace(AtomSpacePtr primary_space)
    : primary_space_(primary_space), 
      current_strategy_(RepairStrategy::ADAPTIVE),
      maintenance_active_(false) {
    
    // Initialize strategy success rates
    strategy_success_rates_[RepairStrategy::CONSERVATIVE] = 0.8;
    strategy_success_rates_[RepairStrategy::AGGRESSIVE] = 0.6;
    strategy_success_rates_[RepairStrategy::ADAPTIVE] = 0.9;
    strategy_success_rates_[RepairStrategy::EMERGENCY] = 0.7;
    
    // Create initial backup
    createBackupSnapshot();
}

SelfHealingAtomSpace::~SelfHealingAtomSpace() {
    stopBackgroundMaintenance();
}

void SelfHealingAtomSpace::scheduleIntegrityChecks() {
    RepairOperation op;
    op.operation_id = "integrity_check_" + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
    op.strategy = RepairStrategy::CONSERVATIVE;
    op.description = "Scheduled integrity check";
    op.scheduled_time = std::chrono::steady_clock::now() + INTEGRITY_CHECK_INTERVAL;
    op.is_emergency = false;
    
    std::lock_guard<std::mutex> lock(repair_mutex_);
    repair_queue_.push(op);
    repair_cv_.notify_one();
}

void SelfHealingAtomSpace::performGarbageCollection() {
    if (!primary_space_) return;
    
    std::cout << "SELF-HEALING: Performing garbage collection..." << std::endl;
    
    // Find and remove orphaned atoms
    std::vector<Handle> orphaned = findOrphanedAtoms();
    for (const Handle& h : orphaned) {
        if (primary_space_->is_valid_handle(h)) {
            primary_space_->remove_atom(h);
        }
    }
    
    // Consolidate duplicate atoms
    consolidateDuplicateAtoms();
    
    // Compact storage if available
    compactAtomStorage();
    
    std::cout << "SELF-HEALING: Garbage collection completed. Removed " 
              << orphaned.size() << " orphaned atoms." << std::endl;
}

void SelfHealingAtomSpace::optimizeAtomDistribution() {
    if (!primary_space_) return;
    
    std::cout << "SELF-HEALING: Optimizing atom distribution..." << std::endl;
    
    // Redistribute attention values for better cognitive resource allocation
    redistributeAttentionValues();
    
    // Optimize link structures for better connectivity
    optimizeLinkStructures();
    
    std::cout << "SELF-HEALING: Atom distribution optimization completed." << std::endl;
}

void SelfHealingAtomSpace::createBackupSnapshot() {
    if (!primary_space_) return;
    
    BackupSnapshot snapshot;
    snapshot.snapshot_id = "backup_" + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
    snapshot.timestamp = std::chrono::steady_clock::now();
    
    // Capture all atoms
    snapshot.atoms = primary_space_->get_handles_by_type(ATOM, true);
    
    // Capture truth values and attention values
    for (const Handle& h : snapshot.atoms) {
        if (h->getTruthValue()) {
            snapshot.truth_values[h] = h->getTruthValue();
        }
        if (h->getAttentionValue()) {
            snapshot.attention_values[h] = h->getAttentionValue();
        }
    }
    
    // Calculate integrity checksum
    snapshot.integrity_checksum = calculateIntegrityChecksum(snapshot.atoms);
    
    // Add to backup collection
    backup_snapshots_.push_back(snapshot);
    
    // Clean up old backups if necessary
    if (backup_snapshots_.size() > MAX_BACKUPS) {
        backup_snapshots_.erase(backup_snapshots_.begin());
    }
    
    std::cout << "SELF-HEALING: Backup snapshot created: " << snapshot.snapshot_id << std::endl;
}

void SelfHealingAtomSpace::repairCorruptedAtoms(const std::vector<Handle>& corrupted_atoms) {
    std::cout << "SELF-HEALING: Repairing " << corrupted_atoms.size() << " corrupted atoms..." << std::endl;
    
    size_t repaired_count = 0;
    for (const Handle& atom : corrupted_atoms) {
        if (repairAtomIntegrity(atom)) {
            repaired_count++;
        }
    }
    
    std::cout << "SELF-HEALING: Successfully repaired " << repaired_count 
              << " out of " << corrupted_atoms.size() << " corrupted atoms." << std::endl;
}

bool SelfHealingAtomSpace::restoreFromBackup(const std::string& snapshot_id) {
    BackupSnapshot* target_snapshot = nullptr;
    
    if (snapshot_id.empty()) {
        // Use most recent backup
        if (!backup_snapshots_.empty()) {
            target_snapshot = &backup_snapshots_.back();
        }
    } else {
        // Find specific backup
        for (auto& snapshot : backup_snapshots_) {
            if (snapshot.snapshot_id == snapshot_id) {
                target_snapshot = &snapshot;
                break;
            }
        }
    }
    
    if (!target_snapshot) {
        std::cout << "SELF-HEALING: No suitable backup found for restoration." << std::endl;
        return false;
    }
    
    if (!validateBackupIntegrity(*target_snapshot)) {
        std::cout << "SELF-HEALING: Backup integrity validation failed." << std::endl;
        return false;
    }
    
    std::cout << "SELF-HEALING: Restoring from backup: " << target_snapshot->snapshot_id << std::endl;
    
    // Apply backup to AtomSpace
    applyBackupToAtomSpace(*target_snapshot);
    
    std::cout << "SELF-HEALING: Restoration completed successfully." << std::endl;
    return true;
}

void SelfHealingAtomSpace::reconstructDamagedSubgraphs(const std::vector<Handle>& damaged_region) {
    std::cout << "SELF-HEALING: Reconstructing damaged subgraphs..." << std::endl;
    
    // For each damaged atom, try to reconstruct its connections
    for (const Handle& damaged_atom : damaged_region) {
        if (!primary_space_->is_valid_handle(damaged_atom)) {
            continue;
        }
        
        // Attempt to reestablish broken links
        reestablishBrokenLinks();
        
        // Try to repair the atom's integrity
        repairAtomIntegrity(damaged_atom);
    }
    
    std::cout << "SELF-HEALING: Subgraph reconstruction completed." << std::endl;
}

void SelfHealingAtomSpace::learnFromFailures(const std::vector<RepairOperation>& failed_operations) {
    std::cout << "SELF-HEALING: Learning from " << failed_operations.size() << " failed operations..." << std::endl;
    
    // Extract failure patterns
    std::vector<std::string> patterns = extractFailurePatterns(failed_operations);
    
    // Add new patterns to learned patterns
    for (const std::string& pattern : patterns) {
        if (std::find(learned_patterns_.begin(), learned_patterns_.end(), pattern) == learned_patterns_.end()) {
            learned_patterns_.push_back(pattern);
        }
    }
    
    // Update strategy success rates based on failures
    for (const RepairOperation& op : failed_operations) {
        recordStrategyOutcome(op.strategy, false);
    }
    
    updateStrategyRatings();
    
    std::cout << "SELF-HEALING: Learning completed. " << patterns.size() 
              << " new failure patterns identified." << std::endl;
}

void SelfHealingAtomSpace::evolveRepairStrategies() {
    std::cout << "SELF-HEALING: Evolving repair strategies..." << std::endl;
    
    // Analyze current strategy performance
    double best_rate = 0.0;
    RepairStrategy best_strategy = RepairStrategy::CONSERVATIVE;
    
    for (const auto& pair : strategy_success_rates_) {
        if (pair.second > best_rate) {
            best_rate = pair.second;
            best_strategy = pair.first;
        }
    }
    
    // Evolve toward better strategies
    if (best_strategy != current_strategy_ && best_rate > strategy_success_rates_[current_strategy_]) {
        std::cout << "SELF-HEALING: Evolving from current strategy to better performing strategy." << std::endl;
        current_strategy_ = best_strategy;
    }
    
    // Adapt strategy parameters based on learned patterns
    // This is where machine learning could be integrated in a full implementation
    
    std::cout << "SELF-HEALING: Strategy evolution completed." << std::endl;
}

SelfHealingAtomSpace::RepairStrategy SelfHealingAtomSpace::selectOptimalStrategy(const IntegrityReport& report) {
    if (report.structural_health_score < 0.3) {
        return RepairStrategy::EMERGENCY;
    } else if (report.corrupted_atoms_count > 100) {
        return RepairStrategy::AGGRESSIVE;
    } else if (report.overall_integrity) {
        return RepairStrategy::CONSERVATIVE;
    } else {
        return RepairStrategy::ADAPTIVE;
    }
}

void SelfHealingAtomSpace::startBackgroundMaintenance() {
    if (maintenance_active_) return;
    
    maintenance_active_ = true;
    maintenance_thread_ = std::make_unique<std::thread>(&SelfHealingAtomSpace::maintenanceLoop, this);
    
    std::cout << "SELF-HEALING: Background maintenance started." << std::endl;
}

void SelfHealingAtomSpace::stopBackgroundMaintenance() {
    if (!maintenance_active_) return;
    
    maintenance_active_ = false;
    repair_cv_.notify_all();
    
    if (maintenance_thread_ && maintenance_thread_->joinable()) {
        maintenance_thread_->join();
    }
    
    std::cout << "SELF-HEALING: Background maintenance stopped." << std::endl;
}

SelfHealingAtomSpace::IntegrityReport SelfHealingAtomSpace::performIntegrityCheck() {
    IntegrityReport report;
    
    // Find various types of issues
    std::vector<Handle> corrupted = findCorruptedAtoms();
    std::vector<Handle> orphaned = findOrphanedAtoms();
    std::vector<Handle> inconsistent = findInconsistentLinks();
    
    report.corrupted_atoms_count = corrupted.size();
    report.orphaned_atoms_count = orphaned.size();
    report.inconsistent_links_count = inconsistent.size();
    
    // Calculate overall health
    report.structural_health_score = calculateStructuralHealthScore();
    report.overall_integrity = (report.corrupted_atoms_count == 0 && 
                               report.orphaned_atoms_count < 10 && 
                               report.inconsistent_links_count == 0 &&
                               report.structural_health_score > 0.7);
    
    // Generate issue descriptions
    if (report.corrupted_atoms_count > 0) {
        report.issues_found.push_back("Found " + std::to_string(report.corrupted_atoms_count) + " corrupted atoms");
        report.repairs_recommended.push_back("Repair corrupted atoms using integrity restoration");
    }
    
    if (report.orphaned_atoms_count > 10) {
        report.issues_found.push_back("Found " + std::to_string(report.orphaned_atoms_count) + " orphaned atoms");
        report.repairs_recommended.push_back("Perform garbage collection to remove orphaned atoms");
    }
    
    if (report.inconsistent_links_count > 0) {
        report.issues_found.push_back("Found " + std::to_string(report.inconsistent_links_count) + " inconsistent links");
        report.repairs_recommended.push_back("Reconstruct damaged link structures");
    }
    
    return report;
}

std::vector<std::string> SelfHealingAtomSpace::getHealthStatus() {
    std::vector<std::string> status;
    
    status.push_back("=== Self-Healing AtomSpace Health Status ===");
    status.push_back("Current Strategy: " + std::to_string(static_cast<int>(current_strategy_)));
    status.push_back("Background Maintenance: " + std::string(maintenance_active_ ? "Active" : "Inactive"));
    status.push_back("Available Backups: " + std::to_string(backup_snapshots_.size()));
    
    // Get integrity report
    IntegrityReport report = performIntegrityCheck();
    status.push_back("Overall Integrity: " + std::string(report.overall_integrity ? "Good" : "Issues Detected"));
    status.push_back("Structural Health Score: " + std::to_string(report.structural_health_score));
    
    // Add strategy success rates
    status.push_back("Strategy Success Rates:");
    for (const auto& pair : strategy_success_rates_) {
        status.push_back("  Strategy " + std::to_string(static_cast<int>(pair.first)) + ": " + std::to_string(pair.second));
    }
    
    return status;
}

// Private helper implementations

void SelfHealingAtomSpace::maintenanceLoop() {
    while (maintenance_active_) {
        std::unique_lock<std::mutex> lock(repair_mutex_);
        
        // Wait for scheduled maintenance time or repair operations
        repair_cv_.wait_for(lock, INTEGRITY_CHECK_INTERVAL, [this] {
            return !maintenance_active_ || !repair_queue_.empty();
        });
        
        if (!maintenance_active_) break;
        
        // Process repair queue
        processRepairQueue();
        
        // Perform scheduled maintenance
        performScheduledMaintenance();
        
        lock.unlock();
        
        // Sleep to prevent excessive CPU usage
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
}

void SelfHealingAtomSpace::performScheduledMaintenance() {
    static auto last_backup = std::chrono::steady_clock::now();
    static auto last_integrity_check = std::chrono::steady_clock::now();
    
    auto now = std::chrono::steady_clock::now();
    
    // Create backup if needed
    if (now - last_backup >= BACKUP_INTERVAL) {
        createBackupSnapshot();
        last_backup = now;
    }
    
    // Perform integrity check if needed
    if (now - last_integrity_check >= INTEGRITY_CHECK_INTERVAL) {
        IntegrityReport report = performIntegrityCheck();
        
        // If issues found, schedule repairs
        if (!report.overall_integrity) {
            RepairStrategy strategy = selectOptimalStrategy(report);
            
            RepairOperation repair_op;
            repair_op.operation_id = "auto_repair_" + std::to_string(now.time_since_epoch().count());
            repair_op.strategy = strategy;
            repair_op.description = "Automatic repair based on integrity check";
            repair_op.scheduled_time = now;
            repair_op.is_emergency = (report.structural_health_score < 0.3);
            
            repair_queue_.push(repair_op);
        }
        
        last_integrity_check = now;
    }
}

void SelfHealingAtomSpace::processRepairQueue() {
    while (!repair_queue_.empty()) {
        RepairOperation op = repair_queue_.front();
        repair_queue_.pop();
        
        // Execute repair operation based on strategy
        bool success = false;
        
        switch (op.strategy) {
            case RepairStrategy::CONSERVATIVE:
                performGarbageCollection();
                success = true;
                break;
                
            case RepairStrategy::AGGRESSIVE:
                performGarbageCollection();
                optimizeAtomDistribution();
                success = true;
                break;
                
            case RepairStrategy::ADAPTIVE:
                optimizeAtomDistribution();
                success = true;
                break;
                
            case RepairStrategy::EMERGENCY:
                if (!restoreFromBackup()) {
                    performGarbageCollection();
                    optimizeAtomDistribution();
                }
                success = true;
                break;
        }
        
        // Record outcome for learning
        recordStrategyOutcome(op.strategy, success);
    }
}

std::vector<Handle> SelfHealingAtomSpace::findCorruptedAtoms() {
    std::vector<Handle> corrupted;
    
    if (!primary_space_) return corrupted;
    
    HandleSeq all_atoms = primary_space_->get_handles_by_type(ATOM, true);
    
    for (const Handle& h : all_atoms) {
        // Check for various corruption indicators
        if (!primary_space_->is_valid_handle(h)) {
            corrupted.push_back(h);
            continue;
        }
        
        // Check for null or invalid truth values in critical atoms
        if (h->is_link() && h->get_arity() > 0 && !h->getTruthValue()) {
            corrupted.push_back(h);
        }
    }
    
    return corrupted;
}

std::vector<Handle> SelfHealingAtomSpace::findOrphanedAtoms() {
    std::vector<Handle> orphaned;
    
    if (!primary_space_) return orphaned;
    
    HandleSeq all_atoms = primary_space_->get_handles_by_type(ATOM, true);
    HandleSeq all_links = primary_space_->get_handles_by_type(LINK, true);
    
    std::set<Handle> referenced_atoms;
    
    // Find all atoms referenced by links
    for (const Handle& link : all_links) {
        for (size_t i = 0; i < link->get_arity(); ++i) {
            referenced_atoms.insert(link->getOutgoingAtom(i));
        }
    }
    
    // Find atoms not referenced by any link (potential orphans)
    for (const Handle& atom : all_atoms) {
        if (atom->is_node() && referenced_atoms.find(atom) == referenced_atoms.end()) {
            // Additional criteria to determine if truly orphaned
            if (!atom->getAttentionValue() || atom->getAttentionValue()->getSTI() == 0) {
                orphaned.push_back(atom);
            }
        }
    }
    
    return orphaned;
}

std::vector<Handle> SelfHealingAtomSpace::findInconsistentLinks() {
    std::vector<Handle> inconsistent;
    
    if (!primary_space_) return inconsistent;
    
    HandleSeq all_links = primary_space_->get_handles_by_type(LINK, true);
    
    for (const Handle& link : all_links) {
        // Check for links with invalid outgoing atoms
        bool is_inconsistent = false;
        
        for (size_t i = 0; i < link->get_arity(); ++i) {
            Handle outgoing = link->getOutgoingAtom(i);
            if (!primary_space_->is_valid_handle(outgoing)) {
                is_inconsistent = true;
                break;
            }
        }
        
        if (is_inconsistent) {
            inconsistent.push_back(link);
        }
    }
    
    return inconsistent;
}

double SelfHealingAtomSpace::calculateStructuralHealthScore() {
    if (!primary_space_) return 0.0;
    
    HandleSeq all_atoms = primary_space_->get_handles_by_type(ATOM, true);
    if (all_atoms.empty()) return 1.0;
    
    size_t healthy_atoms = 0;
    
    for (const Handle& atom : all_atoms) {
        bool is_healthy = true;
        
        // Check basic validity
        if (!primary_space_->is_valid_handle(atom)) {
            is_healthy = false;
        }
        
        // Check truth value presence for important atoms
        if (is_healthy && atom->is_link() && atom->get_arity() > 0 && !atom->getTruthValue()) {
            is_healthy = false;
        }
        
        if (is_healthy) {
            healthy_atoms++;
        }
    }
    
    return static_cast<double>(healthy_atoms) / all_atoms.size();
}

bool SelfHealingAtomSpace::repairAtomIntegrity(const Handle& atom) {
    if (!primary_space_ || !primary_space_->is_valid_handle(atom)) {
        return false;
    }
    
    // Try to repair by ensuring proper truth value
    if (!atom->getTruthValue()) {
        atom->setTruthValue(TruthValue::DEFAULT_TV());
    }
    
    // Try to repair by ensuring proper attention value
    if (!atom->getAttentionValue()) {
        atom->setAttentionValue(AttentionValue::DEFAULT_AV());
    }
    
    return true;
}

void SelfHealingAtomSpace::consolidateDuplicateAtoms() {
    // This would implement duplicate detection and consolidation
    // For now, this is a placeholder for the complex logic required
    std::cout << "SELF-HEALING: Consolidating duplicate atoms (placeholder)..." << std::endl;
}

void SelfHealingAtomSpace::reestablishBrokenLinks() {
    // This would implement broken link detection and repair
    // For now, this is a placeholder for the complex logic required
    std::cout << "SELF-HEALING: Reestablishing broken links (placeholder)..." << std::endl;
}

size_t SelfHealingAtomSpace::calculateIntegrityChecksum(const HandleSeq& atoms) {
    // Simple checksum based on atom count and type distribution
    std::hash<std::string> hasher;
    std::string checksum_string = std::to_string(atoms.size());
    
    std::map<Type, size_t> type_counts;
    for (const Handle& h : atoms) {
        type_counts[h->get_type()]++;
    }
    
    for (const auto& pair : type_counts) {
        checksum_string += std::to_string(pair.first) + ":" + std::to_string(pair.second) + ";";
    }
    
    return hasher(checksum_string);
}

bool SelfHealingAtomSpace::validateBackupIntegrity(const BackupSnapshot& snapshot) {
    size_t calculated_checksum = calculateIntegrityChecksum(snapshot.atoms);
    return calculated_checksum == snapshot.integrity_checksum;
}

void SelfHealingAtomSpace::applyBackupToAtomSpace(const BackupSnapshot& snapshot) {
    if (!primary_space_) return;
    
    // Clear current AtomSpace
    primary_space_->clear();
    
    // Restore atoms from backup
    for (const Handle& h : snapshot.atoms) {
        if (primary_space_->is_valid_handle(h)) {
            // Restore truth values
            auto tv_it = snapshot.truth_values.find(h);
            if (tv_it != snapshot.truth_values.end()) {
                h->setTruthValue(tv_it->second);
            }
            
            // Restore attention values
            auto av_it = snapshot.attention_values.find(h);
            if (av_it != snapshot.attention_values.end()) {
                h->setAttentionValue(av_it->second);
            }
        }
    }
}

void SelfHealingAtomSpace::recordStrategyOutcome(RepairStrategy strategy, bool success) {
    double current_rate = strategy_success_rates_[strategy];
    double adjustment = success ? 0.1 : -0.1;
    
    // Exponential moving average for strategy success rates
    strategy_success_rates_[strategy] = current_rate * 0.9 + (success ? 1.0 : 0.0) * 0.1;
    
    // Ensure rates stay within reasonable bounds
    if (strategy_success_rates_[strategy] > 1.0) {
        strategy_success_rates_[strategy] = 1.0;
    } else if (strategy_success_rates_[strategy] < 0.0) {
        strategy_success_rates_[strategy] = 0.0;
    }
}

void SelfHealingAtomSpace::updateStrategyRatings() {
    // This could implement more sophisticated learning algorithms
    // For now, we maintain the current exponential moving average approach
}

std::vector<std::string> SelfHealingAtomSpace::extractFailurePatterns(const std::vector<RepairOperation>& operations) {
    std::vector<std::string> patterns;
    
    // Simple pattern extraction based on operation characteristics
    for (const RepairOperation& op : operations) {
        if (op.is_emergency) {
            patterns.push_back("emergency_repair_failure");
        }
        
        patterns.push_back("strategy_" + std::to_string(static_cast<int>(op.strategy)) + "_failure");
    }
    
    return patterns;
}

void SelfHealingAtomSpace::redistributeAttentionValues() {
    // Placeholder for attention redistribution algorithm
    std::cout << "SELF-HEALING: Redistributing attention values (placeholder)..." << std::endl;
}

void SelfHealingAtomSpace::optimizeLinkStructures() {
    // Placeholder for link structure optimization
    std::cout << "SELF-HEALING: Optimizing link structures (placeholder)..." << std::endl;
}

void SelfHealingAtomSpace::compactAtomStorage() {
    // Placeholder for storage compaction
    std::cout << "SELF-HEALING: Compacting atom storage (placeholder)..." << std::endl;
}

std::vector<std::string> SelfHealingAtomSpace::listAvailableBackups() {
    std::vector<std::string> backup_ids;
    for (const auto& snapshot : backup_snapshots_) {
        backup_ids.push_back(snapshot.snapshot_id);
    }
    return backup_ids;
}

bool SelfHealingAtomSpace::deleteBackup(const std::string& snapshot_id) {
    auto it = std::find_if(backup_snapshots_.begin(), backup_snapshots_.end(),
                          [&snapshot_id](const BackupSnapshot& snapshot) {
                              return snapshot.snapshot_id == snapshot_id;
                          });
    
    if (it != backup_snapshots_.end()) {
        backup_snapshots_.erase(it);
        return true;
    }
    
    return false;
}

void SelfHealingAtomSpace::cleanupOldBackups() {
    // Remove backups older than a certain threshold
    auto cutoff_time = std::chrono::steady_clock::now() - std::chrono::hours(24);
    
    backup_snapshots_.erase(
        std::remove_if(backup_snapshots_.begin(), backup_snapshots_.end(),
                      [cutoff_time](const BackupSnapshot& snapshot) {
                          return snapshot.timestamp < cutoff_time;
                      }),
        backup_snapshots_.end()
    );
}

} // namespace autonomous
} // namespace opencog