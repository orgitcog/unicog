#ifndef SELF_HEALING_ATOMSPACE_HPP
#define SELF_HEALING_ATOMSPACE_HPP

#include <vector>
#include <memory>
#include <queue>
#include <chrono>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog {
namespace autonomous {

/**
 * Manages self-healing capabilities for AtomSpace, including proactive maintenance,
 * reactive healing, and adaptive mechanisms for continuous improvement.
 */
class SelfHealingAtomSpace {
public:
    enum class RepairStrategy {
        CONSERVATIVE,    // Minimal changes, preserve existing structure
        AGGRESSIVE,      // Significant reorganization for optimization
        ADAPTIVE,        // Strategy selection based on situation
        EMERGENCY        // Rapid response for critical failures
    };

    struct BackupSnapshot {
        std::string snapshot_id;
        std::chrono::time_point<std::chrono::steady_clock> timestamp;
        HandleSeq atoms;
        std::map<Handle, TruthValuePtr> truth_values;
        std::map<Handle, AttentionValuePtr> attention_values;
        size_t integrity_checksum;
    };

    struct RepairOperation {
        std::string operation_id;
        RepairStrategy strategy;
        std::vector<Handle> target_atoms;
        std::string description;
        std::chrono::time_point<std::chrono::steady_clock> scheduled_time;
        bool is_emergency;
    };

    struct IntegrityReport {
        bool overall_integrity;
        std::vector<std::string> issues_found;
        std::vector<std::string> repairs_recommended;
        size_t corrupted_atoms_count;
        size_t orphaned_atoms_count;
        size_t inconsistent_links_count;
        double structural_health_score;
    };

private:
    AtomSpacePtr primary_space_;
    std::vector<BackupSnapshot> backup_snapshots_;
    std::queue<RepairOperation> repair_queue_;
    RepairStrategy current_strategy_;
    
    // Threading for background operations
    std::unique_ptr<std::thread> maintenance_thread_;
    std::mutex repair_mutex_;
    std::condition_variable repair_cv_;
    bool maintenance_active_;
    
    // Adaptive learning for repair strategies
    std::map<RepairStrategy, double> strategy_success_rates_;
    std::vector<std::string> learned_patterns_;
    
    // Configuration parameters
    static constexpr size_t MAX_BACKUPS = 10;
    static constexpr std::chrono::minutes BACKUP_INTERVAL{30};
    static constexpr std::chrono::minutes INTEGRITY_CHECK_INTERVAL{15};

public:
    SelfHealingAtomSpace(AtomSpacePtr primary_space);
    ~SelfHealingAtomSpace();
    
    // Proactive maintenance operations
    void scheduleIntegrityChecks();
    void performGarbageCollection();
    void optimizeAtomDistribution();
    void createBackupSnapshot();
    
    // Reactive healing operations
    void repairCorruptedAtoms(const std::vector<Handle>& corrupted_atoms);
    bool restoreFromBackup(const std::string& snapshot_id = "");
    void reconstructDamagedSubgraphs(const std::vector<Handle>& damaged_region);
    
    // Adaptive mechanisms
    void learnFromFailures(const std::vector<RepairOperation>& failed_operations);
    void evolveRepairStrategies();
    RepairStrategy selectOptimalStrategy(const IntegrityReport& report);
    
    // Control and monitoring
    void startBackgroundMaintenance();
    void stopBackgroundMaintenance();
    IntegrityReport performIntegrityCheck();
    std::vector<std::string> getHealthStatus();
    
    // Configuration and tuning
    void setRepairStrategy(RepairStrategy strategy);
    void setMaintenanceInterval(std::chrono::minutes interval);
    void enableEmergencyMode(bool enabled);
    
    // Backup management
    std::vector<std::string> listAvailableBackups();
    bool deleteBackup(const std::string& snapshot_id);
    void cleanupOldBackups();

private:
    // Internal maintenance operations
    void maintenanceLoop();
    void performScheduledMaintenance();
    void processRepairQueue();
    
    // Integrity checking helpers
    std::vector<Handle> findCorruptedAtoms();
    std::vector<Handle> findOrphanedAtoms();
    std::vector<Handle> findInconsistentLinks();
    double calculateStructuralHealthScore();
    
    // Repair implementation helpers
    bool repairAtomIntegrity(const Handle& atom);
    void removeOrphanedStructures();
    void consolidateDuplicateAtoms();
    void reestablishBrokenLinks();
    
    // Backup and restoration helpers
    size_t calculateIntegrityChecksum(const HandleSeq& atoms);
    bool validateBackupIntegrity(const BackupSnapshot& snapshot);
    void applyBackupToAtomSpace(const BackupSnapshot& snapshot);
    
    // Adaptive learning helpers
    void recordStrategyOutcome(RepairStrategy strategy, bool success);
    void updateStrategyRatings();
    std::vector<std::string> extractFailurePatterns(const std::vector<RepairOperation>& operations);
    
    // Optimization helpers
    void redistributeAttentionValues();
    void optimizeLinkStructures();
    void compactAtomStorage();
};

} // namespace autonomous
} // namespace opencog

#endif // SELF_HEALING_ATOMSPACE_HPP