/*
 * UnsupervisedLearner.cc - Unsupervised Learning implementation
 */

#include "UnsupervisedLearner.h"
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <algorithm>
#include <cmath>
#include <random>
#include <chrono>
#include <unordered_map>
#include <queue>

namespace opencog {
namespace main {

UnsupervisedLearner::UnsupervisedLearner(AtomSpace* atomspace) :
    atomspace_(atomspace),
    pattern_min_support_(0.01),
    pattern_min_confidence_(0.5),
    max_patterns_(10000),
    surprise_threshold_(0.3),
    clustering_method_("kmeans"),
    max_iterations_(100),
    convergence_threshold_(0.001),
    total_patterns_discovered_(0),
    total_clusters_formed_(0),
    total_concepts_created_(0),
    learning_cycles_(0),
    total_learning_time_(0.0)
{
    logger().info("UnsupervisedLearner initialized");
}

UnsupervisedLearner::~UnsupervisedLearner()
{
    logger().info("UnsupervisedLearner destroyed. Total patterns discovered: %zu, "
                 "clusters formed: %zu, concepts created: %zu",
                 total_patterns_discovered_, total_clusters_formed_, total_concepts_created_);
}

LearningResult UnsupervisedLearner::runLearningPipeline(const std::vector<Handle>& input_atoms)
{
    LearningResult result;
    auto start_time = std::chrono::high_resolution_clock::now();
    
    logger().info("Running unsupervised learning pipeline on %zu atoms", input_atoms.size());
    
    if (input_atoms.empty() || !atomspace_) {
        logger().warning("No input atoms or AtomSpace not available");
        return result;
    }
    
    // Step 1: Pattern Mining
    result.discovered_patterns = minePatterns(input_atoms, pattern_min_support_, pattern_min_confidence_);
    logger().info("Discovered %zu patterns", result.discovered_patterns.size());
    
    // Step 2: Clustering
    result.formed_clusters = performClustering(input_atoms);
    logger().info("Formed %zu clusters", result.formed_clusters.size());
    
    // Step 3: Concept Formation
    result.concept_hierarchy = formConcepts(result.discovered_patterns, result.formed_clusters);
    logger().info("Created concept hierarchy with %zu concepts", result.concept_hierarchy.size());
    
    // Calculate learning quality score
    double pattern_score = std::min(1.0, result.discovered_patterns.size() / 100.0);
    double cluster_score = result.formed_clusters.empty() ? 0.0 : 
        std::accumulate(result.formed_clusters.begin(), result.formed_clusters.end(), 0.0,
            [](double sum, const Cluster& c) { return sum + c.cohesion; }) / result.formed_clusters.size();
    double concept_score = std::min(1.0, result.concept_hierarchy.size() / 50.0);
    
    result.learning_quality_score = (pattern_score + cluster_score + concept_score) / 3.0;
    result.iterations_performed = max_iterations_;
    
    // Update statistics
    learning_cycles_++;
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    total_learning_time_ += duration.count() / 1000.0;
    
    logger().info("Learning pipeline completed with quality score: %.3f", 
                 result.learning_quality_score);
    
    return result;
}

std::vector<Pattern> UnsupervisedLearner::minePatterns(const std::vector<Handle>& input_atoms,
                                                       double min_support,
                                                       double min_confidence)
{
    std::vector<Pattern> patterns;
    
    if (input_atoms.empty()) {
        return patterns;
    }
    
    // Find frequent itemsets
    std::map<HandleSeq, int> frequent_itemsets = findFrequentItemsets(input_atoms, min_support);
    
    // Generate association rules from frequent itemsets
    std::vector<Pattern> association_patterns = generateAssociationRules(frequent_itemsets, min_confidence);
    patterns.insert(patterns.end(), association_patterns.begin(), association_patterns.end());
    
    // Mine structural patterns (e.g., common link structures)
    std::map<std::string, std::vector<Handle>> type_groups;
    for (const Handle& atom : input_atoms) {
        type_groups[getAtomSignature(atom)].push_back(atom);
    }
    
    // Create patterns from frequent atom types
    for (const auto& [signature, instances] : type_groups) {
        double support = static_cast<double>(instances.size()) / input_atoms.size();
        if (support >= min_support) {
            Pattern type_pattern;
            type_pattern.pattern_type = "type_pattern";
            type_pattern.frequency = instances.size();
            type_pattern.support = support;
            type_pattern.confidence = 1.0; // Type patterns have full confidence
            type_pattern.instances = instances;
            type_pattern.pattern_size = 1;
            
            // Create pattern representation in AtomSpace
            Handle pattern_node = atomspace_->add_node(CONCEPT_NODE, 
                "type_pattern_" + signature + "_" + std::to_string(patterns.size()));
            type_pattern.pattern_handle = pattern_node;
            
            // Calculate surprise value
            type_pattern.surprise_value = calculateSurprise(type_pattern);
            
            if (type_pattern.surprise_value >= surprise_threshold_) {
                patterns.push_back(type_pattern);
            }
        }
    }
    
    // Mine sequential patterns for links
    for (const Handle& atom : input_atoms) {
        if (atom->is_link() && atom->get_arity() >= 2) {
            HandleSeq outgoing = atom->getOutgoingSet();
            
            // Check for repeated substructures
            for (size_t window = 2; window <= std::min(size_t(4), outgoing.size()); ++window) {
                for (size_t i = 0; i <= outgoing.size() - window; ++i) {
                    HandleSeq subseq(outgoing.begin() + i, outgoing.begin() + i + window);
                    
                    // Count occurrences of this subsequence
                    int count = 0;
                    for (const Handle& other : input_atoms) {
                        if (other->is_link()) {
                            HandleSeq other_out = other->getOutgoingSet();
                            if (std::search(other_out.begin(), other_out.end(),
                                          subseq.begin(), subseq.end()) != other_out.end()) {
                                count++;
                            }
                        }
                    }
                    
                    double support = static_cast<double>(count) / input_atoms.size();
                    if (support >= min_support && count >= 2) {
                        Pattern seq_pattern;
                        seq_pattern.pattern_type = "sequence_pattern";
                        seq_pattern.frequency = count;
                        seq_pattern.support = support;
                        seq_pattern.confidence = 0.8; // Heuristic confidence
                        seq_pattern.pattern_size = window;
                        
                        Handle pattern_link = atomspace_->add_link(PATTERN_LINK, subseq);
                        seq_pattern.pattern_handle = pattern_link;
                        seq_pattern.surprise_value = calculateSurprise(seq_pattern);
                        
                        if (seq_pattern.surprise_value >= surprise_threshold_) {
                            patterns.push_back(seq_pattern);
                        }
                    }
                }
            }
        }
    }
    
    // Sort patterns by surprise value
    std::sort(patterns.begin(), patterns.end(),
              [](const Pattern& a, const Pattern& b) { return a.surprise_value > b.surprise_value; });
    
    // Limit to max patterns
    if (patterns.size() > max_patterns_) {
        patterns.resize(max_patterns_);
    }
    
    // Update statistics
    total_patterns_discovered_ += patterns.size();
    
    // Store patterns in AtomSpace
    Handle pattern_space = atomspace_->add_node(CONCEPT_NODE, "discovered_patterns");
    for (const Pattern& pattern : patterns) {
        atomspace_->add_link(MEMBER_LINK, HandleSeq{pattern.pattern_handle, pattern_space});
        
        // Add pattern metadata
        pattern.pattern_handle->setValue(
            atomspace_->add_node(PREDICATE_NODE, "frequency"),
            createFloatValue(std::vector<double>{pattern.frequency}));
        pattern.pattern_handle->setValue(
            atomspace_->add_node(PREDICATE_NODE, "support"),
            createFloatValue(std::vector<double>{pattern.support}));
        pattern.pattern_handle->setValue(
            atomspace_->add_node(PREDICATE_NODE, "confidence"),
            createFloatValue(std::vector<double>{pattern.confidence}));
        pattern.pattern_handle->setValue(
            atomspace_->add_node(PREDICATE_NODE, "surprise"),
            createFloatValue(std::vector<double>{pattern.surprise_value}));
    }
    
    return patterns;
}

std::vector<Cluster> UnsupervisedLearner::performClustering(const std::vector<Handle>& input_atoms,
                                                           size_t num_clusters,
                                                           double similarity_threshold)
{
    if (input_atoms.empty()) {
        return std::vector<Cluster>();
    }
    
    // Determine number of clusters if not specified
    if (num_clusters == 0) {
        // Use sqrt(n/2) as heuristic for cluster count
        num_clusters = std::max(size_t(2), size_t(std::sqrt(input_atoms.size() / 2.0)));
    }
    
    std::vector<Cluster> clusters;
    
    if (clustering_method_ == "kmeans") {
        clusters = kMeansClustering(input_atoms, num_clusters);
    } else if (clustering_method_ == "hierarchical") {
        clusters = hierarchicalClustering(input_atoms, similarity_threshold);
    } else {
        // Default to k-means
        clusters = kMeansClustering(input_atoms, num_clusters);
    }
    
    // Calculate cluster quality metrics
    for (auto& cluster : clusters) {
        cluster.cohesion = calculateClusterCohesion(cluster);
        cluster.separation = calculateClusterSeparation(cluster, clusters);
        
        // Generate cluster label
        cluster.cluster_label = "cluster_" + std::to_string(cluster.cluster_center->get_hash());
        
        // Store cluster in AtomSpace
        Handle cluster_node = atomspace_->add_node(CONCEPT_NODE, cluster.cluster_label);
        for (const Handle& member : cluster.members) {
            atomspace_->add_link(MEMBER_LINK, HandleSeq{member, cluster_node});
        }
        
        // Add cluster metadata
        cluster_node->setValue(
            atomspace_->add_node(PREDICATE_NODE, "cohesion"),
            createFloatValue(std::vector<double>{cluster.cohesion}));
        cluster_node->setValue(
            atomspace_->add_node(PREDICATE_NODE, "separation"),
            createFloatValue(std::vector<double>{cluster.separation}));
    }
    
    // Update statistics
    total_clusters_formed_ += clusters.size();
    
    return clusters;
}

std::map<Handle, Handle> UnsupervisedLearner::formConcepts(const std::vector<Pattern>& patterns,
                                                          const std::vector<Cluster>& clusters)
{
    std::map<Handle, Handle> concept_hierarchy;
    std::vector<Handle> created_concepts;
    
    // Create concepts from patterns
    for (const Pattern& pattern : patterns) {
        if (pattern.instances.size() >= 3) { // Minimum instances for concept formation
            Handle concept = createAbstractConcept(pattern.instances, pattern.pattern_type);
            created_concepts.push_back(concept);
            
            // Link instances to concept
            for (const Handle& instance : pattern.instances) {
                concept_hierarchy[instance] = concept;
                atomspace_->add_link(INHERITANCE_LINK, HandleSeq{instance, concept});
            }
            
            // Generalize pattern if possible
            if (pattern.pattern_size > 1) {
                Handle general_concept = generalizePattern(pattern);
                if (general_concept != Handle::UNDEFINED) {
                    concept_hierarchy[concept] = general_concept;
                    atomspace_->add_link(INHERITANCE_LINK, HandleSeq{concept, general_concept});
                    created_concepts.push_back(general_concept);
                }
            }
        }
    }
    
    // Create concepts from clusters
    for (const Cluster& cluster : clusters) {
        if (cluster.members.size() >= 2 && cluster.cohesion > 0.5) {
            Handle cluster_concept = createAbstractConcept(cluster.members, "cluster_concept");
            created_concepts.push_back(cluster_concept);
            
            // Link cluster members to concept
            for (const Handle& member : cluster.members) {
                if (concept_hierarchy.find(member) == concept_hierarchy.end()) {
                    concept_hierarchy[member] = cluster_concept;
                    atomspace_->add_link(INHERITANCE_LINK, HandleSeq{member, cluster_concept});
                }
            }
        }
    }
    
    // Build multi-level hierarchy
    buildConceptHierarchy(concept_hierarchy, created_concepts);
    
    // Update statistics
    total_concepts_created_ += created_concepts.size();
    
    return concept_hierarchy;
}

std::map<HandleSeq, int> UnsupervisedLearner::findFrequentItemsets(
    const std::vector<Handle>& atoms, double min_support)
{
    std::map<HandleSeq, int> itemsets;
    size_t min_count = std::ceil(atoms.size() * min_support);
    
    // Find 1-itemsets
    std::map<Handle, int> single_counts;
    for (const Handle& atom : atoms) {
        single_counts[atom]++;
    }
    
    // Keep frequent 1-itemsets
    for (const auto& [atom, count] : single_counts) {
        if (count >= static_cast<int>(min_count)) {
            itemsets[HandleSeq{atom}] = count;
        }
    }
    
    // Generate 2-itemsets from links
    for (const Handle& atom : atoms) {
        if (atom->is_link()) {
            HandleSeq outgoing = atom->getOutgoingSet();
            
            // Generate pairs
            for (size_t i = 0; i < outgoing.size(); ++i) {
                for (size_t j = i + 1; j < outgoing.size(); ++j) {
                    HandleSeq pair = {outgoing[i], outgoing[j]};
                    std::sort(pair.begin(), pair.end());
                    
                    // Count co-occurrences
                    int count = 0;
                    for (const Handle& other : atoms) {
                        if (other->is_link()) {
                            HandleSeq other_out = other->getOutgoingSet();
                            if (std::find(other_out.begin(), other_out.end(), pair[0]) != other_out.end() &&
                                std::find(other_out.begin(), other_out.end(), pair[1]) != other_out.end()) {
                                count++;
                            }
                        }
                    }
                    
                    if (count >= static_cast<int>(min_count)) {
                        itemsets[pair] = count;
                    }
                }
            }
        }
    }
    
    return itemsets;
}

std::vector<Pattern> UnsupervisedLearner::generateAssociationRules(
    const std::map<HandleSeq, int>& itemsets, double min_confidence)
{
    std::vector<Pattern> rules;
    
    // Generate rules from 2-itemsets
    for (const auto& [itemset, count] : itemsets) {
        if (itemset.size() == 2) {
            // Rule: itemset[0] -> itemset[1]
            auto it1 = itemsets.find(HandleSeq{itemset[0]});
            if (it1 != itemsets.end()) {
                double confidence = static_cast<double>(count) / it1->second;
                if (confidence >= min_confidence) {
                    Pattern rule;
                    rule.pattern_type = "association_rule";
                    rule.frequency = count;
                    rule.support = static_cast<double>(count) / itemsets.size();
                    rule.confidence = confidence;
                    rule.pattern_size = 2;
                    
                    // Create implication link for the rule
                    Handle rule_link = atomspace_->add_link(IMPLICATION_LINK, 
                        HandleSeq{itemset[0], itemset[1]});
                    rule_link->setTruthValue(SimpleTruthValue::createTV(confidence, 0.9));
                    rule.pattern_handle = rule_link;
                    
                    rule.surprise_value = calculateSurprise(rule);
                    rules.push_back(rule);
                }
            }
            
            // Rule: itemset[1] -> itemset[0]
            auto it2 = itemsets.find(HandleSeq{itemset[1]});
            if (it2 != itemsets.end()) {
                double confidence = static_cast<double>(count) / it2->second;
                if (confidence >= min_confidence) {
                    Pattern rule;
                    rule.pattern_type = "association_rule";
                    rule.frequency = count;
                    rule.support = static_cast<double>(count) / itemsets.size();
                    rule.confidence = confidence;
                    rule.pattern_size = 2;
                    
                    Handle rule_link = atomspace_->add_link(IMPLICATION_LINK, 
                        HandleSeq{itemset[1], itemset[0]});
                    rule_link->setTruthValue(SimpleTruthValue::createTV(confidence, 0.9));
                    rule.pattern_handle = rule_link;
                    
                    rule.surprise_value = calculateSurprise(rule);
                    rules.push_back(rule);
                }
            }
        }
    }
    
    return rules;
}

double UnsupervisedLearner::calculateSurprise(const Pattern& pattern)
{
    // Calculate surprise as combination of unexpectedness and interestingness
    double expected_frequency = pattern.instances.size() * pattern.support;
    double actual_frequency = pattern.frequency;
    
    // Poisson-based surprise
    double lambda = expected_frequency;
    double poisson_prob = std::exp(-lambda) * std::pow(lambda, actual_frequency) / 
                         std::tgamma(actual_frequency + 1);
    double surprise = -std::log(poisson_prob + 1e-10);
    
    // Normalize surprise value
    surprise = std::tanh(surprise / 10.0);
    
    // Adjust for pattern complexity
    surprise *= (1.0 + 0.1 * pattern.pattern_size);
    
    // Adjust for confidence
    surprise *= pattern.confidence;
    
    return surprise;
}

std::vector<Cluster> UnsupervisedLearner::kMeansClustering(const std::vector<Handle>& atoms, size_t k)
{
    std::vector<Cluster> clusters(k);
    
    if (atoms.size() < k) {
        logger().warning("Too few atoms (%zu) for %zu clusters", atoms.size(), k);
        k = atoms.size();
        clusters.resize(k);
    }
    
    // Initialize cluster centers randomly
    std::vector<Handle> centers;
    std::vector<Handle> shuffled_atoms = atoms;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::shuffle(shuffled_atoms.begin(), shuffled_atoms.end(), gen);
    
    for (size_t i = 0; i < k; ++i) {
        centers.push_back(shuffled_atoms[i]);
        clusters[i].cluster_center = centers[i];
    }
    
    // K-means iterations
    bool converged = false;
    size_t iteration = 0;
    
    while (!converged && iteration < max_iterations_) {
        // Clear previous assignments
        for (auto& cluster : clusters) {
            cluster.members.clear();
        }
        
        // Assign atoms to nearest cluster
        for (const Handle& atom : atoms) {
            size_t best_cluster = 0;
            double best_similarity = calculateSimilarity(atom, centers[0]);
            
            for (size_t i = 1; i < k; ++i) {
                double sim = calculateSimilarity(atom, centers[i]);
                if (sim > best_similarity) {
                    best_similarity = sim;
                    best_cluster = i;
                }
            }
            
            clusters[best_cluster].members.push_back(atom);
        }
        
        // Update cluster centers
        converged = true;
        for (size_t i = 0; i < k; ++i) {
            if (!clusters[i].members.empty()) {
                Handle new_center = findClusterCenter(clusters[i].members);
                if (new_center != centers[i]) {
                    converged = false;
                    centers[i] = new_center;
                    clusters[i].cluster_center = new_center;
                }
            }
        }
        
        iteration++;
    }
    
    // Remove empty clusters
    clusters.erase(
        std::remove_if(clusters.begin(), clusters.end(),
                      [](const Cluster& c) { return c.members.empty(); }),
        clusters.end());
    
    logger().debug("K-means converged in %zu iterations with %zu non-empty clusters", 
                  iteration, clusters.size());
    
    return clusters;
}

std::vector<Cluster> UnsupervisedLearner::hierarchicalClustering(
    const std::vector<Handle>& atoms, double threshold)
{
    std::vector<Cluster> clusters;
    
    // Initialize each atom as its own cluster
    for (const Handle& atom : atoms) {
        Cluster c;
        c.cluster_center = atom;
        c.members = {atom};
        clusters.push_back(c);
    }
    
    // Merge clusters until threshold
    while (clusters.size() > 1) {
        // Find most similar pair of clusters
        size_t merge_i = 0, merge_j = 1;
        double max_similarity = 0.0;
        
        for (size_t i = 0; i < clusters.size(); ++i) {
            for (size_t j = i + 1; j < clusters.size(); ++j) {
                // Average linkage
                double total_sim = 0.0;
                for (const Handle& a : clusters[i].members) {
                    for (const Handle& b : clusters[j].members) {
                        total_sim += calculateSimilarity(a, b);
                    }
                }
                double avg_sim = total_sim / (clusters[i].members.size() * clusters[j].members.size());
                
                if (avg_sim > max_similarity) {
                    max_similarity = avg_sim;
                    merge_i = i;
                    merge_j = j;
                }
            }
        }
        
        // Check if should merge
        if (max_similarity < threshold) {
            break;
        }
        
        // Merge clusters
        clusters[merge_i].members.insert(clusters[merge_i].members.end(),
                                       clusters[merge_j].members.begin(),
                                       clusters[merge_j].members.end());
        clusters[merge_i].cluster_center = findClusterCenter(clusters[merge_i].members);
        clusters.erase(clusters.begin() + merge_j);
    }
    
    return clusters;
}

double UnsupervisedLearner::calculateSimilarity(const Handle& atom1, const Handle& atom2)
{
    if (atom1 == atom2) {
        return 1.0;
    }
    
    double similarity = 0.0;
    
    // Type similarity
    if (atom1->get_type() == atom2->get_type()) {
        similarity += 0.3;
    }
    
    // Structural similarity for links
    if (atom1->is_link() && atom2->is_link()) {
        HandleSeq out1 = atom1->getOutgoingSet();
        HandleSeq out2 = atom2->getOutgoingSet();
        
        // Arity similarity
        if (out1.size() == out2.size()) {
            similarity += 0.2;
            
            // Component similarity
            double component_sim = 0.0;
            for (size_t i = 0; i < out1.size(); ++i) {
                if (out1[i] == out2[i]) {
                    component_sim += 1.0;
                } else if (out1[i]->get_type() == out2[i]->get_type()) {
                    component_sim += 0.5;
                }
            }
            similarity += 0.3 * (component_sim / out1.size());
        }
    }
    
    // Name similarity for nodes
    if (atom1->is_node() && atom2->is_node()) {
        std::string name1 = atom1->get_name();
        std::string name2 = atom2->get_name();
        
        // Simple string similarity
        size_t common_prefix = 0;
        size_t min_len = std::min(name1.length(), name2.length());
        for (size_t i = 0; i < min_len; ++i) {
            if (name1[i] == name2[i]) {
                common_prefix++;
            } else {
                break;
            }
        }
        
        if (min_len > 0) {
            similarity += 0.2 * (common_prefix / static_cast<double>(min_len));
        }
    }
    
    // Truth value similarity
    TruthValuePtr tv1 = atom1->getTruthValue();
    TruthValuePtr tv2 = atom2->getTruthValue();
    if (tv1 && tv2) {
        double strength_diff = std::abs(tv1->get_mean() - tv2->get_mean());
        double confidence_diff = std::abs(tv1->get_confidence() - tv2->get_confidence());
        similarity += 0.1 * (1.0 - strength_diff) + 0.1 * (1.0 - confidence_diff);
    }
    
    return similarity;
}

Handle UnsupervisedLearner::findClusterCenter(const std::vector<Handle>& cluster_members)
{
    if (cluster_members.empty()) {
        return Handle::UNDEFINED;
    }
    
    if (cluster_members.size() == 1) {
        return cluster_members[0];
    }
    
    // Find medoid (member with highest average similarity to all others)
    Handle best_center = cluster_members[0];
    double best_avg_sim = 0.0;
    
    for (const Handle& candidate : cluster_members) {
        double total_sim = 0.0;
        for (const Handle& member : cluster_members) {
            if (candidate != member) {
                total_sim += calculateSimilarity(candidate, member);
            }
        }
        double avg_sim = total_sim / (cluster_members.size() - 1);
        
        if (avg_sim > best_avg_sim) {
            best_avg_sim = avg_sim;
            best_center = candidate;
        }
    }
    
    return best_center;
}

double UnsupervisedLearner::calculateClusterCohesion(const Cluster& cluster)
{
    if (cluster.members.size() <= 1) {
        return 1.0;
    }
    
    double total_similarity = 0.0;
    size_t pair_count = 0;
    
    for (size_t i = 0; i < cluster.members.size(); ++i) {
        for (size_t j = i + 1; j < cluster.members.size(); ++j) {
            total_similarity += calculateSimilarity(cluster.members[i], cluster.members[j]);
            pair_count++;
        }
    }
    
    return pair_count > 0 ? total_similarity / pair_count : 0.0;
}

double UnsupervisedLearner::calculateClusterSeparation(const Cluster& cluster,
                                                      const std::vector<Cluster>& all_clusters)
{
    if (all_clusters.size() <= 1) {
        return 1.0;
    }
    
    double min_inter_cluster_sim = 1.0;
    
    for (const Cluster& other : all_clusters) {
        if (other.cluster_center == cluster.cluster_center) {
            continue;
        }
        
        double total_sim = 0.0;
        size_t pair_count = 0;
        
        for (const Handle& member1 : cluster.members) {
            for (const Handle& member2 : other.members) {
                total_sim += calculateSimilarity(member1, member2);
                pair_count++;
            }
        }
        
        if (pair_count > 0) {
            double avg_sim = total_sim / pair_count;
            min_inter_cluster_sim = std::min(min_inter_cluster_sim, avg_sim);
        }
    }
    
    return 1.0 - min_inter_cluster_sim;
}

Handle UnsupervisedLearner::createAbstractConcept(const std::vector<Handle>& instances,
                                                 const std::string& concept_type)
{
    if (instances.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Extract common features
    std::vector<Handle> common_features = extractCommonFeatures(instances);
    
    // Create concept node
    std::string concept_name = concept_type + "_concept_" + 
                              std::to_string(std::hash<std::vector<Handle>>{}(instances));
    Handle concept = atomspace_->add_node(CONCEPT_NODE, concept_name);
    
    // Add concept properties
    if (!common_features.empty()) {
        Handle feature_set = atomspace_->add_link(SET_LINK, common_features);
        atomspace_->add_link(EVALUATION_LINK, HandleSeq{
            atomspace_->add_node(PREDICATE_NODE, "has_features"),
            concept,
            feature_set
        });
    }
    
    // Calculate concept strength based on instance coherence
    double total_similarity = 0.0;
    size_t pair_count = 0;
    for (size_t i = 0; i < instances.size(); ++i) {
        for (size_t j = i + 1; j < instances.size(); ++j) {
            total_similarity += calculateSimilarity(instances[i], instances[j]);
            pair_count++;
        }
    }
    
    double concept_strength = pair_count > 0 ? total_similarity / pair_count : 1.0;
    concept->setTruthValue(SimpleTruthValue::createTV(concept_strength, 0.9));
    
    // Add metadata
    concept->setValue(
        atomspace_->add_node(PREDICATE_NODE, "instance_count"),
        createFloatValue(std::vector<double>{static_cast<double>(instances.size())}));
    
    concept->setValue(
        atomspace_->add_node(PREDICATE_NODE, "concept_type"),
        createStringValue({concept_type}));
    
    return concept;
}

std::vector<Handle> UnsupervisedLearner::extractCommonFeatures(const std::vector<Handle>& instances)
{
    std::vector<Handle> common_features;
    
    if (instances.empty()) {
        return common_features;
    }
    
    // Extract features from first instance
    std::vector<Handle> candidate_features = extractAtomFeatures(instances[0]);
    
    // Check which features are common to all instances
    for (const Handle& feature : candidate_features) {
        bool is_common = true;
        
        for (size_t i = 1; i < instances.size(); ++i) {
            std::vector<Handle> instance_features = extractAtomFeatures(instances[i]);
            if (std::find(instance_features.begin(), instance_features.end(), feature) == 
                instance_features.end()) {
                is_common = false;
                break;
            }
        }
        
        if (is_common) {
            common_features.push_back(feature);
        }
    }
    
    return common_features;
}

std::vector<Handle> UnsupervisedLearner::extractAtomFeatures(const Handle& atom)
{
    std::vector<Handle> features;
    
    // Type as feature
    Handle type_node = atomspace_->add_node(TYPE_NODE, 
        nameserver().getTypeName(atom->get_type()));
    features.push_back(type_node);
    
    // Arity for links
    if (atom->is_link()) {
        Handle arity_node = atomspace_->add_node(NUMBER_NODE, 
            std::to_string(atom->get_arity()));
        features.push_back(arity_node);
        
        // Outgoing types
        HandleSeq outgoing = atom->getOutgoingSet();
        for (const Handle& out : outgoing) {
            Handle out_type = atomspace_->add_node(TYPE_NODE,
                nameserver().getTypeName(out->get_type()));
            features.push_back(out_type);
        }
    }
    
    // Extract semantic features from names
    if (atom->is_node()) {
        std::string name = atom->get_name();
        
        // Simple feature extraction based on name patterns
        if (name.find("_") != std::string::npos) {
            std::istringstream iss(name);
            std::string token;
            while (std::getline(iss, token, '_')) {
                if (!token.empty() && token.length() > 2) {
                    Handle token_feature = atomspace_->add_node(CONCEPT_NODE, 
                        "feature_" + token);
                    features.push_back(token_feature);
                }
            }
        }
    }
    
    return features;
}

Handle UnsupervisedLearner::generalizePattern(const Pattern& pattern)
{
    if (pattern.pattern_handle == Handle::UNDEFINED) {
        return Handle::UNDEFINED;
    }
    
    // Create generalized version of pattern
    std::string general_name = "general_" + pattern.pattern_type + "_" + 
                              std::to_string(pattern.pattern_handle->get_hash());
    Handle general_concept = atomspace_->add_node(CONCEPT_NODE, general_name);
    
    // Extract pattern structure
    if (pattern.pattern_handle->is_link()) {
        // Create variable nodes for generalization
        HandleSeq variables;
        HandleSeq outgoing = pattern.pattern_handle->getOutgoingSet();
        
        for (size_t i = 0; i < outgoing.size(); ++i) {
            Handle var = atomspace_->add_node(VARIABLE_NODE, "$x" + std::to_string(i));
            variables.push_back(var);
        }
        
        // Create pattern link with variables
        Handle general_pattern = atomspace_->add_link(pattern.pattern_handle->get_type(), variables);
        
        // Connect to concept
        atomspace_->add_link(EVALUATION_LINK, HandleSeq{
            atomspace_->add_node(PREDICATE_NODE, "pattern_template"),
            general_concept,
            general_pattern
        });
    }
    
    // Add generalization metadata
    general_concept->setValue(
        atomspace_->add_node(PREDICATE_NODE, "abstraction_level"),
        createFloatValue(std::vector<double>{2.0})); // Level 2 abstraction
    
    general_concept->setTruthValue(SimpleTruthValue::createTV(
        0.7 * pattern.confidence, 0.8)); // Slightly lower confidence for generalization
    
    return general_concept;
}

void UnsupervisedLearner::buildConceptHierarchy(std::map<Handle, Handle>& hierarchy,
                                               const std::vector<Handle>& concepts)
{
    // Build multi-level hierarchy by finding relationships between concepts
    for (size_t i = 0; i < concepts.size(); ++i) {
        for (size_t j = i + 1; j < concepts.size(); ++j) {
            Handle concept1 = concepts[i];
            Handle concept2 = concepts[j];
            
            // Check if one concept subsumes the other
            ValuePtr level1 = concept1->getValue(
                atomspace_->add_node(PREDICATE_NODE, "abstraction_level"));
            ValuePtr level2 = concept2->getValue(
                atomspace_->add_node(PREDICATE_NODE, "abstraction_level"));
            
            double abs_level1 = 1.0, abs_level2 = 1.0;
            if (level1 && level1->get_type() == FLOAT_VALUE) {
                abs_level1 = FloatValueCast(level1)->value()[0];
            }
            if (level2 && level2->get_type() == FLOAT_VALUE) {
                abs_level2 = FloatValueCast(level2)->value()[0];
            }
            
            // Higher abstraction level subsumes lower
            if (abs_level1 > abs_level2) {
                // Check if concept2's instances are subset of concept1's domain
                IncomingSet concept2_instances = concept2->getIncomingSetByType(INHERITANCE_LINK);
                bool is_subset = true;
                
                for (const Handle& inst_link : concept2_instances) {
                    Handle instance = inst_link->getOutgoingAtom(0);
                    if (hierarchy.find(instance) == hierarchy.end() || 
                        hierarchy[instance] != concept1) {
                        is_subset = false;
                        break;
                    }
                }
                
                if (is_subset && concept2_instances.size() > 0) {
                    hierarchy[concept2] = concept1;
                    atomspace_->add_link(INHERITANCE_LINK, HandleSeq{concept2, concept1});
                }
            } else if (abs_level2 > abs_level1) {
                // Symmetric case
                IncomingSet concept1_instances = concept1->getIncomingSetByType(INHERITANCE_LINK);
                bool is_subset = true;
                
                for (const Handle& inst_link : concept1_instances) {
                    Handle instance = inst_link->getOutgoingAtom(0);
                    if (hierarchy.find(instance) == hierarchy.end() || 
                        hierarchy[instance] != concept2) {
                        is_subset = false;
                        break;
                    }
                }
                
                if (is_subset && concept1_instances.size() > 0) {
                    hierarchy[concept1] = concept2;
                    atomspace_->add_link(INHERITANCE_LINK, HandleSeq{concept1, concept2});
                }
            }
        }
    }
}

std::string UnsupervisedLearner::getAtomSignature(const Handle& atom)
{
    std::string signature = nameserver().getTypeName(atom->get_type());
    
    if (atom->is_link()) {
        signature += "_" + std::to_string(atom->get_arity());
        HandleSeq outgoing = atom->getOutgoingSet();
        for (const Handle& out : outgoing) {
            signature += "_" + nameserver().getTypeName(out->get_type());
        }
    }
    
    return signature;
}

bool UnsupervisedLearner::areAtomsCompatible(const Handle& atom1, const Handle& atom2)
{
    // Basic compatibility check
    if (atom1->get_type() != atom2->get_type()) {
        return false;
    }
    
    if (atom1->is_link() && atom2->is_link()) {
        return atom1->get_arity() == atom2->get_arity();
    }
    
    return true;
}

double UnsupervisedLearner::calculateInformationGain(const Handle& concept,
                                                    const std::vector<Handle>& instances)
{
    // Calculate information gain of using this concept to organize instances
    if (instances.empty()) {
        return 0.0;
    }
    
    // Calculate entropy before concept
    std::map<Type, int> type_counts;
    for (const Handle& inst : instances) {
        type_counts[inst->get_type()]++;
    }
    
    double total_entropy = 0.0;
    for (const auto& [type, count] : type_counts) {
        double p = static_cast<double>(count) / instances.size();
        if (p > 0) {
            total_entropy -= p * std::log2(p);
        }
    }
    
    // Information gain is reduction in entropy
    // For now, return a heuristic based on concept quality
    TruthValuePtr tv = concept->getTruthValue();
    double concept_quality = tv ? tv->get_mean() : 0.5;
    
    return total_entropy * concept_quality;
}

} // namespace main
} // namespace opencog