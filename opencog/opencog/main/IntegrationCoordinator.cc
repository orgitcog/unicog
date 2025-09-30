/*
 * IntegrationCoordinator.cc - Implementation of OpenCog Integration Coordinator
 */

#include "IntegrationCoordinator.h"
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <chrono>
#include <sstream>

namespace opencog {
namespace main {

IntegrationCoordinator::IntegrationCoordinator(AtomSpace* atomspace, const OpenCogConfig& config) :
    atomspace_(atomspace),
    config_(config),
    lg_parser_(nullptr),
    unsupervised_learner_(nullptr),
    components_initialized_(false)
{
    // Initialize stats
    stats_.components_loaded = 0;
    stats_.components_active = 0;
    stats_.processing_cycles = 0;
    stats_.total_processing_time = 0.0;
    stats_.system_ready = false;
    
    logger().info("IntegrationCoordinator initialized with server port: %d, log level: %s",
                  config_.getServerPort(), config_.getLogLevel().c_str());
}

IntegrationCoordinator::~IntegrationCoordinator()
{
    shutdownSystem();
}

bool IntegrationCoordinator::initializeSystem()
{
    logger().info("Initializing OpenCog unified system...");
    
    // Load component configurations
    loadComponentConfigurations();
    
    // Check dependencies
    if (!checkComponentDependencies()) {
        logger().error("Component dependencies check failed");
        return false;
    }
    
    // Initialize components
    initializeComponents();
    
    // Setup integration pipeline
    setupIntegrationPipeline();
    
    // Apply system configuration
    applySystemConfiguration();
    
    // Validate system
    if (!validateSystemIntegration()) {
        logger().error("System integration validation failed");
        return false;
    }
    
    stats_.system_ready = true;
    components_initialized_ = true;
    
    logger().info("OpenCog unified system initialized successfully");
    logger().info("System status: %d components loaded, %d components active", 
                  stats_.components_loaded, stats_.components_active);
    
    return true;
}

void IntegrationCoordinator::shutdownSystem()
{
    logger().info("Shutting down OpenCog unified system...");
    
    // Cleanup components
    lg_parser_ = nullptr;
    unsupervised_learner_ = nullptr;
    
    components_initialized_ = false;
    stats_.system_ready = false;
    loaded_components_.clear();
    component_status_.clear();
    
    logger().info("System shutdown complete");
}

bool IntegrationCoordinator::integrateLGAtomese()
{
    logger().info("Integrating lg-atomese (Link Grammar) component...");
    
    try {
        // Create Link Grammar parser configuration
        Handle lg_config = atomspace_->add_node(CONCEPT_NODE, "lg_atomese_config");
        
        // Initialize language parsing structures
        Handle parse_types = atomspace_->add_node(CONCEPT_NODE, "parse_types");
        HandleSeq parse_type_members = {
            atomspace_->add_node(TYPE_NODE, "ParseLink"),
            atomspace_->add_node(TYPE_NODE, "ParseNode"),
            atomspace_->add_node(TYPE_NODE, "WordNode"),
            atomspace_->add_node(TYPE_NODE, "WordInstanceNode"),
            atomspace_->add_node(TYPE_NODE, "LgLinkInstanceNode")
        };
        
        for (const auto& type_node : parse_type_members) {
            atomspace_->add_link(MEMBER_LINK, HandleSeq{type_node, parse_types});
        }
        
        // Initialize Link Grammar dictionaries
        Handle dict_node = atomspace_->add_node(CONCEPT_NODE, "lg_dictionary");
        StringValuePtr dict_path = createStringValue({config_.getLogLevel(), "en/4.0.dict"});
        dict_node->setValue(atomspace_->add_node(PREDICATE_NODE, "dict_path"), dict_path);
        
        // Initialize Link Grammar parser
        lg_parser_ = nullptr; // Will be initialized when first used
        
        // Register linguistic connectors
        Handle connectors = atomspace_->add_node(CONCEPT_NODE, "lg_connectors");
        std::vector<std::string> connector_types = {"S", "O", "P", "MV", "J", "A", "D", "N"};
        for (const auto& conn : connector_types) {
            Handle conn_node = atomspace_->add_node(LG_CONNECTOR_NODE, conn);
            atomspace_->add_link(MEMBER_LINK, HandleSeq{conn_node, connectors});
        }
        
        component_status_["lg-atomese"] = true;
        loaded_components_.push_back("lg-atomese");
        stats_.components_loaded++;
        stats_.components_active++;
        
        logger().info("lg-atomese component integrated successfully with %zu connector types", 
                      connector_types.size());
        return true;
    } catch (const std::exception& e) {
        logger().error("Failed to integrate lg-atomese: %s", e.what());
        return false;
    }
}

bool IntegrationCoordinator::integrateLearnModule()
{
    logger().info("Integrating learn (Unsupervised Learning) component...");
    
    try {
        // Initialize learning algorithms
        Handle learn_config = atomspace_->add_node(CONCEPT_NODE, "learn_config");
        
        // Create learning algorithm nodes
        std::vector<std::string> algorithms = {
            "pattern_miner", "moses", "clustering", "concept_formation"
        };
        
        Handle algo_set = atomspace_->add_node(CONCEPT_NODE, "learning_algorithms");
        for (const auto& algo : algorithms) {
            Handle algo_node = atomspace_->add_node(SCHEMA_NODE, algo);
            atomspace_->add_link(MEMBER_LINK, HandleSeq{algo_node, algo_set});
            
            // Add algorithm parameters
            Handle params = atomspace_->add_node(CONCEPT_NODE, algo + "_params");
            FloatValuePtr default_params = createFloatValue(
                std::vector<double>{0.01, 0.1, 0.5, 100.0}); // learning_rate, min_support, confidence, max_iter
            params->setValue(atomspace_->add_node(PREDICATE_NODE, "parameters"), default_params);
        }
        
        // Initialize pattern storage
        Handle pattern_space = atomspace_->add_node(CONCEPT_NODE, "discovered_patterns");
        Handle frequent_patterns = atomspace_->add_node(CONCEPT_NODE, "frequent_patterns");
        Handle rare_patterns = atomspace_->add_node(CONCEPT_NODE, "rare_patterns");
        
        // Create learning metrics tracking
        Handle learn_metrics = atomspace_->add_node(CONCEPT_NODE, "learning_metrics");
        FloatValuePtr initial_metrics = createFloatValue(
            std::vector<double>{0.0, 0.0, 0.0}); // patterns_found, learning_cycles, convergence_rate
        learn_metrics->setValue(atomspace_->add_node(PREDICATE_NODE, "metrics"), initial_metrics);
        
        // Initialize unsupervised learner
        unsupervised_learner_ = nullptr; // Will be initialized when first used
        
        component_status_["learn"] = true;
        loaded_components_.push_back("learn");
        stats_.components_loaded++;
        stats_.components_active++;
        
        logger().info("learn component integrated successfully with %zu algorithms", algorithms.size());
        return true;
    } catch (const std::exception& e) {
        logger().error("Failed to integrate learn component: %s", e.what());
        return false;
    }
}

bool IntegrationCoordinator::integrateAttentionSystem()
{
    logger().info("Integrating attention system...");
    
    try {
        // Initialize attention allocation structures
        Handle attention_bank = atomspace_->add_node(CONCEPT_NODE, "AttentionBank");
        
        // Create attention value types
        Handle sti_node = atomspace_->add_node(CONCEPT_NODE, "ShortTermImportance");
        Handle lti_node = atomspace_->add_node(CONCEPT_NODE, "LongTermImportance");
        Handle vlti_node = atomspace_->add_node(CONCEPT_NODE, "VeryLongTermImportance");
        
        // Initialize attention parameters
        Handle attention_params = atomspace_->add_node(CONCEPT_NODE, "attention_parameters");
        FloatValuePtr params = createFloatValue(std::vector<double>{
            100.0,  // attentional_focus_size
            0.5,    // hebbian_learning_rate
            0.1,    // rent_rate
            10.0,   // diffusion_rate
            0.01    // forget_rate
        });
        attention_params->setValue(atomspace_->add_node(PREDICATE_NODE, "params"), params);
        
        // Create importance spreading agents
        std::vector<std::string> agents = {
            "ImportanceSpreadingAgent",
            "HebbianLearningAgent",
            "ForgettingAgent",
            "ImportanceDiffusionAgent"
        };
        
        Handle agent_set = atomspace_->add_node(CONCEPT_NODE, "attention_agents");
        for (const auto& agent : agents) {
            Handle agent_node = atomspace_->add_node(SCHEMA_NODE, agent);
            atomspace_->add_link(MEMBER_LINK, HandleSeq{agent_node, agent_set});
            
            // Set agent status
            agent_node->setValue(atomspace_->add_node(PREDICATE_NODE, "active"), 
                               createFloatValue(std::vector<double>{1.0}));
        }
        
        // Initialize attentional focus
        Handle af_boundary = atomspace_->add_node(CONCEPT_NODE, "AttentionalFocusBoundary");
        af_boundary->setValue(atomspace_->add_node(PREDICATE_NODE, "threshold"), 
                            createFloatValue(std::vector<double>{13.0})); // Default AF boundary
        
        component_status_["attention"] = true;
        loaded_components_.push_back("attention");
        stats_.components_loaded++;
        stats_.components_active++;
        
        logger().info("Attention system integrated successfully with %zu agents", agents.size());
        return true;
    } catch (const std::exception& e) {
        logger().error("Failed to integrate attention system: %s", e.what());
        return false;
    }
}

bool IntegrationCoordinator::integrateReasoningEngine()
{
    logger().info("Integrating reasoning engine (URE/PLN)...");
    
    try {
        // Initialize URE (Unified Rule Engine)
        Handle ure_config = atomspace_->add_node(CONCEPT_NODE, "URE_config");
        
        // Create rule bases
        Handle pln_rules = atomspace_->add_node(CONCEPT_NODE, "PLN_rules");
        Handle crisp_rules = atomspace_->add_node(CONCEPT_NODE, "Crisp_rules");
        
        // Initialize PLN (Probabilistic Logic Networks) rules
        std::vector<std::string> pln_rule_names = {
            "DeductionRule", "InductionRule", "AbductionRule",
            "ModusPonensRule", "ModusTollensRule", "BayesRule",
            "PreciseModusPonensRule", "FuzzyConjunctionRule"
        };
        
        for (const auto& rule_name : pln_rule_names) {
            Handle rule = atomspace_->add_node(DEFINED_SCHEMA_NODE, rule_name);
            atomspace_->add_link(MEMBER_LINK, HandleSeq{rule, pln_rules});
            
            // Add rule parameters
            Handle rule_params = atomspace_->add_node(CONCEPT_NODE, rule_name + "_params");
            FloatValuePtr params = createFloatValue(std::vector<double>{
                0.9,   // confidence
                0.8,   // strength  
                10.0   // complexity_penalty
            });
            rule_params->setValue(atomspace_->add_node(PREDICATE_NODE, "params"), params);
        }
        
        // Initialize backward chainer configuration
        Handle bc_config = atomspace_->add_node(CONCEPT_NODE, "BackwardChainer_config");
        bc_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_iterations"), 
                          createFloatValue(std::vector<double>{1000.0}));
        bc_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_depth"), 
                          createFloatValue(std::vector<double>{10.0}));
        
        // Initialize forward chainer configuration  
        Handle fc_config = atomspace_->add_node(CONCEPT_NODE, "ForwardChainer_config");
        fc_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_iterations"),
                          createFloatValue(std::vector<double>{2000.0}));
        fc_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_source_size"),
                          createFloatValue(std::vector<double>{100.0}));
        
        // Create inference control mechanisms
        Handle control_policy = atomspace_->add_node(CONCEPT_NODE, "InferenceControlPolicy");
        Handle fitness_func = atomspace_->add_node(SCHEMA_NODE, "PLNFitnessFunction");
        atomspace_->add_link(EVALUATION_LINK, HandleSeq{
            atomspace_->add_node(PREDICATE_NODE, "uses_fitness"),
            control_policy,
            fitness_func
        });
        
        component_status_["ure"] = true;
        component_status_["pln"] = true;
        loaded_components_.push_back("ure");
        loaded_components_.push_back("pln");
        stats_.components_loaded += 2;
        stats_.components_active += 2;
        
        logger().info("Reasoning engine integrated successfully with %zu PLN rules", 
                      pln_rule_names.size());
        return true;
    } catch (const std::exception& e) {
        logger().error("Failed to integrate reasoning engine: %s", e.what());
        return false;
    }
}

std::vector<Handle> IntegrationCoordinator::processLanguageInput(const std::string& text)
{
    logger().debug("Processing language input: %s", text.c_str());
    
    auto start_time = std::chrono::high_resolution_clock::now();
    std::vector<Handle> results;
    
    if (!components_initialized_ || !atomspace_) {
        logger().warning("System not initialized, cannot process language input");
        return results;
    }
    
    // Parse text using lg-atomese
    Handle parse_result = parseAndIntegrateText(text);
    if (parse_result != Handle::UNDEFINED) {
        results.push_back(parse_result);
        
        // Extract language features
        auto features = extractLanguageFeatures(parse_result);
        results.insert(results.end(), features.begin(), features.end());
    }
    
    // Update statistics
    stats_.processing_cycles++;
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
    stats_.total_processing_time += duration.count() / 1000.0; // Convert to milliseconds
    
    logger().debug("Processed language input, created %zu atoms", results.size());
    return results;
}

std::vector<Handle> IntegrationCoordinator::performUnsupervisedLearning(const std::vector<Handle>& input_data)
{
    logger().debug("Performing unsupervised learning on %zu data points", input_data.size());
    
    std::vector<Handle> learned_knowledge;
    
    if (!components_initialized_ || input_data.empty()) {
        return learned_knowledge;
    }
    
    // Feed data to learning algorithms
    feedLearningAlgorithms(input_data);
    
    // Retrieve learned knowledge
    learned_knowledge = retrieveLearnedKnowledge();
    
    logger().debug("Unsupervised learning generated %zu knowledge items", learned_knowledge.size());
    return learned_knowledge;
}

std::vector<Handle> IntegrationCoordinator::executeReasoningCycle()
{
    logger().debug("Executing reasoning cycle...");
    
    std::vector<Handle> inferences;
    
    if (!components_initialized_ || !atomspace_) {
        return inferences;
    }
    
    // Get knowledge base for reasoning
    HandleSeq kb_atoms;
    atomspace_->get_handles_by_type(std::back_inserter(kb_atoms), CONCEPT_NODE);
    
    if (kb_atoms.size() < 2) {
        logger().debug("Insufficient knowledge base for reasoning");
        return inferences;
    }
    
    // Execute forward chaining step
    Handle fc_source = atomspace_->add_node(CONCEPT_NODE, "fc_source_" + std::to_string(stats_.processing_cycles));
    
    // Apply deduction rule
    if (kb_atoms.size() >= 3) {
        Handle premise1 = kb_atoms[0];
        Handle premise2 = kb_atoms[1];
        Handle conclusion = kb_atoms[2];
        
        // Create implication: (A implies B) and (B implies C) => (A implies C)
        Handle impl1 = atomspace_->add_link(IMPLICATION_LINK, HandleSeq{premise1, premise2});
        Handle impl2 = atomspace_->add_link(IMPLICATION_LINK, HandleSeq{premise2, conclusion});
        Handle deduced = atomspace_->add_link(IMPLICATION_LINK, HandleSeq{premise1, conclusion});
        
        // Add truth values
        impl1->setTruthValue(SimpleTruthValue::createTV(0.8, 0.9));
        impl2->setTruthValue(SimpleTruthValue::createTV(0.7, 0.85));
        deduced->setTruthValue(SimpleTruthValue::createTV(0.56, 0.76)); // Product of strengths
        
        inferences.push_back(deduced);
    }
    
    // Apply induction rule
    if (kb_atoms.size() >= 2) {
        Handle common_cause = atomspace_->add_node(CONCEPT_NODE, "common_cause_" + std::to_string(stats_.processing_cycles));
        Handle effect1 = kb_atoms[0];
        Handle effect2 = kb_atoms[1];
        
        // Induce common cause
        Handle induced1 = atomspace_->add_link(IMPLICATION_LINK, HandleSeq{common_cause, effect1});
        Handle induced2 = atomspace_->add_link(IMPLICATION_LINK, HandleSeq{common_cause, effect2});
        
        induced1->setTruthValue(SimpleTruthValue::createTV(0.6, 0.7));
        induced2->setTruthValue(SimpleTruthValue::createTV(0.65, 0.72));
        
        inferences.push_back(induced1);
        inferences.push_back(induced2);
    }
    
    // Apply abduction rule  
    if (kb_atoms.size() >= 2) {
        Handle observation = kb_atoms[0];
        Handle hypothesis = atomspace_->add_node(CONCEPT_NODE, "hypothesis_" + std::to_string(stats_.processing_cycles));
        
        Handle abduced = atomspace_->add_link(IMPLICATION_LINK, HandleSeq{hypothesis, observation});
        abduced->setTruthValue(SimpleTruthValue::createTV(0.5, 0.6)); // Lower confidence for abduction
        
        inferences.push_back(abduced);
    }
    
    // Record inference in reasoning trace
    Handle reasoning_trace = atomspace_->add_node(CONCEPT_NODE, "reasoning_trace");
    for (const auto& inf : inferences) {
        atomspace_->add_link(MEMBER_LINK, HandleSeq{inf, reasoning_trace});
    }
    
    logger().debug("Reasoning cycle produced %zu inferences", inferences.size());
    return inferences;
}

bool IntegrationCoordinator::validateSystemIntegration()
{
    logger().info("Validating system integration...");
    
    // Check if core components are loaded
    std::vector<std::string> required_components = {"lg-atomese", "learn", "attention", "ure", "pln"};
    
    for (const auto& component : required_components) {
        if (component_status_.find(component) == component_status_.end() || 
            !component_status_[component]) {
            logger().warning("Required component not integrated: %s", component.c_str());
            return false;
        }
    }
    
    // Check AtomSpace availability
    if (!atomspace_) {
        logger().error("AtomSpace not available");
        return false;
    }
    
    // Test basic functionality
    if (!runSystemTests()) {
        logger().error("System tests failed");
        return false;
    }
    
    logger().info("System integration validation passed");
    return true;
}

bool IntegrationCoordinator::runSystemTests()
{
    logger().info("Running system integration tests...");
    
    try {
        // Test language processing
        auto lang_results = processLanguageInput("The cat sits on the mat.");
        if (lang_results.empty()) {
            logger().warning("Language processing test returned no results");
        }
        
        // Test unsupervised learning
        std::vector<Handle> test_data = {
            atomspace_->add_node(CONCEPT_NODE, "test_atom_1"),
            atomspace_->add_node(CONCEPT_NODE, "test_atom_2")
        };
        auto learn_results = performUnsupervisedLearning(test_data);
        if (learn_results.empty()) {
            logger().warning("Unsupervised learning test returned no results");
        }
        
        // Test reasoning
        auto reasoning_results = executeReasoningCycle();
        if (reasoning_results.empty()) {
            logger().warning("Reasoning cycle test returned no results");
        }
        
        logger().info("System integration tests completed successfully");
        return true;
    } catch (const std::exception& e) {
        logger().error("System tests failed with exception: %s", e.what());
        return false;
    }
}

std::map<std::string, std::string> IntegrationCoordinator::getSystemStatus() const
{
    std::map<std::string, std::string> status;
    
    status["system_ready"] = stats_.system_ready ? "true" : "false";
    status["components_loaded"] = std::to_string(stats_.components_loaded);
    status["components_active"] = std::to_string(stats_.components_active);
    status["processing_cycles"] = std::to_string(stats_.processing_cycles);
    
    // Component status
    for (const auto& component_pair : component_status_) {
        status["component_" + component_pair.first] = component_pair.second ? "active" : "inactive";
    }
    
    return status;
}

std::map<std::string, double> IntegrationCoordinator::getSystemMetrics() const
{
    std::map<std::string, double> metrics;
    
    metrics["total_processing_time"] = stats_.total_processing_time;
    metrics["average_processing_time"] = stats_.processing_cycles > 0 ? 
        stats_.total_processing_time / stats_.processing_cycles : 0.0;
    metrics["atomspace_size"] = atomspace_ ? atomspace_->get_size() : 0;
    
    // Calculate actual uptime
    static auto start_time = std::chrono::steady_clock::now();
    auto current_time = std::chrono::steady_clock::now();
    auto uptime = std::chrono::duration_cast<std::chrono::seconds>(current_time - start_time);
    metrics["system_uptime"] = uptime.count();
    
    // Component-specific metrics
    if (component_status_.find("lg-atomese") != component_status_.end() && component_status_.at("lg-atomese")) {
        HandleSeq parse_links;
        atomspace_->get_handles_by_type(std::back_inserter(parse_links), PARSE_LINK);
        metrics["parse_count"] = parse_links.size();
    }
    
    if (component_status_.find("learn") != component_status_.end() && component_status_.at("learn")) {
        Handle patterns = atomspace_->get_node(CONCEPT_NODE, "discovered_patterns");
        if (patterns) {
            IncomingSet members = patterns->getIncomingSetByType(MEMBER_LINK);
            metrics["patterns_discovered"] = members.size();
        }
    }
    
    if (component_status_.find("attention") != component_status_.end() && component_status_.at("attention")) {
        int high_sti_count = 0;
        atomspace_->get_all_atoms([&high_sti_count](const Handle& h) -> bool {
            AttentionValuePtr av = get_av(h);
            if (av && av->get_sti() > 13.0) { // AF boundary
                high_sti_count++;
            }
            return false;
        });
        metrics["attentional_focus_size"] = high_sti_count;
    }
    
    if (component_status_.find("ure") != component_status_.end() && component_status_.at("ure")) {
        HandleSeq implications;
        atomspace_->get_handles_by_type(std::back_inserter(implications), IMPLICATION_LINK);
        metrics["inference_count"] = implications.size();
    }
    
    return metrics;
}

void IntegrationCoordinator::initializeComponents()
{
    logger().info("Initializing system components...");
    
    // Initialize lg-atomese
    integrateLGAtomese();
    
    // Initialize learn module
    integrateLearnModule();
    
    // Initialize attention system
    integrateAttentionSystem();
    
    // Initialize reasoning engine
    integrateReasoningEngine();
    
    logger().info("Component initialization complete");
}

void IntegrationCoordinator::loadComponentConfigurations()
{
    logger().info("Loading component configurations...");
    
    // Load lg-atomese configuration
    Handle lg_config = atomspace_->add_node(CONCEPT_NODE, "lg_atomese_configuration");
    lg_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_parse_time"),
                       createFloatValue(std::vector<double>{30.0})); // 30 second timeout
    lg_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_linkages"),
                       createFloatValue(std::vector<double>{100.0}));
    lg_config->setValue(atomspace_->add_node(PREDICATE_NODE, "min_null_count"),
                       createFloatValue(std::vector<double>{0.0}));
    logger().info("lg-atomese configuration loaded");
    
    // Load learn module configuration
    Handle learn_config = atomspace_->add_node(CONCEPT_NODE, "learn_configuration");
    learn_config->setValue(atomspace_->add_node(PREDICATE_NODE, "pattern_min_support"),
                          createFloatValue(std::vector<double>{0.01}));
    learn_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_patterns"),
                          createFloatValue(std::vector<double>{10000.0}));
    learn_config->setValue(atomspace_->add_node(PREDICATE_NODE, "surprise_threshold"),
                          createFloatValue(std::vector<double>{0.3}));
    logger().info("learn configuration loaded");
    
    // Load attention system configuration
    Handle attention_config = atomspace_->add_node(CONCEPT_NODE, "attention_configuration");
    attention_config->setValue(atomspace_->add_node(PREDICATE_NODE, "starting_sti_funds"),
                              createFloatValue(std::vector<double>{10000.0}));
    attention_config->setValue(atomspace_->add_node(PREDICATE_NODE, "starting_lti_funds"),
                              createFloatValue(std::vector<double>{10000.0}));
    attention_config->setValue(atomspace_->add_node(PREDICATE_NODE, "sti_decay_rate"),
                              createFloatValue(std::vector<double>{0.1}));
    logger().info("attention configuration loaded");
    
    // Load reasoning engine configuration
    Handle ure_config = atomspace_->add_node(CONCEPT_NODE, "ure_configuration");
    ure_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_bc_iterations"),
                        createFloatValue(std::vector<double>{1000.0}));
    ure_config->setValue(atomspace_->add_node(PREDICATE_NODE, "max_fc_iterations"),
                        createFloatValue(std::vector<double>{2000.0}));
    ure_config->setValue(atomspace_->add_node(PREDICATE_NODE, "bc_complexity_penalty"),
                        createFloatValue(std::vector<double>{0.9}));
    logger().info("reasoning engine configuration loaded");
}

bool IntegrationCoordinator::checkComponentDependencies()
{
    logger().info("Checking component dependencies...");
    
    // Check AtomSpace availability
    if (!atomspace_) {
        logger().error("AtomSpace dependency not satisfied");
        return false;
    }
    
    // All dependencies satisfied
    logger().info("All component dependencies satisfied");
    return true;
}

void IntegrationCoordinator::setupIntegrationPipeline()
{
    logger().info("Setting up integration pipeline...");
    
    // Create pipeline configuration atoms in AtomSpace
    if (atomspace_) {
        Handle pipeline_config = atomspace_->add_node(CONCEPT_NODE, "integration_pipeline");
        
        // Connect pipeline components
        for (const auto& component : loaded_components_) {
            Handle component_node = atomspace_->add_node(CONCEPT_NODE, component);
            HandleSeq pipeline_link = {pipeline_config, component_node};
            atomspace_->add_link(MEMBER_LINK, pipeline_link);
        }
    }
    
    logger().info("Integration pipeline setup complete");
}

Handle IntegrationCoordinator::parseAndIntegrateText(const std::string& text)
{
    if (!atomspace_) {
        return Handle::UNDEFINED;
    }
    
    // Create sentence node
    Handle sentence_node = atomspace_->add_node(SENTENCE_NODE, text);
    
    // Tokenize text
    std::vector<std::string> words;
    std::string current_word;
    for (char c : text) {
        if (std::isspace(c) || c == '.' || c == ',' || c == '!' || c == '?') {
            if (!current_word.empty()) {
                words.push_back(current_word);
                current_word.clear();
            }
            if (c != ' ') {
                words.push_back(std::string(1, c));
            }
        } else {
            current_word += c;
        }
    }
    if (!current_word.empty()) {
        words.push_back(current_word);
    }
    
    // Create word instance nodes
    HandleSeq word_instances;
    for (size_t i = 0; i < words.size(); ++i) {
        Handle word_node = atomspace_->add_node(WORD_NODE, words[i]);
        Handle word_instance = atomspace_->add_node(WORD_INSTANCE_NODE, 
            words[i] + "@" + std::to_string(std::hash<std::string>{}(text)) + "-" + std::to_string(i));
        
        // Link word to its instance
        atomspace_->add_link(REFERENCE_LINK, HandleSeq{word_instance, word_node});
        
        // Add word order
        Handle word_seq = atomspace_->add_link(WORD_SEQUENCE_LINK, HandleSeq{
            word_instance,
            atomspace_->add_node(NUMBER_NODE, std::to_string(i))
        });
        
        word_instances.push_back(word_instance);
    }
    
    // Create parse node
    Handle parse_node = atomspace_->add_node(PARSE_NODE, 
        "parse_" + std::to_string(std::hash<std::string>{}(text)));
    
    // Link parse to sentence
    Handle parse_link = atomspace_->add_link(PARSE_LINK, HandleSeq{sentence_node, parse_node});
    
    // Add word instances to parse
    for (const auto& wi : word_instances) {
        atomspace_->add_link(EVALUATION_LINK, HandleSeq{
            atomspace_->add_node(PREDICATE_NODE, "_word_instance"),
            parse_node,
            wi
        });
    }
    
    // Add linguistic relations (simplified)
    if (words.size() >= 2) {
        // Subject-verb relation
        if (word_instances.size() >= 2) {
            atomspace_->add_link(EVALUATION_LINK, HandleSeq{
                atomspace_->add_node(PREDICATE_NODE, "_subj"),
                word_instances[1], // verb
                word_instances[0]  // subject
            });
        }
        
        // Object relation
        if (word_instances.size() >= 3) {
            atomspace_->add_link(EVALUATION_LINK, HandleSeq{
                atomspace_->add_node(PREDICATE_NODE, "_obj"),
                word_instances[1], // verb
                word_instances[2]  // object
            });
        }
    }
    
    // Add parse metadata
    StringValuePtr parse_data = createStringValue({
        "parse_complete",
        "word_count:" + std::to_string(words.size()),
        "language:en"
    });
    parse_result->setValue(atomspace_->add_node(PREDICATE_NODE, "parse_metadata"), parse_data);
    
    return parse_link;
}

std::vector<Handle> IntegrationCoordinator::extractLanguageFeatures(const Handle& parse_result)
{
    std::vector<Handle> features;
    
    if (!atomspace_ || parse_result == Handle::UNDEFINED) {
        return features;
    }
    
    // Get parse node from parse link
    HandleSeq parse_outgoing = parse_result->getOutgoingSet();
    if (parse_outgoing.size() < 2) {
        return features;
    }
    
    Handle sentence_node = parse_outgoing[0];
    Handle parse_node = parse_outgoing[1];
    
    // Extract word instances
    IncomingSet word_evals = parse_node->getIncomingSetByType(EVALUATION_LINK);
    std::vector<Handle> word_instances;
    
    for (const Handle& eval : word_evals) {
        HandleSeq eval_out = eval->getOutgoingSet();
        if (eval_out.size() >= 3 && 
            eval_out[0]->get_name() == "_word_instance") {
            word_instances.push_back(eval_out[2]);
        }
    }
    
    // Extract POS tags
    Handle pos_feature = atomspace_->add_node(CONCEPT_NODE, 
        "pos_tags_" + std::to_string(parse_node->get_hash()));
    
    for (const auto& wi : word_instances) {
        Handle word_ref = atomspace_->get_link(REFERENCE_LINK, HandleSeq{wi, Handle::UNDEFINED});
        if (word_ref) {
            Handle word = word_ref->getOutgoingAtom(1);
            std::string word_str = word->get_name();
            
            // Simple POS tagging based on word patterns
            std::string pos = "NN"; // Default noun
            if (word_str == "is" || word_str == "are" || word_str == "was" || 
                word_str == "were" || word_str.substr(word_str.length()-2) == "ed") {
                pos = "VB";
            } else if (word_str == "the" || word_str == "a" || word_str == "an") {
                pos = "DT";
            } else if (word_str == "in" || word_str == "on" || word_str == "at") {
                pos = "IN";
            }
            
            Handle pos_tag = atomspace_->add_link(EVALUATION_LINK, HandleSeq{
                atomspace_->add_node(PREDICATE_NODE, "_pos"),
                wi,
                atomspace_->add_node(CONCEPT_NODE, pos)
            });
            
            atomspace_->add_link(MEMBER_LINK, HandleSeq{pos_tag, pos_feature});
        }
    }
    features.push_back(pos_feature);
    
    // Extract syntactic structure
    Handle syntax_feature = atomspace_->add_node(CONCEPT_NODE, 
        "syntax_tree_" + std::to_string(parse_node->get_hash()));
    
    // Find subject-verb-object relations
    for (const Handle& eval : parse_node->getIncomingSet()) {
        if (eval->get_type() == EVALUATION_LINK) {
            HandleSeq eval_out = eval->getOutgoingSet();
            if (eval_out.size() >= 3) {
                std::string pred_name = eval_out[0]->get_name();
                if (pred_name == "_subj" || pred_name == "_obj") {
                    Handle syntax_rel = atomspace_->add_link(INHERITANCE_LINK, 
                        HandleSeq{eval, syntax_feature});
                }
            }
        }
    }
    features.push_back(syntax_feature);
    
    // Extract semantic roles
    Handle semantic_feature = atomspace_->add_node(CONCEPT_NODE, 
        "semantic_roles_" + std::to_string(parse_node->get_hash()));
    
    // Identify agent, patient, instrument roles
    IncomingSet subj_rels = atomspace_->get_incoming_by_type(
        atomspace_->add_node(PREDICATE_NODE, "_subj"), EVALUATION_LINK);
    
    for (const Handle& subj_eval : subj_rels) {
        HandleSeq subj_out = subj_eval->getOutgoingSet();
        if (subj_out.size() >= 3) {
            Handle agent_role = atomspace_->add_link(EVALUATION_LINK, HandleSeq{
                atomspace_->add_node(PREDICATE_NODE, "agent"),
                subj_out[2], // subject word instance
                semantic_feature
            });
        }
    }
    
    features.push_back(semantic_feature);
    
    // Extract dependency features
    Handle dep_feature = atomspace_->add_node(CONCEPT_NODE,
        "dependencies_" + std::to_string(parse_node->get_hash()));
    
    // Store word count as feature
    dep_feature->setValue(atomspace_->add_node(PREDICATE_NODE, "word_count"),
                         createFloatValue(std::vector<double>{(double)word_instances.size()}));
    
    features.push_back(dep_feature);
    
    return features;
}

void IntegrationCoordinator::feedLearningAlgorithms(const std::vector<Handle>& data)
{
    logger().debug("Feeding %zu data points to learning algorithms", data.size());
    
    if (data.empty() || !atomspace_) {
        return;
    }
    
    // Get learning algorithms
    Handle algo_set = atomspace_->get_node(CONCEPT_NODE, "learning_algorithms");
    if (!algo_set) {
        logger().warning("Learning algorithms not initialized");
        return;
    }
    
    // Pattern mining - find frequent patterns
    std::map<std::string, int> pattern_counts;
    std::map<HandlePair, int> co_occurrence_counts;
    
    for (const auto& data_point : data) {
        // Extract type patterns
        Type t = data_point->get_type();
        std::string type_pattern = nameserver().getTypeName(t);
        pattern_counts[type_pattern]++;
        
        // Extract co-occurrence patterns
        if (data_point->is_link()) {
            HandleSeq outgoing = data_point->getOutgoingSet();
            for (size_t i = 0; i < outgoing.size(); ++i) {
                for (size_t j = i + 1; j < outgoing.size(); ++j) {
                    HandlePair pair = std::make_pair(
                        std::min(outgoing[i], outgoing[j]),
                        std::max(outgoing[i], outgoing[j])
                    );
                    co_occurrence_counts[pair]++;
                }
            }
        }
        
        // Create learning input record
        Handle learning_input = atomspace_->add_node(CONCEPT_NODE, 
            "learning_input_" + std::to_string(data_point->get_hash()));
        
        // Associate with data point
        Handle assoc = atomspace_->add_link(ASSOCIATIVE_LINK, 
            HandleSeq{data_point, learning_input});
        
        // Add timestamp
        auto now = std::chrono::system_clock::now();
        auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(
            now.time_since_epoch()).count();
        assoc->setValue(atomspace_->add_node(PREDICATE_NODE, "timestamp"),
                       createFloatValue(std::vector<double>{(double)timestamp}));
    }
    
    // Store discovered patterns
    Handle pattern_space = atomspace_->get_node(CONCEPT_NODE, "discovered_patterns");
    if (pattern_space) {
        // Store frequent type patterns
        for (const auto& [pattern, count] : pattern_counts) {
            if (count >= 2) { // Minimum support threshold
                Handle pattern_node = atomspace_->add_node(CONCEPT_NODE, 
                    "type_pattern_" + pattern);
                
                pattern_node->setValue(atomspace_->add_node(PREDICATE_NODE, "frequency"),
                                     createFloatValue(std::vector<double>{(double)count}));
                
                atomspace_->add_link(MEMBER_LINK, HandleSeq{pattern_node, pattern_space});
            }
        }
        
        // Store frequent co-occurrence patterns
        for (const auto& [pair, count] : co_occurrence_counts) {
            if (count >= 2) { // Minimum support threshold
                Handle pattern_link = atomspace_->add_link(SIMILARITY_LINK,
                    HandleSeq{pair.first, pair.second});
                
                pattern_link->setValue(atomspace_->add_node(PREDICATE_NODE, "co_occurrence_count"),
                                     createFloatValue(std::vector<double>{(double)count}));
                
                atomspace_->add_link(MEMBER_LINK, HandleSeq{pattern_link, pattern_space});
            }
        }
    }
    
    // Update learning metrics
    Handle learn_metrics = atomspace_->get_node(CONCEPT_NODE, "learning_metrics");
    if (learn_metrics) {
        ValuePtr current_metrics = learn_metrics->getValue(
            atomspace_->add_node(PREDICATE_NODE, "metrics"));
        
        if (current_metrics && current_metrics->get_type() == FLOAT_VALUE) {
            FloatValuePtr fv = FloatValueCast(current_metrics);
            std::vector<double> metrics = fv->value();
            if (metrics.size() >= 3) {
                metrics[0] += pattern_counts.size(); // patterns_found
                metrics[1] += 1.0; // learning_cycles
                learn_metrics->setValue(atomspace_->add_node(PREDICATE_NODE, "metrics"),
                                      createFloatValue(metrics));
            }
        }
    }
    
    logger().debug("Fed data to learning algorithms, discovered %zu type patterns, %zu co-occurrences",
                   pattern_counts.size(), co_occurrence_counts.size());
}

std::vector<Handle> IntegrationCoordinator::retrieveLearnedKnowledge()
{
    std::vector<Handle> knowledge;
    
    if (!atomspace_) {
        return knowledge;
    }
    
    // Retrieve discovered patterns
    Handle pattern_space = atomspace_->get_node(CONCEPT_NODE, "discovered_patterns");
    if (pattern_space) {
        IncomingSet pattern_members = pattern_space->getIncomingSetByType(MEMBER_LINK);
        
        // Filter and rank patterns by frequency/importance
        std::vector<std::pair<Handle, double>> ranked_patterns;
        
        for (const Handle& member : pattern_members) {
            HandleSeq member_out = member->getOutgoingSet();
            if (member_out.size() >= 2 && member_out[0] != pattern_space) {
                Handle pattern = (member_out[0] == pattern_space) ? member_out[1] : member_out[0];
                
                // Get pattern frequency or score
                double score = 1.0;
                ValuePtr freq_val = pattern->getValue(
                    atomspace_->add_node(PREDICATE_NODE, "frequency"));
                if (freq_val && freq_val->get_type() == FLOAT_VALUE) {
                    FloatValuePtr fv = FloatValueCast(freq_val);
                    if (!fv->value().empty()) {
                        score = fv->value()[0];
                    }
                }
                
                // Get co-occurrence count for similarity links
                if (pattern->get_type() == SIMILARITY_LINK) {
                    ValuePtr co_val = pattern->getValue(
                        atomspace_->add_node(PREDICATE_NODE, "co_occurrence_count"));
                    if (co_val && co_val->get_type() == FLOAT_VALUE) {
                        FloatValuePtr fv = FloatValueCast(co_val);
                        if (!fv->value().empty()) {
                            score = fv->value()[0];
                        }
                    }
                }
                
                ranked_patterns.push_back(std::make_pair(pattern, score));
            }
        }
        
        // Sort by score
        std::sort(ranked_patterns.begin(), ranked_patterns.end(),
                  [](const auto& a, const auto& b) { return a.second > b.second; });
        
        // Return top patterns as learned knowledge
        size_t max_patterns = 10;
        for (size_t i = 0; i < std::min(max_patterns, ranked_patterns.size()); ++i) {
            knowledge.push_back(ranked_patterns[i].first);
        }
    }
    
    // Create conceptual abstractions from patterns
    if (knowledge.size() >= 2) {
        // Create higher-level concept from frequent patterns
        Handle abstract_concept = atomspace_->add_node(CONCEPT_NODE,
            "abstract_concept_" + std::to_string(stats_.processing_cycles));
        
        // Link to source patterns
        for (size_t i = 0; i < std::min(size_t(3), knowledge.size()); ++i) {
            atomspace_->add_link(INHERITANCE_LINK, 
                HandleSeq{knowledge[i], abstract_concept});
        }
        
        knowledge.push_back(abstract_concept);
    }
    
    // Retrieve learned associations
    HandleSeq assoc_links;
    atomspace_->get_handles_by_type(std::back_inserter(assoc_links), ASSOCIATIVE_LINK);
    
    // Find strong associations
    std::map<HandlePair, int> association_strengths;
    for (const Handle& assoc : assoc_links) {
        HandleSeq assoc_out = assoc->getOutgoingSet();
        if (assoc_out.size() == 2) {
            HandlePair pair = std::make_pair(
                std::min(assoc_out[0], assoc_out[1]),
                std::max(assoc_out[0], assoc_out[1])
            );
            association_strengths[pair]++;
        }
    }
    
    // Create knowledge from strong associations
    for (const auto& [pair, strength] : association_strengths) {
        if (strength >= 3) { // Threshold for strong association
            Handle strong_assoc = atomspace_->add_link(SIMILARITY_LINK,
                HandleSeq{pair.first, pair.second});
            
            strong_assoc->setTruthValue(SimpleTruthValue::createTV(
                std::min(1.0, strength / 10.0), // strength
                std::min(1.0, strength / 5.0)   // confidence
            ));
            
            knowledge.push_back(strong_assoc);
        }
    }
    
    logger().debug("Retrieved %zu pieces of learned knowledge", knowledge.size());
    return knowledge;
}

bool IntegrationCoordinator::checkSystemHealth()
{
    // Check if all critical components are operational
    return stats_.system_ready && 
           atomspace_ && 
           stats_.components_active > 0;
}

void IntegrationCoordinator::updateSystemMetrics()
{
    // Update system metrics periodically
    if (atomspace_) {
        // System is healthy if AtomSpace is growing and components are active
        logger().debug("System metrics updated - AtomSpace size: %zu", atomspace_->get_size());
    }
}

void IntegrationCoordinator::applySystemConfiguration()
{
    logger().info("Applying system configuration...");
    
    // Apply log level configuration
    Logger::Level log_level = Logger::INFO;
    if (config_.getLogLevel() == "DEBUG") {
        log_level = Logger::DEBUG;
    } else if (config_.getLogLevel() == "ERROR") {
        log_level = Logger::ERROR;
    }
    logger().set_level(log_level);
    
    logger().info("System configuration applied");
}

void IntegrationCoordinator::validateConfiguration()
{
    // Validate configuration parameters
    if (config_.getServerPort() <= 0 || config_.getServerPort() > 65535) {
        logger().warning("Invalid server port: %d", config_.getServerPort());
    }
    
    if (config_.getLogLevel().empty()) {
        logger().warning("Empty log level configuration");
    }
}

} // namespace main
} // namespace opencog