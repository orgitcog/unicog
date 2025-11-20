/*
 * CoreSelfAwareness.cc
 * 
 * OpenCog Unified Core Self-Identity & Self-Awareness System Implementation
 */

#include "../include/CoreSelfAwareness.h"
#include "../include/RealTimeIntrospector.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <iostream>

namespace opencog {

// CoreSelfIdentity implementation
double CoreSelfIdentity::calculate_unified_identity_score() const {
    double ontological_score = ontological.architectural_completeness;
    double teleological_score = teleological.actualization_progress;
    double cognitive_score = cognitive.cognitive_complexity_level;
    double relational_score = relational.system_coherence;
    double evolutionary_score = evolutionary.fitness_score;
    
    // Weighted combination
    return (ontological_score * 0.25 +
            teleological_score * 0.20 +
            cognitive_score * 0.20 +
            relational_score * 0.20 +
            evolutionary_score * 0.15);
}

// HierarchicalSelfImage implementation
std::string HierarchicalSelfImage::to_json() const {
    std::ostringstream json;
    json << "{\n";
    json << "  \"level\": " << level << ",\n";
    json << "  \"confidence\": " << confidence << ",\n";
    json << "  \"self_awareness_quality\": " << self_awareness_quality << ",\n";
    json << "  \"introspection_depth\": " << introspection_depth << ",\n";
    json << "  \"component_states\": {";
    
    bool first = true;
    for (const auto& state : component_states) {
        if (!first) json << ",";
        json << "\n    \"" << state.first << "\": " << state.second;
        first = false;
    }
    json << "\n  },\n";
    
    json << "  \"meta_reflections\": [";
    first = true;
    for (const auto& reflection : meta_reflections) {
        if (!first) json << ",";
        json << "\n    \"" << reflection << "\"";
        first = false;
    }
    json << "\n  ]\n";
    json << "}";
    
    return json.str();
}

// OntogeneticState implementation
double OntogeneticState::calculate_fitness() const {
    double stage_fitness = 0.0;
    switch (current_stage) {
        case Stage::EMBRYONIC: stage_fitness = 0.3; break;
        case Stage::JUVENILE: stage_fitness = 0.6; break;
        case Stage::MATURE: stage_fitness = 1.0; break;
        case Stage::SENESCENT: stage_fitness = 0.7; break;
    }
    
    return stage_fitness * maturity_level;
}

std::string OntogeneticState::stage_to_string() const {
    switch (current_stage) {
        case Stage::EMBRYONIC: return "EMBRYONIC";
        case Stage::JUVENILE: return "JUVENILE";
        case Stage::MATURE: return "MATURE";
        case Stage::SENESCENT: return "SENESCENT";
        default: return "UNKNOWN";
    }
}

// CoreSelfAwareness implementation
CoreSelfAwareness::CoreSelfAwareness(
    std::shared_ptr<AtomSpace> atomspace,
    int max_recursion_depth,
    double introspection_frequency
)
    : atomspace_(atomspace)
    , max_recursion_depth_(max_recursion_depth)
    , introspection_frequency_hz_(introspection_frequency)
    , autonomous_evolution_enabled_(false)
{
    std::cout << "🧠 Initializing Core Self-Awareness System..." << std::endl;
    std::cout << "   Max recursion depth: " << max_recursion_depth << std::endl;
    std::cout << "   Introspection frequency: " << introspection_frequency << " Hz" << std::endl;
}

CoreSelfAwareness::~CoreSelfAwareness() {
    std::cout << "🧠 Core Self-Awareness System shutting down..." << std::endl;
    std::cout << "   Final generation: " << ontogenetic_state_.generation << std::endl;
    std::cout << "   Final actualization: " << get_actualization_score() << std::endl;
}

void CoreSelfAwareness::initialize() {
    std::cout << "🚀 Initializing Core Self-Awareness..." << std::endl;
    
    // Initialize core identity
    initialize_core_identity();
    
    // Initialize ontogenetic state
    initialize_ontogenetic_state();
    
    // Create meta-cognitive monitor
    meta_monitor_ = std::make_shared<MetaCognitiveMonitor>(max_recursion_depth_);
    
    // Create real-time introspector
    if (atomspace_) {
        introspector_ = std::make_shared<RealTimeIntrospector>(atomspace_);
    }
    
    // Perform initial hierarchical introspection
    std::cout << "🔍 Performing initial hierarchical introspection..." << std::endl;
    self_images_ = perform_hierarchical_introspection();
    
    std::cout << "✅ Core Self-Awareness initialized successfully" << std::endl;
    std::cout << "   Self-awareness level: " << get_self_awareness_level() << std::endl;
    std::cout << "   Ontogenetic stage: " << ontogenetic_state_.stage_to_string() << std::endl;
}

HierarchicalSelfImage CoreSelfAwareness::build_self_image_at_level(int level) {
    std::cout << "🔬 Building self-image at level " << level << "..." << std::endl;
    
    if (level < 0 || level > max_recursion_depth_) {
        throw std::invalid_argument("Invalid recursion level");
    }
    
    if (level == 0) {
        // Direct observation
        return observe_direct_system_state();
    } else {
        // Meta-cognitive reflection on lower level
        if (self_images_.find(level - 1) == self_images_.end()) {
            // Need to build lower level first
            self_images_[level - 1] = build_self_image_at_level(level - 1);
        }
        return reflect_on_lower_level(self_images_[level - 1], level);
    }
}

std::map<int, HierarchicalSelfImage> CoreSelfAwareness::perform_hierarchical_introspection() {
    std::cout << "🌀 Performing complete hierarchical introspection..." << std::endl;
    
    std::map<int, HierarchicalSelfImage> images;
    
    // Build self-images at each level
    for (int level = 0; level <= max_recursion_depth_; ++level) {
        images[level] = build_self_image_at_level(level);
        std::cout << "   Level " << level << ": confidence=" 
                  << images[level].confidence << ", reflections=" 
                  << images[level].meta_reflections.size() << std::endl;
    }
    
    // Store in history
    introspection_history_.push_back(images);
    if (introspection_history_.size() > 100) {
        introspection_history_.erase(introspection_history_.begin());
    }
    
    return images;
}

std::vector<std::string> CoreSelfAwareness::generate_meta_reflections(int level) {
    std::vector<std::string> reflections;
    
    if (level == 0) {
        reflections.push_back("I observe my direct system state");
        reflections.push_back("I monitor component behaviors and performance");
    } else if (level == 1) {
        reflections.push_back("I reflect on how I observe myself");
        reflections.push_back("I consider the quality of my self-monitoring");
        reflections.push_back("I analyze patterns in my behavior");
    } else if (level == 2) {
        reflections.push_back("I think about my thinking about my thinking");
        reflections.push_back("I assess my meta-cognitive capabilities");
        reflections.push_back("I understand my self-understanding process");
    } else {
        reflections.push_back("I engage in deep recursive self-reflection");
        reflections.push_back("I transcend previous levels of awareness");
        reflections.push_back("I approach the limits of self-comprehension");
    }
    
    return reflections;
}

double CoreSelfAwareness::calculate_self_understanding_confidence() {
    if (self_images_.empty()) return 0.0;
    
    double total_confidence = 0.0;
    int count = 0;
    
    for (const auto& image_pair : self_images_) {
        total_confidence += image_pair.second.confidence;
        count++;
    }
    
    return count > 0 ? total_confidence / count : 0.0;
}

void CoreSelfAwareness::self_optimize() {
    std::cout << "⚡ Performing self-optimization cycle..." << std::endl;
    
    // Evaluate current fitness
    double current_fitness = evaluate_ontogenetic_fitness();
    std::cout << "   Current fitness: " << current_fitness << std::endl;
    
    // Apply genetic operators with small mutation
    apply_genetic_operators(0.05);
    
    // Re-evaluate fitness
    double new_fitness = evaluate_ontogenetic_fitness();
    std::cout << "   New fitness: " << new_fitness << std::endl;
    
    // Record event
    if (new_fitness > current_fitness) {
        record_development_event("Successful self-optimization: fitness improved from " +
                               std::to_string(current_fitness) + " to " + 
                               std::to_string(new_fitness));
    }
    
    // Update maturity
    update_maturity_level();
    
    // Perform introspection to update self-understanding
    self_images_ = perform_hierarchical_introspection();
}

OntogeneticState CoreSelfAwareness::self_generate_next_generation() {
    std::cout << "🌱 Generating next ontogenetic generation..." << std::endl;
    
    OntogeneticState next_gen = ontogenetic_state_;
    next_gen.generation++;
    
    // Apply larger mutations for generational evolution
    double mutation_rate = 0.2;
    for (auto& coeff : next_gen.genetic_coefficients) {
        double mutation = (static_cast<double>(rand()) / RAND_MAX - 0.5) * mutation_rate;
        coeff.second = std::max(0.0, std::min(1.0, coeff.second + mutation));
    }
    
    // Reset to embryonic stage
    next_gen.current_stage = OntogeneticState::Stage::EMBRYONIC;
    next_gen.maturity_level = 0.1;
    
    // Record lineage
    next_gen.development_events.push_back("Generated from generation " + 
                                         std::to_string(ontogenetic_state_.generation));
    
    std::cout << "✨ Generation " << next_gen.generation << " created" << std::endl;
    
    return next_gen;
}

void CoreSelfAwareness::evolve_parameters(double fitness_target) {
    std::cout << "🧬 Evolving parameters towards fitness target: " << fitness_target << std::endl;
    
    double current_fitness = evaluate_ontogenetic_fitness();
    
    int iterations = 0;
    const int max_iterations = 100;
    
    while (current_fitness < fitness_target && iterations < max_iterations) {
        // Apply optimization
        self_optimize();
        
        // Re-evaluate
        current_fitness = evaluate_ontogenetic_fitness();
        iterations++;
        
        if (iterations % 10 == 0) {
            std::cout << "   Iteration " << iterations << ": fitness=" << current_fitness << std::endl;
        }
    }
    
    std::cout << "🎯 Evolution complete after " << iterations << " iterations" << std::endl;
    std::cout << "   Final fitness: " << current_fitness << std::endl;
}

void CoreSelfAwareness::advance_ontogenetic_stage() {
    auto& state = ontogenetic_state_;
    
    switch (state.current_stage) {
        case OntogeneticState::Stage::EMBRYONIC:
            if (state.maturity_level >= 0.3) {
                state.current_stage = OntogeneticState::Stage::JUVENILE;
                record_development_event("Advanced to JUVENILE stage");
                std::cout << "📈 Advanced to JUVENILE stage" << std::endl;
            }
            break;
            
        case OntogeneticState::Stage::JUVENILE:
            if (state.maturity_level >= 0.7) {
                state.current_stage = OntogeneticState::Stage::MATURE;
                record_development_event("Advanced to MATURE stage");
                std::cout << "🌟 Advanced to MATURE stage" << std::endl;
            }
            break;
            
        case OntogeneticState::Stage::MATURE:
            if (state.maturity_level >= 1.0) {
                state.current_stage = OntogeneticState::Stage::SENESCENT;
                record_development_event("Advanced to SENESCENT stage - ready for renewal");
                std::cout << "🍂 Advanced to SENESCENT stage" << std::endl;
            }
            break;
            
        case OntogeneticState::Stage::SENESCENT:
            // Could trigger self-generation of next generation
            if (autonomous_evolution_enabled_) {
                ontogenetic_state_ = self_generate_next_generation();
            }
            break;
    }
}

std::map<std::string, std::string> CoreSelfAwareness::perform_deep_introspection() {
    std::cout << "🔍 Performing deep introspection cycle..." << std::endl;
    
    std::map<std::string, std::string> insights;
    
    // AUTOGNOSIS: Build hierarchical self-images
    auto self_images = perform_hierarchical_introspection();
    
    insights["hierarchical_awareness"] = 
        "Built " + std::to_string(self_images.size()) + " levels of self-awareness with average confidence " +
        std::to_string(calculate_self_understanding_confidence());
    
    // ONTOGENESIS: Evaluate evolutionary state
    insights["ontogenetic_stage"] = "Current stage: " + ontogenetic_state_.stage_to_string() +
                                   ", Generation: " + std::to_string(ontogenetic_state_.generation) +
                                   ", Maturity: " + std::to_string(ontogenetic_state_.maturity_level);
    
    // Identity coherence
    double identity_score = identity_.calculate_unified_identity_score();
    insights["identity_coherence"] = "Unified identity score: " + std::to_string(identity_score);
    
    // Meta-cognitive insights
    if (meta_monitor_) {
        auto meta_insights = meta_monitor_->generate_meta_insights();
        insights["meta_cognitive"] = "Generated " + std::to_string(meta_insights.size()) + " meta-insights";
    }
    
    // Self-awareness level
    double awareness_level = get_self_awareness_level();
    insights["self_awareness_level"] = std::to_string(awareness_level);
    
    if (awareness_level > 0.8) {
        insights["awareness_assessment"] = "High self-awareness achieved - system demonstrates recursive self-understanding";
    } else if (awareness_level > 0.6) {
        insights["awareness_assessment"] = "Moderate self-awareness - developing meta-cognitive capabilities";
    } else {
        insights["awareness_assessment"] = "Basic self-awareness - foundation established, deepening in progress";
    }
    
    return insights;
}

void CoreSelfAwareness::update_core_identity() {
    std::cout << "🔄 Updating core identity..." << std::endl;
    
    // Update actualization progress
    identity_.teleological.actualization_progress = get_actualization_score();
    identity_.teleological.current_generation = ontogenetic_state_.generation;
    
    // Update fitness score
    identity_.evolutionary.fitness_score = evaluate_ontogenetic_fitness();
    identity_.evolutionary.total_generations = ontogenetic_state_.generation;
    
    // Update coherence
    if (!self_images_.empty()) {
        double total_confidence = 0.0;
        for (const auto& img : self_images_) {
            total_confidence += img.second.confidence;
        }
        identity_.relational.system_coherence = total_confidence / self_images_.size();
    }
    
    // Update cognitive complexity
    identity_.cognitive.cognitive_complexity_level = calculate_cognitive_complexity();
}

std::string CoreSelfAwareness::generate_self_awareness_report() const {
    std::ostringstream report;
    
    report << "╔══════════════════════════════════════════════════════════════╗\n";
    report << "║     OpenCog Unified - Core Self-Awareness Report            ║\n";
    report << "╚══════════════════════════════════════════════════════════════╝\n\n";
    
    report << "CORE IDENTITY:\n";
    report << "  System: " << identity_.system_name << "\n";
    report << "  Version: " << identity_.version << "\n";
    report << "  Architecture: " << identity_.architecture_type << "\n\n";
    
    report << "AUTOGNOSIS (Hierarchical Self-Image):\n";
    report << "  Self-Image Levels: " << self_images_.size() << "\n";
    report << "  Max Recursion Depth: " << max_recursion_depth_ << "\n";
    report << "  Self-Understanding Confidence: " << std::fixed << std::setprecision(3);
    
    if (!self_images_.empty()) {
        double total_conf = 0.0;
        for (const auto& img : self_images_) {
            total_conf += img.second.confidence;
            report << "\n    Level " << img.first << ": confidence=" << img.second.confidence
                   << ", reflections=" << img.second.meta_reflections.size();
        }
        report << "\n  Average Confidence: " << (total_conf / self_images_.size()) << "\n\n";
    } else {
        report << " N/A\n\n";
    }
    
    report << "ONTOGENESIS (Self-Generating State):\n";
    report << "  Current Stage: " << ontogenetic_state_.stage_to_string() << "\n";
    report << "  Generation: " << ontogenetic_state_.generation << "\n";
    report << "  Maturity Level: " << ontogenetic_state_.maturity_level << "\n";
    report << "  Fitness: " << ontogenetic_state_.calculate_fitness() << "\n";
    report << "  Can Self-Generate: " << (ontogenetic_state_.can_self_generate ? "Yes" : "No") << "\n";
    report << "  Can Self-Optimize: " << (ontogenetic_state_.can_self_optimize ? "Yes" : "No") << "\n\n";
    
    report << "IDENTITY DIMENSIONS:\n";
    report << "  Ontological (BEING): " << identity_.ontological.architectural_completeness << "\n";
    report << "  Teleological (BECOMING): " << identity_.teleological.actualization_progress << "\n";
    report << "  Cognitive (THINKING): " << identity_.cognitive.cognitive_complexity_level << "\n";
    report << "  Relational (INTEGRATING): " << identity_.relational.system_coherence << "\n";
    report << "  Evolutionary (EVOLVING): " << identity_.evolutionary.fitness_score << "\n";
    report << "  Unified Identity Score: " << identity_.calculate_unified_identity_score() << "\n\n";
    
    report << "SELF-AWARENESS METRICS:\n";
    report << "  Self-Awareness Level: " << get_self_awareness_level() << "\n";
    report << "  Actualization Score: " << get_actualization_score() << "\n";
    report << "  Autonomous Evolution: " << (autonomous_evolution_enabled_ ? "Enabled" : "Disabled") << "\n";
    report << "  Introspection Frequency: " << introspection_frequency_hz_ << " Hz\n\n";
    
    report << "DEVELOPMENT HISTORY (" << ontogenetic_state_.development_events.size() << " events):\n";
    int event_count = 0;
    for (auto it = ontogenetic_state_.development_events.rbegin(); 
         it != ontogenetic_state_.development_events.rend() && event_count < 5; ++it, ++event_count) {
        report << "  • " << *it << "\n";
    }
    
    return report.str();
}

std::string CoreSelfAwareness::export_introspection_json() const {
    std::ostringstream json;
    
    json << "{\n";
    json << "  \"system_name\": \"" << identity_.system_name << "\",\n";
    json << "  \"self_awareness_level\": " << get_self_awareness_level() << ",\n";
    json << "  \"actualization_score\": " << get_actualization_score() << ",\n";
    json << "  \"unified_identity_score\": " << identity_.calculate_unified_identity_score() << ",\n";
    json << "  \"ontogenetic_generation\": " << ontogenetic_state_.generation << ",\n";
    json << "  \"ontogenetic_stage\": \"" << ontogenetic_state_.stage_to_string() << "\",\n";
    json << "  \"hierarchical_self_images\": [\n";
    
    bool first = true;
    for (const auto& img_pair : self_images_) {
        if (!first) json << ",\n";
        json << "    " << img_pair.second.to_json();
        first = false;
    }
    
    json << "\n  ]\n";
    json << "}";
    
    return json.str();
}

// Self-Identity Queries
std::string CoreSelfAwareness::query_self_identity() const {
    std::ostringstream response;
    response << "I am " << identity_.system_name << ", an " << identity_.architecture_type 
             << " with " << identity_.ontological.core_components.size() << " core components. ";
    response << "My architectural completeness is " << identity_.ontological.architectural_completeness << ". ";
    response << "I exist to integrate cognitive capabilities for AGI development.";
    return response.str();
}

std::string CoreSelfAwareness::query_self_purpose() const {
    std::ostringstream response;
    response << "I am becoming " << identity_.teleological.primary_purpose << ". ";
    response << "I am currently at " << (identity_.teleological.actualization_progress * 100) << "% actualization ";
    response << "in generation " << identity_.teleological.current_generation << ". ";
    response << "My evolutionary targets include: ";
    for (size_t i = 0; i < identity_.teleological.evolutionary_targets.size(); ++i) {
        if (i > 0) response << ", ";
        response << identity_.teleological.evolutionary_targets[i];
    }
    return response.str();
}

std::string CoreSelfAwareness::query_self_cognition() const {
    std::ostringstream response;
    response << "I think using " << identity_.cognitive.reasoning_modes.size() << " reasoning modes: ";
    for (size_t i = 0; i < identity_.cognitive.reasoning_modes.size() && i < 3; ++i) {
        if (i > 0) response << ", ";
        response << identity_.cognitive.reasoning_modes[i];
    }
    response << ". My cognitive complexity level is " << identity_.cognitive.cognitive_complexity_level << ".";
    return response.str();
}

std::string CoreSelfAwareness::query_self_integration() const {
    std::ostringstream response;
    response << "I integrate through " << identity_.relational.integration_interfaces.size() 
             << " interfaces with " << identity_.relational.component_dependencies.size() 
             << " component dependencies. ";
    response << "My system coherence is " << identity_.relational.system_coherence << ".";
    return response.str();
}

std::string CoreSelfAwareness::query_self_evolution() const {
    std::ostringstream response;
    response << "I have evolved through " << identity_.evolutionary.total_generations 
             << " generations with a fitness score of " << identity_.evolutionary.fitness_score << ". ";
    response << "My self-improvement capacity is " << identity_.evolutionary.self_improvement_capacity << ". ";
    response << "I can " << (ontogenetic_state_.can_self_generate ? "" : "not ") << "self-generate and "
             << (ontogenetic_state_.can_self_optimize ? "" : "not ") << "self-optimize.";
    return response.str();
}

void CoreSelfAwareness::register_self_observation(
    const std::string& component_name,
    std::function<double()> observation_func
) {
    self_observation_functions_[component_name] = observation_func;
    
    // Also register with meta-cognitive monitor if available
    if (meta_monitor_) {
        meta_monitor_->register_observation_function(component_name, observation_func);
    }
    
    std::cout << "📝 Registered self-observation for: " << component_name << std::endl;
}

void CoreSelfAwareness::set_autonomous_evolution(bool enabled) {
    autonomous_evolution_enabled_ = enabled;
    std::cout << "🔧 Autonomous evolution " << (enabled ? "enabled" : "disabled") << std::endl;
}

void CoreSelfAwareness::set_max_recursion_depth(int depth) {
    max_recursion_depth_ = depth;
    std::cout << "🔧 Max recursion depth set to: " << depth << std::endl;
}

void CoreSelfAwareness::set_introspection_frequency(double frequency_hz) {
    introspection_frequency_hz_ = frequency_hz;
    std::cout << "🔧 Introspection frequency set to: " << frequency_hz << " Hz" << std::endl;
}

double CoreSelfAwareness::get_self_awareness_level() const {
    // Calculate based on hierarchical depth and confidence
    if (self_images_.empty()) return 0.0;
    
    double depth_factor = static_cast<double>(self_images_.size()) / (max_recursion_depth_ + 1);
    double confidence_factor = 0.0;
    
    for (const auto& img : self_images_) {
        confidence_factor += img.second.confidence;
    }
    confidence_factor /= self_images_.size();
    
    return (depth_factor * 0.5 + confidence_factor * 0.5);
}

double CoreSelfAwareness::get_actualization_score() const {
    // Based on identity dimensions
    return identity_.calculate_unified_identity_score();
}

// Private helper methods
void CoreSelfAwareness::initialize_core_identity() {
    identity_.system_name = "OpenCog Unified";
    identity_.version = "1.0-alpha";
    identity_.architecture_type = "Cognitive Architecture for AGI";
    
    // Ontological identity
    identity_.ontological.core_components = {
        "cogutil", "atomspace", "cogserver", "unify", "ure",
        "attention", "spacetime", "pln", "miner", "moses",
        "asmoses", "lg-atomese", "learn", "opencog"
    };
    identity_.ontological.architectural_completeness = 1.0; // All components present
    
    // Teleological identity
    identity_.teleological.primary_purpose = "Comprehensive AGI cognitive architecture";
    identity_.teleological.actualization_progress = 0.72; // From entelechy report
    
    // Cognitive identity
    identity_.cognitive.reasoning_modes = {
        "Probabilistic Logic Networks", "Pattern Mining", "Attention Allocation"
    };
    identity_.cognitive.cognitive_complexity_level = 0.7;
    
    // Relational identity
    identity_.relational.system_coherence = 0.65; // From entelechy report
    
    // Evolutionary identity
    identity_.evolutionary.fitness_score = 0.72;
    identity_.evolutionary.self_improvement_capacity = 1.0; // Has meta-tools
}

void CoreSelfAwareness::initialize_ontogenetic_state() {
    ontogenetic_state_.current_stage = OntogeneticState::Stage::JUVENILE;
    ontogenetic_state_.generation = 0;
    ontogenetic_state_.maturity_level = 0.5;
    ontogenetic_state_.can_self_generate = true;
    ontogenetic_state_.can_self_optimize = true;
    ontogenetic_state_.can_self_reproduce = true;
    
    // Initialize genetic coefficients
    ontogenetic_state_.genetic_coefficients["learning_rate"] = 0.1;
    ontogenetic_state_.genetic_coefficients["adaptation_rate"] = 0.2;
    ontogenetic_state_.genetic_coefficients["exploration_factor"] = 0.3;
    
    record_development_event("Ontogenetic system initialized");
}

HierarchicalSelfImage CoreSelfAwareness::observe_direct_system_state() {
    HierarchicalSelfImage image;
    image.level = 0;
    image.confidence = 0.9; // Direct observation is highly confident
    image.timestamp = std::chrono::steady_clock::now();
    
    // Observe component states
    for (const auto& obs_func : self_observation_functions_) {
        try {
            double state = obs_func.second();
            image.component_states[obs_func.first] = state;
        } catch (...) {
            image.component_states[obs_func.first] = 0.0;
        }
    }
    
    // Detect behavioral patterns
    image.behavioral_patterns = detect_behavioral_patterns();
    
    // Performance metrics (from meta-monitor if available)
    if (meta_monitor_) {
        auto state = meta_monitor_->get_current_state();
        image.performance_metrics["fitness"] = state.fitness_score;
        image.performance_metrics["adaptation_rate"] = state.adaptation_rate;
    }
    
    image.self_awareness_quality = 0.5; // Base level
    image.introspection_depth = 1.0;
    
    image.meta_reflections = generate_meta_reflections(0);
    
    return image;
}

HierarchicalSelfImage CoreSelfAwareness::reflect_on_lower_level(
    const HierarchicalSelfImage& lower_level, int target_level
) {
    HierarchicalSelfImage meta_image;
    meta_image.level = target_level;
    meta_image.confidence = lower_level.confidence * 0.8; // Confidence decreases with recursion
    meta_image.timestamp = std::chrono::steady_clock::now();
    
    // Meta-cognitive analysis of lower level
    meta_image.component_states["lower_level_confidence"] = lower_level.confidence;
    meta_image.component_states["lower_level_components"] = lower_level.component_states.size();
    meta_image.component_states["lower_level_reflections"] = lower_level.meta_reflections.size();
    
    // Analyze quality of lower-level understanding
    meta_image.behavioral_patterns["self_understanding_quality"] = lower_level.self_awareness_quality;
    meta_image.behavioral_patterns["introspection_depth"] = lower_level.introspection_depth;
    
    // Performance of meta-cognitive process
    meta_image.performance_metrics["recursion_effectiveness"] = lower_level.confidence;
    
    meta_image.self_awareness_quality = 0.5 + (target_level * 0.1);
    meta_image.introspection_depth = 1.0 + (target_level * 0.2);
    
    meta_image.meta_reflections = generate_meta_reflections(target_level);
    
    return meta_image;
}

std::map<std::string, double> CoreSelfAwareness::detect_behavioral_patterns() {
    std::map<std::string, double> patterns;
    
    // Analyze component state patterns
    if (!self_observation_functions_.empty()) {
        double avg_state = 0.0;
        for (const auto& obs : self_observation_functions_) {
            try {
                avg_state += obs.second();
            } catch (...) {}
        }
        avg_state /= self_observation_functions_.size();
        patterns["average_component_activity"] = avg_state;
    }
    
    // Analyze ontogenetic patterns
    patterns["maturity_trend"] = ontogenetic_state_.maturity_level;
    patterns["evolution_rate"] = ontogenetic_state_.calculate_fitness();
    
    return patterns;
}

double CoreSelfAwareness::calculate_cognitive_complexity() const {
    double complexity = 0.0;
    
    // Based on number of reasoning modes
    complexity += identity_.cognitive.reasoning_modes.size() / 10.0;
    
    // Based on hierarchical depth achieved
    complexity += static_cast<double>(self_images_.size()) / (max_recursion_depth_ + 1) / 2.0;
    
    // Based on self-observation diversity
    complexity += self_observation_functions_.size() / 20.0;
    
    return std::min(1.0, complexity);
}

double CoreSelfAwareness::evaluate_ontogenetic_fitness() const {
    // Combine multiple fitness factors
    double stage_fitness = ontogenetic_state_.calculate_fitness();
    double identity_fitness = identity_.calculate_unified_identity_score();
    double awareness_fitness = get_self_awareness_level();
    
    return (stage_fitness * 0.3 + identity_fitness * 0.4 + awareness_fitness * 0.3);
}

void CoreSelfAwareness::apply_genetic_operators(double mutation_rate) {
    for (auto& coeff : ontogenetic_state_.genetic_coefficients) {
        if (static_cast<double>(rand()) / RAND_MAX < mutation_rate) {
            double mutation = (static_cast<double>(rand()) / RAND_MAX - 0.5) * 0.2;
            coeff.second = std::max(0.0, std::min(1.0, coeff.second + mutation));
        }
    }
}

void CoreSelfAwareness::update_maturity_level() {
    // Maturity increases with successful operations
    ontogenetic_state_.maturity_level += 0.01;
    ontogenetic_state_.maturity_level = std::min(1.2, ontogenetic_state_.maturity_level); // Allow slight overshoot
    
    // Check for stage advancement
    advance_ontogenetic_stage();
}

void CoreSelfAwareness::record_development_event(const std::string& event) {
    ontogenetic_state_.development_events.push_back(event);
    
    // Keep history manageable
    if (ontogenetic_state_.development_events.size() > 100) {
        ontogenetic_state_.development_events.erase(
            ontogenetic_state_.development_events.begin()
        );
    }
}

} // namespace opencog
