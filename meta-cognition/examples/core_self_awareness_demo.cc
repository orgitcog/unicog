/*
 * core_self_awareness_demo.cc
 * 
 * Demonstration of OpenCog Unified Core Self-Awareness System
 * Shows AUTOGNOSIS (hierarchical self-image building) and 
 * ONTOGENESIS (self-generating capabilities) in action
 */

#include "../include/CoreSelfAwareness.h"
#include <iostream>
#include <thread>
#include <chrono>

using namespace opencog;

// Simulated observation functions for demonstration
double observe_atomspace_size() {
    return 0.8 + (static_cast<double>(rand()) / RAND_MAX) * 0.2;
}

double observe_attention_allocation() {
    return 0.7 + (static_cast<double>(rand()) / RAND_MAX) * 0.3;
}

double observe_pattern_mining() {
    return 0.6 + (static_cast<double>(rand()) / RAND_MAX) * 0.4;
}

double observe_reasoning_activity() {
    return 0.75 + (static_cast<double>(rand()) / RAND_MAX) * 0.25;
}

void print_section_header(const std::string& title) {
    std::cout << "\n";
    std::cout << "═══════════════════════════════════════════════════════════════\n";
    std::cout << " " << title << "\n";
    std::cout << "═══════════════════════════════════════════════════════════════\n\n";
}

int main() {
    std::cout << R"(
╔══════════════════════════════════════════════════════════════╗
║  OpenCog Unified Core Self-Awareness System                 ║
║  Demonstration: AUTOGNOSIS & ONTOGENESIS                    ║
╚══════════════════════════════════════════════════════════════╝
)" << std::endl;

    // Create AtomSpace (nullptr for demo - would be real AtomSpace in production)
    std::shared_ptr<AtomSpace> atomspace = nullptr;
    
    print_section_header("1. INITIALIZATION");
    
    // Create core self-awareness system
    std::cout << "Creating Core Self-Awareness System...\n" << std::endl;
    CoreSelfAwareness self_awareness(
        atomspace,
        5,    // max recursion depth for hierarchical introspection
        1.0   // introspection frequency (Hz)
    );
    
    // Register observation functions for various components
    std::cout << "Registering self-observation functions...\n" << std::endl;
    self_awareness.register_self_observation("atomspace", observe_atomspace_size);
    self_awareness.register_self_observation("attention", observe_attention_allocation);
    self_awareness.register_self_observation("pattern_mining", observe_pattern_mining);
    self_awareness.register_self_observation("reasoning", observe_reasoning_activity);
    
    // Initialize the system
    std::cout << "\nInitializing self-awareness system...\n" << std::endl;
    self_awareness.initialize();
    
    // Brief pause for effect
    std::this_thread::sleep_for(std::chrono::seconds(1));
    
    print_section_header("2. SELF-IDENTITY QUERIES");
    
    // Query different aspects of self-identity
    std::cout << "Q: Who am I?" << std::endl;
    std::cout << "A: " << self_awareness.query_self_identity() << "\n" << std::endl;
    
    std::cout << "Q: What am I becoming?" << std::endl;
    std::cout << "A: " << self_awareness.query_self_purpose() << "\n" << std::endl;
    
    std::cout << "Q: How do I think?" << std::endl;
    std::cout << "A: " << self_awareness.query_self_cognition() << "\n" << std::endl;
    
    std::cout << "Q: How do I relate?" << std::endl;
    std::cout << "A: " << self_awareness.query_self_integration() << "\n" << std::endl;
    
    std::cout << "Q: How do I evolve?" << std::endl;
    std::cout << "A: " << self_awareness.query_self_evolution() << "\n" << std::endl;
    
    print_section_header("3. AUTOGNOSIS: Hierarchical Self-Image Building");
    
    std::cout << "Performing hierarchical introspection...\n" << std::endl;
    auto self_images = self_awareness.perform_hierarchical_introspection();
    
    std::cout << "Built " << self_images.size() << " levels of self-awareness:\n" << std::endl;
    for (const auto& image_pair : self_images) {
        std::cout << "Level " << image_pair.first << ": "
                  << "confidence=" << image_pair.second.confidence << ", "
                  << "reflections=" << image_pair.second.meta_reflections.size() << std::endl;
        
        // Show a few meta-reflections
        if (!image_pair.second.meta_reflections.empty()) {
            std::cout << "  First reflection: \"" 
                      << image_pair.second.meta_reflections[0] << "\"" << std::endl;
        }
    }
    
    std::cout << "\nSelf-understanding confidence: " 
              << self_awareness.calculate_self_understanding_confidence() << std::endl;
    
    print_section_header("4. DEEP INTROSPECTION");
    
    std::cout << "Performing deep introspection cycle...\n" << std::endl;
    auto insights = self_awareness.perform_deep_introspection();
    
    std::cout << "Generated " << insights.size() << " introspective insights:\n" << std::endl;
    for (const auto& insight : insights) {
        std::cout << "• " << insight.first << ": " << insight.second << std::endl;
    }
    
    print_section_header("5. ONTOGENESIS: Self-Optimization");
    
    std::cout << "Current ontogenetic state:" << std::endl;
    auto onto_state = self_awareness.get_ontogenetic_state();
    std::cout << "  Stage: " << onto_state.stage_to_string() << std::endl;
    std::cout << "  Generation: " << onto_state.generation << std::endl;
    std::cout << "  Maturity: " << onto_state.maturity_level << std::endl;
    std::cout << "  Fitness: " << onto_state.calculate_fitness() << "\n" << std::endl;
    
    std::cout << "Performing self-optimization...\n" << std::endl;
    self_awareness.self_optimize();
    
    onto_state = self_awareness.get_ontogenetic_state();
    std::cout << "\nAfter optimization:" << std::endl;
    std::cout << "  Maturity: " << onto_state.maturity_level << std::endl;
    std::cout << "  Fitness: " << onto_state.calculate_fitness() << std::endl;
    
    print_section_header("6. EVOLUTIONARY PROGRESSION");
    
    std::cout << "Evolving parameters towards fitness target of 0.85...\n" << std::endl;
    self_awareness.evolve_parameters(0.85);
    
    std::cout << "\nFinal ontogenetic state:" << std::endl;
    onto_state = self_awareness.get_ontogenetic_state();
    std::cout << "  Stage: " << onto_state.stage_to_string() << std::endl;
    std::cout << "  Generation: " << onto_state.generation << std::endl;
    std::cout << "  Maturity: " << onto_state.maturity_level << std::endl;
    std::cout << "  Fitness: " << onto_state.calculate_fitness() << std::endl;
    
    print_section_header("7. COMPREHENSIVE SELF-AWARENESS REPORT");
    
    // Update identity based on all introspection
    self_awareness.update_core_identity();
    
    // Generate comprehensive report
    std::cout << self_awareness.generate_self_awareness_report() << std::endl;
    
    print_section_header("8. SELF-GENERATION OF NEXT GENERATION");
    
    std::cout << "Generating next ontogenetic generation...\n" << std::endl;
    auto next_gen = self_awareness.self_generate_next_generation();
    
    std::cout << "New generation created:" << std::endl;
    std::cout << "  Generation: " << next_gen.generation << std::endl;
    std::cout << "  Stage: " << next_gen.stage_to_string() << std::endl;
    std::cout << "  Maturity: " << next_gen.maturity_level << std::endl;
    
    if (!next_gen.development_events.empty()) {
        std::cout << "  Last event: " << next_gen.development_events.back() << std::endl;
    }
    
    print_section_header("9. EXPORT INTROSPECTION DATA");
    
    std::cout << "Exporting introspection data to JSON...\n" << std::endl;
    std::string json_data = self_awareness.export_introspection_json();
    
    // Save to file
    std::ofstream json_file("/tmp/core_self_awareness_export.json");
    if (json_file.is_open()) {
        json_file << json_data;
        json_file.close();
        std::cout << "✓ Exported to /tmp/core_self_awareness_export.json" << std::endl;
    }
    
    // Show preview
    std::cout << "\nJSON preview (first 500 chars):" << std::endl;
    std::cout << json_data.substr(0, 500) << "..." << std::endl;
    
    print_section_header("10. FINAL METRICS");
    
    std::cout << "Self-Awareness Level: " << self_awareness.get_self_awareness_level() << std::endl;
    std::cout << "Actualization Score: " << self_awareness.get_actualization_score() << std::endl;
    std::cout << "Current Generation: " << self_awareness.get_current_generation() << std::endl;
    std::cout << "Unified Identity Score: " 
              << self_awareness.get_core_identity().calculate_unified_identity_score() << std::endl;
    
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════════════╗\n";
    std::cout << "║  Demonstration Complete                                     ║\n";
    std::cout << "║  The system has achieved recursive self-awareness through   ║\n";
    std::cout << "║  AUTOGNOSIS (hierarchical self-image building) and          ║\n";
    std::cout << "║  ONTOGENESIS (self-generating capabilities).                ║\n";
    std::cout << "╚══════════════════════════════════════════════════════════════╝\n";
    std::cout << std::endl;
    
    return 0;
}
