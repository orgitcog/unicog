/*
 * neural_symbolic_demo.cc
 *
 * Demonstration of Enhanced Neural-Symbolic Integration Bridge
 * Shows basic functionality and integration capabilities
 */

#include "../include/NeuralSymbolicBridge.h"
#include <iostream>
#include <vector>
#include <random>

using namespace opencog;

void demonstrate_basic_integration() {
    std::cout << "\n=== Basic Neural-Symbolic Integration Demo ===" << std::endl;
    
    // Create bridge
    NeuralSymbolicBridge bridge(0.01, 0.7, 100);
    
    // Create some mappings
    bridge.create_mapping("concept_mapping_1", "layer_visual", "ConceptNode_Cat", 0.8);
    bridge.create_mapping("concept_mapping_2", "layer_audio", "ConceptNode_Meow", 0.7);
    bridge.create_mapping("relation_mapping_1", "layer_semantic", "RelationNode_HasSound", 0.6);
    
    // Create neural representation
    NeuralRepresentation neural_rep;
    neural_rep.layer_id = "layer_visual";
    neural_rep.activations = {0.8, 0.6, 0.9, 0.3};
    neural_rep.weights = {0.5, 0.7};
    neural_rep.confidence = 0.85;
    neural_rep.timestamp = 12345;
    neural_rep.attention_scores["visual_features"] = 0.9;
    
    // Convert to symbolic
    SymbolicRepresentation symbolic_result = bridge.neural_to_symbolic(neural_rep);
    
    std::cout << "Neural to Symbolic conversion:" << std::endl;
    std::cout << "  Input: Neural layer '" << neural_rep.layer_id << "' with confidence " << neural_rep.confidence << std::endl;
    std::cout << "  Output: Symbolic atom '" << symbolic_result.atom_name << "' of type '" << symbolic_result.atom_type << "'" << std::endl;
    std::cout << "  Semantic weight: " << symbolic_result.semantic_weight << std::endl;
    
    // Create symbolic representation
    SymbolicRepresentation symbolic_rep;
    symbolic_rep.atom_type = "ConceptNode";
    symbolic_rep.atom_name = "ConceptNode_Cat";
    symbolic_rep.semantic_weight = 0.9;
    symbolic_rep.version = 54321;
    symbolic_rep.truth_values["strength"] = 0.8;
    symbolic_rep.truth_values["confidence"] = 0.7;
    symbolic_rep.attention_values["concept_salience"] = 0.95;
    
    // Convert to neural
    NeuralRepresentation neural_result = bridge.symbolic_to_neural(symbolic_rep);
    
    std::cout << "\nSymbolic to Neural conversion:" << std::endl;
    std::cout << "  Input: Symbolic atom '" << symbolic_rep.atom_name << "' with weight " << symbolic_rep.semantic_weight << std::endl;
    std::cout << "  Output: Neural layer '" << neural_result.layer_id << "' with confidence " << neural_result.confidence << std::endl;
    std::cout << "  Activations: ";
    for (double activation : neural_result.activations) {
        std::cout << activation << " ";
    }
    std::cout << std::endl;
}

void demonstrate_attention_integration() {
    std::cout << "\n=== Attention-Driven Integration Demo ===" << std::endl;
    
    NeuralSymbolicBridge bridge(0.02, 0.6, 50);
    
    // Set up attention weights
    std::map<std::string, double> neural_attention = {
        {"layer_visual", 0.9},
        {"layer_audio", 0.7},
        {"layer_semantic", 0.8}
    };
    
    std::map<std::string, double> symbolic_attention = {
        {"ConceptNode_Cat", 0.95},
        {"ConceptNode_Dog", 0.6},
        {"RelationNode_HasSound", 0.8}
    };
    
    bridge.set_neural_attention(neural_attention);
    bridge.set_symbolic_attention(symbolic_attention);
    
    // Create test patterns
    std::vector<NeuralRepresentation> neural_patterns;
    std::vector<SymbolicRepresentation> symbolic_patterns;
    
    // Generate random neural patterns
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);
    
    for (int i = 0; i < 3; ++i) {
        NeuralRepresentation neural;
        neural.layer_id = (i == 0) ? "layer_visual" : (i == 1) ? "layer_audio" : "layer_semantic";
        neural.confidence = dis(gen);
        neural.timestamp = 10000 + i;
        neural.activations = {dis(gen), dis(gen), dis(gen)};
        neural_patterns.push_back(neural);
    }
    
    // Generate symbolic patterns
    std::vector<std::string> concept_names = {"ConceptNode_Cat", "ConceptNode_Dog", "RelationNode_HasSound"};
    for (int i = 0; i < 3; ++i) {
        SymbolicRepresentation symbolic;
        symbolic.atom_type = "ConceptNode";
        symbolic.atom_name = concept_names[i];
        symbolic.semantic_weight = dis(gen);
        symbolic.version = 20000 + i;
        symbolic.truth_values["strength"] = dis(gen);
        symbolic_patterns.push_back(symbolic);
    }
    
    // Perform attention integration
    auto attention_result = bridge.integrate_with_attention(neural_patterns, symbolic_patterns);
    
    std::cout << "Attention Integration Results:" << std::endl;
    std::cout << "  Attention focus: " << attention_result.attention_focus << std::endl;
    std::cout << "  Highlighted neural patterns: " << attention_result.highlighted_neural.size() << std::endl;
    std::cout << "  Highlighted symbolic patterns: " << attention_result.highlighted_symbols.size() << std::endl;
    std::cout << "  Integration context: " << attention_result.integration_context << std::endl;
}

void demonstrate_emergent_patterns() {
    std::cout << "\n=== Emergent Pattern Detection Demo ===" << std::endl;
    
    NeuralSymbolicBridge bridge(0.015, 0.5, 200);
    
    // Create correlated neural and symbolic histories
    std::vector<NeuralRepresentation> neural_history;
    std::vector<SymbolicRepresentation> symbolic_history;
    
    // Generate correlated patterns
    for (int i = 0; i < 5; ++i) {
        NeuralRepresentation neural;
        neural.layer_id = "layer_pattern_" + std::to_string(i % 3);
        neural.confidence = 0.7 + (i * 0.05);
        neural.timestamp = 30000 + i * 100;
        neural.activations = {0.8, 0.6 + i * 0.1, 0.5};
        neural_history.push_back(neural);
        
        SymbolicRepresentation symbolic;
        symbolic.atom_type = "ConceptNode";
        symbolic.atom_name = "Pattern_" + std::to_string(i % 3);
        symbolic.semantic_weight = 0.6 + (i * 0.08);
        symbolic.version = 40000 + i * 100;
        symbolic.truth_values["strength"] = 0.75 + i * 0.03;
        symbolic_history.push_back(symbolic);
    }
    
    // Detect emergent patterns
    auto emergent_patterns = bridge.detect_emergent_patterns(neural_history, symbolic_history);
    
    std::cout << "Emergent Pattern Detection Results:" << std::endl;
    std::cout << "  Total patterns detected: " << emergent_patterns.size() << std::endl;
    
    for (const auto& pattern : emergent_patterns) {
        std::cout << "  Pattern ID: " << pattern.pattern_id << std::endl;
        std::cout << "    Strength: " << pattern.pattern_strength << std::endl;
        std::cout << "    Confidence: " << pattern.emergence_confidence << std::endl;
        std::cout << "    Description: " << pattern.description << std::endl;
        std::cout << "    Neural components: " << pattern.neural_components.size() << std::endl;
        std::cout << "    Symbolic components: " << pattern.symbolic_components.size() << std::endl;
    }
}

void demonstrate_adaptive_learning() {
    std::cout << "\n=== Adaptive Learning Demo ===" << std::endl;
    
    NeuralSymbolicBridge bridge(0.05, 0.6, 100);
    
    // Create initial mapping
    bridge.create_mapping("adaptive_mapping", "learning_layer", "ConceptNode_Learning", 0.5);
    
    std::cout << "Initial mapping strength: 0.5" << std::endl;
    
    // Simulate successful integrations
    for (int i = 0; i < 5; ++i) {
        double success_rate = 0.8 + (i * 0.02); // Increasing success
        double confidence = 0.7 + (i * 0.05);
        
        bridge.update_mapping_strength("adaptive_mapping", success_rate, confidence);
        
        auto stats = bridge.get_integration_statistics();
        std::cout << "  Iteration " << (i + 1) << " - Mapping strength: " 
                  << stats.mapping_strengths["adaptive_mapping"] << std::endl;
    }
    
    // Get final statistics
    auto final_stats = bridge.get_integration_statistics();
    std::cout << "\nFinal Integration Statistics:" << std::endl;
    std::cout << "  Total integrations: " << final_stats.total_integrations << std::endl;
    std::cout << "  Average integration time: " << final_stats.average_integration_time_ms << " ms" << std::endl;
    std::cout << "  Integration accuracy: " << final_stats.integration_accuracy << std::endl;
    std::cout << "  Active mappings: " << final_stats.active_mappings << std::endl;
    std::cout << "  Neural attention coverage: " << final_stats.neural_attention_coverage << std::endl;
    std::cout << "  Symbolic attention coverage: " << final_stats.symbolic_attention_coverage << std::endl;
}

int main() {
    std::cout << "🧠 OpenCog Unified: Enhanced Neural-Symbolic Integration Demo" << std::endl;
    std::cout << "=============================================================" << std::endl;
    
    try {
        demonstrate_basic_integration();
        demonstrate_attention_integration();
        demonstrate_emergent_patterns();
        demonstrate_adaptive_learning();
        
        std::cout << "\n✅ All demonstrations completed successfully!" << std::endl;
        std::cout << "Enhanced Neural-Symbolic Integration Bridge is ready for Phase III deployment." << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error during demonstration: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}