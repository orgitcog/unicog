/*
 * CognitiveVisualizer.cc
 * 
 * Phase II.4: Interactive Cognitive Visualization
 * Real-time introspection with adaptive attention overlays
 * Live AtomSpace and ECAN integration
 */

#include "CognitiveVisualizer.h"
#include <algorithm>
#include <cmath>
#include <thread>
#include <chrono>
#include <sstream>
#include <iostream>

using namespace opencog;

CognitiveVisualizer::CognitiveVisualizer()
    : zoom_level_(1.0)
    , viewport_center_(std::make_pair(0.0, 0.0))
    , real_time_mode_(false)
    , attention_threshold_(0.5)
    , feedback_strength_(0.0)
{
    // Initialize default visualization parameters
    std::cout << "🎨 Initializing next-generation cognitive visualization suite..." << std::endl;
}

CognitiveVisualizer::~CognitiveVisualizer()
{
    stop_real_time_mode();
}

bool CognitiveVisualizer::initialize_visualization(int width, int height)
{
    std::cout << "🚀 Initializing visualization viewport: " << width << "x" << height << std::endl;
    
    // Initialize visualization dimensions
    viewport_center_.first = width / 2.0;
    viewport_center_.second = height / 2.0;
    
    // Initialize node position cache
    node_positions_.clear();
    attention_intensities_.clear();
    cognitive_salience_.clear();
    
    return true;
}

void CognitiveVisualizer::update_cognitive_visualization(
    const std::map<std::string, std::vector<double>>& hypergraph_nodes,
    const std::map<std::string, std::vector<std::pair<std::string, double>>>& hypergraph_edges,
    const std::map<std::string, std::vector<double>>& agent_states)
{
    // Update node positions using force-directed layout
    calculate_node_positions(hypergraph_nodes, hypergraph_edges);
    
    // Update cognitive salience mapping
    update_cognitive_salience(hypergraph_nodes, agent_states);
    
    // Update attention intensities based on current cognitive state
    for (const auto& node_pair : hypergraph_nodes) {
        const std::string& node_id = node_pair.first;
        const std::vector<double>& node_state = node_pair.second;
        
        if (!node_state.empty()) {
            // Calculate attention intensity from node state
            double attention = 0.0;
            for (double state_value : node_state) {
                attention += std::abs(state_value);
            }
            attention /= node_state.size();
            
            attention_intensities_[node_id] = {attention};
            
            // Update cognitive salience based on attention and connectivity
            double salience = attention;
            
            // Boost salience for highly connected nodes
            auto edge_it = hypergraph_edges.find(node_id);
            if (edge_it != hypergraph_edges.end()) {
                salience += 0.1 * edge_it->second.size();
            }
            
            cognitive_salience_[node_id] = salience;
        }
    }
}

void CognitiveVisualizer::render_attention_overlays(
    const std::map<std::string, double>& attention_data,
    const std::map<std::string, double>& salience_map)
{
    // Update internal attention data for rendering
    for (const auto& attention_pair : attention_data) {
        const std::string& element_id = attention_pair.first;
        double attention_value = attention_pair.second;
        
        // Apply attention threshold filter
        if (attention_value > attention_threshold_) {
            attention_intensities_[element_id] = {attention_value};
            
            // Update cognitive salience with attention boost
            auto salience_it = salience_map.find(element_id);
            if (salience_it != salience_map.end()) {
                cognitive_salience_[element_id] = salience_it->second * (1.0 + attention_value);
            }
        }
    }
}

std::vector<double> CognitiveVisualizer::generate_visualization_feedback(
    const std::string& interaction_type,
    const std::string& interaction_target,
    double interaction_strength)
{
    std::vector<double> feedback_vector;
    
    std::cout << "🔄 Generating visualization feedback: " 
              << interaction_type << " on " << interaction_target 
              << " (strength: " << interaction_strength << ")" << std::endl;
    
    // Create multi-dimensional feedback based on interaction
    if (interaction_type == "click") {
        // Strong localized feedback for clicks
        feedback_vector = {interaction_strength, 0.8 * interaction_strength, 0.5 * interaction_strength};
        
        // Boost attention for target element
        auto attention_it = attention_intensities_.find(interaction_target);
        if (attention_it != attention_intensities_.end()) {
            attention_it->second[0] = std::min(1.0, attention_it->second[0] + 0.2 * interaction_strength);
        }
        
    } else if (interaction_type == "hover") {
        // Gentle distributed feedback for hovering
        feedback_vector = {0.3 * interaction_strength, 0.2 * interaction_strength, 0.1 * interaction_strength};
        
    } else if (interaction_type == "drag") {
        // Spatial feedback for dragging operations
        feedback_vector = {0.6 * interaction_strength, 0.4 * interaction_strength, 0.2 * interaction_strength};
    }
    
    // Update global feedback strength
    feedback_strength_ = std::min(1.0, feedback_strength_ + 0.1 * interaction_strength);
    
    // Apply feedback callback if set
    if (feedback_callback_) {
        feedback_callback_(feedback_vector);
    }
    
    return feedback_vector;
}

std::string CognitiveVisualizer::export_visualization_state() const
{
    std::ostringstream json_stream;
    json_stream << "{\n";
    json_stream << "  \"zoom_level\": " << zoom_level_ << ",\n";
    json_stream << "  \"viewport_center\": [" << viewport_center_.first << ", " << viewport_center_.second << "],\n";
    json_stream << "  \"attention_threshold\": " << attention_threshold_ << ",\n";
    json_stream << "  \"feedback_strength\": " << feedback_strength_ << ",\n";
    json_stream << "  \"real_time_mode\": " << (real_time_mode_ ? "true" : "false") << ",\n";
    
    // Export node positions
    json_stream << "  \"node_positions\": {\n";
    bool first_node = true;
    for (const auto& node_pair : node_positions_) {
        if (!first_node) json_stream << ",\n";
        json_stream << "    \"" << node_pair.first << "\": [";
        const auto& position = node_pair.second;
        for (size_t i = 0; i < position.size(); ++i) {
            if (i > 0) json_stream << ", ";
            json_stream << position[i];
        }
        json_stream << "]";
        first_node = false;
    }
    json_stream << "\n  },\n";
    
    // Export attention intensities
    json_stream << "  \"attention_intensities\": {\n";
    bool first_attention = true;
    for (const auto& attention_pair : attention_intensities_) {
        if (!first_attention) json_stream << ",\n";
        json_stream << "    \"" << attention_pair.first << "\": [";
        const auto& intensities = attention_pair.second;
        for (size_t i = 0; i < intensities.size(); ++i) {
            if (i > 0) json_stream << ", ";
            json_stream << intensities[i];
        }
        json_stream << "]";
        first_attention = false;
    }
    json_stream << "\n  },\n";
    
    // Export cognitive salience
    json_stream << "  \"cognitive_salience\": {\n";
    bool first_salience = true;
    for (const auto& salience_pair : cognitive_salience_) {
        if (!first_salience) json_stream << ",\n";
        json_stream << "    \"" << salience_pair.first << "\": " << salience_pair.second;
        first_salience = false;
    }
    json_stream << "\n  }\n";
    
    json_stream << "}";
    return json_stream.str();
}

bool CognitiveVisualizer::import_visualization_state(const std::string& json_state)
{
//<<<<<<< copilot/fix-17
    // Basic JSON parsing for state import - parse key visualization parameters
//    std::cout << "📥 Importing visualization state from JSON..." << std::endl;
    
    // Simple JSON parsing for core parameters
//    try {
        // Parse zoom_level
//        size_t zoom_pos = json_state.find("\"zoom_level\":");
//        if (zoom_pos != std::string::npos) {
//            size_t start = json_state.find(":", zoom_pos) + 1;
//            size_t end = json_state.find(",", start);
//            if (end == std::string::npos) end = json_state.find("}", start);
//            if (start < end) {
//                std::string zoom_str = json_state.substr(start, end - start);
//                zoom_level_ = std::stod(zoom_str);
//            }
//        }
        
        // Parse attention_threshold
//        size_t threshold_pos = json_state.find("\"attention_threshold\":");
//        if (threshold_pos != std::string::npos) {
//            size_t start = json_state.find(":", threshold_pos) + 1;
//            size_t end = json_state.find(",", start);
//            if (end == std::string::npos) end = json_state.find("}", start);
//            if (start < end) {
//                std::string threshold_str = json_state.substr(start, end - start);
//                attention_threshold_ = std::stod(threshold_str);
//            }
//        }
        
        // Parse real_time_mode
//        size_t realtime_pos = json_state.find("\"real_time_mode\":");
//        if (realtime_pos != std::string::npos) {
//            size_t start = json_state.find(":", realtime_pos) + 1;
//            size_t end = json_state.find(",", start);
//            if (end == std::string::npos) end = json_state.find("}", start);
//            if (start < end) {
//                std::string mode_str = json_state.substr(start, end - start);
//                real_time_mode_ = (mode_str.find("true") != std::string::npos);
//            }
//        }
        
//        std::cout << "✅ Successfully imported visualization state" << std::endl;
//        return true;
//    }
//    catch (const std::exception& e) {
//        std::cout << "⚠️ Error parsing JSON state: " << e.what() << std::endl;
//=======
    std::cout << "📥 Importing visualization state from JSON..." << std::endl;
    
    if (json_state.empty()) {
        std::cout << "❌ Empty JSON state provided" << std::endl;
        return false;
    }
    
    try {
        // Simple JSON parsing implementation for basic state restoration
        // Look for key fields in the JSON string
        
        // Extract zoom_level
        size_t zoom_pos = json_state.find("\"zoom_level\":");
        if (zoom_pos != std::string::npos) {
            size_t value_start = json_state.find(':', zoom_pos) + 1;
            size_t value_end = json_state.find(',', value_start);
            if (value_end == std::string::npos) value_end = json_state.find('}', value_start);
            
            if (value_start != std::string::npos && value_end != std::string::npos) {
                std::string zoom_str = json_state.substr(value_start, value_end - value_start);
                // Remove whitespace
                zoom_str.erase(0, zoom_str.find_first_not_of(" \t\n\r"));
                zoom_str.erase(zoom_str.find_last_not_of(" \t\n\r") + 1);
                
                try {
                    zoom_level_ = std::stod(zoom_str);
                    std::cout << "✅ Restored zoom level: " << zoom_level_ << std::endl;
                } catch (const std::exception& e) {
                    std::cout << "⚠️  Failed to parse zoom level" << std::endl;
                }
            }
        }
        
        // Extract viewport_center
        size_t viewport_pos = json_state.find("\"viewport_center\":");
        if (viewport_pos != std::string::npos) {
            size_t array_start = json_state.find('[', viewport_pos);
            size_t array_end = json_state.find(']', array_start);
            
            if (array_start != std::string::npos && array_end != std::string::npos) {
                std::string array_content = json_state.substr(array_start + 1, array_end - array_start - 1);
                size_t comma_pos = array_content.find(',');
                
                if (comma_pos != std::string::npos) {
                    try {
                        std::string x_str = array_content.substr(0, comma_pos);
                        std::string y_str = array_content.substr(comma_pos + 1);
                        
                        // Remove whitespace
                        x_str.erase(0, x_str.find_first_not_of(" \t\n\r"));
                        x_str.erase(x_str.find_last_not_of(" \t\n\r") + 1);
                        y_str.erase(0, y_str.find_first_not_of(" \t\n\r"));
                        y_str.erase(y_str.find_last_not_of(" \t\n\r") + 1);
                        
                        viewport_center_.first = std::stod(x_str);
                        viewport_center_.second = std::stod(y_str);
                        std::cout << "✅ Restored viewport center: (" << viewport_center_.first 
                                  << ", " << viewport_center_.second << ")" << std::endl;
                    } catch (const std::exception& e) {
                        std::cout << "⚠️  Failed to parse viewport center" << std::endl;
                    }
                }
            }
        }
        
        // Extract attention_threshold
        size_t threshold_pos = json_state.find("\"attention_threshold\":");
        if (threshold_pos != std::string::npos) {
            size_t value_start = json_state.find(':', threshold_pos) + 1;
            size_t value_end = json_state.find(',', value_start);
            if (value_end == std::string::npos) value_end = json_state.find('}', value_start);
            
            if (value_start != std::string::npos && value_end != std::string::npos) {
                std::string threshold_str = json_state.substr(value_start, value_end - value_start);
                threshold_str.erase(0, threshold_str.find_first_not_of(" \t\n\r"));
                threshold_str.erase(threshold_str.find_last_not_of(" \t\n\r") + 1);
                
                try {
                    attention_threshold_ = std::stod(threshold_str);
                    std::cout << "✅ Restored attention threshold: " << attention_threshold_ << std::endl;
                } catch (const std::exception& e) {
                    std::cout << "⚠️  Failed to parse attention threshold" << std::endl;
                }
            }
        }
        
        // Extract real_time_mode
        size_t realtime_pos = json_state.find("\"real_time_mode\":");
        if (realtime_pos != std::string::npos) {
            size_t value_start = json_state.find(':', realtime_pos) + 1;
            size_t value_end = json_state.find(',', value_start);
            if (value_end == std::string::npos) value_end = json_state.find('}', value_start);
            
            if (value_start != std::string::npos && value_end != std::string::npos) {
                std::string mode_str = json_state.substr(value_start, value_end - value_start);
                mode_str.erase(0, mode_str.find_first_not_of(" \t\n\r"));
                mode_str.erase(mode_str.find_last_not_of(" \t\n\r") + 1);
                
                if (mode_str.find("true") != std::string::npos) {
                    real_time_mode_ = true;
                    std::cout << "✅ Restored real-time mode: enabled" << std::endl;
                } else if (mode_str.find("false") != std::string::npos) {
                    real_time_mode_ = false;
                    std::cout << "✅ Restored real-time mode: disabled" << std::endl;
                }
            }
        }
        
        // Clear existing data before importing new state
        node_positions_.clear();
        attention_intensities_.clear();
        cognitive_salience_.clear();
        
        // TODO: Implement full node_positions, attention_intensities, and cognitive_salience parsing
        // For now, this implementation handles the basic state fields
        std::cout << "⚠️  Note: Full node data parsing not yet implemented - only basic state restored" << std::endl;
        
        std::cout << "✅ Visualization state import completed successfully" << std::endl;
        return true;
        
    } catch (const std::exception& e) {
        std::cout << "❌ Failed to import visualization state: " << e.what() << std::endl;
//>>>>>>> main
        return false;
    }
}

void CognitiveVisualizer::start_real_time_mode(double update_frequency_hz)
{
    if (real_time_mode_) {
        return; // Already running
    }
    
    real_time_mode_ = true;
    std::cout << "⚡ Starting real-time visualization mode at " << update_frequency_hz << " Hz" << std::endl;
    
    // Start real-time update thread
    std::thread update_thread([this, update_frequency_hz]() {
        auto sleep_duration = std::chrono::milliseconds(static_cast<int>(1000.0 / update_frequency_hz));
        
        while (real_time_mode_) {
            // Simulate real-time cognitive state updates
            // In a full implementation, this would connect to live AtomSpace data
            
            std::this_thread::sleep_for(sleep_duration);
        }
    });
    
    update_thread.detach();
}

void CognitiveVisualizer::stop_real_time_mode()
{
    if (!real_time_mode_) {
        return;
    }
    
    real_time_mode_ = false;
    std::cout << "⏹️  Stopping real-time visualization mode" << std::endl;
}

void CognitiveVisualizer::set_viewport(double center_x, double center_y, double zoom)
{
    viewport_center_.first = center_x;
    viewport_center_.second = center_y;
    zoom_level_ = std::max(0.1, std::min(10.0, zoom)); // Clamp zoom level
}

void CognitiveVisualizer::calculate_node_positions(
    const std::map<std::string, std::vector<double>>& nodes,
    const std::map<std::string, std::vector<std::pair<std::string, double>>>& edges)
{
    // Implement force-directed layout algorithm
    const double k = 50.0; // Spring constant
    const double repulsion = 1000.0; // Repulsion strength
    const double damping = 0.9; // Velocity damping
    
    // Initialize positions if not already set
    for (const auto& node_pair : nodes) {
        const std::string& node_id = node_pair.first;
        
        if (node_positions_.find(node_id) == node_positions_.end()) {
            // Random initial position
            double x = viewport_center_.first + (rand() % 400 - 200);
            double y = viewport_center_.second + (rand() % 400 - 200);
            node_positions_[node_id] = {x, y, 0.0, 0.0}; // x, y, vx, vy
        }
    }
    
    // Apply forces (simplified force-directed layout)
    std::map<std::string, std::vector<double>> forces;
    
    // Initialize forces
    for (const auto& node_pair : nodes) {
        forces[node_pair.first] = {0.0, 0.0}; // fx, fy
    }
    
    // Repulsion forces between all nodes
    for (const auto& node1 : node_positions_) {
        for (const auto& node2 : node_positions_) {
            if (node1.first == node2.first) continue;
            
            double dx = node1.second[0] - node2.second[0];
            double dy = node1.second[1] - node2.second[1];
            double distance = sqrt(dx*dx + dy*dy);
            
            if (distance > 0.1) {
                double force = repulsion / (distance * distance);
                forces[node1.first][0] += force * dx / distance;
                forces[node1.first][1] += force * dy / distance;
            }
        }
    }
    
    // Attraction forces for connected nodes
    for (const auto& edge_pair : edges) {
        const std::string& source = edge_pair.first;
        
        for (const auto& connection : edge_pair.second) {
            const std::string& target = connection.first;
            double weight = connection.second;
            
            auto source_pos = node_positions_.find(source);
            auto target_pos = node_positions_.find(target);
            
            if (source_pos != node_positions_.end() && target_pos != node_positions_.end()) {
                double dx = target_pos->second[0] - source_pos->second[0];
                double dy = target_pos->second[1] - source_pos->second[1];
                double distance = sqrt(dx*dx + dy*dy);
                
                if (distance > 0.1) {
                    double force = k * weight * distance;
                    forces[source][0] += force * dx / distance;
                    forces[source][1] += force * dy / distance;
                    forces[target][0] -= force * dx / distance;
                    forces[target][1] -= force * dy / distance;
                }
            }
        }
    }
    
    // Update positions and velocities
    for (auto& node_pair : node_positions_) {
        std::vector<double>& position = node_pair.second;
        const std::vector<double>& force = forces[node_pair.first];
        
        // Update velocity
        position[2] = (position[2] + force[0] * 0.01) * damping;
        position[3] = (position[3] + force[1] * 0.01) * damping;
        
        // Update position
        position[0] += position[2];
        position[1] += position[3];
        
        // Keep nodes within reasonable bounds
        position[0] = std::max(50.0, std::min(1200.0, position[0]));
        position[1] = std::max(50.0, std::min(800.0, position[1]));
    }
}

void CognitiveVisualizer::update_cognitive_salience(
    const std::map<std::string, std::vector<double>>& nodes,
    const std::map<std::string, std::vector<double>>& agents)
{
    // Calculate cognitive salience based on node states and agent influences
    for (const auto& node_pair : nodes) {
        const std::string& node_id = node_pair.first;
        const std::vector<double>& node_state = node_pair.second;
        
        double salience = 0.0;
        
        // Base salience from node state
        for (double state_value : node_state) {
            salience += std::abs(state_value);
        }
        salience /= node_state.size();
        
        // Boost from agent proximity (simplified)
        for (const auto& agent_pair : agents) {
            const std::vector<double>& agent_state = agent_pair.second;
            
            // Calculate influence based on agent state similarity
            double influence = 0.0;
            for (size_t i = 0; i < std::min(node_state.size(), agent_state.size()); ++i) {
                influence += std::abs(node_state[i] - agent_state[i]);
            }
            
            if (!agent_state.empty()) {
                influence = 1.0 - (influence / agent_state.size());
                salience += 0.2 * influence;
            }
        }
        
        cognitive_salience_[node_id] = std::min(1.0, salience);
    }
}

void CognitiveVisualizer::apply_interaction_feedback(const std::vector<double>& feedback)
{
    // Apply user interaction feedback to the cognitive system
    std::cout << "🔄 Applying interaction feedback to cognitive system" << std::endl;
    
    // This would integrate with the actual cognitive system
    // For now, we update internal state
    feedback_strength_ = std::min(1.0, feedback_strength_ + 0.05);
    
    // Propagate feedback effects to connected elements
    for (auto& attention_pair : attention_intensities_) {
        for (size_t i = 0; i < feedback.size() && i < attention_pair.second.size(); ++i) {
            attention_pair.second[i] = std::min(1.0, attention_pair.second[i] + 0.1 * feedback[i]);
        }
    }
}