/*
 * CognitiveVisualizer.h
 * 
 * Phase II.4: Interactive Cognitive Visualization
 * Real-time introspection with adaptive attention overlays
 */

#ifndef _OPENCOG_COGNITIVE_VISUALIZER_H
#define _OPENCOG_COGNITIVE_VISUALIZER_H

#include <memory>
#include <vector>
#include <string>
#include <map>
#include <functional>

namespace opencog {

/**
 * Cognitive Visualizer
 * 
 * Main visualization engine for real-time cognitive introspection.
 * Recursive attention: Visualization feedback influences underlying cognitive focus.
 */
class CognitiveVisualizer
{
private:
    // Visualization state
    std::map<std::string, std::vector<double>> node_positions_;
    std::map<std::string, std::vector<double>> attention_intensities_;
    std::map<std::string, double> cognitive_salience_;
    
    // Rendering parameters
    double zoom_level_;
    std::pair<double, double> viewport_center_;
    bool real_time_mode_;
    
    // Adaptive attention parameters
    double attention_threshold_;
    double feedback_strength_;
    
    // Callback for cognitive feedback
    std::function<void(const std::vector<double>&)> feedback_callback_;

public:
    CognitiveVisualizer();
    ~CognitiveVisualizer();

    /**
     * Initialize visualization system
     * 
     * @param width Viewport width
     * @param height Viewport height
     * @return Success flag
     */
    bool initialize_visualization(int width, int height);

    /**
     * Update visualization with current cognitive state
     * 
     * @param hypergraph_nodes Current node states
     * @param hypergraph_edges Current edge states
     * @param agent_states Current agent states (if using distributed cognition)
     */
    void update_cognitive_visualization(
        const std::map<std::string, std::vector<double>>& hypergraph_nodes,
        const std::map<std::string, std::vector<std::pair<std::string, double>>>& hypergraph_edges,
        const std::map<std::string, std::vector<double>>& agent_states = {});

    /**
     * Render adaptive attention overlays
     * Visual cues that adapt to emergent cognitive salience
     * 
     * @param attention_data Current attention allocation state
     * @param salience_map Cognitive salience for different elements
     */
    void render_attention_overlays(
        const std::map<std::string, double>& attention_data,
        const std::map<std::string, double>& salience_map);

    /**
     * Generate cognitive feedback from visualization interactions
     * Recursive pathway: User interactions influence cognitive focus
     * 
     * @param interaction_type Type of user interaction (click, hover, etc.)
     * @param interaction_target Target element of interaction
     * @param interaction_strength Strength/intensity of interaction
     * @return Feedback vector for cognitive system
     */
    std::vector<double> generate_visualization_feedback(
        const std::string& interaction_type,
        const std::string& interaction_target,
        double interaction_strength);

    /**
     * Set callback function for cognitive feedback
     * 
     * @param callback Function to call when generating feedback
     */
    void set_feedback_callback(std::function<void(const std::vector<double>&)> callback) {
        feedback_callback_ = callback;
    }

    /**
     * Export current visualization state as JSON
     * 
     * @return JSON string representation of current state
     */
    std::string export_visualization_state() const;

    /**
     * Import visualization state from JSON
     * 
     * @param json_state JSON string with visualization state
     * @return Success flag
     */
    bool import_visualization_state(const std::string& json_state);

    /**
     * Start real-time visualization mode
     * 
     * @param update_frequency_hz Frequency of visualization updates
     */
    void start_real_time_mode(double update_frequency_hz = 30.0);

    /**
     * Stop real-time visualization mode
     */
    void stop_real_time_mode();

    /**
     * Set visualization viewport (zoom and pan)
     * 
     * @param center_x X coordinate of viewport center
     * @param center_y Y coordinate of viewport center
     * @param zoom Zoom level (1.0 = normal)
     */
    void set_viewport(double center_x, double center_y, double zoom);

    /**
     * Get current cognitive salience mapping
     */
    std::map<std::string, double> get_cognitive_salience() const {
        return cognitive_salience_;
    }

    /**
     * Set attention threshold for adaptive overlays
     */
    void set_attention_threshold(double threshold) {
        attention_threshold_ = threshold;
    }

private:
    /**
     * Calculate node positions using force-directed layout
     */
    void calculate_node_positions(
        const std::map<std::string, std::vector<double>>& nodes,
        const std::map<std::string, std::vector<std::pair<std::string, double>>>& edges);

    /**
     * Update cognitive salience based on current state
     */
    void update_cognitive_salience(
        const std::map<std::string, std::vector<double>>& nodes,
        const std::map<std::string, std::vector<double>>& agents);

    /**
     * Apply user interaction feedback to cognitive system
     */
    void apply_interaction_feedback(const std::vector<double>& feedback);
};

} // namespace opencog

#endif // _OPENCOG_COGNITIVE_VISUALIZER_H