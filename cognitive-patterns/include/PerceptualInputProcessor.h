/*
 * PerceptualInputProcessor.h
 * 
 * Phase II.1: Perceptual Input Layer (Scheme/C++ Integration)
 * Recursive attention allocation and adaptive signal gating for input prioritization
 */

#ifndef _OPENCOG_PERCEPTUAL_INPUT_PROCESSOR_H
#define _OPENCOG_PERCEPTUAL_INPUT_PROCESSOR_H

#include <memory>
#include <vector>
#include <functional>

namespace opencog {

/**
 * Recursive Perceptual Input Processor
 * 
 * Implements adaptive signal gating for input prioritization with recursive
 * attention allocation. Provides Scheme/C++ integration for cognitive 
 * representation processing.
 */
class PerceptualInputProcessor
{
private:
    // Attention allocation weights (recursive feedback mechanism)
    std::vector<double> attention_weights_;
    
    // Signal gating threshold for input prioritization
    double gating_threshold_;
    
    // Recursive processing depth limit
    int max_recursion_depth_;

public:
    PerceptualInputProcessor(double threshold = 0.5, int max_depth = 10);
    ~PerceptualInputProcessor();

    /**
     * Process perceptual input with recursive attention allocation
     * 
     * @param input_signals Vector of input signal strengths
     * @param cognitive_context Current cognitive state context
     * @return Processed and prioritized signal vector
     */
    std::vector<double> process_recursive_input(
        const std::vector<double>& input_signals,
        const std::vector<double>& cognitive_context);

    /**
     * Adaptive signal gating based on cognitive salience
     * 
     * @param signal Input signal strength
     * @param context_weight Contextual importance weight
     * @return Gated signal (0.0 if below threshold, scaled if above)
     */
    double adaptive_signal_gate(double signal, double context_weight);

    /**
     * Update attention weights based on feedback
     * Recursive pathway: processed outputs influence future attention allocation
     * 
     * @param feedback_signals Feedback from processed cognitive objects
     */
    void update_attention_weights(const std::vector<double>& feedback_signals);

    /**
     * Get current attention allocation state
     */
    std::vector<double> get_attention_state() const { return attention_weights_; }

    /**
     * Set gating threshold for adaptive processing
     */
    void set_gating_threshold(double threshold) { gating_threshold_ = threshold; }
};

} // namespace opencog

#endif // _OPENCOG_PERCEPTUAL_INPUT_PROCESSOR_H