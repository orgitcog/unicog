/*
 * PerceptualInputProcessor.cc
 * 
 * Implementation of Phase II.1: Perceptual Input Layer
 * Recursive attention allocation and adaptive signal gating
 */

#include "PerceptualInputProcessor.h"
#include <algorithm>
#include <numeric>
#include <cmath>

namespace opencog {

PerceptualInputProcessor::PerceptualInputProcessor(double threshold, int max_depth)
    : gating_threshold_(threshold), max_recursion_depth_(max_depth)
{
    // Initialize with uniform attention weights
    attention_weights_.resize(10, 1.0);  // Default to 10 attention channels
}

PerceptualInputProcessor::~PerceptualInputProcessor()
{
    // Cleanup if needed
}

std::vector<double> PerceptualInputProcessor::process_recursive_input(
    const std::vector<double>& input_signals,
    const std::vector<double>& cognitive_context)
{
    std::vector<double> processed_signals;
    processed_signals.reserve(input_signals.size());

    // Ensure attention weights match input size
    if (attention_weights_.size() != input_signals.size()) {
        attention_weights_.resize(input_signals.size(), 1.0);
    }

    // Recursive processing with adaptive attention allocation
    for (size_t i = 0; i < input_signals.size(); ++i) {
        double context_weight = (i < cognitive_context.size()) ? 
                                cognitive_context[i] : 1.0;
        
        // Apply attention weight and context
        double attended_signal = input_signals[i] * attention_weights_[i] * context_weight;
        
        // Adaptive signal gating
        double gated_signal = adaptive_signal_gate(attended_signal, context_weight);
        
        processed_signals.push_back(gated_signal);
    }

    // Recursive feedback: normalize and update attention based on output
    double signal_sum = std::accumulate(processed_signals.begin(), 
                                       processed_signals.end(), 0.0);
    if (signal_sum > 0.0) {
        for (auto& signal : processed_signals) {
            signal /= signal_sum;  // Normalize
        }
    }

    return processed_signals;
}

double PerceptualInputProcessor::adaptive_signal_gate(double signal, double context_weight)
{
    // Adaptive threshold based on context weight
    double adaptive_threshold = gating_threshold_ * (2.0 - context_weight);
    
    if (std::abs(signal) < adaptive_threshold) {
        return 0.0;  // Gate out weak signals
    }

    // Scale signal above threshold with sigmoid-like function
    double scaled_signal = signal * (1.0 + std::tanh(context_weight - 0.5));
    
    return scaled_signal;
}

void PerceptualInputProcessor::update_attention_weights(
    const std::vector<double>& feedback_signals)
{
    // Recursive pathway: feedback influences future attention allocation
    if (feedback_signals.size() != attention_weights_.size()) {
        return;  // Size mismatch, skip update
    }

    // Learning rate for attention adaptation
    const double learning_rate = 0.1;

    for (size_t i = 0; i < attention_weights_.size(); ++i) {
        // Update attention weights based on feedback strength
        // Stronger feedback increases attention weight for that channel
        double feedback_strength = std::abs(feedback_signals[i]);
        attention_weights_[i] += learning_rate * feedback_strength;
        
        // Clamp to reasonable bounds
        attention_weights_[i] = std::max(0.1, std::min(attention_weights_[i], 2.0));
    }

    // Normalize attention weights to maintain balance
    double total_weight = std::accumulate(attention_weights_.begin(), 
                                         attention_weights_.end(), 0.0);
    if (total_weight > 0.0) {
        for (auto& weight : attention_weights_) {
            weight = (weight / total_weight) * attention_weights_.size();
        }
    }
}

} // namespace opencog