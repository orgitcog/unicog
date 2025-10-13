/**
 * AttentionValue.h
 *
 * Attention value representation for Economic Attention Networks (ECAN)
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ATTENTION_VALUE_H
#define _OPENCOG_ATTENTION_VALUE_H

#include <memory>
#include <array>
#include <algorithm>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{

/**
 * ECAN Attention Tensor Signature (6-dimensional)
 * As specified in Phase 2 requirements
 */
struct ECANAttentionTensor {
    double short_term_importance;   // [0.0, 1.0]
    double long_term_importance;    // [0.0, 1.0] 
    double urgency;                 // [0.0, 1.0]
    double confidence;              // [0.0, 1.0]
    double spreading_factor;        // [0.0, 1.0]
    double decay_rate;              // [0.0, 1.0]
    
    ECANAttentionTensor(double sti = 0.5, double lti = 0.5, double urg = 0.5,
                       double conf = 0.5, double spread = 0.5, double decay = 0.95)
        : short_term_importance(sti), long_term_importance(lti), urgency(urg),
          confidence(conf), spreading_factor(spread), decay_rate(decay) {}
          
    // Convert to vector for tensor operations
    std::array<double, 6> to_array() const {
        return {short_term_importance, long_term_importance, urgency, 
                confidence, spreading_factor, decay_rate};
    }
    
    // Load from vector
    void from_array(const std::array<double, 6>& arr) {
        short_term_importance = arr[0];
        long_term_importance = arr[1]; 
        urgency = arr[2];
        confidence = arr[3];
        spreading_factor = arr[4];
        decay_rate = arr[5];
    }
    
    // Normalize to [0.0, 1.0] range
    void normalize() {
        auto clamp = [](double& val) { val = std::max(0.0, std::min(1.0, val)); };
        clamp(short_term_importance);
        clamp(long_term_importance);
        clamp(urgency);
        clamp(confidence);
        clamp(spreading_factor);
        clamp(decay_rate);
    }
    
    // Economic value computation
    double computeEconomicValue() const {
        return (short_term_importance * 0.4 + 
                long_term_importance * 0.3 +
                urgency * 0.2 +
                confidence * 0.1);
    }
    
    // Spreading potential
    double computeSpreadingPotential() const {
        return spreading_factor * confidence * short_term_importance;
    }
};

/**
 * AttentionValue represents the attention parameters for an atom
 * in the Economic Attention Networks (ECAN) framework
 * Enhanced with ECAN tensor signature
 */
class AttentionValue
{
private:
    short sti; // Short Term Importance (legacy)
    short lti; // Long Term Importance (legacy)
    short vlti; // Very Long Term Importance (legacy)
    
    // ECAN Attention Tensor for advanced processing
    ECANAttentionTensor ecan_tensor;
    
public:
    // Default attention values
    static const short DEFAULTATOMSTI = 0;
    static const short DEFAULTATOMLTI = 0;
    static const short DEFAULTATOMVLTI = 0;

    // Constructors
    AttentionValue(short sti = DEFAULTATOMSTI, 
                   short lti = DEFAULTATOMLTI, 
                   short vlti = DEFAULTATOMVLTI);
    
    AttentionValue(const AttentionValue& av);
    
    // Constructor with ECAN tensor
    AttentionValue(const ECANAttentionTensor& tensor);
    
    // Destructor
    ~AttentionValue() = default;
    
    // Assignment operator
    AttentionValue& operator=(const AttentionValue& av);
    
    // Getters
    short getSTI() const { return sti; }
    short getLTI() const { return lti; }
    short getVLTI() const { return vlti; }
    
    // Setters
    void setSTI(short stiValue) { 
        sti = stiValue; 
        ecan_tensor.short_term_importance = stiValue / 100.0;
        ecan_tensor.normalize();
    }
    void setLTI(short ltiValue) { 
        lti = ltiValue;
        ecan_tensor.long_term_importance = ltiValue / 100.0;
        ecan_tensor.normalize();
    }
    void setVLTI(short vltiValue) { vlti = vltiValue; }
    
    // ECAN tensor accessors
    const ECANAttentionTensor& getECANTensor() const { return ecan_tensor; }
    void setECANTensor(const ECANAttentionTensor& tensor) {
        ecan_tensor = tensor;
        ecan_tensor.normalize();
        // Update legacy values
        sti = static_cast<short>(tensor.short_term_importance * 100);
        lti = static_cast<short>(tensor.long_term_importance * 100);
    }
    
    // Update specific tensor components
    void setUrgency(double urgency) { 
        ecan_tensor.urgency = std::max(0.0, std::min(1.0, urgency)); 
    }
    void setConfidence(double confidence) { 
        ecan_tensor.confidence = std::max(0.0, std::min(1.0, confidence)); 
    }
    void setSpreadingFactor(double factor) { 
        ecan_tensor.spreading_factor = std::max(0.0, std::min(1.0, factor)); 
    }
    void setDecayRate(double rate) { 
        ecan_tensor.decay_rate = std::max(0.0, std::min(1.0, rate)); 
    }
    
    // Attention value operations
    AttentionValue& operator+=(const AttentionValue& av);
    AttentionValue& operator-=(const AttentionValue& av);
    AttentionValue operator+(const AttentionValue& av) const;
    AttentionValue operator-(const AttentionValue& av) const;
    
    // Comparison operators
    bool operator==(const AttentionValue& av) const;
    bool operator!=(const AttentionValue& av) const;
    
    // Utility methods
    std::string toString() const;
    bool isZero() const;
    
    // ECAN-specific operations
    double computeEconomicValue() const {
        return ecan_tensor.computeEconomicValue();
    }
    
    double computeSpreadingPotential() const {
        return ecan_tensor.computeSpreadingPotential();
    }
    
    // Static factory methods
    static AttentionValue factory(short sti, short lti = DEFAULTATOMLTI, 
                                short vlti = DEFAULTATOMVLTI);
    static AttentionValue fromECANTensor(const ECANAttentionTensor& tensor);
};

} // namespace opencog

#endif // _OPENCOG_ATTENTION_VALUE_H