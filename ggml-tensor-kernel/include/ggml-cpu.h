/*
 * ggml-cpu.h - Minimal GGML CPU stub
 */

#ifndef GGML_CPU_H
#define GGML_CPU_H

#include "ggml.h"

#ifdef __cplusplus
extern "C" {
#endif

// CPU backend initialization - actual implementation
static inline void ggml_cpu_init(void) {
    // Initialize CPU-specific optimizations and features
    // This is a minimal implementation for stub mode
    // In a full implementation, this would:
    // - Detect CPU features (AVX, AVX2, AVX512, etc.)
    // - Set up thread pools
    // - Initialize SIMD instruction sets
    // - Configure memory alignment
    
    // For stub mode, we just ensure basic initialization
    // No-op but provides a hook for future CPU-specific setup
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        // Basic initialization complete
    }
}

#ifdef __cplusplus
}
#endif

#endif // GGML_CPU_H