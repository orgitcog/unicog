/*
 * ggml-cpu.h - Minimal GGML CPU stub
 */

#ifndef GGML_CPU_H
#define GGML_CPU_H

#include "ggml.h"

#ifdef __cplusplus
extern "C" {
#endif

// CPU backend initialization (stub - does nothing)
static inline void ggml_cpu_init(void) {
    // Stub implementation
}

#ifdef __cplusplus
}
#endif

#endif // GGML_CPU_H