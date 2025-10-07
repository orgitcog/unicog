/*
 * ggml_stub.c - Minimal GGML stub implementation for compilation
 */

#include "../include/ggml.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Context structure (simplified)
struct ggml_context {
    void* mem_buffer;
    size_t mem_size;
    size_t mem_used;
};

// Context management
struct ggml_context* ggml_init(struct ggml_init_params params) {
    struct ggml_context* ctx = (struct ggml_context*)malloc(sizeof(struct ggml_context));
    if (!ctx) return NULL;
    
    if (params.mem_buffer) {
        ctx->mem_buffer = params.mem_buffer;
    } else {
        ctx->mem_buffer = malloc(params.mem_size);
    }
    ctx->mem_size = params.mem_size;
    ctx->mem_used = 0;
    
    return ctx;
}

void ggml_free(struct ggml_context* ctx) {
    if (ctx) {
        free(ctx->mem_buffer);
        free(ctx);
    }
}

// Helper to allocate tensor
static struct ggml_tensor* ggml_new_tensor_impl(struct ggml_context* ctx, enum ggml_type type, int n_dims, const int64_t* ne) {
    if (!ctx) return NULL;
    
    struct ggml_tensor* tensor = (struct ggml_tensor*)malloc(sizeof(struct ggml_tensor));
    if (!tensor) return NULL;
    
    tensor->type = type;
    tensor->n_dims = n_dims;
    
    size_t type_size = 4; // Assume F32 for simplicity
    size_t total_elements = 1;
    
    for (int i = 0; i < n_dims; i++) {
        tensor->ne[i] = ne[i];
        total_elements *= ne[i];
        tensor->nb[i] = (i == 0) ? type_size : tensor->nb[i-1] * tensor->ne[i-1];
    }
    for (int i = n_dims; i < 4; i++) {
        tensor->ne[i] = 1;
        tensor->nb[i] = tensor->nb[i-1] * tensor->ne[i-1];
    }
    
    size_t data_size = total_elements * type_size;
    tensor->data = malloc(data_size);
    if (!tensor->data) {
        free(tensor);
        return NULL;
    }
    
    memset(tensor->data, 0, data_size);
    memset(tensor->name, 0, sizeof(tensor->name));
    
    return tensor;
}

// Tensor creation functions
struct ggml_tensor* ggml_new_tensor_1d(struct ggml_context* ctx, enum ggml_type type, int64_t ne0) {
    int64_t ne[4] = {ne0, 1, 1, 1};
    return ggml_new_tensor_impl(ctx, type, 1, ne);
}

struct ggml_tensor* ggml_new_tensor_2d(struct ggml_context* ctx, enum ggml_type type, int64_t ne0, int64_t ne1) {
    int64_t ne[4] = {ne0, ne1, 1, 1};
    return ggml_new_tensor_impl(ctx, type, 2, ne);
}

struct ggml_tensor* ggml_new_tensor_3d(struct ggml_context* ctx, enum ggml_type type, int64_t ne0, int64_t ne1, int64_t ne2) {
    int64_t ne[4] = {ne0, ne1, ne2, 1};
    return ggml_new_tensor_impl(ctx, type, 3, ne);
}

struct ggml_tensor* ggml_new_tensor_4d(struct ggml_context* ctx, enum ggml_type type, int64_t ne0, int64_t ne1, int64_t ne2, int64_t ne3) {
    int64_t ne[4] = {ne0, ne1, ne2, ne3};
    return ggml_new_tensor_impl(ctx, type, 4, ne);
}

// Tensor operations
struct ggml_tensor* ggml_dup_tensor(struct ggml_context* ctx, const struct ggml_tensor* src) {
    if (!src) return NULL;
    struct ggml_tensor* dst = ggml_new_tensor_impl(ctx, src->type, src->n_dims, src->ne);
    if (dst && src->data && dst->data) {
        size_t data_size = ggml_nelements(src) * 4; // Assume F32
        memcpy(dst->data, src->data, data_size);
    }
    return dst;
}

struct ggml_tensor* ggml_set_zero(struct ggml_tensor* tensor) {
    if (tensor && tensor->data) {
        size_t data_size = ggml_nelements(tensor) * 4;
        memset(tensor->data, 0, data_size);
    }
    return tensor;
}

struct ggml_tensor* ggml_set_f32(struct ggml_tensor* tensor, float value) {
    if (tensor && tensor->data) {
        float* data = (float*)tensor->data;
        size_t n = ggml_nelements(tensor);
        for (size_t i = 0; i < n; i++) {
            data[i] = value;
        }
    }
    return tensor;
}

void ggml_set_name(struct ggml_tensor* tensor, const char* name) {
    if (tensor && name) {
        strncpy(tensor->name, name, sizeof(tensor->name) - 1);
        tensor->name[sizeof(tensor->name) - 1] = '\0';
    }
}

// Math operations (simplified stubs)
struct ggml_tensor* ggml_add(struct ggml_context* ctx, struct ggml_tensor* a, struct ggml_tensor* b) {
    return ggml_dup_tensor(ctx, a); // Stub: just return copy of a
}

struct ggml_tensor* ggml_mul(struct ggml_context* ctx, struct ggml_tensor* a, struct ggml_tensor* b) {
    return ggml_dup_tensor(ctx, a); // Stub: just return copy of a
}

struct ggml_tensor* ggml_mul_mat(struct ggml_context* ctx, struct ggml_tensor* a, struct ggml_tensor* b) {
    return ggml_dup_tensor(ctx, a); // Stub: just return copy of a
}

struct ggml_tensor* ggml_soft_max(struct ggml_context* ctx, struct ggml_tensor* a) {
    return ggml_dup_tensor(ctx, a); // Stub: just return copy of a
}

struct ggml_tensor* ggml_relu(struct ggml_context* ctx, struct ggml_tensor* a) {
    return ggml_dup_tensor(ctx, a); // Stub: just return copy of a
}

struct ggml_tensor* ggml_new_f32(struct ggml_context* ctx, float value) {
    struct ggml_tensor* tensor = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, 1);
    if (tensor && tensor->data) {
        *(float*)tensor->data = value;
    }
    return tensor;
}

// Accessor functions
float* ggml_get_data_f32(const struct ggml_tensor* tensor) {
    return tensor ? (float*)tensor->data : NULL;
}

int ggml_n_dims(const struct ggml_tensor* tensor) {
    return tensor ? tensor->n_dims : 0;
}

size_t ggml_nelements(const struct ggml_tensor* tensor) {
    if (!tensor) return 0;
    size_t n = 1;
    for (int i = 0; i < tensor->n_dims; i++) {
        n *= tensor->ne[i];
    }
    return n;
}