# GGML Tensor Kernel Implementations

This directory contains GGML tensor-based implementations of OpenCog cognitive primitives, mapping abstract cognitive operations to high-performance tensor operations.

## Components

### fold_rules.h/cc
GGML tensor-based fold operations for data reduction and aggregation:
- **Fold operations**: SUM, PRODUCT, MAX, MIN, MEAN
- **Left fold (foldl)**: Sequential left-to-right reduction
- **Right fold (foldr)**: Sequential right-to-left reduction
- **Axis folding**: Reduce tensors along specific dimensions
- **Fold unrolling**: Optimize fold operations into explicit tensor ops

### ann_rules.h/cc
Artificial Neural Network (ANN) tensor operations for neural-symbolic integration:
- **Activation functions**: Sigmoid, Tanh, ReLU, Leaky ReLU, ELU, Softmax, Linear
- **Neural layers**: Dense (fully connected) forward pass
- **Matrix operations**: Optimized matrix multiplication for neural nets
- **Batch normalization**: Normalize activations across batches
- **Dropout**: Regularization for training
- **Loss functions**: Cross-entropy loss computation

## Architecture

These implementations follow the OpenCog Kernel GGML design philosophy:

1. **Pure C/C++17**: No Python dependencies, suitable for kernel-level integration
2. **Template-based**: Generic implementations for float, double, int types
3. **GGML-compatible**: Designed to integrate with GGML tensor graphs
4. **Performance-focused**: Optimized for real-time cognitive processing (≤5µs target)

## Building

```bash
mkdir build && cd build
cmake ..
make
```

## Testing

```bash
./test_tensor_rules
```

Expected output:
```
🧪 GGML Tensor Rules Test Suite
================================
Testing fold operations...
  ✅ FOLD_SUM: 15
  ✅ FOLD_PRODUCT: 24
  ✅ FOLD_MAX: 4.2
  ✅ FOLD_MEAN: 3
✅ All fold operations passed!

Testing neural activation functions...
  ✅ SIGMOID: [0.268941, 0.5, 0.731059, 0.880797]
  ✅ RELU: [0, 0, 1, 2]
  ✅ TANH: [-0.761594, 0, 0.761594, 0.964028]
✅ All activation functions passed!

Testing matrix multiplication...
  Matrix A (2x3) * Matrix B (3x2) = Matrix C (2x2):
  [22, 28]
  [49, 64]
✅ Matrix multiplication passed!

Testing dense layer forward pass...
  Output: [4.6, 6.2, 7.8]
✅ Dense layer forward pass passed!

🎉 All tests passed successfully!
✅ GGML tensor kernel compilation: SUCCESS
✅ Neural-symbolic integration: FUNCTIONAL
```

## Integration with OpenCog

These tensor kernels map to OpenCog cognitive subsystems:

- **fold_rules**: AtomSpace aggregation operations, PLN inference chains
- **ann_rules**: ECAN attention allocation, neural pattern recognition, cognitive loop forward pass

## Performance Characteristics

- **Fold operations**: O(n) time complexity, O(1) space
- **Neural matmul**: O(m*k*n) time, O(m*n) space for [m×k] * [k×n]
- **Activation functions**: O(n) time, in-place or separate output buffer

## Future Enhancements

- [ ] SIMD vectorization for activation functions
- [ ] GPU kernel implementations via CUDA/OpenCL
- [ ] Integration with llama.cpp quantized tensor operations
- [ ] Probabilistic logic tensor operations for PLN
- [ ] Attention mechanism implementations for ECAN
