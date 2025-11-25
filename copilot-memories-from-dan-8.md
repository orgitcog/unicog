# Copilot Memories

Hey Copilot it's me Dan. You tried to save these memories but tit failed so i saved them here for you :-)

---

Perfect! Let me now store some useful memories about this implementation:

Call to store_memory
Input
{
  "subject": "GGML tensor kernels",
  "fact": "GGML/Tensor directory contains tensor-based implementations of fold_rules and ann_rules for neural-symbolic integration",
  "citations": "GGML/Tensor/fold_rules.h, GGML/Tensor/ann_rules.h, GGML_TENSOR_IMPLEMENTATION_SUMMARY.md",
  "reason": "This memory will help future tasks related to tensor operations, neural-symbolic integration, and GGML kernel development. The GGML/Tensor directory is a critical component for cognitive primitive implementations and knowing its structure and purpose will be essential for any modifications or extensions to the tensor kernel system.",
  "category": "file_specific"
}
Failed to store memory with error: Resource not found

Call to store_memory
Input
{
  "subject": "memory safety patterns",
  "fact": "Tensor structs use Rule of Five: copy deleted, move enabled, destructors check null pointers",
  "citations": "GGML/Tensor/fold_rules.h:32-73, GGML/Tensor/ann_rules.h:46-116",
  "reason": "This memory captures the memory safety pattern used in tensor implementations. Future code working with RAII objects, especially those managing raw pointers, should follow this same pattern to avoid double-free errors and memory leaks. This is a codebase convention that should be maintained consistently.",
  "category": "general"
}
Failed to store memory with error: Resource not found

Call to store_memory
Input
{
  "subject": "testing practices",
  "fact": "Use test-ggml-tensor-kernels.sh for automated verification of tensor kernel implementations",
  "citations": "test-ggml-tensor-kernels.sh",
  "reason": "This memory documents the automated testing script for tensor kernels. Future modifications to the GGML/Tensor code should run this script to ensure no regressions. It's also a useful template for creating similar verification scripts for other components.",
  "category": "general"
}
Failed to store memory with error: Resource not found

The memory storage failed, but that's okay. Let me create a final summary of what was accomplished
