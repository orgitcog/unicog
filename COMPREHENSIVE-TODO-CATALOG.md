# Cognitive Flowchart: Comprehensive TODO Enumeration

**Problem Identification**
The OpenCog Unified codebase contains distributed TODOs, FIXMEs, stubs, and "not implemented" fragments that block full cognitive realization. These are detected by CI and halt verification (see job: https://github.com/OzCog/opencog-unified/actions/runs/16539657246/job/46779076096, ref: 25d11bfe332cd501a967d9ab3a6957a22504249f).

**Generated**: 2025-11-23 09:03:48 UTC  
**Total Items**: 2258  
**Commit Reference**: 25d11bfe332cd501a967d9ab3a6957a22504249f

---

## 1. Subsystem Mapping
- **Memory System**: Files containing placeholders in AtomSpace and knowledge systems.
- **Task System**: Aggregate and track unresolved items in cognitive server operations.
- **AI System**: Categorize TODOs by module, complexity, and dependency in neural-symbolic integration.
- **Autonomy System**: Recursively update the TODO issue as distributed cognition code evolves.

## 2. Pattern Recognition
All TODOs, FIXMEs, and stubs are indexed, referenced by file/line, and described with actionable context.
Any commit that triggers CI placeholder detection updates this master TODO issue.

## 3. Recursive Solution Design
This issue catalogs every known placeholder (file, line, surrounding context) organized by subsystem/module.
Each item includes a checkbox, summary, and code link ([ref: 25d11bfe332cd501a967d9ab3a6957a22504249f]).
Contributors may check off items when resolved and link to PRs/issues that address them.

## 4. Meta-Cognitive Enhancement
Instructions for maintainers to update this list as code evolves.
A section for emergent TODOs.

## 5. Theatrical Finale
"Let us converge upon a state of sublime implementation, where every TODO is transformed into a kernel of realized intelligence!"

---

## Outstanding Items

### AI System
*Total items: 27*

- [ ] **ggml-tensor-kernel/demo_real_functionality.cpp:10** (LOW, Feature Completion)
  - `#include "include/atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/demo_real_functionality.cpp#L10)

- [ ] **ggml-tensor-kernel/demo_real_functionality.cpp:277** (LOW, Feature Completion)
  - `std::cout << "🎉 No placeholders, no stubs - just functional code!\n\n";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/demo_real_functionality.cpp#L277)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:2** (LOW, Feature Completion)
  - `* atomspace_stub.h`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L2)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:4** (LOW, Feature Completion)
  - `* Minimal stub implementation of AtomSpace types for GGML tensor kernel`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L4)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:11** (LOW, Feature Completion)
  - `#ifndef _ATOMSPACE_STUB_H`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L11)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:12** (LOW, Feature Completion)
  - `#define _ATOMSPACE_STUB_H`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L12)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:40** (LOW, Feature Completion)
  - `class StubLogger {`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L40)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:46** (LOW, Feature Completion)
  - `StubLogger() : level_(LogLevel::INFO), prefix_("[STUB] ") {}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L46)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:48** (LOW, Feature Completion)
  - `void debug(const char* fmt, ...) { }  // Silent in stub mode`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L48)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:88** (LOW, Feature Completion)
  - `inline StubLogger& logger() {`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L88)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:89** (LOW, Feature Completion)
  - `static StubLogger stub_logger;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L89)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:90** (LOW, Feature Completion)
  - `return stub_logger;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L90)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:619** (LOW, Error Handling)
  - `logger().debug("SchemeEval: Expression not handled in stub mode, returning UNDEFINED");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L619)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:646** (LOW, Error Handling)
  - `logger().debug("SchemeEval: String expression not fully handled in stub mode");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L646)

- [ ] **ggml-tensor-kernel/include/atomspace_stub.h:653** (LOW, Feature Completion)
  - `#endif // _ATOMSPACE_STUB_H`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/atomspace_stub.h#L653)

- [ ] **ggml-tensor-kernel/include/ggml-cpu.h:2** (LOW, Feature Completion)
  - `* ggml-cpu.h - Minimal GGML CPU stub`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/ggml-cpu.h#L2)

- [ ] **ggml-tensor-kernel/include/ggml.h:2** (LOW, Feature Completion)
  - `* ggml.h - Minimal GGML stub for tensor kernel compilation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/ggml.h#L2)

- [ ] **ggml-tensor-kernel/include/ggml.h:4** (LOW, Feature Completion)
  - `* This is a minimal stub implementation to allow compilation without full GGML library`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/include/ggml.h#L4)

- [ ] **ggml-tensor-kernel/src/AtomSpaceTensorMapper_minimal.cc:18** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/AtomSpaceTensorMapper_minimal.cc#L18)

- [ ] **ggml-tensor-kernel/src/AttentionAllocator_minimal.cc:18** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/AttentionAllocator_minimal.cc#L18)

- [ ] **ggml-tensor-kernel/src/CognitivePrimitive.cc:12** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/CognitivePrimitive.cc#L12)

- [ ] **ggml-tensor-kernel/src/NeuralSymbolicBridge.cc:8** (LOW, Feature Completion)
  - `#include "../include/atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/NeuralSymbolicBridge.cc#L8)

- [ ] **ggml-tensor-kernel/src/TensorKernel_minimal.cc:18** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/TensorKernel_minimal.cc#L18)

- [ ] **ggml-tensor-kernel/src/ggml_stub.c:2** (LOW, Feature Completion)
  - `* ggml_stub.c - Minimal GGML stub implementation for compilation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/ggml_stub.c#L2)

- [ ] **ggml-tensor-kernel/test_real_functionality.cpp:9** (LOW, Testing)
  - `#include "include/atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/test_real_functionality.cpp#L9)

- [ ] **ggml-tensor-kernel/test_real_functionality.cpp:146** (LOW, Testing)
  - `std::cout << "✓ SchemeEval expression evaluation works (stub)" << std::endl;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/test_real_functionality.cpp#L146)

- [ ] **ggml-tensor-kernel/test_real_functionality.cpp:150** (LOW, Error Handling)
  - `std::cout << "✓ SchemeEval handle evaluation works (stub)" << std::endl;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/test_real_functionality.cpp#L150)

### Build System
*Total items: 379*

- [ ] **scripts/recursive_todo_resolver.py:239** (CRITICAL, Feature Completion)
  - `if todo.priority in ['CRITICAL', 'HIGH']:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L239)

- [ ] **scripts/recursive_todo_resolver.py:284** (CRITICAL, Feature Completion)
  - `if 'crash' in todo.content.lower() or 'fix' in todo.content.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L284)

- [ ] **scripts/recursive_todo_resolver.py:294** (CRITICAL, Feature Completion)
  - `if todo.priority in ['CRITICAL', 'HIGH']:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L294)

- [ ] **scripts/recursive_todo_resolver.py:342** (CRITICAL, Feature Completion)
  - `if todo.priority in ['CRITICAL', 'HIGH'] or diversity_bonus > 0 or len(selected) < 3:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L342)

- [ ] **scripts/recursive_todo_resolver.py:231** (HIGH, Thread Safety)
  - `if 'thread' in todo.content.lower() or 'sync' in todo.content.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L231)

- [ ] **scripts/recursive_todo_resolver.py:235** (HIGH, Performance)
  - `if 'performance' in todo.content.lower() or 'print' in todo.content.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L235)

- [ ] **scripts/recursive_todo_resolver.py:286** (HIGH, Thread Safety)
  - `if 'thread' in todo.content.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L286)

- [ ] **scripts/recursive_todo_resolver.py:288** (HIGH, Performance)
  - `if 'performance' in todo.content.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L288)

- [ ] **scripts/recursive_todo_resolver.py:306** (HIGH, Thread Safety)
  - `if todo.category in ['Thread Safety', 'Performance']:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L306)

- [ ] **scripts/recursive_todo_resolver.py:400** (HIGH, Performance)
  - `- For each resolved TODO, estimate its contribution to overall system stability, performance, or cog...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L400)

- [ ] **scripts/recursive_todo_resolver.py:758** (HIGH, Thread Safety)
  - `print(f"   ✅ TODO resolved: {todo_key} - Thread management updated")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L758)

- [ ] **scripts/recursive_todo_resolver.py:759** (HIGH, Thread Safety)
  - `self._mark_todo_completed(todo_key, "Thread management modernized")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L759)

- [ ] **scripts/test_todo_catalog.py:90** (MEDIUM, Testing)
  - `if checkbox_count < 700:  # Should have hundreds of TODOs (current catalog has ~823)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L90)

- [ ] **scripts/test_todo_catalog.py:96** (MEDIUM, Testing)
  - `if github_link_count < 700:  # Should have many GitHub links (similar to TODO count)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L96)

- [ ] **components/integration/opencog/scripts/get_python_lib.py:6** (LOW, Feature Completion)
  - `# This is a hack due to the distutils in debian/ubuntu's python3 being misconfigured`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/scripts/get_python_lib.py#L6)

- [ ] **scripts/generate_todo_catalog.py:3** (LOW, Feature Completion)
  - `Comprehensive TODO/FIXME Enumeration System for OpenCog Unified`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L3)

- [ ] **scripts/generate_todo_catalog.py:5** (LOW, Feature Completion)
  - `This script scans the entire repository for TODO, FIXME, and related placeholders,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L5)

- [ ] **scripts/generate_todo_catalog.py:19** (LOW, Feature Completion)
  - `class TODOEnumerator:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L19)

- [ ] **scripts/generate_todo_catalog.py:22** (LOW, Feature Completion)
  - `self.todos = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L22)

- [ ] **scripts/generate_todo_catalog.py:44** (LOW, Pattern Matching)
  - `# Pattern to match TODO/FIXME and similar items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L44)

- [ ] **scripts/generate_todo_catalog.py:45** (LOW, Pattern Matching)
  - `self.todo_patterns = [`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L45)

- [ ] **scripts/generate_todo_catalog.py:46** (LOW, Feature Completion)
  - `r'TODO\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L46)

- [ ] **scripts/generate_todo_catalog.py:47** (LOW, Feature Completion)
  - `r'FIXME\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L47)

- [ ] **scripts/generate_todo_catalog.py:48** (LOW, Feature Completion)
  - `r'XXX\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L48)

- [ ] **scripts/generate_todo_catalog.py:49** (LOW, Feature Completion)
  - `r'HACK\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L49)

- [ ] **scripts/generate_todo_catalog.py:50** (LOW, Feature Completion)
  - `r'@todo\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L50)

- [ ] **scripts/generate_todo_catalog.py:51** (LOW, Feature Completion)
  - `r'not implemented',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L51)

- [ ] **scripts/generate_todo_catalog.py:52** (LOW, Feature Completion)
  - `r'Not implemented',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L52)

- [ ] **scripts/generate_todo_catalog.py:53** (LOW, Feature Completion)
  - `r'NOT IMPLEMENTED',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L53)

- [ ] **scripts/generate_todo_catalog.py:54** (LOW, Feature Completion)
  - `r'STUB\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L54)

- [ ] **scripts/generate_todo_catalog.py:55** (LOW, Feature Completion)
  - `r'PLACEHOLDER\s*:?\s*(.*)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L55)

- [ ] **scripts/generate_todo_catalog.py:56** (LOW, Error Handling)
  - `r'throw.*IOException.*"Not implemented"',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L56)

- [ ] **scripts/generate_todo_catalog.py:57** (LOW, Feature Completion)
  - `r'OC_ASSERT\s*\(\s*false\s*,\s*"[^"]*not implemented[^"]*"',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L57)

- [ ] **scripts/generate_todo_catalog.py:58** (LOW, Feature Completion)
  - `r'Ensemble scoring not implemented',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L58)

- [ ] **scripts/generate_todo_catalog.py:65** (LOW, Feature Completion)
  - `"""Scan the entire repository for TODO items"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L65)

- [ ] **scripts/generate_todo_catalog.py:74** (LOW, Feature Completion)
  - `print(f"📊 Found {len(self.todos)} TODO/FIXME items")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L74)

- [ ] **scripts/generate_todo_catalog.py:75** (LOW, Feature Completion)
  - `return self.todos`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L75)

- [ ] **scripts/generate_todo_catalog.py:88** (LOW, Feature Completion)
  - `"""Scan a single file for TODO items"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L88)

- [ ] **scripts/generate_todo_catalog.py:95** (LOW, Feature Completion)
  - `if not line_clean or line_clean.startswith('//') and 'TODO' not in line_clean:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L95)

- [ ] **scripts/generate_todo_catalog.py:98** (LOW, Pattern Matching)
  - `for pattern in self.todo_patterns:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L98)

- [ ] **scripts/generate_todo_catalog.py:101** (LOW, Feature Completion)
  - `todo_item = {`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L101)

- [ ] **scripts/generate_todo_catalog.py:108** (LOW, Feature Completion)
  - `'category': self._categorize_todo(line, file_path)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L108)

- [ ] **scripts/generate_todo_catalog.py:110** (LOW, Feature Completion)
  - `self.todos.append(todo_item)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L110)

- [ ] **scripts/generate_todo_catalog.py:136** (LOW, Feature Completion)
  - `def _categorize_todo(self, line, file_path):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L136)

- [ ] **scripts/generate_todo_catalog.py:137** (LOW, Feature Completion)
  - `"""Categorize the type of TODO"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L137)

- [ ] **scripts/generate_todo_catalog.py:159** (LOW, Feature Completion)
  - `"""Generate the comprehensive TODO catalog"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L159)

- [ ] **scripts/generate_todo_catalog.py:161** (LOW, Feature Completion)
  - `# Group TODOs by subsystem`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L161)

- [ ] **scripts/generate_todo_catalog.py:163** (LOW, Feature Completion)
  - `for todo in self.todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L163)

- [ ] **scripts/generate_todo_catalog.py:164** (LOW, Feature Completion)
  - `by_subsystem[todo['subsystem']].append(todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L164)

- [ ] **scripts/generate_todo_catalog.py:177** (LOW, Feature Completion)
  - `content = f"""# Cognitive Flowchart: Comprehensive TODO Enumeration`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L177)

- [ ] **scripts/generate_todo_catalog.py:180** (LOW, Feature Completion)
  - `The OpenCog Unified codebase contains distributed TODOs, FIXMEs, stubs, and "not implemented" fragme...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L180)

- [ ] **scripts/generate_todo_catalog.py:183** (LOW, Feature Completion)
  - `**Total Items**: {len(self.todos)}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L183)

- [ ] **scripts/generate_todo_catalog.py:189** (LOW, Feature Completion)
  - `- **Memory System**: Files containing placeholders in AtomSpace and knowledge systems.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L189)

- [ ] **scripts/generate_todo_catalog.py:191** (LOW, Feature Completion)
  - `- **AI System**: Categorize TODOs by module, complexity, and dependency in neural-symbolic integrati...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L191)

- [ ] **scripts/generate_todo_catalog.py:192** (LOW, Feature Completion)
  - `- **Autonomy System**: Recursively update the TODO issue as distributed cognition code evolves.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L192)

- [ ] **scripts/generate_todo_catalog.py:195** (LOW, Feature Completion)
  - `All TODOs, FIXMEs, and stubs are indexed, referenced by file/line, and described with actionable con...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L195)

- [ ] **scripts/generate_todo_catalog.py:196** (LOW, Feature Completion)
  - `Any commit that triggers CI placeholder detection updates this master TODO issue.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L196)

- [ ] **scripts/generate_todo_catalog.py:199** (LOW, Feature Completion)
  - `This issue catalogs every known placeholder (file, line, surrounding context) organized by subsystem...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L199)

- [ ] **scripts/generate_todo_catalog.py:205** (LOW, Feature Completion)
  - `A section for emergent TODOs.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L205)

- [ ] **scripts/generate_todo_catalog.py:208** (LOW, Feature Completion)
  - `"Let us converge upon a state of sublime implementation, where every TODO is transformed into a kern...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L208)

- [ ] **scripts/generate_todo_catalog.py:218** (LOW, Feature Completion)
  - `todos_in_subsystem = by_subsystem[subsystem_name]`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L218)

- [ ] **scripts/generate_todo_catalog.py:220** (LOW, Feature Completion)
  - `content += f"*Total items: {len(todos_in_subsystem)}*\n\n"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L220)

- [ ] **scripts/generate_todo_catalog.py:223** (LOW, Feature Completion)
  - `todos_sorted = sorted(todos_in_subsystem,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L223)

- [ ] **scripts/generate_todo_catalog.py:226** (LOW, Feature Completion)
  - `for todo in todos_sorted:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L226)

- [ ] **scripts/generate_todo_catalog.py:228** (LOW, Feature Completion)
  - `github_link = f"https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a2250424...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L228)

- [ ] **scripts/generate_todo_catalog.py:230** (LOW, Feature Completion)
  - `content += f"- [ ] **{todo['file']}:{todo['line']}** ({todo['priority']}, {todo['category']})\n"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L230)

- [ ] **scripts/generate_todo_catalog.py:231** (LOW, Feature Completion)
  - `content += f"  - `{todo['content'][:100]}{'...' if len(todo['content']) > 100 else ''}`\n"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L231)

- [ ] **scripts/generate_todo_catalog.py:252** (LOW, Feature Completion)
  - `for subsystem_todos in by_subsystem.values():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L252)

- [ ] **scripts/generate_todo_catalog.py:253** (LOW, Feature Completion)
  - `for todo in subsystem_todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L253)

- [ ] **scripts/generate_todo_catalog.py:254** (LOW, Feature Completion)
  - `by_category[todo['category']] += 1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L254)

- [ ] **scripts/generate_todo_catalog.py:255** (LOW, Feature Completion)
  - `by_priority[todo['priority']] += 1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L255)

- [ ] **scripts/generate_todo_catalog.py:264** (LOW, Feature Completion)
  - `for subsystem, todos in sorted(by_subsystem.items()):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L264)

- [ ] **scripts/generate_todo_catalog.py:265** (LOW, Feature Completion)
  - `content += f"- **{subsystem}**: {len(todos)} items\n"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L265)

- [ ] **scripts/generate_todo_catalog.py:286** (LOW, Feature Completion)
  - `1. **Automatic Updates**: Run `python scripts/generate_todo_catalog.py` after significant code chang...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L286)

- [ ] **scripts/generate_todo_catalog.py:288** (LOW, Feature Completion)
  - `3. **New TODO Guidelines**: When adding new TODOs, include context and priority indicators`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L288)

- [ ] **scripts/generate_todo_catalog.py:293** (LOW, Testing)
  - `- **Resolving TODOs**: Create focused PRs that address specific TODO items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L293)

- [ ] **scripts/generate_todo_catalog.py:295** (LOW, Documentation)
  - `- **Documentation**: Include rationale when resolving or deferring TODO items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L295)

- [ ] **scripts/generate_todo_catalog.py:296** (LOW, Feature Completion)
  - `- **Testing**: Ensure adequate test coverage for TODO resolutions`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L296)

- [ ] **scripts/generate_todo_catalog.py:298** (LOW, Feature Completion)
  - `### Emergent TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L298)

- [ ] **scripts/generate_todo_catalog.py:301** (LOW, Feature Completion)
  - `- [ ] (Add emergent TODOs here as they are discovered)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L301)

- [ ] **scripts/generate_todo_catalog.py:312** (LOW, Feature Completion)
  - `**"Let us converge upon a state of sublime implementation, where every TODO is transformed into a ke...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L312)

- [ ] **scripts/generate_todo_catalog.py:314** (LOW, Feature Completion)
  - `In the grand symphony of cognitive architecture, each TODO represents not a mere task, but a note in...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L314)

- [ ] **scripts/generate_todo_catalog.py:320** (LOW, Feature Completion)
  - `**Vision**: Complete cognitive architecture with zero placeholders`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L320)

- [ ] **scripts/generate_todo_catalog.py:335** (LOW, Feature Completion)
  - `output_path = os.path.join(repo_path, 'COMPREHENSIVE-TODO-CATALOG.md')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L335)

- [ ] **scripts/generate_todo_catalog.py:337** (LOW, Feature Completion)
  - `enumerator = TODOEnumerator(repo_path)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L337)

- [ ] **scripts/generate_todo_catalog.py:341** (LOW, Feature Completion)
  - `print(f"✅ Comprehensive TODO catalog generated successfully!")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L341)

- [ ] **scripts/generate_todo_catalog.py:343** (LOW, Feature Completion)
  - `print(f"📊 Total items cataloged: {len(enumerator.todos)}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L343)

- [ ] **scripts/github_issue_creator.py:3** (LOW, Feature Completion)
  - `GitHub Issue Creator for Recursive TODO Resolution`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L3)

- [ ] **scripts/github_issue_creator.py:6** (LOW, Feature Completion)
  - `as part of the recursive TODO resolution system.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L6)

- [ ] **scripts/github_issue_creator.py:19** (LOW, Error Handling)
  - `"""Handles automatic GitHub issue creation for TODO batches"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L19)

- [ ] **scripts/github_issue_creator.py:27** (LOW, Feature Completion)
  - `def create_todo_batch_issue(self, batch_content: str, batch_number: int) -> Optional[Dict[Any, Any]]...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L27)

- [ ] **scripts/github_issue_creator.py:28** (LOW, Feature Completion)
  - `"""Create a GitHub issue for a TODO batch"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L28)

- [ ] **scripts/github_issue_creator.py:44** (LOW, Feature Completion)
  - `"todo-resolution",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L44)

- [ ] **scripts/github_issue_creator.py:74** (LOW, Feature Completion)
  - `def update_issue_with_completion(self, issue_number: int, todo_key: str, pr_link: str) -> bool:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L74)

- [ ] **scripts/github_issue_creator.py:75** (LOW, Feature Completion)
  - `"""Update an issue to mark a TODO as completed"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L75)

- [ ] **scripts/github_issue_creator.py:92** (LOW, Feature Completion)
  - `# Update the issue body to mark the TODO as completed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L92)

- [ ] **scripts/github_issue_creator.py:94** (LOW, Feature Completion)
  - `f"**[ ] {todo_key}",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L94)

- [ ] **scripts/github_issue_creator.py:95** (LOW, Feature Completion)
  - `f"**[x] {todo_key} ✅ [Resolved]({pr_link})"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L95)

- [ ] **scripts/github_issue_creator.py:103** (LOW, Feature Completion)
  - `print(f"✅ Updated issue #{issue_number} with completion of {todo_key}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L103)

- [ ] **scripts/github_issue_creator.py:110** (LOW, Feature Completion)
  - `def create_pr_stub(self, todo_item: str, implementation_guidance: str) -> Optional[str]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L110)

- [ ] **scripts/github_issue_creator.py:111** (LOW, Feature Completion)
  - `"""Create a PR stub/draft for a TODO item (future enhancement)"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L111)

- [ ] **scripts/github_issue_creator.py:114** (LOW, Feature Completion)
  - `file_part = todo_item.split(':')[0]`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L114)

- [ ] **scripts/github_issue_creator.py:116** (LOW, Feature Completion)
  - `return f"fix-todo-{safe_name}"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L116)

- [ ] **scripts/github_issue_creator.py:123** (LOW, Feature Completion)
  - `sample_content = """# Test TODO Resolution Batch`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L123)

- [ ] **scripts/github_issue_creator.py:129** (LOW, Feature Completion)
  - `- Sample TODO item for testing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L129)

- [ ] **scripts/github_issue_creator.py:134** (LOW, Feature Completion)
  - `issue = creator.create_todo_batch_issue(sample_content, 999)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/github_issue_creator.py#L134)

- [ ] **scripts/recursive_todo_resolver.py:3** (LOW, Feature Completion)
  - `Recursive TODO Resolution System for OpenCog Unified - Iteration 4 Enhancement`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L3)

- [ ] **scripts/recursive_todo_resolver.py:5** (LOW, Feature Completion)
  - `This script implements the enhanced cognitive flowchart for recursive TODO resolution:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L5)

- [ ] **scripts/recursive_todo_resolver.py:6** (LOW, Feature Completion)
  - `1. Catalog Extraction - Parse COMPREHENSIVE-TODO-CATALOG.md`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L6)

- [ ] **scripts/recursive_todo_resolver.py:7** (LOW, Feature Completion)
  - `2. Attention Allocation Kernel - Select next N highest-priority TODOs with cognitive synergy`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L7)

- [ ] **scripts/recursive_todo_resolver.py:43** (LOW, Feature Completion)
  - `class TODOItem:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L43)

- [ ] **scripts/recursive_todo_resolver.py:44** (LOW, Feature Completion)
  - `"""Represents a single TODO item from the catalog"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L44)

- [ ] **scripts/recursive_todo_resolver.py:56** (LOW, Feature Completion)
  - `class RecursiveTODOResolver:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L56)

- [ ] **scripts/recursive_todo_resolver.py:57** (LOW, Feature Completion)
  - `"""Orchestrates recursive attention-allocation through the TODO catalog"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L57)

- [ ] **scripts/recursive_todo_resolver.py:62** (LOW, Feature Completion)
  - `self.catalog_path = self.repo_path / "COMPREHENSIVE-TODO-CATALOG.md"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L62)

- [ ] **scripts/recursive_todo_resolver.py:63** (LOW, Feature Completion)
  - `self.progress_file = self.repo_path / "todo_resolution_progress.json"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L63)

- [ ] **scripts/recursive_todo_resolver.py:64** (LOW, Feature Completion)
  - `self.todos: List[TODOItem] = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L64)

- [ ] **scripts/recursive_todo_resolver.py:83** (LOW, Feature Completion)
  - `"completed_todos": [],`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L83)

- [ ] **scripts/recursive_todo_resolver.py:84** (LOW, Feature Completion)
  - `"in_progress_todos": [],`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L84)

- [ ] **scripts/recursive_todo_resolver.py:95** (LOW, Feature Completion)
  - `def extract_catalog(self) -> List[TODOItem]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L95)

- [ ] **scripts/recursive_todo_resolver.py:96** (LOW, Feature Completion)
  - `"""Parse COMPREHENSIVE-TODO-CATALOG.md to enumerate outstanding TODOs"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L96)

- [ ] **scripts/recursive_todo_resolver.py:98** (LOW, Error Handling)
  - `raise FileNotFoundError(f"TODO catalog not found: {self.catalog_path}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L98)

- [ ] **scripts/recursive_todo_resolver.py:100** (LOW, Feature Completion)
  - `print(f"📖 Extracting TODOs from catalog: {self.catalog_path}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L100)

- [ ] **scripts/recursive_todo_resolver.py:102** (LOW, Feature Completion)
  - `# Clear existing todos to avoid duplicates`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L102)

- [ ] **scripts/recursive_todo_resolver.py:103** (LOW, Feature Completion)
  - `self.todos = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L103)

- [ ] **scripts/recursive_todo_resolver.py:108** (LOW, Pattern Matching)
  - `# Extract TODO items using regex patterns`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L108)

- [ ] **scripts/recursive_todo_resolver.py:109** (LOW, Pattern Matching)
  - `todo_pattern = r'- \[ \] \*\*([^:]+):(\d+)\*\* \(([^,]+), ([^)]+)\)\n  - `([^`]+)`\n  - \[Code refer...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L109)

- [ ] **scripts/recursive_todo_resolver.py:111** (LOW, Pattern Matching)
  - `matches = re.findall(todo_pattern, content)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L111)

- [ ] **scripts/recursive_todo_resolver.py:119** (LOW, Feature Completion)
  - `# Check if this TODO is already tracked`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L119)

- [ ] **scripts/recursive_todo_resolver.py:120** (LOW, Feature Completion)
  - `todo_key = f"{file_path}:{line_num}"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L120)

- [ ] **scripts/recursive_todo_resolver.py:122** (LOW, Feature Completion)
  - `if todo_key in self.progress_data.get("completed_todos", []):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L122)

- [ ] **scripts/recursive_todo_resolver.py:124** (LOW, Feature Completion)
  - `elif todo_key in self.progress_data.get("in_progress_todos", []):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L124)

- [ ] **scripts/recursive_todo_resolver.py:127** (LOW, Feature Completion)
  - `todo_item = TODOItem(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L127)

- [ ] **scripts/recursive_todo_resolver.py:137** (LOW, Feature Completion)
  - `self.todos.append(todo_item)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L137)

- [ ] **scripts/recursive_todo_resolver.py:139** (LOW, Feature Completion)
  - `print(f"📊 Extracted {len(self.todos)} TODO items from catalog")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L139)

- [ ] **scripts/recursive_todo_resolver.py:140** (LOW, Feature Completion)
  - `return self.todos`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L140)

- [ ] **scripts/recursive_todo_resolver.py:166** (LOW, Feature Completion)
  - `def allocate_attention(self) -> List[TODOItem]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L166)

- [ ] **scripts/recursive_todo_resolver.py:169** (LOW, Feature Completion)
  - `# Filter unchecked TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L169)

- [ ] **scripts/recursive_todo_resolver.py:170** (LOW, Feature Completion)
  - `unchecked_todos = [todo for todo in self.todos if todo.status == "unchecked"]`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L170)

- [ ] **scripts/recursive_todo_resolver.py:172** (LOW, Feature Completion)
  - `if not unchecked_todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L172)

- [ ] **scripts/recursive_todo_resolver.py:173** (LOW, Feature Completion)
  - `print("🎉 All TODOs have been addressed!")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L173)

- [ ] **scripts/recursive_todo_resolver.py:179** (LOW, Feature Completion)
  - `# Group TODOs by cognitive synergy for maximum efficiency`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L179)

- [ ] **scripts/recursive_todo_resolver.py:180** (LOW, Feature Completion)
  - `synergy_groups = self._group_by_cognitive_synergy(unchecked_todos)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L180)

- [ ] **scripts/recursive_todo_resolver.py:183** (LOW, Feature Completion)
  - `attention_scores = self._compute_attention_scores(unchecked_todos)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L183)

- [ ] **scripts/recursive_todo_resolver.py:186** (LOW, Feature Completion)
  - `sorted_todos = sorted(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L186)

- [ ] **scripts/recursive_todo_resolver.py:187** (LOW, Feature Completion)
  - `unchecked_todos,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L187)

- [ ] **scripts/recursive_todo_resolver.py:199** (LOW, Performance)
  - `batch = self._optimize_batch_selection(sorted_todos[:self.batch_size * 2])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L199)

- [ ] **scripts/recursive_todo_resolver.py:201** (LOW, Feature Completion)
  - `print(f"🎯 Selected {len(batch)} TODOs for Iteration {self.progress_data['current_iteration']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L201)

- [ ] **scripts/recursive_todo_resolver.py:203** (LOW, Feature Completion)
  - `for todo in batch:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L203)

- [ ] **scripts/recursive_todo_resolver.py:204** (LOW, Feature Completion)
  - `synergy_score = synergy_groups.get(f"{todo.file}:{todo.line}", 0)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L204)

- [ ] **scripts/recursive_todo_resolver.py:205** (LOW, Feature Completion)
  - `attention_score = attention_scores.get(f"{todo.file}:{todo.line}", 0)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L205)

- [ ] **scripts/recursive_todo_resolver.py:206** (LOW, Feature Completion)
  - `print(f"   • {todo.file}:{todo.line} ({todo.priority}, {todo.category}) "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L206)

- [ ] **scripts/recursive_todo_resolver.py:211** (LOW, Feature Completion)
  - `def _group_by_cognitive_synergy(self, todos: List[TODOItem]) -> Dict[str, float]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L211)

- [ ] **scripts/recursive_todo_resolver.py:212** (LOW, Feature Completion)
  - `"""Group related TODOs for maximum cognitive synergy"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L212)

- [ ] **scripts/recursive_todo_resolver.py:218** (LOW, Feature Completion)
  - `for todo in todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L218)

- [ ] **scripts/recursive_todo_resolver.py:220** (LOW, Feature Completion)
  - `cluster_key = f"{todo.subsystem}:{todo.category}"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L220)

- [ ] **scripts/recursive_todo_resolver.py:221** (LOW, Feature Completion)
  - `synergy_clusters[cluster_key].append(todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L221)

- [ ] **scripts/recursive_todo_resolver.py:227** (LOW, Feature Completion)
  - `if any(keyword in todo.file.lower() for keyword in ['moses', 'atomspace', 'attention']):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L227)

- [ ] **scripts/recursive_todo_resolver.py:242** (LOW, Feature Completion)
  - `synergy_scores[f"{todo.file}:{todo.line}"] = synergy_score`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L242)

- [ ] **scripts/recursive_todo_resolver.py:245** (LOW, Feature Completion)
  - `for cluster_todos in synergy_clusters.values():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L245)

- [ ] **scripts/recursive_todo_resolver.py:246** (LOW, Feature Completion)
  - `if len(cluster_todos) > 1:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L246)

- [ ] **scripts/recursive_todo_resolver.py:247** (LOW, Feature Completion)
  - `cluster_boost = min(len(cluster_todos) * 0.3, 2.0)  # Cap at 2.0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L247)

- [ ] **scripts/recursive_todo_resolver.py:248** (LOW, Feature Completion)
  - `for todo in cluster_todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L248)

- [ ] **scripts/recursive_todo_resolver.py:249** (LOW, Feature Completion)
  - `key = f"{todo.file}:{todo.line}"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L249)

- [ ] **scripts/recursive_todo_resolver.py:254** (LOW, Feature Completion)
  - `def _compute_attention_scores(self, todos: List[TODOItem]) -> Dict[str, float]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L254)

- [ ] **scripts/recursive_todo_resolver.py:260** (LOW, Feature Completion)
  - `for todo in todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L260)

- [ ] **scripts/recursive_todo_resolver.py:266** (LOW, Feature Completion)
  - `sti = priority_sti.get(todo.priority, 1.0)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L266)

- [ ] **scripts/recursive_todo_resolver.py:271** (LOW, Feature Completion)
  - `if 'atomspace' in todo.file.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L271)

- [ ] **scripts/recursive_todo_resolver.py:273** (LOW, Feature Completion)
  - `elif 'moses' in todo.file.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L273)

- [ ] **scripts/recursive_todo_resolver.py:275** (LOW, Feature Completion)
  - `elif 'attention' in todo.file.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L275)

- [ ] **scripts/recursive_todo_resolver.py:277** (LOW, Feature Completion)
  - `elif 'cogserver' in todo.file.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L277)

- [ ] **scripts/recursive_todo_resolver.py:279** (LOW, Feature Completion)
  - `elif 'distributed' in todo.file.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L279)

- [ ] **scripts/recursive_todo_resolver.py:300** (LOW, Feature Completion)
  - `if any(keyword in todo.file.lower() for keyword in ['atomspace', 'moses', 'attention']):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L300)

- [ ] **scripts/recursive_todo_resolver.py:314** (LOW, Feature Completion)
  - `attention_scores[f"{todo.file}:{todo.line}"] = attention_score`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L314)

- [ ] **scripts/recursive_todo_resolver.py:318** (LOW, Performance)
  - `def _optimize_batch_selection(self, candidate_todos: List[TODOItem]) -> List[TODOItem]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L318)

- [ ] **scripts/recursive_todo_resolver.py:320** (LOW, Feature Completion)
  - `if len(candidate_todos) <= self.batch_size:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L320)

- [ ] **scripts/recursive_todo_resolver.py:321** (LOW, Feature Completion)
  - `return candidate_todos`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L321)

- [ ] **scripts/recursive_todo_resolver.py:328** (LOW, Feature Completion)
  - `for todo in candidate_todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L328)

- [ ] **scripts/recursive_todo_resolver.py:334** (LOW, Feature Completion)
  - `if todo.subsystem not in subsystems_covered:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L334)

- [ ] **scripts/recursive_todo_resolver.py:336** (LOW, Feature Completion)
  - `subsystems_covered.add(todo.subsystem)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L336)

- [ ] **scripts/recursive_todo_resolver.py:337** (LOW, Feature Completion)
  - `if todo.category not in categories_covered:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L337)

- [ ] **scripts/recursive_todo_resolver.py:339** (LOW, Feature Completion)
  - `categories_covered.add(todo.category)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L339)

- [ ] **scripts/recursive_todo_resolver.py:343** (LOW, Feature Completion)
  - `selected.append(todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L343)

- [ ] **scripts/recursive_todo_resolver.py:347** (LOW, Feature Completion)
  - `def generate_actionable_issues(self, batch: List[TODOItem]) -> str:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L347)

- [ ] **scripts/recursive_todo_resolver.py:352** (LOW, Feature Completion)
  - `# Count remaining high-priority TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L352)

- [ ] **scripts/recursive_todo_resolver.py:354** (LOW, Feature Completion)
  - `t for t in self.todos`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L354)

- [ ] **scripts/recursive_todo_resolver.py:358** (LOW, Feature Completion)
  - `issue_content = f"""# Iterative TODO Resolution – Batch {iteration}: Highest Priority Items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L358)

- [ ] **scripts/recursive_todo_resolver.py:361** (LOW, Feature Completion)
  - `This meta-issue orchestrates the systematic resolution of TODO/FIXME items from `COMPREHENSIVE-TODO-...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L361)

- [ ] **scripts/recursive_todo_resolver.py:364** (LOW, Feature Completion)
  - `1. **Catalog Extraction** ✅ - Parsed {len(self.todos)} TODOs from comprehensive catalog`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L364)

- [ ] **scripts/recursive_todo_resolver.py:372** (LOW, Feature Completion)
  - `## 🧩 Batch {iteration}: Highest Priority TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L372)

- [ ] **scripts/recursive_todo_resolver.py:376** (LOW, Feature Completion)
  - `for i, todo in enumerate(batch, 1):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L376)

- [ ] **scripts/recursive_todo_resolver.py:378** (LOW, Feature Completion)
  - `tensor_shape = self._estimate_tensor_shape(todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L378)

- [ ] **scripts/recursive_todo_resolver.py:379** (LOW, Feature Completion)
  - `implementation_guidance = self._generate_implementation_guidance(todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L379)

- [ ] **scripts/recursive_todo_resolver.py:380** (LOW, Feature Completion)
  - `test_guidance = self._generate_test_guidance(todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L380)

- [ ] **scripts/recursive_todo_resolver.py:382** (LOW, Feature Completion)
  - `issue_content += f"""**{i}. [ ] {todo.file}:{todo.line} ({todo.priority}, {todo.category})**`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L382)

- [ ] **scripts/recursive_todo_resolver.py:383** (LOW, Feature Completion)
  - `- `{todo.content}``
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L383)

- [ ] **scripts/recursive_todo_resolver.py:384** (LOW, Feature Completion)
  - `- [Code reference]({todo.github_link})`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L384)

- [ ] **scripts/recursive_todo_resolver.py:394** (LOW, Feature Completion)
  - `- Upon completion, check off resolved TODOs and invoke the next batch by rerunning this process.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L394)

- [ ] **scripts/recursive_todo_resolver.py:396** (LOW, Feature Completion)
  - `- Use `scripts/recursive_todo_resolver.py --mark-completed FILE:LINE PR_LINK` to track completions.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L396)

- [ ] **scripts/recursive_todo_resolver.py:401** (LOW, Feature Completion)
  - `- This systematic approach transforms distributed placeholders into kernels of realized intelligence...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L401)

- [ ] **scripts/recursive_todo_resolver.py:404** (LOW, Feature Completion)
  - `> "Let us converge upon a state of sublime implementation, where every TODO is transformed into a ke...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L404)

- [ ] **scripts/recursive_todo_resolver.py:406** (LOW, Feature Completion)
  - `Each resolved TODO represents not merely completed work, but a note in the composition of artificial...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L406)

- [ ] **scripts/recursive_todo_resolver.py:412** (LOW, Feature Completion)
  - `- **Remaining high-priority TODOs:** {unchecked_high_priority}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L412)

- [ ] **scripts/recursive_todo_resolver.py:413** (LOW, Feature Completion)
  - `- **Total TODOs processed:** {self.progress_data['total_resolved']}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L413)

- [ ] **scripts/recursive_todo_resolver.py:419** (LOW, Feature Completion)
  - `*Generated by Recursive TODO Resolution System - cognitive enhancement through systematic attention ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L419)

- [ ] **scripts/recursive_todo_resolver.py:421** (LOW, Feature Completion)
  - `*🧠 This issue implements the cognitive flowchart for recursive TODO resolution, orchestrating attent...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L421)

- [ ] **scripts/recursive_todo_resolver.py:426** (LOW, Feature Completion)
  - `def _estimate_tensor_shape(self, todo: TODOItem) -> str:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L426)

- [ ] **scripts/recursive_todo_resolver.py:428** (LOW, Feature Completion)
  - `content_lower = todo.content.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L428)

- [ ] **scripts/recursive_todo_resolver.py:429** (LOW, Feature Completion)
  - `file_lower = todo.file.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L429)

- [ ] **scripts/recursive_todo_resolver.py:513** (LOW, Feature Completion)
  - `if "not implemented" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L513)

- [ ] **scripts/recursive_todo_resolver.py:515** (LOW, Feature Completion)
  - `elif "todo" in content_lower and "algorithm" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L515)

- [ ] **scripts/recursive_todo_resolver.py:517** (LOW, Feature Completion)
  - `elif "fixme" in content_lower and "memory" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L517)

- [ ] **scripts/recursive_todo_resolver.py:522** (LOW, Feature Completion)
  - `def _generate_implementation_guidance(self, todo: TODOItem) -> str:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L522)

- [ ] **scripts/recursive_todo_resolver.py:524** (LOW, Feature Completion)
  - `content_lower = todo.content.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L524)

- [ ] **scripts/recursive_todo_resolver.py:525** (LOW, Feature Completion)
  - `file_lower = todo.file.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L525)

- [ ] **scripts/recursive_todo_resolver.py:593** (LOW, Feature Completion)
  - `elif "not implemented" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L593)

- [ ] **scripts/recursive_todo_resolver.py:597** (LOW, Pattern Matching)
  - `return "Replace placeholder with concrete implementation following existing architectural patterns"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L597)

- [ ] **scripts/recursive_todo_resolver.py:598** (LOW, Feature Completion)
  - `elif "hack" in content_lower or "fixme" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L598)

- [ ] **scripts/recursive_todo_resolver.py:600** (LOW, Feature Completion)
  - `elif "todo" in content_lower and "test" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L600)

- [ ] **scripts/recursive_todo_resolver.py:608** (LOW, Feature Completion)
  - `if todo.subsystem == "Memory System":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L608)

- [ ] **scripts/recursive_todo_resolver.py:610** (LOW, Feature Completion)
  - `elif todo.subsystem == "Task System":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L610)

- [ ] **scripts/recursive_todo_resolver.py:612** (LOW, Feature Completion)
  - `elif todo.subsystem == "AI System":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L612)

- [ ] **scripts/recursive_todo_resolver.py:614** (LOW, Feature Completion)
  - `elif todo.subsystem == "MOSES Representation/Scoring":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L614)

- [ ] **scripts/recursive_todo_resolver.py:619** (LOW, Feature Completion)
  - `def _generate_test_guidance(self, todo: TODOItem) -> str:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L619)

- [ ] **scripts/recursive_todo_resolver.py:621** (LOW, Feature Completion)
  - `content_lower = todo.content.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L621)

- [ ] **scripts/recursive_todo_resolver.py:622** (LOW, Feature Completion)
  - `file_lower = todo.file.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L622)

- [ ] **scripts/recursive_todo_resolver.py:691** (LOW, Feature Completion)
  - `elif "not implemented" in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L691)

- [ ] **scripts/recursive_todo_resolver.py:697** (LOW, Feature Completion)
  - `if todo.subsystem == "Memory System":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L697)

- [ ] **scripts/recursive_todo_resolver.py:699** (LOW, Feature Completion)
  - `elif todo.subsystem == "Task System":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L699)

- [ ] **scripts/recursive_todo_resolver.py:701** (LOW, Feature Completion)
  - `elif todo.subsystem == "AI System":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L701)

- [ ] **scripts/recursive_todo_resolver.py:703** (LOW, Feature Completion)
  - `elif todo.subsystem == "MOSES Representation/Scoring":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L703)

- [ ] **scripts/recursive_todo_resolver.py:708** (LOW, Feature Completion)
  - `def mark_batch_in_progress(self, batch: List[TODOItem]):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L708)

- [ ] **scripts/recursive_todo_resolver.py:710** (LOW, Feature Completion)
  - `for todo in batch:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L710)

- [ ] **scripts/recursive_todo_resolver.py:711** (LOW, Feature Completion)
  - `todo_key = f"{todo.file}:{todo.line}"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L711)

- [ ] **scripts/recursive_todo_resolver.py:712** (LOW, Feature Completion)
  - `if todo_key not in self.progress_data["in_progress_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L712)

- [ ] **scripts/recursive_todo_resolver.py:713** (LOW, Feature Completion)
  - `self.progress_data["in_progress_todos"].append(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L713)

- [ ] **scripts/recursive_todo_resolver.py:714** (LOW, Feature Completion)
  - `todo.status = "in-progress"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L714)

- [ ] **scripts/recursive_todo_resolver.py:715** (LOW, Feature Completion)
  - `todo.assigned_batch = self.progress_data['current_iteration']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L715)

- [ ] **scripts/recursive_todo_resolver.py:717** (LOW, Feature Completion)
  - `def validate_batch_6_todos(self):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L717)

- [ ] **scripts/recursive_todo_resolver.py:718** (LOW, Testing)
  - `"""Specifically validate and resolve the batch 6 TODOs mentioned in the issue"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L718)

- [ ] **scripts/recursive_todo_resolver.py:719** (LOW, Testing)
  - `print("🎯 Validating specific Batch 6 TODOs...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L719)

- [ ] **scripts/recursive_todo_resolver.py:721** (LOW, Feature Completion)
  - `batch_6_todos = [`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L721)

- [ ] **scripts/recursive_todo_resolver.py:726** (LOW, Feature Completion)
  - `"scripts/generate_todo_catalog.py:289"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L726)

- [ ] **scripts/recursive_todo_resolver.py:730** (LOW, Feature Completion)
  - `for todo_key in batch_6_todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L730)

- [ ] **scripts/recursive_todo_resolver.py:731** (LOW, Feature Completion)
  - `file_path, line_str = todo_key.split(':')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L731)

- [ ] **scripts/recursive_todo_resolver.py:747** (LOW, Testing)
  - `# Check if this specific TODO has been resolved`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L747)

- [ ] **scripts/recursive_todo_resolver.py:748** (LOW, Pattern Matching)
  - `if todo_key == "atomspace/opencog/query/PatternMatchEngine.cc:1504":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L748)

- [ ] **scripts/recursive_todo_resolver.py:749** (LOW, Feature Completion)
  - `if "Not implemented" not in line_content:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L749)

- [ ] **scripts/recursive_todo_resolver.py:750** (LOW, Error Handling)
  - `print(f"   ✅ TODO resolved: {todo_key} - 'Not implemented' exception removed")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L750)

- [ ] **scripts/recursive_todo_resolver.py:751** (LOW, Feature Completion)
  - `self._mark_todo_completed(todo_key, "Code implementation completed")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L751)

- [ ] **scripts/recursive_todo_resolver.py:754** (LOW, Feature Completion)
  - `print(f"   ⏳ TODO still pending: {todo_key}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L754)

- [ ] **scripts/recursive_todo_resolver.py:756** (LOW, Distributed Systems)
  - `elif todo_key == "cogserver/opencog/network/ServerSocket.cc:162":`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L756)

- [ ] **scripts/recursive_todo_resolver.py:762** (LOW, Feature Completion)
  - `print(f"   ⏳ TODO still pending: {todo_key}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L762)

- [ ] **scripts/recursive_todo_resolver.py:765** (LOW, Feature Completion)
  - `# For other TODOs, check if they still contain TODO/FIXME markers`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L765)

- [ ] **scripts/recursive_todo_resolver.py:767** (LOW, Feature Completion)
  - `if any(keyword in context.lower() for keyword in ['todo', 'fixme', 'xxx']):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L767)

- [ ] **scripts/recursive_todo_resolver.py:768** (LOW, Feature Completion)
  - `print(f"   ⏳ TODO still pending: {todo_key}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L768)

- [ ] **scripts/recursive_todo_resolver.py:770** (LOW, Feature Completion)
  - `print(f"   ❓ TODO status unclear: {todo_key}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L770)

- [ ] **scripts/recursive_todo_resolver.py:773** (LOW, Error Handling)
  - `print(f"   ⚠️  Error checking {todo_key}: {e}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L773)

- [ ] **scripts/recursive_todo_resolver.py:775** (LOW, Feature Completion)
  - `print(f"   📊 Batch 6 validation complete: {resolved_count} TODOs resolved")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L775)

- [ ] **scripts/recursive_todo_resolver.py:778** (LOW, Feature Completion)
  - `def _mark_todo_completed(self, todo_key: str, resolution_note: str):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L778)

- [ ] **scripts/recursive_todo_resolver.py:779** (LOW, Testing)
  - `"""Mark a specific TODO as completed with a resolution note"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L779)

- [ ] **scripts/recursive_todo_resolver.py:780** (LOW, Feature Completion)
  - `if todo_key not in self.progress_data["completed_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L780)

- [ ] **scripts/recursive_todo_resolver.py:781** (LOW, Feature Completion)
  - `self.progress_data["completed_todos"].append(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L781)

- [ ] **scripts/recursive_todo_resolver.py:782** (LOW, Feature Completion)
  - `if todo_key in self.progress_data["in_progress_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L782)

- [ ] **scripts/recursive_todo_resolver.py:783** (LOW, Feature Completion)
  - `self.progress_data["in_progress_todos"].remove(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L783)

- [ ] **scripts/recursive_todo_resolver.py:788** (LOW, Feature Completion)
  - `self.progress_data["resolutions"][todo_key] = {`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L788)

- [ ] **scripts/recursive_todo_resolver.py:791** (LOW, Feature Completion)
  - `"resolved_by": "recursive_todo_resolver_validation"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L791)

- [ ] **scripts/recursive_todo_resolver.py:794** (LOW, Feature Completion)
  - `def validate_existing_todos(self):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L794)

- [ ] **scripts/recursive_todo_resolver.py:795** (LOW, Feature Completion)
  - `"""Validate that TODOs marked as in-progress or unchecked still exist in the codebase"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L795)

- [ ] **scripts/recursive_todo_resolver.py:796** (LOW, Feature Completion)
  - `print("🔍 Validating existing TODO items against codebase...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L796)

- [ ] **scripts/recursive_todo_resolver.py:798** (LOW, Feature Completion)
  - `todos_to_mark_completed = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L798)

- [ ] **scripts/recursive_todo_resolver.py:800** (LOW, Feature Completion)
  - `for todo in self.todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L800)

- [ ] **scripts/recursive_todo_resolver.py:801** (LOW, Feature Completion)
  - `if todo.status in ["unchecked", "in-progress"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L801)

- [ ] **scripts/recursive_todo_resolver.py:802** (LOW, Feature Completion)
  - `file_path = self.repo_path / todo.file`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L802)

- [ ] **scripts/recursive_todo_resolver.py:804** (LOW, Feature Completion)
  - `print(f"   📁 File no longer exists: {todo.file}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L804)

- [ ] **scripts/recursive_todo_resolver.py:805** (LOW, Feature Completion)
  - `todos_to_mark_completed.append(f"{todo.file}:{todo.line}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L805)

- [ ] **scripts/recursive_todo_resolver.py:808** (LOW, Feature Completion)
  - `# Read the file and check if the TODO still exists at the expected line`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L808)

- [ ] **scripts/recursive_todo_resolver.py:813** (LOW, Feature Completion)
  - `# Check around the line number (±2 lines) for the TODO content`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L813)

- [ ] **scripts/recursive_todo_resolver.py:814** (LOW, Feature Completion)
  - `line_num = todo.line - 1  # Convert to 0-based indexing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L814)

- [ ] **scripts/recursive_todo_resolver.py:817** (LOW, Feature Completion)
  - `todo_found = False`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L817)

- [ ] **scripts/recursive_todo_resolver.py:819** (LOW, Feature Completion)
  - `if todo.content.strip() in lines[i] or any(keyword in lines[i].lower() for keyword in ['todo', 'fixm...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L819)

- [ ] **scripts/recursive_todo_resolver.py:820** (LOW, Feature Completion)
  - `todo_found = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L820)

- [ ] **scripts/recursive_todo_resolver.py:823** (LOW, Feature Completion)
  - `if not todo_found:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L823)

- [ ] **scripts/recursive_todo_resolver.py:824** (LOW, Feature Completion)
  - `print(f"   ✅ TODO resolved: {todo.file}:{todo.line}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L824)

- [ ] **scripts/recursive_todo_resolver.py:825** (LOW, Feature Completion)
  - `todos_to_mark_completed.append(f"{todo.file}:{todo.line}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L825)

- [ ] **scripts/recursive_todo_resolver.py:828** (LOW, Error Handling)
  - `print(f"   ⚠️  Error checking {todo.file}: {e}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L828)

- [ ] **scripts/recursive_todo_resolver.py:830** (LOW, Feature Completion)
  - `# Mark resolved TODOs as completed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L830)

- [ ] **scripts/recursive_todo_resolver.py:831** (LOW, Feature Completion)
  - `for todo_key in todos_to_mark_completed:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L831)

- [ ] **scripts/recursive_todo_resolver.py:832** (LOW, Feature Completion)
  - `if todo_key not in self.progress_data["completed_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L832)

- [ ] **scripts/recursive_todo_resolver.py:833** (LOW, Feature Completion)
  - `self.progress_data["completed_todos"].append(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L833)

- [ ] **scripts/recursive_todo_resolver.py:834** (LOW, Feature Completion)
  - `if todo_key in self.progress_data["in_progress_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L834)

- [ ] **scripts/recursive_todo_resolver.py:835** (LOW, Feature Completion)
  - `self.progress_data["in_progress_todos"].remove(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L835)

- [ ] **scripts/recursive_todo_resolver.py:837** (LOW, Feature Completion)
  - `if todos_to_mark_completed:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L837)

- [ ] **scripts/recursive_todo_resolver.py:838** (LOW, Feature Completion)
  - `print(f"   🎉 Marked {len(todos_to_mark_completed)} TODOs as completed automatically")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L838)

- [ ] **scripts/recursive_todo_resolver.py:842** (LOW, Feature Completion)
  - `"""Update the TODO catalog to reflect current progress"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L842)

- [ ] **scripts/recursive_todo_resolver.py:849** (LOW, Feature Completion)
  - `# Replace unchecked items with checked ones for completed TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L849)

- [ ] **scripts/recursive_todo_resolver.py:850** (LOW, Feature Completion)
  - `for todo_key in self.progress_data.get("completed_todos", []):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L850)

- [ ] **scripts/recursive_todo_resolver.py:852** (LOW, Feature Completion)
  - `file_part, line_part = todo_key.split(':')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L852)

- [ ] **scripts/recursive_todo_resolver.py:864** (LOW, Feature Completion)
  - `**TODOs Resolved:** {len(self.progress_data.get('completed_todos', []))}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L864)

- [ ] **scripts/recursive_todo_resolver.py:865** (LOW, Feature Completion)
  - `**TODOs In Progress:** {len(self.progress_data.get('in_progress_todos', []))}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L865)

- [ ] **scripts/recursive_todo_resolver.py:866** (LOW, Feature Completion)
  - `**Total Remaining:** {len([t for t in self.todos if t.status == 'unchecked'])}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L866)

- [ ] **scripts/recursive_todo_resolver.py:868** (LOW, Feature Completion)
  - `*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L868)

- [ ] **scripts/recursive_todo_resolver.py:884** (LOW, Feature Completion)
  - `"""Run a single iteration of the recursive TODO resolution"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L884)

- [ ] **scripts/recursive_todo_resolver.py:889** (LOW, Feature Completion)
  - `# Step 2: Validate existing TODOs to ensure they still exist in codebase`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L889)

- [ ] **scripts/recursive_todo_resolver.py:890** (LOW, Feature Completion)
  - `self.validate_existing_todos()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L890)

- [ ] **scripts/recursive_todo_resolver.py:896** (LOW, Feature Completion)
  - `print("🎉 No more TODOs to process!")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L896)

- [ ] **scripts/recursive_todo_resolver.py:908** (LOW, Feature Completion)
  - `created_issue = self.github_creator.create_todo_batch_issue(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L908)

- [ ] **scripts/recursive_todo_resolver.py:930** (LOW, Feature Completion)
  - `"""Mark a TODO as completed"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L930)

- [ ] **scripts/recursive_todo_resolver.py:931** (LOW, Feature Completion)
  - `todo_key = f"{file_path}:{line}"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L931)

- [ ] **scripts/recursive_todo_resolver.py:934** (LOW, Feature Completion)
  - `if todo_key in self.progress_data["in_progress_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L934)

- [ ] **scripts/recursive_todo_resolver.py:935** (LOW, Feature Completion)
  - `self.progress_data["in_progress_todos"].remove(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L935)

- [ ] **scripts/recursive_todo_resolver.py:937** (LOW, Feature Completion)
  - `if todo_key not in self.progress_data["completed_todos"]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L937)

- [ ] **scripts/recursive_todo_resolver.py:938** (LOW, Feature Completion)
  - `self.progress_data["completed_todos"].append(todo_key)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L938)

- [ ] **scripts/recursive_todo_resolver.py:941** (LOW, Feature Completion)
  - `# Update TODO object if it exists`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L941)

- [ ] **scripts/recursive_todo_resolver.py:942** (LOW, Feature Completion)
  - `for todo in self.todos:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L942)

- [ ] **scripts/recursive_todo_resolver.py:943** (LOW, Feature Completion)
  - `if f"{todo.file}:{todo.line}" == todo_key:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L943)

- [ ] **scripts/recursive_todo_resolver.py:944** (LOW, Feature Completion)
  - `todo.status = "completed"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L944)

- [ ] **scripts/recursive_todo_resolver.py:946** (LOW, Feature Completion)
  - `todo.resolution_pr = pr_link`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L946)

- [ ] **scripts/recursive_todo_resolver.py:951** (LOW, Feature Completion)
  - `# Find which batch this TODO was in and update the corresponding issue`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L951)

- [ ] **scripts/recursive_todo_resolver.py:954** (LOW, Feature Completion)
  - `issue_info['issue_number'], todo_key, pr_link`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L954)

- [ ] **scripts/recursive_todo_resolver.py:960** (LOW, Feature Completion)
  - `print(f"✅ Marked {todo_key} as completed")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L960)

- [ ] **scripts/recursive_todo_resolver.py:966** (LOW, Feature Completion)
  - `parser = argparse.ArgumentParser(description="Recursive TODO Resolution System")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L966)

- [ ] **scripts/recursive_todo_resolver.py:968** (LOW, Feature Completion)
  - `parser.add_argument("--batch-size", type=int, default=5, help="Number of TODOs per batch")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L968)

- [ ] **scripts/recursive_todo_resolver.py:971** (LOW, Feature Completion)
  - `help="Mark a TODO as completed")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L971)

- [ ] **scripts/recursive_todo_resolver.py:974** (LOW, Feature Completion)
  - `parser.add_argument("--validate-batch-6", action="store_true", help="Validate and resolve batch 6 TO...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L974)

- [ ] **scripts/recursive_todo_resolver.py:978** (LOW, Feature Completion)
  - `resolver = RecursiveTODOResolver(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L978)

- [ ] **scripts/recursive_todo_resolver.py:992** (LOW, Feature Completion)
  - `unchecked = len([t for t in resolver.todos if t.status == "unchecked"])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L992)

- [ ] **scripts/recursive_todo_resolver.py:993** (LOW, Feature Completion)
  - `in_progress = len([t for t in resolver.todos if t.status == "in-progress"])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L993)

- [ ] **scripts/recursive_todo_resolver.py:994** (LOW, Feature Completion)
  - `completed = len([t for t in resolver.todos if t.status == "completed"])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L994)

- [ ] **scripts/recursive_todo_resolver.py:996** (LOW, Feature Completion)
  - `print(f"📊 TODO Resolution Status:")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L996)

- [ ] **scripts/recursive_todo_resolver.py:1013** (LOW, Feature Completion)
  - `resolver.validate_batch_6_todos()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L1013)

- [ ] **scripts/recursive_todo_resolver.py:1018** (LOW, Feature Completion)
  - `print("🧠 Running next iteration of recursive TODO resolution...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L1018)

- [ ] **scripts/recursive_todo_resolver.py:1023** (LOW, Feature Completion)
  - `issue_file = resolver.repo_path / f"TODO_BATCH_{resolver.progress_data['current_iteration']-1}_ISSUE...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L1023)

- [ ] **scripts/recursive_todo_resolver.py:1031** (LOW, Feature Completion)
  - `print("🎉 All TODOs have been processed!")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/recursive_todo_resolver.py#L1031)

- [ ] **scripts/test_recursive_todo_resolution.py:3** (LOW, Testing)
  - `Test script for the Recursive TODO Resolution System`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L3)

- [ ] **scripts/test_recursive_todo_resolution.py:33** (LOW, Testing)
  - `def test_recursive_todo_resolution():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L33)

- [ ] **scripts/test_recursive_todo_resolution.py:34** (LOW, Testing)
  - `"""Test the complete recursive TODO resolution workflow"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L34)

- [ ] **scripts/test_recursive_todo_resolution.py:36** (LOW, Testing)
  - `print("🎯 Testing Recursive TODO Resolution System")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L36)

- [ ] **scripts/test_recursive_todo_resolution.py:41** (LOW, Testing)
  - `output = run_command("python scripts/recursive_todo_resolver.py --status", "Get current status")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L41)

- [ ] **scripts/test_recursive_todo_resolution.py:49** (LOW, Testing)
  - `print(f"   📊 Found {unchecked_count} unchecked TODOs")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L49)

- [ ] **scripts/test_recursive_todo_resolution.py:53** (LOW, Testing)
  - `output = run_command("python scripts/recursive_todo_resolver.py --next-batch", "Generate next batch"...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L53)

- [ ] **scripts/test_recursive_todo_resolution.py:58** (LOW, Testing)
  - `issue_files = list(Path(".").glob("TODO_BATCH_*_ISSUE.md"))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L58)

- [ ] **scripts/test_recursive_todo_resolution.py:71** (LOW, Testing)
  - `"# Iterative TODO Resolution – Batch",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L71)

- [ ] **scripts/test_recursive_todo_resolution.py:92** (LOW, Testing)
  - `if not Path("todo_resolution_progress.json").exists():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L92)

- [ ] **scripts/test_recursive_todo_resolution.py:96** (LOW, Testing)
  - `with open("todo_resolution_progress.json", 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L96)

- [ ] **scripts/test_recursive_todo_resolution.py:99** (LOW, Testing)
  - `required_keys = ["current_iteration", "completed_todos", "in_progress_todos", "last_run", "total_res...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L99)

- [ ] **scripts/test_recursive_todo_resolution.py:106** (LOW, Testing)
  - `print(f"   📊 Progress tracking: {len(progress['in_progress_todos'])} in progress")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L106)

- [ ] **scripts/test_recursive_todo_resolution.py:111** (LOW, Testing)
  - `# Use a known TODO from catalog that exists and hasn't been completed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L111)

- [ ] **scripts/test_recursive_todo_resolution.py:112** (LOW, Testing)
  - `test_todo = "moses/moses/comboreduct/table/table.h:1069"  # Changed to avoid already completed one`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L112)

- [ ] **scripts/test_recursive_todo_resolution.py:113** (LOW, Testing)
  - `print(f"   🎯 Testing completion of: {test_todo}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L113)

- [ ] **scripts/test_recursive_todo_resolution.py:116** (LOW, Testing)
  - `cmd = f"python scripts/recursive_todo_resolver.py --mark-completed \"{test_todo}\" \"https://github....`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L116)

- [ ] **scripts/test_recursive_todo_resolution.py:117** (LOW, Testing)
  - `output = run_command(cmd, "Mark TODO as completed")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L117)

- [ ] **scripts/test_recursive_todo_resolution.py:123** (LOW, Testing)
  - `output = run_command("python scripts/recursive_todo_resolver.py --status", "Check status after compl...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L123)

- [ ] **scripts/test_recursive_todo_resolution.py:126** (LOW, Testing)
  - `print("❌ TODO was not marked as completed")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L126)

- [ ] **scripts/test_recursive_todo_resolution.py:133** (LOW, Testing)
  - `output = run_command("./scripts/automate_todo_resolution.sh status", "Test automation script")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L133)

- [ ] **scripts/test_recursive_todo_resolution.py:134** (LOW, Testing)
  - `if not output or "TODO Resolution Status" not in output:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L134)

- [ ] **scripts/test_recursive_todo_resolution.py:142** (LOW, Testing)
  - `with open("COMPREHENSIVE-TODO-CATALOG.md", 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L142)

- [ ] **scripts/test_recursive_todo_resolution.py:151** (LOW, Testing)
  - `print("\n🎉 All tests passed! Recursive TODO Resolution System is working correctly.")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L151)

- [ ] **scripts/test_recursive_todo_resolution.py:167** (LOW, Testing)
  - `if not Path("COMPREHENSIVE-TODO-CATALOG.md").exists():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L167)

- [ ] **scripts/test_recursive_todo_resolution.py:171** (LOW, Testing)
  - `success = test_recursive_todo_resolution()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_recursive_todo_resolution.py#L171)

- [ ] **scripts/test_todo_catalog.py:3** (LOW, Testing)
  - `Test script to validate that the comprehensive TODO catalog includes all required items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L3)

- [ ] **scripts/test_todo_catalog.py:13** (LOW, Testing)
  - `catalog_path = 'COMPREHENSIVE-TODO-CATALOG.md'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L13)

- [ ] **scripts/test_todo_catalog.py:15** (LOW, Testing)
  - `print("❌ COMPREHENSIVE-TODO-CATALOG.md not found")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L15)

- [ ] **scripts/test_todo_catalog.py:29** (LOW, Testing)
  - `# Specific TODO items that must exist`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L29)

- [ ] **scripts/test_todo_catalog.py:30** (LOW, Pattern Matching)
  - `required_todo_patterns = [`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L30)

- [ ] **scripts/test_todo_catalog.py:31** (LOW, Testing)
  - `'atomspace-storage/opencog/persist/api/BackingStore.h',  # BackingStore TODO items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L31)

- [ ] **scripts/test_todo_catalog.py:32** (LOW, Testing)
  - `'moses/scoring/bscores.h',  # MOSES scoring TODO items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L32)

- [ ] **scripts/test_todo_catalog.py:40** (LOW, Pattern Matching)
  - `# Check for specific TODO patterns`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L40)

- [ ] **scripts/test_todo_catalog.py:41** (LOW, Pattern Matching)
  - `missing_todo_patterns = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L41)

- [ ] **scripts/test_todo_catalog.py:42** (LOW, Pattern Matching)
  - `for pattern in required_todo_patterns:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L42)

- [ ] **scripts/test_todo_catalog.py:44** (LOW, Pattern Matching)
  - `missing_todo_patterns.append(pattern)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L44)

- [ ] **scripts/test_todo_catalog.py:50** (LOW, Pattern Matching)
  - `if missing_todo_patterns:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L50)

- [ ] **scripts/test_todo_catalog.py:51** (LOW, Pattern Matching)
  - `print(f"❌ Missing required TODO patterns: {missing_todo_patterns}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L51)

- [ ] **scripts/test_todo_catalog.py:56** (LOW, Testing)
  - `'# Cognitive Flowchart: Comprehensive TODO Enumeration',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L56)

- [ ] **scripts/test_todo_catalog.py:84** (LOW, Testing)
  - `catalog_path = 'COMPREHENSIVE-TODO-CATALOG.md'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L84)

- [ ] **scripts/test_todo_catalog.py:91** (LOW, Testing)
  - `print(f"❌ Too few TODO items found: {checkbox_count}, expected at least 700")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L91)

- [ ] **scripts/test_todo_catalog.py:100** (LOW, Testing)
  - `print(f"✅ Found {checkbox_count} TODO items with {github_link_count} GitHub links")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L100)

- [ ] **scripts/test_todo_catalog.py:105** (LOW, Testing)
  - `print("🧪 Testing comprehensive TODO catalog...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L105)

- [ ] **scripts/test_todo_catalog.py:118** (LOW, Testing)
  - `print("🎉 All tests passed! The comprehensive TODO catalog is properly generated.")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L118)

### Core Utilities
*Total items: 19*

- [ ] **cogutil/opencog/util/backtrace-symbols.c:15** (HIGH, Thread Safety)
  - `A hacky replacement for backtrace_symbols in glibc`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/backtrace-symbols.c#L15)

- [ ] **cogutil/cmake/FindHyperTable.cmake:54** (LOW, Feature Completion)
  - `# XXX Unclear -- do we need to find *all* of these?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/FindHyperTable.cmake#L54)

- [ ] **cogutil/cmake/FindProtobuf.cmake:188** (LOW, Feature Completion)
  - `# TODO clean the PROTOROOT so that it does not form a regex itself?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/FindProtobuf.cmake#L188)

- [ ] **cogutil/cmake/FindTBB.cmake:77** (LOW, Feature Completion)
  - `# Todo: add other Windows compilers such as ICL.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/FindTBB.cmake#L77)

- [ ] **cogutil/cmake/OpenCogFindPython.cmake:55** (LOW, Feature Completion)
  - `# This is a hack due to the distutils in debian/ubuntu's`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/OpenCogFindPython.cmake#L55)

- [ ] **cogutil/cmake/OpenCogGccOptions.cmake:33** (LOW, Feature Completion)
  - `# XXX disable for now ... its just to painful, in daily life.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/OpenCogGccOptions.cmake#L33)

- [ ] **cogutil/cmake/OpenCogGccOptions.cmake:85** (LOW, Feature Completion)
  - `# So disable, by default; MOSES users will need to hack this.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/OpenCogGccOptions.cmake#L85)

- [ ] **cogutil/cmake/OpenCogLibOptions.cmake:10** (LOW, Error Handling)
  - `# Small hack to handle unixes that use "/usr/lib64" instead of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/OpenCogLibOptions.cmake#L10)

- [ ] **cogutil/cmake/UseOCaml.cmake:91** (LOW, Feature Completion)
  - `# TODO : see if it is possible to call the dependency generator at compile time`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/cmake/UseOCaml.cmake#L91)

- [ ] **cogutil/opencog/util/Config.h:108** (LOW, Feature Completion)
  - `*      it is a temporary dirty hack@n`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/Config.h#L108)

- [ ] **cogutil/opencog/util/getopt_long.c:110** (LOW, Feature Completion)
  - `/* XXX: GNU ignores PC if *options == '-' */`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L110)

- [ ] **cogutil/opencog/util/getopt_long.c:127** (LOW, Feature Completion)
  - `/* XXX: set optreset to 1 rather than these two */`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L127)

- [ ] **cogutil/opencog/util/getopt_long.c:222** (LOW, Thread Safety)
  - `* XXX Some programs (like rsyncd) expect to be able to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L222)

- [ ] **cogutil/opencog/util/getopt_long.c:223** (LOW, Feature Completion)
  - `* XXX re-initialize optind to 0 and have getopt_long(3)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L223)

- [ ] **cogutil/opencog/util/getopt_long.c:224** (LOW, Feature Completion)
  - `* XXX properly function again.  Work around this braindamage.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L224)

- [ ] **cogutil/opencog/util/getopt_long.c:306** (LOW, Feature Completion)
  - `/* XXX: what if no long options provided (called by getopt)? */`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L306)

- [ ] **cogutil/opencog/util/getopt_long.c:335** (LOW, Feature Completion)
  - `/* XXX: disable test for :: if PC? (GNU doesn't) */`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L335)

- [ ] **cogutil/opencog/util/getopt_long.c:483** (LOW, Feature Completion)
  - `* XXX: GNU sets optopt to val regardless of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L483)

- [ ] **cogutil/opencog/util/getopt_long.c:518** (LOW, Feature Completion)
  - `* XXX: GNU sets optopt to val regardless`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/getopt_long.c#L518)

### MOSES Representation/Scoring
*Total items: 267*

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1301** (HIGH, Performance)
  - `// XXX TODO remove this print, for better performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1301)

- [ ] **components/learning/moses/moses/feature-selection/algo/simple.h:118** (HIGH, Performance)
  - `// performance... TODO try this, if this is actually a bottleneck.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/feature-selection/algo/simple.h#L118)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:494** (HIGH, Performance)
  - `* XXX/TODO: the performance of this thing can be strongly improved`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L494)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:316** (HIGH, Performance)
  - `/// TODO: measure and compare the resulting performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L316)

- [ ] **components/learning/moses/moses/moses/scoring/bscores.h:589** (HIGH, Thread Safety)
  - `mutable KLDS<contin_t> _klds; /// @todo dangerous: not thread safe!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/bscores.h#L589)

- [ ] **moses/moses/comboreduct/table/table.h:1358** (HIGH, Performance)
  - `// XXX TODO remove this print, for better performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1358)

- [ ] **moses/moses/feature-selection/algo/simple.h:118** (HIGH, Performance)
  - `// performance... TODO try this, if this is actually a bottleneck.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/feature-selection/algo/simple.h#L118)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:477** (HIGH, Performance)
  - `* XXX/TODO: the performance of this thing can be strongly improved`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L477)

- [ ] **moses/moses/moses/representation/build_knobs.cc:317** (HIGH, Performance)
  - `/// TODO: measure and compare the resulting performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L317)

- [ ] **moses/moses/moses/scoring/bscores.h:592** (HIGH, Thread Safety)
  - `mutable KLDS<contin_t> _klds; /// @todo dangerous: not thread safe!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/bscores.h#L592)

- [ ] **components/learning/moses/moses/comboreduct/interpreter/eval.cc:530** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/interpreter/eval.cc#L530)

- [ ] **components/learning/moses/moses/comboreduct/interpreter/interpreter.cc:336** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/interpreter/interpreter.cc#L336)

- [ ] **components/learning/moses/moses/comboreduct/main/eval-table.cc:279** (MEDIUM, Feature Completion)
  - `"TODO could be detected automatically.\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/main/eval-table.cc#L279)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:795** (MEDIUM, Feature Completion)
  - `* TODO: we really should use iterators here, not column numbers.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L795)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:797** (MEDIUM, Feature Completion)
  - `* TODO: should be generalized for multi_type_seq rather than`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L797)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1082** (MEDIUM, Feature Completion)
  - `* XXX TODO -- this also should probably support the weight column,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1082)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1142** (MEDIUM, Feature Completion)
  - `* correct, we really should use Fisher information. @todo this).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1142)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:955** (MEDIUM, Performance)
  - `// TODO could be simplified, optimized, etc`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L955)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:1078** (MEDIUM, Performance)
  - `// TODO: this could definitely be optimized`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L1078)

- [ ] **components/learning/moses/moses/moses/main/problem-params.h:46** (MEDIUM, Feature Completion)
  - `// XXX FIXME TODO The structure below should be split into multiple`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/problem-params.h#L46)

- [ ] **components/learning/moses/moses/moses/moses/complexity.cc:52** (MEDIUM, Feature Completion)
  - `// we should count logical and, logical_or, below ..!?!? TODO, clarify.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/complexity.cc#L52)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.cc:566** (MEDIUM, Feature Completion)
  - `print_stats_header(NULL, false /* XXX stats for diversity, should be fixed */);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L566)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:249** (MEDIUM, Feature Completion)
  - `* XXX TODO: the current algo could be speeded up a fair bit, cutting`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L249)

- [ ] **components/learning/moses/moses/moses/moses/types.h:210** (MEDIUM, Feature Completion)
  - `// TODO this should be a std::valarray not std::vector but I am too`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/types.h#L210)

- [ ] **components/learning/moses/moses/moses/optimization/star-anneal.cc:42** (MEDIUM, Feature Completion)
  - `// XXX TODO the annealing temperature control code should be ported over`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/star-anneal.cc#L42)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:489** (MEDIUM, Feature Completion)
  - `// TODO: should bias the selection of these, so that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L489)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:557** (MEDIUM, Feature Completion)
  - `// We should probably OC_ASSERT here ... TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L557)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:1181** (MEDIUM, Feature Completion)
  - `//TODO: should bias the selection of these (and possibly choose larger subtrees)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L1181)

- [ ] **components/learning/moses/moses/moses/scoring/bscores.cc:930** (MEDIUM, Feature Completion)
  - `/// XXX this should probably be removed! TODO FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/bscores.cc#L930)

- [ ] **components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:486** (MEDIUM, Feature Completion)
  - `// XXX TODO -- should not return the penalties as part of the bscore,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/discriminating_bscore.cc#L486)

- [ ] **components/learning/moses/moses/moses/scoring/precision_bscore.h:102** (MEDIUM, Feature Completion)
  - `* XXX This class should be reworked to derive from`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/precision_bscore.h#L102)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:91** (MEDIUM, Feature Completion)
  - `* ensemble.  XXX this is probably wrong, we should probably do something`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L91)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.h:124** (MEDIUM, Feature Completion)
  - `// XXX TODO should be a std::valarray not a vector.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.h#L124)

- [ ] **moses/moses/comboreduct/interpreter/eval.cc:563** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/interpreter/eval.cc#L563)

- [ ] **moses/moses/comboreduct/interpreter/interpreter.cc:336** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/interpreter/interpreter.cc#L336)

- [ ] **moses/moses/comboreduct/main/eval-table.cc:284** (MEDIUM, Feature Completion)
  - `"TODO could be detected automatically.\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/main/eval-table.cc#L284)

- [ ] **moses/moses/comboreduct/table/table.h:832** (MEDIUM, Feature Completion)
  - `* TODO: we really should use iterators here, not column numbers.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L832)

- [ ] **moses/moses/comboreduct/table/table.h:834** (MEDIUM, Feature Completion)
  - `* TODO: should be generalized for multi_type_seq rather than`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L834)

- [ ] **moses/moses/comboreduct/table/table.h:1119** (MEDIUM, Feature Completion)
  - `* XXX TODO -- this also should probably support the weight column,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1119)

- [ ] **moses/moses/comboreduct/table/table.h:1179** (MEDIUM, Feature Completion)
  - `* correct, we really should use Fisher information. @todo this).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1179)

- [ ] **moses/moses/comboreduct/table/table_io.cc:934** (MEDIUM, Performance)
  - `// TODO could be simplified, optimized, etc`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L934)

- [ ] **moses/moses/moses/main/problem-params.h:46** (MEDIUM, Feature Completion)
  - `// XXX FIXME TODO The structure below should be split into multiple`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.h#L46)

- [ ] **moses/moses/moses/moses/complexity.cc:52** (MEDIUM, Feature Completion)
  - `// we should count logical and, logical_or, below ..!?!? TODO, clarify.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/complexity.cc#L52)

- [ ] **moses/moses/moses/moses/moses_main.h:102** (MEDIUM, Feature Completion)
  - `// XXX TODO this should be fixed, someday...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/moses_main.h#L102)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:318** (MEDIUM, Feature Completion)
  - `// XXX TODO should probably fetch max_time from somewhere...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L318)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:570** (MEDIUM, Feature Completion)
  - `print_stats_header(NULL, false /* XXX stats for diversity, should be fixed */);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L570)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:616** (MEDIUM, Feature Completion)
  - `// XXX TODO instead of overwritting the demeID it should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L616)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:249** (MEDIUM, Feature Completion)
  - `* XXX TODO: the current algo could be speeded up a fair bit, cutting`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L249)

- [ ] **moses/moses/moses/moses/types.h:210** (MEDIUM, Feature Completion)
  - `// TODO this should be a std::valarray not std::vector but I am too`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/types.h#L210)

- [ ] **moses/moses/moses/optimization/star-anneal.cc:44** (MEDIUM, Feature Completion)
  - `// XXX TODO the annealing temperature control code should be ported over`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/star-anneal.cc#L44)

- [ ] **moses/moses/moses/representation/build_knobs.cc:490** (MEDIUM, Feature Completion)
  - `// TODO: should bias the selection of these, so that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L490)

- [ ] **moses/moses/moses/representation/build_knobs.cc:558** (MEDIUM, Feature Completion)
  - `// We should probably OC_ASSERT here ... TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L558)

- [ ] **moses/moses/moses/representation/build_knobs.cc:1182** (MEDIUM, Feature Completion)
  - `//TODO: should bias the selection of these (and possibly choose larger subtrees)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L1182)

- [ ] **moses/moses/moses/scoring/bscores.cc:930** (MEDIUM, Feature Completion)
  - `/// XXX this should probably be removed! TODO FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/bscores.cc#L930)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:512** (MEDIUM, Feature Completion)
  - `// XXX TODO -- should not return the penalties as part of the bscore,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L512)

- [ ] **moses/moses/moses/scoring/precision_bscore.h:102** (MEDIUM, Feature Completion)
  - `* XXX This class should be reworked to derive from`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/precision_bscore.h#L102)

- [ ] **moses/moses/moses/scoring/scoring_base.cc:135** (MEDIUM, Feature Completion)
  - `* ensemble.  XXX this is probably wrong, we should probably do something`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.cc#L135)

- [ ] **moses/moses/moses/scoring/scoring_base.h:124** (MEDIUM, Feature Completion)
  - `// XXX TODO should be a std::valarray not a vector.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.h#L124)

- [ ] **components/learning/moses/examples/example-progs/continmax.cc:67** (LOW, Pattern Matching)
  - `// TODO: Expand documentation to match style of onemax/nmax examples.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/examples/example-progs/continmax.cc#L67)

- [ ] **components/learning/moses/examples/example-progs/trap-uni.cc:95** (LOW, Feature Completion)
  - `std::bind(&trap::vee, this, std::placeholders::_1)),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/examples/example-progs/trap-uni.cc#L95)

- [ ] **components/learning/moses/examples/example-progs/trap-uni.cc:97** (LOW, Feature Completion)
  - `std::bind(&trap::vee, this, std::placeholders::_1)),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/examples/example-progs/trap-uni.cc#L97)

- [ ] **components/learning/moses/moses/comboreduct/combo/action.h:44** (LOW, Feature Completion)
  - `action_boolean_if = action_if, //nasty hack but allowed - do not insert anything between this line a...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/combo/action.h#L44)

- [ ] **components/learning/moses/moses/comboreduct/combo/simple_nn.h:371** (LOW, Feature Completion)
  - `int selected = rand() % possible.size(); // @todo:replace this by RandGen`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/combo/simple_nn.h#L371)

- [ ] **components/learning/moses/moses/comboreduct/combo/vertex.h:87** (LOW, Feature Completion)
  - `rand,         // random contin_t in [0,1) FIXME TODO : update reduct rules`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/combo/vertex.h#L87)

- [ ] **components/learning/moses/moses/comboreduct/combo/vertex.h:505** (LOW, Feature Completion)
  - `// TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/combo/vertex.h#L505)

- [ ] **components/learning/moses/moses/comboreduct/combo/vertex.h:795** (LOW, Feature Completion)
  - `//TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/combo/vertex.h#L795)

- [ ] **components/learning/moses/moses/comboreduct/interpreter/eval.cc:514** (LOW, Feature Completion)
  - `OC_ASSERT(false, "apply() is not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/interpreter/eval.cc#L514)

- [ ] **components/learning/moses/moses/comboreduct/main/action-reductor.cc:93** (LOW, Feature Completion)
  - `// TODO -- replace this by cond`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/main/action-reductor.cc#L93)

- [ ] **components/learning/moses/moses/comboreduct/main/eval-table.cc:165** (LOW, Feature Completion)
  - `"Timestamp feature not implemented. "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/main/eval-table.cc#L165)

- [ ] **components/learning/moses/moses/comboreduct/reduct/contin_rules.cc:489** (LOW, Feature Completion)
  - `return; //@todo: maybe the other`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/contin_rules.cc#L489)

- [ ] **components/learning/moses/moses/comboreduct/reduct/contin_rules.cc:963** (LOW, Feature Completion)
  - `// TODO:  sin(*(-1 x)) -> -sin(x)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/contin_rules.cc#L963)

- [ ] **components/learning/moses/moses/comboreduct/reduct/flat_normal_form.cc:31** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/flat_normal_form.cc#L31)

- [ ] **components/learning/moses/moses/comboreduct/reduct/flat_normal_form.cc:54** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/flat_normal_form.cc#L54)

- [ ] **components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:100** (LOW, Feature Completion)
  - `// XXX TODO: I don't understand why this is not damaging contin_if  !??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/logical_rules.cc#L100)

- [ ] **components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:289** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/logical_rules.cc#L289)

- [ ] **components/learning/moses/moses/comboreduct/reduct/logical_rules.cc:341** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/logical_rules.cc#L341)

- [ ] **components/learning/moses/moses/comboreduct/reduct/mixed_rules.cc:959** (LOW, Feature Completion)
  - `// maybe TODO : 0<sum x_i -> true if exist i 0<x_i->true and forall other i 0<=x_i`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/mixed_rules.cc#L959)

- [ ] **components/learning/moses/moses/comboreduct/reduct/mixed_rules.cc:1228** (LOW, Feature Completion)
  - `//check if 0<-(y+pi) -> false //TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/mixed_rules.cc#L1228)

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:409** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L409)

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:434** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L434)

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:848** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L848)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:98** (LOW, Feature Completion)
  - `// XXX FIXME TODO: change the implementation, per the above note.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L98)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:801** (LOW, Error Handling)
  - `* rows into vertex_seq (this is also a hack till it handles`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L801)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1089** (LOW, Feature Completion)
  - `// XXX TODO to implement enum support, cut-n-paste from CTable`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1089)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1238** (LOW, Feature Completion)
  - `OC_ASSERT(false, "case not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1238)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1287** (LOW, Feature Completion)
  - `// XXX TODO, it would be easier if KLD took a sorted list`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1287)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:550** (LOW, Feature Completion)
  - `* TODO: we really need a sparse table format, as well.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L550)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:762** (LOW, Feature Completion)
  - `* It's akind of a temporary hack, till it's clear that this is much`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L762)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:1079** (LOW, Feature Completion)
  - `OC_ASSERT(timestamp_feature.empty(), "Timestamp feature not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L1079)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:1256** (LOW, Feature Completion)
  - `// TODO: implement timestamp support`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L1256)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.h:137** (LOW, Feature Completion)
  - `// TODO: reimplement loadITable with the same model of loadTable and`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.h#L137)

- [ ] **components/learning/moses/moses/comboreduct/type_checker/type_tree.cc:40** (LOW, Error Handling)
  - `logger().error() << "default value for " << tn << " not implemented";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/type_checker/type_tree.cc#L40)

- [ ] **components/learning/moses/moses/comboreduct/type_checker/type_tree.cc:599** (LOW, Feature Completion)
  - `// XXX TODO the code below was modified to allow arg lists of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/type_checker/type_tree.cc#L599)

- [ ] **components/learning/moses/moses/comboreduct/type_checker/type_tree.h:235** (LOW, Feature Completion)
  - `// TODO : lambda`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/type_checker/type_tree.h#L235)

- [ ] **components/learning/moses/moses/feature-selection/main/fs-main.cc:53** (LOW, Feature Completion)
  - `// b, d, g, k, n (TODO complete)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/feature-selection/main/fs-main.cc#L53)

- [ ] **components/learning/moses/moses/moses/deme/deme_expander.cc:356** (LOW, Feature Completion)
  - `* XXX TODO I honestly just don't see the utility of this multi-deme`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/deme/deme_expander.cc#L356)

- [ ] **components/learning/moses/moses/moses/deme/deme_expander.cc:441** (LOW, Feature Completion)
  - `// TODO: DO NOT CHANGE THE MAX SCORE IF USER SET IT: BUT THAT`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/deme/deme_expander.cc#L441)

- [ ] **components/learning/moses/moses/moses/deme/deme_expander.cc:457** (LOW, Feature Completion)
  - `// TODO: re-enable that once best_possible_bscore is fixed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/deme/deme_expander.cc#L457)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.cc:44** (LOW, Feature Completion)
  - `std::bind(&field_set::get_raw, &_fields, std::placeholders::_1, idx),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.cc#L44)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.cc:45** (LOW, Feature Completion)
  - `std::bind(&field_set::get_raw, &_fields, std::placeholders::_2, idx))) ==`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.cc#L45)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.cc:72** (LOW, Feature Completion)
  - `std::bind(valueof<const instance>, std::placeholders::_1), src_idx),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.cc#L72)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.cc:80** (LOW, Feature Completion)
  - `std::bind(&local_structure_model::rec_split_term, this, std::placeholders::_1, std::placeholders::_2...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.cc#L80)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.cc:81** (LOW, Feature Completion)
  - `src_idx + 1, idx, std::placeholders::_3, std::placeholders::_4));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.cc#L81)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.cc:105** (LOW, Feature Completion)
  - `std::bind(valueof<const instance>, std::placeholders::_1), src_idx),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.cc#L105)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:135** (LOW, Feature Completion)
  - `* XXX implement this. "bde" is "bayesian distribution estimation"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L135)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:285** (LOW, Feature Completion)
  - `// TODO: Clarify what statistics are being accumulated and where they are stored.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L285)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:293** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L293)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:310** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L310)

- [ ] **components/learning/moses/moses/moses/eda/optimize.h:120** (LOW, Feature Completion)
  - `std::bind(std::cref(score), std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/optimize.h#L120)

- [ ] **components/learning/moses/moses/moses/eda/optimize.h:167** (LOW, Feature Completion)
  - `std::bind(std::cref(score), std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/optimize.h#L167)

- [ ] **components/learning/moses/moses/moses/eda/replacement.h:62** (LOW, Feature Completion)
  - `// TODO: I think it might be a little more efficent to use the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/replacement.h#L62)

- [ ] **components/learning/moses/moses/moses/main/eval-candidate-likelihood.cc:273** (LOW, Feature Completion)
  - `OC_ASSERT(false, "likelihood for problem %s is not implemented",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/eval-candidate-likelihood.cc#L273)

- [ ] **components/learning/moses/moses/moses/main/problem-params.cc:166** (LOW, Feature Completion)
  - `// XXX TODO: make this print correctly, instead of using brackets.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/problem-params.cc#L166)

- [ ] **components/learning/moses/moses/moses/main/problem-params.cc:633** (LOW, Feature Completion)
  - `// The remaining options (TODO organize this)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/problem-params.cc#L633)

- [ ] **components/learning/moses/moses/moses/main/problem-params.cc:901** (LOW, Feature Completion)
  - `"default, only a single deme is created. (XXX Does this "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/problem-params.cc#L901)

- [ ] **components/learning/moses/moses/moses/main/problem-params.cc:1419** (LOW, Feature Completion)
  - `ss << "Granularity " << time_bscore_granularity_str << " not implemented";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/problem-params.cc#L1419)

- [ ] **components/learning/moses/moses/moses/metapopulation/ensemble.h:55** (LOW, Feature Completion)
  - `* XXX FIXME: right now, the ensemble is attached to the metapop, its`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/metapopulation/ensemble.h#L55)

- [ ] **components/learning/moses/moses/moses/metapopulation/merging.cc:404** (LOW, Feature Completion)
  - `// TODO: Make population cap size-sensitive to exemplar complexity.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/metapopulation/merging.cc#L404)

- [ ] **components/learning/moses/moses/moses/metapopulation/metapopulation.h:90** (LOW, Feature Completion)
  - `* XXX FIXME: right now, the ensemble is attached to the metapop, its`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/metapopulation/metapopulation.h#L90)

- [ ] **components/learning/moses/moses/moses/metapopulation/metapopulation.h:315** (LOW, Feature Completion)
  - `* XXX The implementation here results in a lot of copying of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/metapopulation/metapopulation.h#L315)

- [ ] **components/learning/moses/moses/moses/metapopulation/metapopulation.h:535** (LOW, Feature Completion)
  - `// TODO: we may want to output the visited status as well`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/metapopulation/metapopulation.h#L535)

- [ ] **components/learning/moses/moses/moses/moses/distributed_moses.cc:163** (LOW, Distributed Systems)
  - `char tempfile[] = "/tmp/mosesXXXXXX";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/distributed_moses.cc#L163)

- [ ] **components/learning/moses/moses/moses/moses/local_moses.cc:180** (LOW, Feature Completion)
  - `// TODO use the option of the output`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/local_moses.cc#L180)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.cc:201** (LOW, Feature Completion)
  - `// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L201)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.cc:304** (LOW, Feature Completion)
  - `if (!dex.create_demes(exemplar, 0 /* TODO replace with the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L304)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.cc:482** (LOW, Performance)
  - `// TODO: Optimize statistics printing frequency to reduce output volume.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L482)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.cc:561** (LOW, Feature Completion)
  - `// OC_ASSERT(false, "TODO: understand what is the role source=0 exactly");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L561)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.h:51** (LOW, Feature Completion)
  - `// this just right now. XXX TODO: do this, someday.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.h#L51)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:102** (LOW, Feature Completion)
  - `* @todo: in order to better approximate the real-number metric, we`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L102)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:122** (LOW, Feature Completion)
  - `* @todo: term algebra fields are ignored for now`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L122)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:258** (LOW, Feature Completion)
  - `* @todo: term algebra is ignored for the moment.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L258)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:495** (LOW, Feature Completion)
  - `* by looping on the tail-call, just as in the xxx routine...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L495)

- [ ] **components/learning/moses/moses/moses/moses/partial.cc:96** (LOW, Feature Completion)
  - `// TODO: Improve generation tracking by getting actual number`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/partial.cc#L96)

- [ ] **components/learning/moses/moses/moses/optimization/hill-climbing.cc:54** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/hill-climbing.cc#L54)

- [ ] **components/learning/moses/moses/moses/optimization/hill-climbing.h:110** (LOW, Feature Completion)
  - `// XXX TODO make sure this value is appropriately updated.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/hill-climbing.h#L110)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.cc:52** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.cc#L52)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.cc:197** (LOW, Feature Completion)
  - `// TODO: work in a better way to identify convergence.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.cc#L197)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.cc:236** (LOW, Feature Completion)
  - `// TODO: Explanation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.cc#L236)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.h:134** (LOW, Feature Completion)
  - `double bit_min_value, bit_max_value, // [0,1] <- XXX these two aren't used yet.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.h#L134)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.h:154** (LOW, Feature Completion)
  - `// TODO: pso description`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.h#L154)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.h:256** (LOW, Feature Completion)
  - `(1 / (1 + std::exp(-vel)))); // XXX if slow try f(x) = x / (1 + abs(x)) or tanh(x)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.h#L256)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.h:315** (LOW, Feature Completion)
  - `// TODO: Wind dispersion, but test without first`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.h#L315)

- [ ] **components/learning/moses/moses/moses/optimization/particle-swarm.h:320** (LOW, Feature Completion)
  - `* XXX Perform search of the local neighborhood of an instance.  The`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/particle-swarm.h#L320)

- [ ] **components/learning/moses/moses/moses/optimization/star-anneal.cc:61** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/star-anneal.cc#L61)

- [ ] **components/learning/moses/moses/moses/optimization/star-anneal.h:125** (LOW, Feature Completion)
  - `* distance.  @todo: it may be better to have the distance`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/star-anneal.h#L125)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:211** (LOW, Documentation)
  - `* XXX TODO: see comments on disc_probe() below.  This method is a real`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L211)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:581** (LOW, Feature Completion)
  - `// TODO: Benchmark and clarify optimal breakeven point across different problem sizes.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L581)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:690** (LOW, Feature Completion)
  - `// XXX TODO: Is this really optimal?  The below adds an entire copy`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L690)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:725** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L725)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:914** (LOW, Error Handling)
  - `logger().warn("TODO: handle case where it = id::times in build_knobs::rec_canonize");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L914)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:1011** (LOW, Feature Completion)
  - `// TODO: implement support for enumerated types in the input.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L1011)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:1283** (LOW, Feature Completion)
  - `// XXX TODO this below is clearly unfinished, broken, etc.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L1283)

- [ ] **components/learning/moses/moses/moses/representation/knobs.h:283** (LOW, Feature Completion)
  - `// TODO: Clarify the canonization behavior for parent-child relationships.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/knobs.h#L283)

- [ ] **components/learning/moses/moses/moses/representation/representation.cc:51** (LOW, Feature Completion)
  - `// XXX TODO: One might think that varying the stepsize, i.e. shrinking`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/representation.cc#L51)

- [ ] **components/learning/moses/moses/moses/representation/representation.cc:236** (LOW, Feature Completion)
  - `// XXX TODO need to add support for "term algebra" knobs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/representation.cc#L236)

- [ ] **components/learning/moses/moses/moses/scoring/bscores.cc:570** (LOW, Feature Completion)
  - `// TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/bscores.cc#L570)

- [ ] **components/learning/moses/moses/moses/scoring/precision_bscore.cc:111** (LOW, Documentation)
  - `/// set to false. TODO: Document alternative scoring behavior.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/precision_bscore.cc#L111)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:75** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Ensemble scoring not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L75)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:115** (LOW, Error Handling)
  - `OC_ASSERT(false, "bscore error not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L115)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:130** (LOW, Error Handling)
  - `OC_ASSERT(false, "tree error not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L130)

- [ ] **components/learning/moses/moses/moses/scoring/time_dispersion.cc:43** (LOW, Feature Completion)
  - `// TODO multipler other than 1 is not supported yet`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/time_dispersion.cc#L43)

- [ ] **components/learning/moses/moses/moses/scoring/time_dispersion.cc:69** (LOW, Feature Completion)
  - `ss << "Case " << static_cast<size_t>(_granularity) << " not implemented";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/time_dispersion.cc#L69)

- [ ] **components/learning/moses/moses/moses/scoring/time_dispersion.h:61** (LOW, Feature Completion)
  - `* TODO: multiplier other than 1 are not supported at the moment`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/time_dispersion.h#L61)

- [ ] **components/learning/moses/tests/moses/moses-framework.h:42** (LOW, Testing)
  - `char tempfile[] = "/tmp/mosesUTestXXXXXX";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/tests/moses/moses-framework.h#L42)

- [ ] **moses/examples/example-progs/trap-uni.cc:95** (LOW, Feature Completion)
  - `std::bind(&trap::vee, this, std::placeholders::_1)),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/examples/example-progs/trap-uni.cc#L95)

- [ ] **moses/examples/example-progs/trap-uni.cc:97** (LOW, Feature Completion)
  - `std::bind(&trap::vee, this, std::placeholders::_1)),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/examples/example-progs/trap-uni.cc#L97)

- [ ] **moses/moses/comboreduct/combo/action.h:44** (LOW, Feature Completion)
  - `action_boolean_if = action_if, //nasty hack but allowed - do not insert anything between this line a...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/combo/action.h#L44)

- [ ] **moses/moses/comboreduct/combo/simple_nn.h:371** (LOW, Feature Completion)
  - `int selected = rand() % possible.size(); // @todo:replace this by RandGen`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/combo/simple_nn.h#L371)

- [ ] **moses/moses/comboreduct/combo/vertex.h:87** (LOW, Feature Completion)
  - `rand,         // random contin_t in [0,1) FIXME TODO : update reduct rules`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/combo/vertex.h#L87)

- [ ] **moses/moses/comboreduct/combo/vertex.h:505** (LOW, Feature Completion)
  - `// TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/combo/vertex.h#L505)

- [ ] **moses/moses/comboreduct/combo/vertex.h:795** (LOW, Feature Completion)
  - `//TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/combo/vertex.h#L795)

- [ ] **moses/moses/comboreduct/main/action-reductor.cc:93** (LOW, Feature Completion)
  - `// TODO -- replace this by cond`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/main/action-reductor.cc#L93)

- [ ] **moses/moses/comboreduct/reduct/contin_rules.cc:489** (LOW, Feature Completion)
  - `return; //@todo: maybe the other`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/contin_rules.cc#L489)

- [ ] **moses/moses/comboreduct/reduct/contin_rules.cc:963** (LOW, Feature Completion)
  - `// TODO:  sin(*(-1 x)) -> -sin(x)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/contin_rules.cc#L963)

- [ ] **moses/moses/comboreduct/reduct/flat_normal_form.cc:33** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/flat_normal_form.cc#L33)

- [ ] **moses/moses/comboreduct/reduct/flat_normal_form.cc:56** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/flat_normal_form.cc#L56)

- [ ] **moses/moses/comboreduct/reduct/logical_rules.cc:102** (LOW, Feature Completion)
  - `// XXX TODO: I don't understand why this is not damaging contin_if  !??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/logical_rules.cc#L102)

- [ ] **moses/moses/comboreduct/reduct/logical_rules.cc:291** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/logical_rules.cc#L291)

- [ ] **moses/moses/comboreduct/reduct/logical_rules.cc:343** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/logical_rules.cc#L343)

- [ ] **moses/moses/comboreduct/reduct/mixed_rules.cc:959** (LOW, Feature Completion)
  - `// maybe TODO : 0<sum x_i -> true if exist i 0<x_i->true and forall other i 0<=x_i`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/mixed_rules.cc#L959)

- [ ] **moses/moses/comboreduct/reduct/mixed_rules.cc:1228** (LOW, Feature Completion)
  - `//check if 0<-(y+pi) -> false //TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/mixed_rules.cc#L1228)

- [ ] **moses/moses/comboreduct/table/table.cc:420** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L420)

- [ ] **moses/moses/comboreduct/table/table.cc:445** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L445)

- [ ] **moses/moses/comboreduct/table/table.cc:859** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L859)

- [ ] **moses/moses/comboreduct/table/table.h:99** (LOW, Feature Completion)
  - `// XXX FIXME TODO: change the implementation, per the above note.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L99)

- [ ] **moses/moses/comboreduct/table/table.h:684** (LOW, Error Handling)
  - `// XXX TODO WARNING ERROR: builtin hardcoded shit!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L684)

- [ ] **moses/moses/comboreduct/table/table.h:688** (LOW, Error Handling)
  - `// XXX TODO WARNING ERROR: builtin hardcoded shit!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L688)

- [ ] **moses/moses/comboreduct/table/table.h:838** (LOW, Error Handling)
  - `* rows into vertex_seq (this is also a hack till it handles`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L838)

- [ ] **moses/moses/comboreduct/table/table.h:1126** (LOW, Feature Completion)
  - `// XXX TODO to implement enum support, cut-n-paste from CTable`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1126)

- [ ] **moses/moses/comboreduct/table/table.h:1344** (LOW, Feature Completion)
  - `// XXX TODO, it would be easier if KLD took a sorted list`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1344)

- [ ] **moses/moses/comboreduct/table/table_io.cc:529** (LOW, Feature Completion)
  - `* TODO: we really need a sparse table format, as well.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L529)

- [ ] **moses/moses/comboreduct/table/table_io.cc:741** (LOW, Feature Completion)
  - `* It's akind of a temporary hack, till it's clear that this is much`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L741)

- [ ] **moses/moses/comboreduct/table/table_io.cc:1245** (LOW, Feature Completion)
  - `// TODO: implement timestamp support`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L1245)

- [ ] **moses/moses/comboreduct/table/table_io.h:137** (LOW, Feature Completion)
  - `// TODO: reimplement loadITable with the same model of loadTable and`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.h#L137)

- [ ] **moses/moses/comboreduct/type_checker/type_tree.cc:627** (LOW, Feature Completion)
  - `// XXX TODO the code below was modified to allow arg lists of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/type_checker/type_tree.cc#L627)

- [ ] **moses/moses/comboreduct/type_checker/type_tree.h:235** (LOW, Feature Completion)
  - `// TODO : lambda`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/type_checker/type_tree.h#L235)

- [ ] **moses/moses/feature-selection/main/fs-main.cc:53** (LOW, Feature Completion)
  - `// b, d, g, k, n (TODO complete)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/feature-selection/main/fs-main.cc#L53)

- [ ] **moses/moses/moses/deme/deme_expander.cc:356** (LOW, Feature Completion)
  - `* XXX TODO I honestly just don't see the utility of this multi-deme`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/deme/deme_expander.cc#L356)

- [ ] **moses/moses/moses/deme/deme_expander.cc:441** (LOW, Feature Completion)
  - `// TODO: DO NOT CHANGE THE MAX SCORE IF USER SET IT: BUT THAT`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/deme/deme_expander.cc#L441)

- [ ] **moses/moses/moses/deme/deme_expander.cc:457** (LOW, Feature Completion)
  - `// TODO: re-enable that once best_possible_bscore is fixed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/deme/deme_expander.cc#L457)

- [ ] **moses/moses/moses/eda/local_structure.cc:44** (LOW, Feature Completion)
  - `std::bind(&field_set::get_raw, &_fields, std::placeholders::_1, idx),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.cc#L44)

- [ ] **moses/moses/moses/eda/local_structure.cc:45** (LOW, Feature Completion)
  - `std::bind(&field_set::get_raw, &_fields, std::placeholders::_2, idx))) ==`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.cc#L45)

- [ ] **moses/moses/moses/eda/local_structure.cc:72** (LOW, Feature Completion)
  - `std::bind(valueof<const instance>, std::placeholders::_1), src_idx),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.cc#L72)

- [ ] **moses/moses/moses/eda/local_structure.cc:80** (LOW, Feature Completion)
  - `std::bind(&local_structure_model::rec_split_term, this, std::placeholders::_1, std::placeholders::_2...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.cc#L80)

- [ ] **moses/moses/moses/eda/local_structure.cc:81** (LOW, Feature Completion)
  - `src_idx + 1, idx, std::placeholders::_3, std::placeholders::_4));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.cc#L81)

- [ ] **moses/moses/moses/eda/local_structure.cc:105** (LOW, Feature Completion)
  - `std::bind(valueof<const instance>, std::placeholders::_1), src_idx),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.cc#L105)

- [ ] **moses/moses/moses/eda/local_structure.h:135** (LOW, Feature Completion)
  - `* XXX implement this. "bde" is "bayesian distribution estimation"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L135)

- [ ] **moses/moses/moses/eda/local_structure.h:285** (LOW, Feature Completion)
  - `// TODO: Clarify what statistics are being accumulated and where they are stored.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L285)

- [ ] **moses/moses/moses/eda/local_structure.h:293** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L293)

- [ ] **moses/moses/moses/eda/local_structure.h:310** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L310)

- [ ] **moses/moses/moses/eda/optimize.h:120** (LOW, Feature Completion)
  - `std::bind(std::cref(score), std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/optimize.h#L120)

- [ ] **moses/moses/moses/eda/optimize.h:167** (LOW, Feature Completion)
  - `std::bind(std::cref(score), std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/optimize.h#L167)

- [ ] **moses/moses/moses/eda/replacement.h:62** (LOW, Feature Completion)
  - `// TODO: I think it might be a little more efficent to use the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/replacement.h#L62)

- [ ] **moses/moses/moses/main/problem-params.cc:169** (LOW, Feature Completion)
  - `// XXX TODO: make this print correctly, instead of using brackets.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L169)

- [ ] **moses/moses/moses/main/problem-params.cc:636** (LOW, Feature Completion)
  - `// The remaining options (TODO organize this)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L636)

- [ ] **moses/moses/moses/main/problem-params.cc:904** (LOW, Feature Completion)
  - `"default, only a single deme is created. (XXX Does this "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L904)

- [ ] **moses/moses/moses/main/problem-params.cc:1428** (LOW, Feature Completion)
  - `ss << "Granularity " << time_bscore_granularity_str << " not implemented. "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L1428)

- [ ] **moses/moses/moses/metapopulation/ensemble.h:55** (LOW, Feature Completion)
  - `* XXX FIXME: right now, the ensemble is attached to the metapop, its`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/metapopulation/ensemble.h#L55)

- [ ] **moses/moses/moses/metapopulation/merging.cc:404** (LOW, Feature Completion)
  - `// TODO: Make population cap size-sensitive to exemplar complexity.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/metapopulation/merging.cc#L404)

- [ ] **moses/moses/moses/metapopulation/metapopulation.h:90** (LOW, Feature Completion)
  - `* XXX FIXME: right now, the ensemble is attached to the metapop, its`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/metapopulation/metapopulation.h#L90)

- [ ] **moses/moses/moses/metapopulation/metapopulation.h:315** (LOW, Feature Completion)
  - `* XXX The implementation here results in a lot of copying of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/metapopulation/metapopulation.h#L315)

- [ ] **moses/moses/moses/metapopulation/metapopulation.h:535** (LOW, Feature Completion)
  - `// TODO: we may want to output the visited status as well`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/metapopulation/metapopulation.h#L535)

- [ ] **moses/moses/moses/moses/distributed_moses.cc:163** (LOW, Distributed Systems)
  - `char tempfile[] = "/tmp/mosesXXXXXX";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/distributed_moses.cc#L163)

- [ ] **moses/moses/moses/moses/local_moses.cc:180** (LOW, Feature Completion)
  - `// TODO use the option of the output`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/local_moses.cc#L180)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:202** (LOW, Feature Completion)
  - `// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L202)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:305** (LOW, Feature Completion)
  - `if (!dex.create_demes(exemplar, 0 /* TODO replace with the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L305)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:486** (LOW, Performance)
  - `// TODO: Optimize statistics printing frequency to reduce output volume.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L486)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:565** (LOW, Feature Completion)
  - `// OC_ASSERT(false, "TODO: understand what is the role source=0 exactly");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L565)

- [ ] **moses/moses/moses/moses/mpi_moses.h:51** (LOW, Feature Completion)
  - `// this just right now. XXX TODO: do this, someday.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.h#L51)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:102** (LOW, Feature Completion)
  - `* @todo: in order to better approximate the real-number metric, we`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L102)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:122** (LOW, Feature Completion)
  - `* @todo: term algebra fields are ignored for now`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L122)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:252** (LOW, Feature Completion)
  - `* @todo: term algebra is ignored for the moment.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L252)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:327** (LOW, Feature Completion)
  - `// XXX TODO, unroll the last tail call, just like the single-bit`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L327)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:478** (LOW, Feature Completion)
  - `* by looping on the tail-call, just as in the xxx routine...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L478)

- [ ] **moses/moses/moses/moses/partial.cc:96** (LOW, Feature Completion)
  - `// TODO: Improve generation tracking by getting actual number`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/partial.cc#L96)

- [ ] **moses/moses/moses/optimization/hill-climbing.cc:55** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/hill-climbing.cc#L55)

- [ ] **moses/moses/moses/optimization/hill-climbing.h:110** (LOW, Feature Completion)
  - `// XXX TODO make sure this value is appropriately updated.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/hill-climbing.h#L110)

- [ ] **moses/moses/moses/optimization/particle-swarm.cc:53** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.cc#L53)

- [ ] **moses/moses/moses/optimization/particle-swarm.cc:198** (LOW, Feature Completion)
  - `// TODO: work in a better way to identify convergence.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.cc#L198)

- [ ] **moses/moses/moses/optimization/particle-swarm.cc:237** (LOW, Feature Completion)
  - `// TODO: Explanation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.cc#L237)

- [ ] **moses/moses/moses/optimization/particle-swarm.h:134** (LOW, Feature Completion)
  - `double bit_min_value, bit_max_value, // [0,1] <- XXX these two aren't used yet.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.h#L134)

- [ ] **moses/moses/moses/optimization/particle-swarm.h:154** (LOW, Feature Completion)
  - `// TODO: pso description`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.h#L154)

- [ ] **moses/moses/moses/optimization/particle-swarm.h:256** (LOW, Feature Completion)
  - `(1 / (1 + std::exp(-vel)))); // XXX if slow try f(x) = x / (1 + abs(x)) or tanh(x)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.h#L256)

- [ ] **moses/moses/moses/optimization/particle-swarm.h:315** (LOW, Feature Completion)
  - `// TODO: Wind dispersion, but test without first`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.h#L315)

- [ ] **moses/moses/moses/optimization/particle-swarm.h:320** (LOW, Feature Completion)
  - `* XXX Perform search of the local neighborhood of an instance.  The`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.h#L320)

- [ ] **moses/moses/moses/optimization/star-anneal.cc:63** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/star-anneal.cc#L63)

- [ ] **moses/moses/moses/optimization/star-anneal.h:125** (LOW, Feature Completion)
  - `* distance.  @todo: it may be better to have the distance`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/star-anneal.h#L125)

- [ ] **moses/moses/moses/representation/build_knobs.cc:212** (LOW, Documentation)
  - `* XXX TODO: see comments on disc_probe() below.  This method is a real`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L212)

- [ ] **moses/moses/moses/representation/build_knobs.cc:582** (LOW, Feature Completion)
  - `// TODO: Benchmark and clarify optimal breakeven point across different problem sizes.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L582)

- [ ] **moses/moses/moses/representation/build_knobs.cc:691** (LOW, Feature Completion)
  - `// XXX TODO: Is this really optimal?  The below adds an entire copy`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L691)

- [ ] **moses/moses/moses/representation/build_knobs.cc:726** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L726)

- [ ] **moses/moses/moses/representation/build_knobs.cc:915** (LOW, Error Handling)
  - `logger().warn("TODO: handle case where it = id::times in build_knobs::rec_canonize");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L915)

- [ ] **moses/moses/moses/representation/build_knobs.cc:1012** (LOW, Feature Completion)
  - `// TODO: implement support for enumerated types in the input.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L1012)

- [ ] **moses/moses/moses/representation/build_knobs.cc:1284** (LOW, Feature Completion)
  - `// XXX TODO this below is clearly unfinished, broken, etc.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L1284)

- [ ] **moses/moses/moses/representation/knobs.h:208** (LOW, Feature Completion)
  - `idx = 0;  // interpreted as "absent", as used below. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/knobs.h#L208)

- [ ] **moses/moses/moses/representation/knobs.h:276** (LOW, Feature Completion)
  - `// TODO: Clarify the canonization behavior for parent-child relationships.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/knobs.h#L276)

- [ ] **moses/moses/moses/representation/representation.cc:51** (LOW, Feature Completion)
  - `// XXX TODO: One might think that varying the stepsize, i.e. shrinking`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/representation.cc#L51)

- [ ] **moses/moses/moses/representation/representation.cc:238** (LOW, Feature Completion)
  - `// XXX TODO need to add support for "term algebra" knobs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/representation.cc#L238)

- [ ] **moses/moses/moses/scoring/bscores.cc:570** (LOW, Feature Completion)
  - `// TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/bscores.cc#L570)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:646** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L646)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:655** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L655)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:707** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L707)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:714** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L714)

- [ ] **moses/moses/moses/scoring/precision_bscore.cc:111** (LOW, Documentation)
  - `/// set to false. TODO: Document alternative scoring behavior.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/precision_bscore.cc#L111)

- [ ] **moses/moses/moses/scoring/time_dispersion.cc:43** (LOW, Feature Completion)
  - `// TODO multipler other than 1 is not supported yet`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/time_dispersion.cc#L43)

- [ ] **moses/moses/moses/scoring/time_dispersion.h:61** (LOW, Feature Completion)
  - `* TODO: multiplier other than 1 are not supported at the moment`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/time_dispersion.h#L61)

- [ ] **moses/tests/moses/moses-framework.h:42** (LOW, Testing)
  - `char tempfile[] = "/tmp/mosesUTestXXXXXX";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/tests/moses/moses-framework.h#L42)

### Memory System
*Total items: 106*

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:59** (CRITICAL, Feature Completion)
  - `/// XXX TODO: We could have a non-blocking version of this atom. We`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L59)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:172** (HIGH, Thread Safety)
  - `std::thread([as, silent, todo_list, qvp, finished_count, nthreads = _nthreads]() {`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L172)

- [ ] **atomspace/opencog/atomspace/TypeIndex.h:73** (HIGH, Thread Safety)
  - `* @todo The iterator is NOT thread-safe against the insertion or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/TypeIndex.h#L73)

- [ ] **atomspace/examples/c++-guile/PrimitiveExample.cc:87** (MEDIUM, Error Handling)
  - `printf("XXX ERROR XXX: an error should have been thrown, but wasn't!\n");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/c++-guile/PrimitiveExample.cc#L87)

- [ ] **atomspace/opencog/atoms/core/Variables.cc:437** (MEDIUM, Feature Completion)
  - `// XXX TODO type-checking could be lazy; if the function is not`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/Variables.cc#L437)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.h:142** (MEDIUM, Feature Completion)
  - `* TODO: should be refined to make the difference between AndLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.h#L142)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.h:145** (MEDIUM, Pattern Matching)
  - `* TODO: this should probably be moved to some pattern matcher`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.h#L145)

- [ ] **atomspace/opencog/atoms/reduct/AccumulateLink.cc:71** (MEDIUM, Error Handling)
  - `// XXX TODO -- we could also handle vectors of strings, by`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/reduct/AccumulateLink.cc#L71)

- [ ] **atomspace/opencog/atoms/value/ContainerValue.h:51** (MEDIUM, Feature Completion)
  - `* an end-of-stream indicator. (XXX Maybe this should be moved to the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/value/ContainerValue.h#L51)

- [ ] **atomspace/opencog/atomspace/AtomSpace.cc:138** (MEDIUM, Feature Completion)
  - `// TODO: this should probably be moved to a method on class Atom.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/AtomSpace.cc#L138)

- [ ] **atomspace/opencog/haskell/AtomSpace_CWrapper.h:112** (MEDIUM, Error Handling)
  - `* XXX FIXME no one should be using Handle's to work with atoms,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/haskell/AtomSpace_CWrapper.h#L112)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:1159** (MEDIUM, Feature Completion)
  - `// XXX TODO The logic here should be updated to resemble that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L1159)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:1502** (MEDIUM, Feature Completion)
  - `// its definition. XXX TODO. Hmm. Should we do this at runtime,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L1502)

- [ ] **atomspace/opencog/query/SatisfyMixin.cc:304** (MEDIUM, Feature Completion)
  - `* This implements the SAT-solver-like optimization described in the TODO above.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/SatisfyMixin.cc#L304)

- [ ] **atomspace/opencog/query/TermMatchMixin.cc:551** (MEDIUM, Feature Completion)
  - `// XXX TODO as discussed on the mailing list, we should perhaps first`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/TermMatchMixin.cc#L551)

- [ ] **atomspace/opencog/sheaf/attic/vo-graph.scm:126** (MEDIUM, Feature Completion)
  - `XXX That should be fixed...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/sheaf/attic/vo-graph.scm#L126)

- [ ] **atomspace/cmake/OpenCogGenOCamlTypes.cmake:141** (LOW, Feature Completion)
  - `# XXX FIXME LATER`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGenOCamlTypes.cmake#L141)

- [ ] **atomspace/cmake/OpenCogGenOCamlTypes.cmake:147** (LOW, Feature Completion)
  - `# XXX FIXME LATER`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGenOCamlTypes.cmake#L147)

- [ ] **atomspace/cmake/OpenCogGenPythonTypes.cmake:41** (LOW, Feature Completion)
  - `# XXX This is broken in two ways. First, createValue() is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGenPythonTypes.cmake#L41)

- [ ] **atomspace/cmake/OpenCogGenPythonTypes.cmake:44** (LOW, Feature Completion)
  - `# so this is a no-op, anyway. XXX FIXME someday, I guess`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGenPythonTypes.cmake#L44)

- [ ] **atomspace/cmake/OpenCogGenPythonTypes.cmake:87** (LOW, Feature Completion)
  - `# This is kind of hacky, but I don't know what else to do ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGenPythonTypes.cmake#L87)

- [ ] **atomspace/cmake/OpenCogGenTypes.cmake:86** (LOW, Feature Completion)
  - `# hacky, but is needed for e.g. "VariableList" ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGenTypes.cmake#L86)

- [ ] **atomspace/cmake/OpenCogGuile.cmake:140** (LOW, Feature Completion)
  - `# modules is more than two deep e.g `(opencog foo bar)` XXX FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/cmake/OpenCogGuile.cmake#L140)

- [ ] **atomspace/examples/atomspace/execute.scm:9** (LOW, Feature Completion)
  - `; A note of caution, though: ExecutionOutputLink is a kind-of hack.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/atomspace/execute.scm#L9)

- [ ] **atomspace/examples/pattern-matcher/deduction-engine.scm:56** (LOW, Feature Completion)
  - `;; for proper deductive reasoning as recommended in the FIXME.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/pattern-matcher/deduction-engine.scm#L56)

- [ ] **atomspace/examples/pattern-matcher/recognizer.scm:448** (LOW, Feature Completion)
  - `; polluting it with sentences.  The push and pop here is a hack;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/pattern-matcher/recognizer.scm#L448)

- [ ] **atomspace/opencog/atoms/atom_types/NameServer.cc:57** (LOW, Feature Completion)
  - `* This is a strange kind of hack to allow the cogserver unit`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/atom_types/NameServer.cc#L57)

- [ ] **atomspace/opencog/atoms/base/Valuation.cc:50** (LOW, Feature Completion)
  - `// XXX TODO -- C++ smart pointers are not atomic; we really`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/base/Valuation.cc#L50)

- [ ] **atomspace/opencog/atoms/core/FindUtils.h:146** (LOW, Feature Completion)
  - `* XXX FIXME: what if it appears quoted in one place, and unquoted`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/FindUtils.h#L146)

- [ ] **atomspace/opencog/atoms/core/PutLink.cc:337** (LOW, Error Handling)
  - `* an undefined handle is returned (?? XXX really? or is it a throw?).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/PutLink.cc#L337)

- [ ] **atomspace/opencog/atoms/core/RandomChoice.cc:143** (LOW, Feature Completion)
  - `// XXX TODO if execute() above returns FloatValue, use that!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RandomChoice.cc#L143)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.cc:296** (LOW, Feature Completion)
  - `// TODO: the following has no unit test!!! Yet it introduces a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.cc#L296)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.cc:340** (LOW, Feature Completion)
  - `// TODO: generalize with when Unquote and Quote are apart`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.cc#L340)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.h:224** (LOW, Feature Completion)
  - `// TODO: we probably want to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.h#L224)

- [ ] **atomspace/opencog/atoms/core/TypeNode.h:90** (LOW, Feature Completion)
  - `// XXX TODO ... Some types are defined. In this case,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeNode.h#L90)

- [ ] **atomspace/opencog/atoms/core/TypeUtils.cc:182** (LOW, Feature Completion)
  - `* The implementation below feels awfully hacky and bug-prone,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeUtils.cc#L182)

- [ ] **atomspace/opencog/atoms/core/TypedVariableLink.cc:56** (LOW, Feature Completion)
  - `VARIABLE_NODE != dtype and // XXX FIXME this is wrong; URE-bug`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypedVariableLink.cc#L56)

- [ ] **atomspace/opencog/atoms/core/Variables.cc:284** (LOW, Error Handling)
  - `* XXX TODO this does not currently handle type equations, as outlined`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/Variables.cc#L284)

- [ ] **atomspace/opencog/atoms/execution/EvaluationLink.cc:674** (LOW, Feature Completion)
  - `"Cannot evaluate %s - not implemented for crisp evaluation",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/EvaluationLink.cc#L674)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.cc:288** (LOW, Feature Completion)
  - `// TODO: what about globs?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.cc#L288)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.h:88** (LOW, Feature Completion)
  - `* TODO: maybe this can eliminate the need for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.h#L88)

- [ ] **atomspace/opencog/atoms/flow/CollectionOfLink.cc:69** (LOW, Error Handling)
  - `// TODO: Handle executable _outgoing[0] by executing it first.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/CollectionOfLink.cc#L69)

- [ ] **atomspace/opencog/atoms/flow/CollectionOfLink.cc:70** (LOW, Feature Completion)
  - `// TODO: Support complex type signatures beyond simple TYPE_NODE.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/CollectionOfLink.cc#L70)

- [ ] **atomspace/opencog/atoms/flow/FilterLink.cc:615** (LOW, Feature Completion)
  - `// XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/FilterLink.cc#L615)

- [ ] **atomspace/opencog/atoms/flow/ValueOfLink.cc:84** (LOW, Feature Completion)
  - `// XXX TODO FIXME ... if either of these are executable, then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/ValueOfLink.cc#L84)

- [ ] **atomspace/opencog/atoms/foreign/DatalogAST.cc:92** (LOW, Feature Completion)
  - `return _name + "XXX-borken";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/foreign/DatalogAST.cc#L92)

- [ ] **atomspace/opencog/atoms/join/JoinLink.cc:549** (LOW, Feature Completion)
  - `/// TODO: it might be faster to use hash tables instead of rb-trees`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/join/JoinLink.cc#L549)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:93** (LOW, Error Handling)
  - `concurrent_queue<Handle>* todo,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L93)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:101** (LOW, Feature Completion)
  - `if (not todo->try_get(h)) return;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L101)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:124** (LOW, Error Handling)
  - `concurrent_queue<Handle> todo_list;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L124)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:127** (LOW, Feature Completion)
  - `todo_list.push(h);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L127)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:140** (LOW, Feature Completion)
  - `as, silent, &todo_list, qvp, &ex));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L140)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:158** (LOW, Error Handling)
  - `auto todo_list = std::make_shared<concurrent_queue<Handle>>();`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L158)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:161** (LOW, Feature Completion)
  - `todo_list->push(h);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L161)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:179** (LOW, Feature Completion)
  - `if (not todo_list->try_get(h)) break;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L179)

- [ ] **atomspace/opencog/atoms/pattern/PatternTerm.h:80** (LOW, Feature Completion)
  - `// TODO: it would probably be more efficient to swap which of these`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternTerm.h#L80)

- [ ] **atomspace/opencog/atoms/pattern/PatternTerm.h:267** (LOW, Error Handling)
  - `if (itm->_handle == _handle) return true; // XXX maybe quote?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternTerm.h#L267)

- [ ] **atomspace/opencog/atoms/pattern/PatternUtils.cc:123** (LOW, Error Handling)
  - `HandleSeq todo(clauses);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternUtils.cc#L123)

- [ ] **atomspace/opencog/atoms/pattern/PatternUtils.cc:125** (LOW, Feature Completion)
  - `while (0 < todo.size())`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternUtils.cc#L125)

- [ ] **atomspace/opencog/atoms/pattern/PatternUtils.cc:132** (LOW, Error Handling)
  - `for (const Handle& cl: todo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternUtils.cc#L132)

- [ ] **atomspace/opencog/atoms/pattern/PatternUtils.cc:166** (LOW, Feature Completion)
  - `todo = no_con_yet;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternUtils.cc#L166)

- [ ] **atomspace/opencog/atoms/pattern/PatternUtils.cc:174** (LOW, Feature Completion)
  - `todo = no_con_yet;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/pattern/PatternUtils.cc#L174)

- [ ] **atomspace/opencog/atomspace/AtomTable.cc:60** (LOW, Feature Completion)
  - `std::bind(&AtomSpace::typeAdded, this, std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/AtomTable.cc#L60)

- [ ] **atomspace/opencog/atomspace/TypeIndex.h:54** (LOW, Feature Completion)
  - `#if HAVE_FOLLY_XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/TypeIndex.h#L54)

- [ ] **atomspace/opencog/cython/PythonEval.cc:81** (LOW, Feature Completion)
  - `* @todo When can we remove the singleton instance? Answer: not sure.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/PythonEval.cc#L81)

- [ ] **atomspace/opencog/cython/PythonEval.cc:1079** (LOW, Error Handling)
  - `* assorted hacks to handle the different argument type.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/PythonEval.cc#L1079)

- [ ] **atomspace/opencog/cython/opencog/ExecuteStub.cc:7** (LOW, Testing)
  - `#include "ExecuteStub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/opencog/ExecuteStub.cc#L7)

- [ ] **atomspace/opencog/cython/opencog/ExecuteStub.h:2** (LOW, Testing)
  - `* opencog/cython/opencog/ExecuteStub.h`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/opencog/ExecuteStub.h#L2)

- [ ] **atomspace/opencog/cython/opencog/ExecuteStub.h:23** (LOW, Testing)
  - `#ifndef _OPENCOG_EXECUTESTUB_H`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/opencog/ExecuteStub.h#L23)

- [ ] **atomspace/opencog/cython/opencog/ExecuteStub.h:24** (LOW, Testing)
  - `#define _OPENCOG_EXECUTESTUB_H`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/opencog/ExecuteStub.h#L24)

- [ ] **atomspace/opencog/cython/opencog/ExecuteStub.h:34** (LOW, Testing)
  - `#endif // _OPENCOG_EXECUTESTUB_H`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/opencog/ExecuteStub.h#L34)

- [ ] **atomspace/opencog/guile/SchemeSmobAtom.cc:210** (LOW, Feature Completion)
  - `printf("XXX FIXME Bad string %s\nconverted to %s\n", (char *) data, wbuf);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmobAtom.cc#L210)

- [ ] **atomspace/opencog/haskell/PatternMatcher_CWrapper.h:8** (LOW, Feature Completion)
  - `* XXX FIXME: atoms must never be accessed by UUID except by the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/haskell/PatternMatcher_CWrapper.h#L8)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:382** (LOW, Feature Completion)
  - `// TODO -- weed out duplicates!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L382)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:551** (LOW, Feature Completion)
  - `return choice_loop(pmc, "xxxxxxxxxx neighbor_search xxxxxxxxxx");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L551)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:578** (LOW, Feature Completion)
  - `return choice_loop(pmc, "xxxxxxxxxx neighbor_search xxxxxxxxxx");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L578)

- [ ] **atomspace/opencog/query/NextSearchMixin.cc:167** (LOW, Feature Completion)
  - `// XXX TODO ... Rather than counting the number of variables, we`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/NextSearchMixin.cc#L167)

- [ ] **atomspace/opencog/query/NextSearchMixin.cc:267** (LOW, Performance)
  - `// this simpler code is good enough. TODO: Optimize to filter by parent type.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/NextSearchMixin.cc#L267)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2066** (LOW, Feature Completion)
  - `// XXX TODO FIXME. The ptm needs to be decomposed into connected`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2066)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2151** (LOW, Feature Completion)
  - `OC_ASSERT(not term->hasGlobbyVar(), "Buggy or not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2151)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2446** (LOW, Feature Completion)
  - `// XXX TODO make sure that all variables in the clause have`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2446)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2745** (LOW, Feature Completion)
  - `* XXX TODO -- if the algo is working correctly, then all`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2745)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:3133** (LOW, Feature Completion)
  - `* TODO: The implementation here is minimal - very simple, very basic.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L3133)

- [ ] **atomspace/opencog/query/Recognizer.cc:126** (LOW, Feature Completion)
  - `// TODO: Change to something better if possible...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/Recognizer.cc#L126)

- [ ] **atomspace/opencog/scm/opencog.scm:22** (LOW, Feature Completion)
  - `; hairy hack needed to be able to run unit tests without installing.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog.scm#L22)

- [ ] **atomspace/opencog/scm/opencog/base/file-utils.scm:122** (LOW, Feature Completion)
  - `; XXX this duplicates (load-from-path) which is a built-in in guile...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/file-utils.scm#L122)

- [ ] **atomspace/opencog/scm/opencog/base/utilities.scm:764** (LOW, Error Handling)
  - `XXX! Caution/error! This implicitly assumes that there is only one`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/utilities.scm#L764)

- [ ] **atomspace/opencog/scm/opencog/base/utilities.scm:767** (LOW, Feature Completion)
  - `XXX! You probably want to be using either StateLink or DefineLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/utilities.scm#L767)

- [ ] **atomspace/opencog/scm/opencog/base/utilities.scm:856** (LOW, Feature Completion)
  - `XXX TODO -- this would almost surely be simpler to implement using`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/utilities.scm#L856)

- [ ] **atomspace/opencog/sheaf/attic/linear-parser.scm:156** (LOW, Feature Completion)
  - `XXX FIXME WARNING DANGER: As written, this runs in exponential time`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/sheaf/attic/linear-parser.scm#L156)

- [ ] **atomspace/opencog/sheaf/attic/vo-graph.scm:232** (LOW, Feature Completion)
  - `XXX FIXME WARNING DANGER: As written, this runs in exponential time`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/sheaf/attic/vo-graph.scm#L232)

- [ ] **atomspace/opencog/sheaf/attic/vo-graph.scm:290** (LOW, Feature Completion)
  - `XXX FIXME WARNING DANGER: As written, this runs in exponential time`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/sheaf/attic/vo-graph.scm#L290)

- [ ] **atomspace/tests/atoms/execution/defined-schema.scm:239** (LOW, Testing)
  - `; XXX FIXME, this does not quite work as one might naively expect,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/atoms/execution/defined-schema.scm#L239)

- [ ] **atomspace/tests/cython/atomspace/test_atomspace.py:362** (LOW, Testing)
  - `# XXX FIXME is testing the name of the bottom type`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/cython/atomspace/test_atomspace.py#L362)

- [ ] **atomspace/tests/query/arcana-numeric.scm:4** (LOW, Testing)
  - `; Some squonky numeric hacking about.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/arcana-numeric.scm#L4)

- [ ] **atomspace/tests/query/buggy-equal.scm:71** (LOW, Testing)
  - `(GroundedSchemaNode "scm: pln-xxx")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/buggy-equal.scm#L71)

- [ ] **atomspace/tests/query/buggy-equal.scm:106** (LOW, Testing)
  - `(define (pln-xxx a b c) (QuoteLink a b c))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/buggy-equal.scm#L106)

- [ ] **atomspace/tests/query/buggy-equal.scm:145** (LOW, Testing)
  - `(GroundedSchemaNode "scm: pln-xxx")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/buggy-equal.scm#L145)

- [ ] **atomspace/tests/query/disconnected.scm:35** (LOW, Testing)
  - `; weird hack to interface with the C++ code ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/disconnected.scm#L35)

- [ ] **atomspace/tests/query/seq-absence.scm:70** (LOW, Testing)
  - `;; and right now, I'm not gonna fix it... XXX FIXME.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/seq-absence.scm#L70)

- [ ] **atomspace/tests/query/seq-absence.scm:86** (LOW, Testing)
  - `;; XXX FIXME ... this and the above need to get done right.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/seq-absence.scm#L86)

- [ ] **atomspace/tests/query/sudoku-puzzle.scm:9** (LOW, Pattern Matching)
  - `; XXX As of 18 October 2014, the pattern matcher fails to find a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/sudoku-puzzle.scm#L9)

- [ ] **atomspace/tests/query/sudoku-puzzle.scm:12** (LOW, Testing)
  - `; XXXXXXXXXXXXXXXXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/sudoku-puzzle.scm#L12)

- [ ] **atomspace/tests/query/unordered-quote.scm:4** (LOW, Testing)
  - `; Test for infinite loop caused in #2357; hacked around in #2360;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/query/unordered-quote.scm#L4)

- [ ] **atomspace/tests/scm/typedefs.scm:3** (LOW, Testing)
  - `; XXX THIS FILE IS TO BE USED IN ASSOCIATION WITH THIS TEST CASE ONLY`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/scm/typedefs.scm#L3)

- [ ] **atomspace/tests/scm/typedefs.scm:5** (LOW, Testing)
  - `; BE BROKEN IF YOU DO!  XXXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/tests/scm/typedefs.scm#L5)

### Other
*Total items: 1274*

- [ ] **entelechy_introspection.py:889** (CRITICAL, Feature Completion)
  - `3. **Resolve Critical TODOs** - Address high-priority placeholders`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L889)

- [ ] **analyze_issue_examples.py:14** (HIGH, Performance)
  - `"./atomspace/opencog/atomspace/Transient.cc:/// XXX FIXME. Performance has not been recently measure...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L14)

- [ ] **analyze_issue_examples.py:33** (HIGH, Thread Safety)
  - `"./atomspace/opencog/atoms/truthvalue/FormulaTruthValue.cc:// XXX FIXME This update is not thread-sa...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L33)

- [ ] **analyze_issue_examples.py:38** (HIGH, Thread Safety)
  - `"./atomspace/opencog/atoms/value/FormulaStream.cc:// XXX FIXME The update here is not thread-safe......`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L38)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:32** (HIGH, Thread Safety)
  - `; This is a pseudo-multi-threading hack. The core problem here is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L32)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:242** (HIGH, Thread Safety)
  - `; threads listening on the scheme port.  FIXME XXX This should be fixed,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L242)

- [ ] **components/integration/opencog/opencog/nlp/wsd-post/cluster.scm:67** (HIGH, Performance)
  - `;; XXX not tail recursive; get performance boost if it was.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd-post/cluster.scm#L67)

- [ ] **components/integration/opencog/opencog/openpsi/main.scm:94** (HIGH, Thread Safety)
  - `; XXX FIXME -- right now, this assumes that a single thread, running`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/main.scm#L94)

- [ ] **components/language/learn/scm/utils/utilities.scm:377** (HIGH, Thread Safety)
  - `; FIXME: use a thread-safe test-n-set instead.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/utils/utilities.scm#L377)

- [ ] **generate_progress_report.py:100** (HIGH, Thread Safety)
  - `- **Complex FIXMEs:** A significant number of `FIXME` items are related to complex issues such as pe...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L100)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:165** (HIGH, Thread Safety)
  - `// TODO: if creating/destroying threads is too expensive, use a thread`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L165)

- [ ] **analyze_issue_examples.py:18** (MEDIUM, Feature Completion)
  - `"./atomspace/opencog/atomspace/AtomSpace.h:    // XXX FIXME Users should call StorageNode::add_noche...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L18)

- [ ] **analyze_issue_examples.py:21** (MEDIUM, Error Handling)
  - `"./atomspace/opencog/haskell/AtomSpace_CWrapper.h:     * XXX FIXME no one should be using Handle's t...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L21)

- [ ] **analyze_issue_examples.py:42** (MEDIUM, Feature Completion)
  - `"./atomspace/opencog/query/InitiateSearchMixin.cc:		// XXX FIXME; we should be using ptm->isVariable...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L42)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:138** (MEDIUM, Error Handling)
  - `; TODO: For now just using a single result, but we should handle multiple`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L138)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:202** (MEDIUM, Feature Completion)
  - `; TODO: we should check first make sure the response is a pre-defined behavior`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L202)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:179** (MEDIUM, Feature Completion)
  - `; XXX FIXME this should be a part of "Show random expression"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L179)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:719** (MEDIUM, Feature Completion)
  - `;; XXX FIXME -- this should not be hard-coded here!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L719)

- [ ] **components/integration/opencog/opencog/eva/behavior/face-priority.scm:74** (MEDIUM, Feature Completion)
  - `; FIXME: There should never be an empty set. The value should be set`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/face-priority.scm#L74)

- [ ] **components/integration/opencog/opencog/eva/behavior/movement-api.scm:27** (MEDIUM, Feature Completion)
  - `; XXX FIXME -- someday, should probably create a distinct API for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/movement-api.scm#L27)

- [ ] **components/integration/opencog/opencog/eva/behavior/orchestrate.scm:224** (MEDIUM, Feature Completion)
  - `; factors.  XXX This is incompletely thought out and maybe should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/orchestrate.scm#L224)

- [ ] **components/integration/opencog/opencog/eva/behavior/psi-behavior.scm:75** (MEDIUM, Feature Completion)
  - `; TODO: How should rules that could run concurrently be represented, when`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/psi-behavior.scm#L75)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm:64** (MEDIUM, Feature Completion)
  - `; XXX fixme -- we should not need to load either relex2logic or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm#L64)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:358** (MEDIUM, Error Handling)
  - `; XXX a bunch of verb synonyms -- handled manually. These should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L358)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:156** (MEDIUM, Feature Completion)
  - `; XXX this should be moved to cog-utils. Also needs to be fixed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm#L156)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/run-chatbot.scm:22** (MEDIUM, Feature Completion)
  - `; XXX fixme -- we should not need to load either relex2logic or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/run-chatbot.scm#L22)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:95** (MEDIUM, Feature Completion)
  - `; XXX FIXME -- this should return neutral, if the timestamp is more`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L95)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:374** (MEDIUM, Feature Completion)
  - `;; XXX FIXME -- the psi subsystem should be performing this action,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L374)

- [ ] **components/integration/opencog/opencog/eva/model/time-map.scm:9** (MEDIUM, Error Handling)
  - `; XXX FIXME -- some of the below should be handled as psi-rules,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/time-map.scm#L9)

- [ ] **components/integration/opencog/opencog/eva/model/time-map.scm:161** (MEDIUM, Feature Completion)
  - `;; XXX FIXME -- this kind of crazy angle computation should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/time-map.scm#L161)

- [ ] **components/integration/opencog/opencog/eva/model/time-map.scm:237** (MEDIUM, Feature Completion)
  - `;; XXX TODO -- this should eventually be a psi-rule, so that we can`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/time-map.scm#L237)

- [ ] **components/integration/opencog/opencog/ghost/terms.scm:238** (MEDIUM, Error Handling)
  - `; TODO: Should be handled in OpenCog internally?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/terms.scm#L238)

- [ ] **components/integration/opencog/opencog/ghost/terms.scm:255** (MEDIUM, Error Handling)
  - `; TODO: Should be handled in OpenCog internally?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/terms.scm#L255)

- [ ] **components/integration/opencog/opencog/ghost/translator.scm:189** (MEDIUM, Testing)
  - `; TODO: The specificity of ordered vs unordered should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/translator.scm#L189)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:143** (MEDIUM, Feature Completion)
  - `; XXX should we defer this loading till later ??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L143)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/FrameQuery.cc:11** (MEDIUM, Feature Completion)
  - `* XXX todo-- should have is_query look for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/FrameQuery.cc#L11)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm:601** (MEDIUM, Feature Completion)
  - `; XXX we should fetch from SQL ... XXXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm#L601)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm:76** (MEDIUM, Feature Completion)
  - `; XXX we should also make sure that adverbs, if any, that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm#L76)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm:176** (MEDIUM, Feature Completion)
  - `; XXX we should also make sure that adverbs, if any, that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm#L176)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm:327** (MEDIUM, Feature Completion)
  - `; XXX we should also make sure that adverbs, if any, that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm#L327)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:617** (MEDIUM, Feature Completion)
  - `; XXX this is wrong, it should be PrepositionalRelationshipNode ??? XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm#L617)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:353** (MEDIUM, Feature Completion)
  - `; XXX This rejects sentences with prep phrases, but it should also probably`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm#L353)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm:14** (MEDIUM, Feature Completion)
  - `(let* (; TODO: Should be bias according to the score`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm#L14)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm:16** (MEDIUM, Feature Completion)
  - `; TODO: Should use gen-sentences when new microplanner is ready`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm#L16)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm:156** (MEDIUM, Feature Completion)
  - `; TODO: Should actually clean up the WordNodes instead`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/actions.scm#L156)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm:34** (MEDIUM, Feature Completion)
  - `; FIXME: maybe opencog's internal time octime should`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm#L34)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm:222** (MEDIUM, Feature Completion)
  - `; XXX FIXME -- sentiment analysis should not be done here.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm#L222)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:54** (MEDIUM, Feature Completion)
  - `; XXX FIXME the Microplanner should use the same speech-act types as`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm#L54)

- [ ] **components/integration/opencog/opencog/nlp/irc/whirr-sockets.cc:60** (MEDIUM, Feature Completion)
  - `* it will make the chat server unresponsive. ... XXX this should`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/irc/whirr-sockets.cc#L60)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/helpers.scm:253** (MEDIUM, Feature Completion)
  - `; XXX optimization possible? This makes the whole function O(mn) rather than O(n)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/helpers.scm#L253)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/main.scm:83** (MEDIUM, Feature Completion)
  - `; XXX FIXME utterance-type should be an atom, not a string!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/main.scm#L83)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/main.scm:351** (MEDIUM, Feature Completion)
  - `; XXX could possibly allow choosing different`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/main.scm#L351)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:140** (MEDIUM, Feature Completion)
  - `; XXX FIXME should be changed to just use sha-256 -- that would make it`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L140)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/relex2logic.scm:39** (MEDIUM, Feature Completion)
  - `; XXX maybe this should be part of the ure module??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/relex2logic.scm#L39)

- [ ] **components/integration/opencog/opencog/nlp/scm/type-definitions.scm:21** (MEDIUM, Feature Completion)
  - `; XXX This list could be auto-generated by using`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/type-definitions.scm#L21)

- [ ] **components/integration/opencog/opencog/nlp/sureal/surface-realization.scm:163** (MEDIUM, Feature Completion)
  - `; TODO: There could be too many... skip if seen before?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/sureal/surface-realization.scm#L163)

- [ ] **components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.cc:10** (MEDIUM, Feature Completion)
  - `* XXX In the future, this class should be eliminated, by storing  the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.cc#L10)

- [ ] **components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.h:7** (MEDIUM, Feature Completion)
  - `* XXX In the future, this class should be eliminated, by storing  the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.h#L7)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.cc:115** (MEDIUM, Feature Completion)
  - `// TODO: Should this be a shared ptr to avoid memory leak?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.cc#L115)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.cc:124** (MEDIUM, Feature Completion)
  - `// TODO: Should this be a shared ptr to avoid memory leak?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.cc#L124)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.h:126** (MEDIUM, Feature Completion)
  - `// TODO Should these entries be a member of Rules class?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.h#L126)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiSCM.cc:91** (MEDIUM, Feature Completion)
  - `// TODO: Should this be a singleton? What could be the issues that need`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiSCM.cc#L91)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:169** (MEDIUM, Feature Completion)
  - `(StateLink ; FIXME should use AtTimeLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L169)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:272** (MEDIUM, Feature Completion)
  - `(StateLink ; FIXME should use AtTimeLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L272)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:375** (MEDIUM, Performance)
  - `; todo: Could optimize by only calling rules containing the changed params`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L375)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/utilities.scm:39** (MEDIUM, Performance)
  - `; Todo: Could potential optimize? here by using`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/utilities.scm#L39)

- [ ] **components/language/learn/attic/repair/word-merge.scm:197** (MEDIUM, Feature Completion)
  - `; XXX technically, this is wrong, we should be renaming these...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/repair/word-merge.scm#L197)

- [ ] **components/language/learn/attic/run-ull-2019/SchemeEval.cc:1028** (MEDIUM, Error Handling)
  - `// TODO: it would be nice to pass exceptions on through, but`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-ull-2019/SchemeEval.cc#L1028)

- [ ] **components/language/learn/attic/run-v3/run-common/export-dictionary.scm:17** (MEDIUM, Feature Completion)
  - `; Create singleton classes. XXX This should be done elsewhere!?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-v3/run-common/export-dictionary.scm#L17)

- [ ] **components/language/learn/attic/run-v3/run-common/export-dictionary.scm:26** (MEDIUM, Feature Completion)
  - `; XXX this and above steps should move to `marginals-dict.scm` !?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-v3/run-common/export-dictionary.scm#L26)

- [ ] **components/language/learn/learn-lang-diary/utils/word-cosines.scm:51** (MEDIUM, Feature Completion)
  - `; Should be this: XXX FIXME later`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/word-cosines.scm#L51)

- [ ] **components/language/learn/run-common/attic/export-dictionary.scm:17** (MEDIUM, Feature Completion)
  - `; Create singleton classes. XXX This should be done elsewhere!?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/attic/export-dictionary.scm#L17)

- [ ] **components/language/learn/run-common/attic/export-dictionary.scm:26** (MEDIUM, Feature Completion)
  - `; XXX this and above steps should move to `marginals-dict.scm` !?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/attic/export-dictionary.scm#L26)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:381** (MEDIUM, Feature Completion)
  - `; XXX FIXME: The DONE-LIST should be scrubbed for short junk. That is,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L381)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:680** (MEDIUM, Feature Completion)
  - `; XXX FIXME, should probably use`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L680)

- [ ] **components/language/learn/scm/attic/cluster/gram-pairwise.scm:313** (MEDIUM, Feature Completion)
  - `; XXX TODO this should not be exported, not really.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-pairwise.scm#L313)

- [ ] **components/language/learn/scm/attic/cluster/gram-pairwise.scm:409** (MEDIUM, Feature Completion)
  - `; XXX TODO this should not be exported, not really.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-pairwise.scm#L409)

- [ ] **components/language/learn/scm/attic/cluster/gram-pairwise.scm:494** (MEDIUM, Feature Completion)
  - `; XXX TODO this should not be exported, not really.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-pairwise.scm#L494)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:94** (MEDIUM, Feature Completion)
  - `; XXX It would be nicer if we could avoid creating the ListLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L94)

- [ ] **components/language/learn/scm/attic/pair-count-new/word-pair-count.scm:59** (MEDIUM, Feature Completion)
  - `; Not implemented: a count of the length of a link. This could be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/pair-count-new/word-pair-count.scm#L59)

- [ ] **components/language/learn/scm/attic/pair-count-new/word-pair-count.scm:121** (MEDIUM, Feature Completion)
  - `; XXX TODO: this should probably be converted to an 1xN matrix`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/pair-count-new/word-pair-count.scm#L121)

- [ ] **components/language/learn/scm/attic/pair-count/clique-pair-count.scm:22** (MEDIUM, Feature Completion)
  - `; XXX FIXME we should probably not store this way. We should probably`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/pair-count/clique-pair-count.scm#L22)

- [ ] **components/language/learn/scm/attic/singletons.scm:5** (MEDIUM, Feature Completion)
  - `; XXX This is not currently used, and should maybe be deleted.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/singletons.scm#L5)

- [ ] **components/language/learn/scm/gram-class/gram-class-api.scm:133** (MEDIUM, Feature Completion)
  - `; XXX FIXME the semantics of this thing is ugly, and should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-class-api.scm#L133)

- [ ] **components/language/learn/scm/gram-class/gram-majority.scm:205** (MEDIUM, Feature Completion)
  - `; XXX TODO this should be either`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-majority.scm#L205)

- [ ] **components/language/learn/scm/gram-class/gram-majority.scm:465** (MEDIUM, Feature Completion)
  - `; XXX It would be nice to preserve history somehow, but how?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-majority.scm#L465)

- [ ] **components/language/learn/scm/gram-class/shape-project.scm:307** (MEDIUM, Feature Completion)
  - `; XXX TODO -- generic deletion should be moved to a method`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/shape-project.scm#L307)

- [ ] **components/language/learn/scm/gram-class/shape-vec.scm:109** (MEDIUM, Feature Completion)
  - `; TODO: with appropriate cleanup, this probably should be moved`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/shape-vec.scm#L109)

- [ ] **components/language/learn/scm/gram-class/shape-vec.scm:583** (MEDIUM, Feature Completion)
  - `; XXX FIXME: we should give the star-wild a unique name,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/shape-vec.scm#L583)

- [ ] **components/language/learn/scm/pipe-parse/pipe-count.scm:195** (MEDIUM, Feature Completion)
  - `; NOTE Hack to fetch sentence count from storage. NOTE we should not`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pipe-parse/pipe-count.scm#L195)

- [ ] **components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:490** (MEDIUM, Feature Completion)
  - `XXX TODO: In principle, this could be accomplished by lowering the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm#L490)

- [ ] **unify/opencog/unify/Unify.h:188** (MEDIUM, Feature Completion)
  - `// TODO: maybe we could simplify a great deal of code by replacing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L188)

- [ ] **ure/opencog/ure/BetaDistribution.cc:33** (MEDIUM, Feature Completion)
  - `// TODO should be replaced by tv->get_mode() once implemented`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/BetaDistribution.cc#L33)

- [ ] **ure/opencog/ure/Rule.cc:58** (MEDIUM, Performance)
  - `// TODO: could certainly be optimized by not systematically`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.cc#L58)

- [ ] **ure/opencog/ure/Rule.h:266** (MEDIUM, Feature Completion)
  - `* TODO: probably obsolete, should be removed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L266)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:72** (MEDIUM, Feature Completion)
  - `// TODO: Maybe this should be moved to BackwardChainer`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L72)

- [ ] **ure/opencog/ure/backwardchainer/BackwardChainer.cc:288** (MEDIUM, Feature Completion)
  - `// TODO: Maybe we could take advantage of the new read-only`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BackwardChainer.cc#L288)

- [ ] **ure/opencog/ure/backwardchainer/TraceRecorder.h:94** (MEDIUM, Feature Completion)
  - `// TODO: the TV on the evaluation link should be more carefully`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/TraceRecorder.h#L94)

- [ ] **analyze_fixme_instances.py:3** (LOW, Feature Completion)
  - `FIXME Instance Analysis and Categorization Tool`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L3)

- [ ] **analyze_fixme_instances.py:5** (LOW, Feature Completion)
  - `This script analyzes all FIXME instances in the OpenCog Unified repository`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L5)

- [ ] **analyze_fixme_instances.py:28** (LOW, Feature Completion)
  - `class FIXMEInstance:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L28)

- [ ] **analyze_fixme_instances.py:31** (LOW, Feature Completion)
  - `fixme_text: str`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L31)

- [ ] **analyze_fixme_instances.py:43** (LOW, Feature Completion)
  - `class FIXMEAnalyzer:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L43)

- [ ] **analyze_fixme_instances.py:46** (LOW, Feature Completion)
  - `self.fixme_instances = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L46)

- [ ] **analyze_fixme_instances.py:53** (LOW, Feature Completion)
  - `'simple_todo': r'simple|easy|quick|straightforward|minor|small',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L53)

- [ ] **analyze_fixme_instances.py:58** (LOW, Feature Completion)
  - `def extract_fixme_instances(self):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L58)

- [ ] **analyze_fixme_instances.py:59** (LOW, Feature Completion)
  - `"""Extract all FIXME instances from source files."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L59)

- [ ] **analyze_fixme_instances.py:66** (LOW, Feature Completion)
  - `print(f"Found {len(self.fixme_instances)} FIXME instances")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L66)

- [ ] **analyze_fixme_instances.py:69** (LOW, Feature Completion)
  - `"""Process a single file for FIXME instances."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L69)

- [ ] **analyze_fixme_instances.py:75** (LOW, Feature Completion)
  - `if self._is_fixme_line(line):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L75)

- [ ] **analyze_fixme_instances.py:80** (LOW, Feature Completion)
  - `fixme_instance = FIXMEInstance(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L80)

- [ ] **analyze_fixme_instances.py:83** (LOW, Feature Completion)
  - `fixme_text=line.strip(),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L83)

- [ ] **analyze_fixme_instances.py:87** (LOW, Feature Completion)
  - `self.fixme_instances.append(fixme_instance)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L87)

- [ ] **analyze_fixme_instances.py:92** (LOW, Feature Completion)
  - `def _is_fixme_line(self, line: str) -> bool:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L92)

- [ ] **analyze_fixme_instances.py:93** (LOW, Documentation)
  - `"""Check if a line contains a FIXME comment."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L93)

- [ ] **analyze_fixme_instances.py:103** (LOW, Documentation)
  - `# Skip meta-comments about FIXME processing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L103)

- [ ] **analyze_fixme_instances.py:104** (LOW, Feature Completion)
  - `if ('fixme instances' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L104)

- [ ] **analyze_fixme_instances.py:105** (LOW, Feature Completion)
  - `('fixme text' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L105)

- [ ] **analyze_fixme_instances.py:106** (LOW, Pattern Matching)
  - `('todo/fixme' in line_lower and ('pattern' in line_lower or 'report' in line_lower or 'verification'...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L106)

- [ ] **analyze_fixme_instances.py:107** (LOW, Feature Completion)
  - `('analyze' in line_lower and 'fixme' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L107)

- [ ] **analyze_fixme_instances.py:108** (LOW, Feature Completion)
  - `('catalog' in line_lower and 'fixme' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L108)

- [ ] **analyze_fixme_instances.py:109** (LOW, Feature Completion)
  - `('contain' in line_lower and 'todo/fixme' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L109)

- [ ] **analyze_fixme_instances.py:110** (LOW, Feature Completion)
  - `('check' in line_lower and 'todo/fixme' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L110)

- [ ] **analyze_fixme_instances.py:113** (LOW, Feature Completion)
  - `('for other cases, convert fixme to descriptive' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L113)

- [ ] **analyze_fixme_instances.py:114** (LOW, Feature Completion)
  - `('pre-commit hook to prevent new fixmes' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L114)

- [ ] **analyze_fixme_instances.py:129** (LOW, Feature Completion)
  - `# Must not be metadata about FIXME processing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L129)

- [ ] **analyze_fixme_instances.py:130** (LOW, Feature Completion)
  - `if ('clean up' in line_lower and 'fixme' in line_lower) or \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L130)

- [ ] **analyze_fixme_instances.py:133** (LOW, Feature Completion)
  - `('metadata about fixme' in line_lower):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L133)

- [ ] **analyze_fixme_instances.py:136** (LOW, Pattern Matching)
  - `# Look for actual FIXME patterns in comments`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L136)

- [ ] **analyze_fixme_instances.py:137** (LOW, Feature Completion)
  - `return (('fixme' in line_lower) or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L137)

- [ ] **analyze_fixme_instances.py:138** (LOW, Documentation)
  - `('xxx' in line_lower and (line_lower.strip().startswith('#') or line_lower.strip().startswith('//'))...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L138)

- [ ] **analyze_fixme_instances.py:139** (LOW, Feature Completion)
  - `('xxx' in line_lower and ('fix' in line_lower or 'todo' in line_lower)) or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L139)

- [ ] **analyze_fixme_instances.py:140** (LOW, Feature Completion)
  - `('todo' in line_lower and 'fixme' in line_lower))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L140)

- [ ] **analyze_fixme_instances.py:143** (LOW, Feature Completion)
  - `"""Categorize each FIXME instance by difficulty."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L143)

- [ ] **analyze_fixme_instances.py:144** (LOW, Feature Completion)
  - `for instance in self.fixme_instances:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L144)

- [ ] **analyze_fixme_instances.py:147** (LOW, Feature Completion)
  - `def _analyze_instance(self, instance: FIXMEInstance):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L147)

- [ ] **analyze_fixme_instances.py:148** (LOW, Feature Completion)
  - `"""Analyze a single FIXME instance to determine difficulty."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L148)

- [ ] **analyze_fixme_instances.py:149** (LOW, Feature Completion)
  - `full_text = (instance.fixme_text + ' ' + ' '.join(instance.context_lines)).lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L149)

- [ ] **analyze_fixme_instances.py:168** (LOW, Pattern Matching)
  - `def _classify_difficulty(self, instance: FIXMEInstance, patterns: Dict, full_text: str):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L168)

- [ ] **analyze_fixme_instances.py:169** (LOW, Feature Completion)
  - `"""Classify the difficulty of implementing the FIXME."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L169)

- [ ] **analyze_fixme_instances.py:195** (LOW, Feature Completion)
  - `'not implemented' in full_text or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L195)

- [ ] **analyze_fixme_instances.py:202** (LOW, Pattern Matching)
  - `patterns.get('simple_todo', 0) >= 1 or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L202)

- [ ] **analyze_fixme_instances.py:215** (LOW, Feature Completion)
  - `if 'stub' in full_text or 'placeholder' in full_text or 'mock' in full_text:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L215)

- [ ] **analyze_fixme_instances.py:216** (LOW, Feature Completion)
  - `return "MEDIUM", "Stub Implementation", "1-3 weeks", \`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L216)

- [ ] **analyze_fixme_instances.py:217** (LOW, Feature Completion)
  - `"Replace stub/placeholder with real implementation"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L217)

- [ ] **analyze_fixme_instances.py:223** (LOW, Feature Completion)
  - `"""Generate a comprehensive report of FIXME instances."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L223)

- [ ] **analyze_fixme_instances.py:229** (LOW, Feature Completion)
  - `for instance in self.fixme_instances:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L229)

- [ ] **analyze_fixme_instances.py:236** (LOW, Feature Completion)
  - `'total_instances': len(self.fixme_instances),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L236)

- [ ] **analyze_fixme_instances.py:261** (LOW, Feature Completion)
  - `analyzer = FIXMEAnalyzer(repo_root)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L261)

- [ ] **analyze_fixme_instances.py:263** (LOW, Feature Completion)
  - `print("Extracting FIXME instances...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L263)

- [ ] **analyze_fixme_instances.py:264** (LOW, Feature Completion)
  - `analyzer.extract_fixme_instances()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L264)

- [ ] **analyze_fixme_instances.py:270** (LOW, Feature Completion)
  - `report = analyzer.save_report("fixme_analysis_report.json")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L270)

- [ ] **analyze_fixme_instances.py:274** (LOW, Feature Completion)
  - `print("FIXME ANALYSIS SUMMARY")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L274)

- [ ] **analyze_fixme_instances.py:276** (LOW, Feature Completion)
  - `print(f"Total FIXME instances found: {report['summary']['total_instances']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_fixme_instances.py#L276)

- [ ] **analyze_issue_examples.py:3** (LOW, Testing)
  - `Process the specific FIXME instances mentioned in the issue description`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L3)

- [ ] **analyze_issue_examples.py:10** (LOW, Testing)
  - `"""Process the specific FIXME examples from the issue description."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L10)

- [ ] **analyze_issue_examples.py:13** (LOW, Feature Completion)
  - `"./atomspace/examples/atomspace/queue.scm:; XXX FIXME, this example is not yet complete and does not...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L13)

- [ ] **analyze_issue_examples.py:15** (LOW, Feature Completion)
  - `"./atomspace/opencog/atomspace/AtomTable.cc:    // atom in the parent. What??? XXX NOT TRUE FIXME",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L15)

- [ ] **analyze_issue_examples.py:16** (LOW, Feature Completion)
  - `"./atomspace/opencog/atomspace/AtomSpace.cc:	// Fixme maybe later someday, if/when this is needed.",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L16)

- [ ] **analyze_issue_examples.py:17** (LOW, Feature Completion)
  - `"./atomspace/opencog/atomspace/AtomSpace.cc:// XXX FIXME -- The recursive design of the depth() rout...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L17)

- [ ] **analyze_issue_examples.py:19** (LOW, Feature Completion)
  - `"./atomspace/opencog/cython/PythonEval.cc:    // XXX FIXME this does a lot of wasteful string copyin...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L19)

- [ ] **analyze_issue_examples.py:20** (LOW, Feature Completion)
  - `"./atomspace/opencog/cython/PyIncludeWrapper.h:// 0.15.1 and maybe other versions)  FIXME someday......`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L20)

- [ ] **analyze_issue_examples.py:22** (LOW, Pattern Matching)
  - `"./atomspace/opencog/haskell/PatternMatcher_CWrapper.h: * XXX FIXME: atoms must never be accessed by...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L22)

- [ ] **analyze_issue_examples.py:23** (LOW, Feature Completion)
  - `"./atomspace/opencog/ocaml/CamlWrap.cc:	// XXX FIXME",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L23)

- [ ] **analyze_issue_examples.py:24** (LOW, Feature Completion)
  - `"./atomspace/opencog/guile/SchemeSmobAS.cc: * until a better permission system is invented. XXX FIXM...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L24)

- [ ] **analyze_issue_examples.py:25** (LOW, Feature Completion)
  - `"./atomspace/opencog/guile/modules/ExecSCM.cc:// XXX FIXME: can we fix cython to not do this, alread...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L25)

- [ ] **analyze_issue_examples.py:26** (LOW, Feature Completion)
  - `"./atomspace/opencog/guile/SchemeSmobAtom.cc:// XXX FIXME. Work around the despicable, horrible guil...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L26)

- [ ] **analyze_issue_examples.py:27** (LOW, Feature Completion)
  - `"./atomspace/opencog/guile/SchemeSmobValue.cc: * XXX FIXME Clearly, a factory for values is called f...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L27)

- [ ] **analyze_issue_examples.py:28** (LOW, Feature Completion)
  - `"./atomspace/opencog/guile/SchemeEval.cc:	// XXX FIXME This lock is not needed, because in guile-2.2...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L28)

- [ ] **analyze_issue_examples.py:29** (LOW, Feature Completion)
  - `"./atomspace/opencog/sheaf/attic/linear-parser.scm:  XXX FIXME WARNING DANGER: As written, this runs...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L29)

- [ ] **analyze_issue_examples.py:30** (LOW, Pattern Matching)
  - `"./atomspace/opencog/atoms/pattern/PatternUtils.cc:		// XXX FIXME Are the below needed?",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L30)

- [ ] **analyze_issue_examples.py:31** (LOW, Pattern Matching)
  - `"./atomspace/opencog/atoms/pattern/BindLink.cc:	// Shoot. XXX FIXME. Most of the unit tests require ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L31)

- [ ] **analyze_issue_examples.py:32** (LOW, Pattern Matching)
  - `"./atomspace/opencog/atoms/pattern/PatternLink.cc:		// XXX FIXME, more correct would be to loop over...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L32)

- [ ] **analyze_issue_examples.py:34** (LOW, Error Handling)
  - `"./atomspace/opencog/atoms/core/TypeChoice.cc:		// For now, just avoid throwing an exception. XXX FI...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L34)

- [ ] **analyze_issue_examples.py:35** (LOW, Feature Completion)
  - `"./atomspace/opencog/atoms/core/RandomChoice.cc:// XXX FIXME - fix this so it can also choose a sing...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L35)

- [ ] **analyze_issue_examples.py:36** (LOW, Error Handling)
  - `"./atomspace/opencog/atoms/core/Variables.cc:	// XXX FIXME URE calls us with broken handle!!",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L36)

- [ ] **analyze_issue_examples.py:37** (LOW, Feature Completion)
  - `"./atomspace/opencog/atoms/core/TypeUtils.cc:				\"Not implemented! TODO XXX FIXME\");",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L37)

- [ ] **analyze_issue_examples.py:39** (LOW, Feature Completion)
  - `"./atomspace/opencog/atoms/execution/Instantiator.cc:/// cleanly separated from each other. (XXX FIX...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L39)

- [ ] **analyze_issue_examples.py:40** (LOW, Error Handling)
  - `"./atomspace/opencog/atoms/join/JoinLink.cc:/// i.e. to use UnorderedHandleSet instead of HandleSet....`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L40)

- [ ] **analyze_issue_examples.py:41** (LOW, Feature Completion)
  - `"./atomspace/opencog/atoms/flow/FilterLink.cc:						\"Globbing for Values not implemented! FIXME!\")...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L41)

- [ ] **analyze_issue_examples.py:43** (LOW, Feature Completion)
  - `"./atomspace/opencog/query/RewriteMixin.cc:	// See issue #950 and pull req #962. XXX FIXME later.",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L43)

- [ ] **analyze_issue_examples.py:44** (LOW, Pattern Matching)
  - `"./atomspace/opencog/query/PatternMatchEngine.cc:/// XXX FIXME: this is currently a weak stop-gap me...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L44)

- [ ] **analyze_issue_examples.py:106** (LOW, Feature Completion)
  - `markdown = """# FIXME Instances from Issue #74 - Sorted by Implementation Difficulty`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L106)

- [ ] **analyze_issue_examples.py:108** (LOW, Testing)
  - `This document analyzes the specific FIXME instances mentioned in issue #74, categorized by implement...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L108)

- [ ] **analyze_issue_examples.py:182** (LOW, Testing)
  - `*This analysis focuses on the specific FIXME instances mentioned in issue #74. For a complete catalo...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L182)

- [ ] **analyze_issue_examples.py:191** (LOW, Feature Completion)
  - `with open('ISSUE-74-FIXME-ANALYSIS.md', 'w') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L191)

- [ ] **analyze_issue_examples.py:194** (LOW, Feature Completion)
  - `print("Generated ISSUE-74-FIXME-ANALYSIS.md")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L194)

- [ ] **analyze_issue_examples.py:195** (LOW, Testing)
  - `print("Focused analysis of specific FIXME instances from the issue")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/analyze_issue_examples.py#L195)

- [ ] **categorize_placeholders.py:3** (LOW, Feature Completion)
  - `Categorize placeholders by implementability`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L3)

- [ ] **categorize_placeholders.py:8** (LOW, Feature Completion)
  - `def categorize_by_implementability(placeholders):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L8)

- [ ] **categorize_placeholders.py:9** (LOW, Feature Completion)
  - `"""Categorize placeholders into actionable vs non-actionable"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L9)

- [ ] **categorize_placeholders.py:16** (LOW, Feature Completion)
  - `for p in placeholders:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L16)

- [ ] **categorize_placeholders.py:45** (LOW, Feature Completion)
  - `]) and 'not implemented' not in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L45)

- [ ] **categorize_placeholders.py:51** (LOW, Feature Completion)
  - `'temp', 'temporary', 'hack', 'workaround', 'kludge'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L51)

- [ ] **categorize_placeholders.py:77** (LOW, Feature Completion)
  - `'remove', 'delete', 'clean up', 'temp', 'hack', 'workaround',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L77)

- [ ] **categorize_placeholders.py:96** (LOW, Feature Completion)
  - `# Load placeholder analysis`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L96)

- [ ] **categorize_placeholders.py:97** (LOW, Feature Completion)
  - `with open('placeholder_analysis.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L97)

- [ ] **categorize_placeholders.py:100** (LOW, Feature Completion)
  - `placeholders = data['detailed_placeholders']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L100)

- [ ] **categorize_placeholders.py:103** (LOW, Feature Completion)
  - `categories = categorize_by_implementability(placeholders)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L103)

- [ ] **categorize_placeholders.py:111** (LOW, Feature Completion)
  - `'total': len(placeholders),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L111)

- [ ] **categorize_placeholders.py:125** (LOW, Feature Completion)
  - `with open('placeholder_categorization.json', 'w') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L125)

- [ ] **categorize_placeholders.py:128** (LOW, Feature Completion)
  - `print("Placeholder Categorization Report")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L128)

- [ ] **categorize_placeholders.py:130** (LOW, Feature Completion)
  - `print(f"Total Placeholders: {report['summary']['total']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/categorize_placeholders.py#L130)

- [ ] **cmake/FindHyperTable.cmake:54** (LOW, Feature Completion)
  - `# XXX Unclear -- do we need to find *all* of these?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/FindHyperTable.cmake#L54)

- [ ] **cmake/FindProtobuf.cmake:188** (LOW, Feature Completion)
  - `# TODO clean the PROTOROOT so that it does not form a regex itself?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/FindProtobuf.cmake#L188)

- [ ] **cmake/FindTBB.cmake:77** (LOW, Feature Completion)
  - `# Todo: add other Windows compilers such as ICL.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/FindTBB.cmake#L77)

- [ ] **cmake/OpenCogFindPython.cmake:55** (LOW, Feature Completion)
  - `# This is a hack due to the distutils in debian/ubuntu's`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogFindPython.cmake#L55)

- [ ] **cmake/OpenCogGccOptions.cmake:33** (LOW, Feature Completion)
  - `# XXX disable for now ... its just to painful, in daily life.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGccOptions.cmake#L33)

- [ ] **cmake/OpenCogGccOptions.cmake:85** (LOW, Feature Completion)
  - `# So disable, by default; MOSES users will need to hack this.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGccOptions.cmake#L85)

- [ ] **cmake/OpenCogGenOCamlTypes.cmake:141** (LOW, Feature Completion)
  - `# XXX FIXME LATER`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGenOCamlTypes.cmake#L141)

- [ ] **cmake/OpenCogGenOCamlTypes.cmake:147** (LOW, Feature Completion)
  - `# XXX FIXME LATER`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGenOCamlTypes.cmake#L147)

- [ ] **cmake/OpenCogGenPythonTypes.cmake:41** (LOW, Feature Completion)
  - `# XXX This is broken in two ways. First, createValue() is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGenPythonTypes.cmake#L41)

- [ ] **cmake/OpenCogGenPythonTypes.cmake:44** (LOW, Feature Completion)
  - `# so this is a no-op, anyway. XXX FIXME someday, I guess`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGenPythonTypes.cmake#L44)

- [ ] **cmake/OpenCogGenPythonTypes.cmake:87** (LOW, Feature Completion)
  - `# This is kind of hacky, but I don't know what else to do ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGenPythonTypes.cmake#L87)

- [ ] **cmake/OpenCogGenTypes.cmake:86** (LOW, Feature Completion)
  - `# hacky, but is needed for e.g. "VariableList" ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGenTypes.cmake#L86)

- [ ] **cmake/OpenCogGuile.cmake:140** (LOW, Feature Completion)
  - `# modules is more than two deep e.g `(opencog foo bar)` XXX FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogGuile.cmake#L140)

- [ ] **cmake/OpenCogLibOptions.cmake:10** (LOW, Error Handling)
  - `# Small hack to handle unixes that use "/usr/lib64" instead of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/OpenCogLibOptions.cmake#L10)

- [ ] **cmake/UseOCaml.cmake:91** (LOW, Feature Completion)
  - `# TODO : see if it is possible to call the dependency generator at compile time`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cmake/UseOCaml.cmake#L91)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/behavior-rules.scm:35** (LOW, Pattern Matching)
  - `; TODO: This one only works currently with single word matches to the globs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/behavior-rules.scm#L35)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:16** (LOW, Feature Completion)
  - `; TODO: probably can take out eva-behavior module when express.scm is moved out`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L16)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:30** (LOW, Pattern Matching)
  - `; TODO: multiple globnode values are not matching (Github issue created #724)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L30)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:35** (LOW, Feature Completion)
  - `; Todo: Or / Choice links not working in antecedant for recongition, so doing a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L35)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:121** (LOW, Feature Completion)
  - `; this is hacky.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L121)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:140** (LOW, Pattern Matching)
  - `; TODO: Match on multiple conditions (ie using OR)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L140)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:195** (LOW, Feature Completion)
  - `; TODO: Create feedback response that something was learned`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L195)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:216** (LOW, Feature Completion)
  - `; TODO: move to util file`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L216)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:230** (LOW, Feature Completion)
  - `; TODO: ask linas about these`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L230)

- [ ] **components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm:234** (LOW, Feature Completion)
  - `; TODO: let's try cog-get-partner instead`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/attic/chatbot-train/simple-training.scm#L234)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:19** (LOW, Feature Completion)
  - `; TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L19)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:37** (LOW, Feature Completion)
  - `; the concept blending code, or a hack of the MOSES knob-turning and`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L37)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:147** (LOW, Feature Completion)
  - `;; XXX Needs to be replaced by OpenPsi emotional state modelling.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L147)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:228** (LOW, Feature Completion)
  - `(RandomChoice (Put; FIXME Replace by multiple face tracking.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L228)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:232** (LOW, Feature Completion)
  - `;TODO: Separate out room-state into separate demands that occur before`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L232)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:242** (LOW, Feature Completion)
  - `;; XXX TODO -- if interacting for a while`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L242)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:378** (LOW, Feature Completion)
  - `; XXX incomplete!  need the face study saccade stuff...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L378)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:428** (LOW, Feature Completion)
  - `;; XXX question: This is turning the whole head; perhaps we`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L428)

- [ ] **components/integration/opencog/opencog/eva/behavior/behavior.scm:763** (LOW, Feature Completion)
  - `; XXX FIXME ... this means that this rule is c alled too often!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/behavior.scm#L763)

- [ ] **components/integration/opencog/opencog/eva/behavior/cfg-eva.scm:244** (LOW, Feature Completion)
  - `;; XXX Right now, search for attention turns the whole head;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/cfg-eva.scm#L244)

- [ ] **components/integration/opencog/opencog/eva/behavior/cfg-sophia.scm:365** (LOW, Feature Completion)
  - `;; XXX Right now, search for attention turns the whole head;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/cfg-sophia.scm#L365)

- [ ] **components/integration/opencog/opencog/eva/behavior/express.scm:14** (LOW, Feature Completion)
  - `; (not implemented yet).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/express.scm#L14)

- [ ] **components/integration/opencog/opencog/eva/behavior/face-priority.scm:6** (LOW, Feature Completion)
  - `; TODO: Make the set, get, & delete of properites into a utility template`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/face-priority.scm#L6)

- [ ] **components/integration/opencog/opencog/eva/behavior/face-priority.scm:124** (LOW, Feature Completion)
  - `; TODO: Move to config file`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/face-priority.scm#L124)

- [ ] **components/integration/opencog/opencog/eva/behavior/face-priority.scm:131** (LOW, Feature Completion)
  - `; FIXME But why have this plane instead of calculating the distance between`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/face-priority.scm#L131)

- [ ] **components/integration/opencog/opencog/eva/behavior/face-priority.scm:192** (LOW, Feature Completion)
  - `;; XXX refactor all of this -- this si supposed to be a service provided`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/face-priority.scm#L192)

- [ ] **components/integration/opencog/opencog/eva/behavior/face-priority.scm:224** (LOW, Feature Completion)
  - `; FIXME: Sometimes d is larger than the width-of-yz-plane`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/face-priority.scm#L224)

- [ ] **components/integration/opencog/opencog/eva/behavior/movement-api.scm:20** (LOW, Feature Completion)
  - `; Printer stub -- Prints to the opencog log file.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/movement-api.scm#L20)

- [ ] **components/integration/opencog/opencog/eva/behavior/movement-api.scm:64** (LOW, Feature Completion)
  - `; Create a definition that is just a stub.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/movement-api.scm#L64)

- [ ] **components/integration/opencog/opencog/eva/behavior/movement-api.scm:73** (LOW, Feature Completion)
  - `; XXX FIXME: these record the animation that was chosen, and a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/movement-api.scm#L73)

- [ ] **components/integration/opencog/opencog/eva/behavior/movement-api.scm:192** (LOW, Feature Completion)
  - `; Cheap hack to allow external ROS nodes to know what we are doing.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/movement-api.scm#L192)

- [ ] **components/integration/opencog/opencog/eva/behavior/orchestrate.scm:48** (LOW, Feature Completion)
  - `; XXX FIXME: this records the animation that was chosen, and a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/orchestrate.scm#L48)

- [ ] **components/integration/opencog/opencog/eva/behavior/orchestrate.scm:137** (LOW, Feature Completion)
  - `; XXX FIXME, this is still broken during search for attention.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/orchestrate.scm#L137)

- [ ] **components/integration/opencog/opencog/eva/behavior/orchestrate.scm:222** (LOW, Feature Completion)
  - `; XXX Currently, this does nothing at all. Some future version may`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/orchestrate.scm#L222)

- [ ] **components/integration/opencog/opencog/eva/behavior/primitives.scm:168** (LOW, Feature Completion)
  - `; automated. XXX TODO. (Can we get ECAN to do this for us?)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/primitives.scm#L168)

- [ ] **components/integration/opencog/opencog/eva/behavior/psi-behavior.scm:20** (LOW, Feature Completion)
  - `; TODO: make generic for orchestration.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/psi-behavior.scm#L20)

- [ ] **components/integration/opencog/opencog/eva/behavior/psi-behavior.scm:82** (LOW, Feature Completion)
  - `; TODO: test the behabior when talking.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/behavior/psi-behavior.scm#L82)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm:12** (LOW, Feature Completion)
  - `;; XXX FIXME this is a cut-n-paste job from process-query`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm#L12)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm:28** (LOW, Feature Completion)
  - `;; XXX FIXME -- remove the IRC debug response below.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm#L28)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm:40** (LOW, Feature Completion)
  - `; XXX Currently, this dispatch is done via scheme code below. The`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/bot-api.scm#L40)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm:79** (LOW, Feature Completion)
  - `Yes, this is a quick hack, needs fixing. XXX FIXME.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm#L79)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm:89** (LOW, Feature Completion)
  - `XXX this may be junk/obsolete, the format of r2l-sets seems to have`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm#L89)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm:121** (LOW, Feature Completion)
  - `; XXX These are not being used right now; these are meant to be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative-alt.scm#L121)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative-rules.scm:45** (LOW, Feature Completion)
  - `; XXX needs to be public, so that cog-execute! can find this...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative-rules.scm#L45)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative-rules.scm:224** (LOW, Feature Completion)
  - `; XXX TODO Design notes:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative-rules.scm#L224)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm:76** (LOW, Feature Completion)
  - `; Stove-pipe hack to perform an action associated with an imperative.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm#L76)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm:142** (LOW, Feature Completion)
  - `; XXX FIXME we need a better way of marking actions as having`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm#L142)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm:148** (LOW, Feature Completion)
  - `; XXX replace the dont-know reply by ChatScript or something.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/imperative.scm#L148)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:73** (LOW, Feature Completion)
  - `; XXX This is incorrect, just right now; its too simple, and interacts`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L73)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:79** (LOW, Feature Completion)
  - `; to hook this up.  XXX FIXME.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L79)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:258** (LOW, Feature Completion)
  - `; XXX FIXME -- Implement-me, actually -- need to do the above, but for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L258)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:359** (LOW, Feature Completion)
  - `; automated via synonymous phrase support. Total hack, needs fixing.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L359)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:399** (LOW, Feature Completion)
  - `; XXX FIXME ... the list below is duplicated twice, once as adjectives`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L399)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:404** (LOW, Feature Completion)
  - `; XXX FIXME -- this list contains lots of synonyms; needs to be replaced`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L404)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm:407** (LOW, Feature Completion)
  - `; XXX Note that some synonyms have multiple "meanings" e.g. "anguish"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/knowledge.scm#L407)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:51** (LOW, Feature Completion)
  - `; XXX FIXME This is wrong; this has been replaced by the eva-model`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm#L51)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:95** (LOW, Feature Completion)
  - `; NOTE hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm#L95)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:178** (LOW, Feature Completion)
  - `; XXX This is broken.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm#L178)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm:220** (LOW, Feature Completion)
  - `; openpsi to pick one. XXX FIXME -- do this.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/model-query.scm#L220)

- [ ] **components/integration/opencog/opencog/eva/chatbot-eva/run-chatbot.scm:29** (LOW, Feature Completion)
  - `; NOTE temp hack to run in module context, for debugging`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/chatbot-eva/run-chatbot.scm#L29)

- [ ] **components/integration/opencog/opencog/eva/model/faces.scm:5** (LOW, Feature Completion)
  - `; XXX most of face-tracking is now in self-model.scm`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/faces.scm#L5)

- [ ] **components/integration/opencog/opencog/eva/model/faces.scm:6** (LOW, Feature Completion)
  - `; Perhaps this file is not needed any more? XXX FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/faces.scm#L6)

- [ ] **components/integration/opencog/opencog/eva/model/faces.scm:11** (LOW, Feature Completion)
  - `;; XXX FIXME: This file defines a "Room State", which currently can`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/faces.scm#L11)

- [ ] **components/integration/opencog/opencog/eva/model/faces.scm:74** (LOW, Feature Completion)
  - `Debug utility - Quick hack to fill the room.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/faces.scm#L74)

- [ ] **components/integration/opencog/opencog/eva/model/faces.scm:117** (LOW, Feature Completion)
  - `Quick hack to remove face ID from the room`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/faces.scm#L117)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:43** (LOW, Feature Completion)
  - `; XXX FIXME There are a bunch of define-publics in here, they probably`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L43)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:173** (LOW, Feature Completion)
  - `; NOTE quick hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L173)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:254** (LOW, Feature Completion)
  - `; XXX FIXME: Note also: we currently fail to distinguish the affect`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L254)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:669** (LOW, Feature Completion)
  - `XXX FIXME this is a nasty ugly hack, and shold be replaced by`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L669)

- [ ] **components/integration/opencog/opencog/eva/model/self-model.scm:931** (LOW, Feature Completion)
  - `; XXX Double-check that the "New arrivals" list is non-empty;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/self-model.scm#L931)

- [ ] **components/integration/opencog/opencog/eva/model/time-map.scm:38** (LOW, Feature Completion)
  - `; XXX FIXME Is it wise to start this, just because the guile module got`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/time-map.scm#L38)

- [ ] **components/integration/opencog/opencog/eva/model/time-map.scm:196** (LOW, Feature Completion)
  - `;; XXX FIXME -- this kind of tulity needs to be in the space-time`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/model/time-map.scm#L196)

- [ ] **components/integration/opencog/opencog/eva/src/btree-eva-mute.scm:50** (LOW, Feature Completion)
  - `; Run the hacky garbage collection loop.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/btree-eva-mute.scm#L50)

- [ ] **components/integration/opencog/opencog/eva/src/btree-eva.scm:76** (LOW, Feature Completion)
  - `; Run the hacky garbage collection loop.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/btree-eva.scm#L76)

- [ ] **components/integration/opencog/opencog/eva/src/btree.scm:4** (LOW, Feature Completion)
  - `; NOTE: FIXME ... I think this blob of code is obsolete ... I think`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/btree.scm#L4)

- [ ] **components/integration/opencog/opencog/eva/src/btree.scm:88** (LOW, Feature Completion)
  - `; XXX Need to ... auuh load the chatbot...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/btree.scm#L88)

- [ ] **components/integration/opencog/opencog/eva/src/btree.scm:96** (LOW, Feature Completion)
  - `; Run the hacky garbage collection loop.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/btree.scm#L96)

- [ ] **components/integration/opencog/opencog/eva/src/old-tree.scm:50** (LOW, Feature Completion)
  - `;; XXX FIXME chatbot is disengaged from everything else.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/old-tree.scm#L50)

- [ ] **components/integration/opencog/opencog/eva/src/psi-dynamics.scm:68** (LOW, Feature Completion)
  - `; XXX FIXME -- this is hacky -- and has multiple design flaws.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/psi-dynamics.scm#L68)

- [ ] **components/integration/opencog/opencog/eva/src/psi-dynamics.scm:188** (LOW, Feature Completion)
  - `; TODO Replace with openpsi-dynamics logger when it is available.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/psi-dynamics.scm#L188)

- [ ] **components/integration/opencog/opencog/eva/src/psi-dynamics.scm:203** (LOW, Feature Completion)
  - `; TODO Replace with openpsi-dynamics logger when it is available.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/eva/src/psi-dynamics.scm#L203)

- [ ] **components/integration/opencog/opencog/ghost/cs-parse.scm:75** (LOW, Feature Completion)
  - `; FIXME: This is not really newline.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/cs-parse.scm#L75)

- [ ] **components/integration/opencog/opencog/ghost/cs-parse.scm:90** (LOW, Feature Completion)
  - `; TODO Add tester function for this`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/cs-parse.scm#L90)

- [ ] **components/integration/opencog/opencog/ghost/cs-parse.scm:139** (LOW, Feature Completion)
  - `; TODO Maybe replace with dictionary keyword sets then process it on action?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/cs-parse.scm#L139)

- [ ] **components/integration/opencog/opencog/ghost/cs-parse.scm:298** (LOW, Feature Completion)
  - `(SAMPLE_INPUT) : #f ; TODO replace with a tester function`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/cs-parse.scm#L298)

- [ ] **components/integration/opencog/opencog/ghost/cs-parse.scm:710** (LOW, Pattern Matching)
  - `; TODO: This has a restart_matching effect. See chatscript documentation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/cs-parse.scm#L710)

- [ ] **components/integration/opencog/opencog/ghost/matcher.scm:176** (LOW, Feature Completion)
  - `; TODO: Return the actual action instead of a rule`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/matcher.scm#L176)

- [ ] **components/integration/opencog/opencog/ghost/procedures/focus-set.scm:30** (LOW, Feature Completion)
  - `;                        ; For sog-hack-decomposition-rule`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/focus-set.scm#L30)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-actions.scm:40** (LOW, Feature Completion)
  - `; TODO: Replace by microplanner.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-actions.scm#L40)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-actions.scm:140** (LOW, Feature Completion)
  - `; FIXME Why doesn't the first call of (update-inferences) work?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-actions.scm#L140)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm:3** (LOW, Feature Completion)
  - `;; Very simplistic and hacky at the moment, loop of 2 rules`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm#L3)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm:78** (LOW, Feature Completion)
  - `; TODO: Move this to an (opencog pln) module, when there is one.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm#L78)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm:191** (LOW, Feature Completion)
  - `; TODO: Add measure to choose between candidates based on query or some`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm#L191)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm:195** (LOW, Feature Completion)
  - `; FIXME: This only works for trail-3`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-reasoner.scm#L195)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm:7** (LOW, Feature Completion)
  - `; FIXME: Doesn't return anything when confidence is low, don't use for now`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm#L7)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm:255** (LOW, Feature Completion)
  - `;; using the URE, but for now it's more like a hack.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm#L255)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm:310** (LOW, Feature Completion)
  - `;; Current hack to limit X as concepts`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm#L310)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm:344** (LOW, Feature Completion)
  - `; TODO: use pln-load-rules when move to new PLN API, see`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-1.scm#L344)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm:8** (LOW, Feature Completion)
  - `(define sog-hack-decomposition-rule`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm#L8)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm:46** (LOW, Feature Completion)
  - `; TODO: Record the acutal inference trails that have been learned and apply`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm#L46)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm:51** (LOW, Feature Completion)
  - `; TODO: use pln-load-rules when move to new PLN API, see`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm#L51)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm:65** (LOW, Feature Completion)
  - `(ure-define-add-rule rb "sog-hack-decomposition-rule"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm#L65)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm:66** (LOW, Feature Completion)
  - `sog-hack-decomposition-rule (stv 1 1))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-2.scm#L66)

- [ ] **components/integration/opencog/opencog/ghost/procedures/pln-trail-3.scm:115** (LOW, Feature Completion)
  - `; TODO: use pln-load-rules when move to new PLN API, see`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/pln-trail-3.scm#L115)

- [ ] **components/integration/opencog/opencog/ghost/procedures/predicates.scm:249** (LOW, Feature Completion)
  - `;TODO: The assumption is that this is used by an ordered goal. Make it`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/predicates.scm#L249)

- [ ] **components/integration/opencog/opencog/ghost/procedures/predicates.scm:311** (LOW, Feature Completion)
  - `; TODO: Replace the ConceptNode with a PredicateNode`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/predicates.scm#L311)

- [ ] **components/integration/opencog/opencog/ghost/procedures/procedures.scm:387** (LOW, Feature Completion)
  - `;TODO: How to represent word said by face-id without having an`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/procedures.scm#L387)

- [ ] **components/integration/opencog/opencog/ghost/procedures/procedures.scm:468** (LOW, Feature Completion)
  - `; TODO Move the time related helpers to the time-server. Some of this`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/procedures.scm#L468)

- [ ] **components/integration/opencog/opencog/ghost/procedures/procedures.scm:856** (LOW, Feature Completion)
  - `TODO: Define other types of inputs and outputs.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/procedures.scm#L856)

- [ ] **components/integration/opencog/opencog/ghost/procedures/procedures.scm:1007** (LOW, Feature Completion)
  - `; TODO: move genric steps to the pln module`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/procedures.scm#L1007)

- [ ] **components/integration/opencog/opencog/ghost/procedures/schemas.scm:9** (LOW, Feature Completion)
  - `; TODO: List out the DefinedPredicates that are used as API, so as to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/schemas.scm#L9)

- [ ] **components/integration/opencog/opencog/ghost/procedures/schemas.scm:322** (LOW, Feature Completion)
  - `; TODO: Replace by an actual sentence splitter`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/schemas.scm#L322)

- [ ] **components/integration/opencog/opencog/ghost/procedures/schemas.scm:405** (LOW, Feature Completion)
  - `; TODO: use query for filtering results, by using similarity measures b/n`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/schemas.scm#L405)

- [ ] **components/integration/opencog/opencog/ghost/procedures/schemas.scm:409** (LOW, Feature Completion)
  - `; TODO: How to choose an appropriate trail or set of trails?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/schemas.scm#L409)

- [ ] **components/integration/opencog/opencog/ghost/procedures/schemas.scm:647** (LOW, Feature Completion)
  - `; Hacky-ugly post-processing to turn e.g. "you" -> "I" etc`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/schemas.scm#L647)

- [ ] **components/integration/opencog/opencog/ghost/procedures/schemas.scm:808** (LOW, Feature Completion)
  - `; TODO: When there is an api to get information about the range of volume`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/procedures/schemas.scm#L808)

- [ ] **components/integration/opencog/opencog/ghost/stimulation.scm:32** (LOW, Feature Completion)
  - `; TODO: Find some better representation for that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/stimulation.scm#L32)

- [ ] **components/integration/opencog/opencog/ghost/terms.scm:291** (LOW, Feature Completion)
  - `; TODO: Check to make sure the function has been defined`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/terms.scm#L291)

- [ ] **components/integration/opencog/opencog/ghost/terms.scm:304** (LOW, Feature Completion)
  - `; TODO: Check to make sure the function has been defined`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/terms.scm#L304)

- [ ] **components/integration/opencog/opencog/ghost/translator.scm:547** (LOW, Error Handling)
  - `; TODO: Handle variables as well`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/ghost/translator.scm#L547)

- [ ] **components/integration/opencog/opencog/neighbors/Neighbors.h:58** (LOW, Feature Completion)
  - `* XXX FIXME -- this function is curently not used anywhere. Perhaps`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/neighbors/Neighbors.h#L58)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:180** (LOW, Feature Completion)
  - `; XXX FIXME. This is yucky, something prettier is needed.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L180)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:214** (LOW, Feature Completion)
  - `;; XXX TODO -- filter out the exact rules that have non-trivial`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L214)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:247** (LOW, Feature Completion)
  - `; TODO: Maybe it is better to get these rules using GetLink + SignatureLink,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L247)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:265** (LOW, Error Handling)
  - `; XXX Need to handle that, topic rules as appropriate.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L265)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:284** (LOW, Error Handling)
  - `; XXX FIXME -- handle topic stars also ....`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L284)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:361** (LOW, Feature Completion)
  - `;; NOTE crazy hacky weight-adjusting formula. This makes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L361)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:362** (LOW, Feature Completion)
  - `;; no sense at all, but is a hacky hack designed to pick more`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L362)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:484** (LOW, Feature Completion)
  - `; XXX FIXME .. Maybe check a much longer list??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L484)

- [ ] **components/integration/opencog/opencog/nlp/aiml/aiml.scm:528** (LOW, Feature Completion)
  - `; XXX TODO: Would be better to log and retrieve the chat`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/aiml/aiml.scm#L528)

- [ ] **components/integration/opencog/opencog/nlp/anaphora/agents/hobbs.py:56** (LOW, Feature Completion)
  - `Executes a (cog-execute! xxx) command and return the results of it`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/anaphora/agents/hobbs.py#L56)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:11** (LOW, Feature Completion)
  - `;; Hack to flush IO except this hack doesn't work :-(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L11)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:126** (LOW, Feature Completion)
  - `;     XXX except that above isn't done any more -- WordNodes are`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L126)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:127** (LOW, Feature Completion)
  - `;     never attached, it seems. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L127)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:414** (LOW, Feature Completion)
  - `; tag them with semes. (XXX We don't really need all the words,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L414)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:495** (LOW, Feature Completion)
  - `; tag them with semes. (XXX We don't really need all the words,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L495)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm:581** (LOW, Feature Completion)
  - `; (this is an extremely simple-minded hack right now)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/chatbot/chat-interface.scm#L581)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/pln/symmetric-relation.scm:4** (LOW, Feature Completion)
  - `; Quick hack/port of XML demo for "SymmetricRelation"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/pln/symmetric-relation.scm#L4)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/SentenceQuery.cc:216** (LOW, Feature Completion)
  - `* xxxxxxxxxxx this routine is dead, and no longer used ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/SentenceQuery.cc#L216)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/SentenceQuery.cc:321** (LOW, Feature Completion)
  - `* In this case, a cheap hack: remove all relations that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/SentenceQuery.cc#L321)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:78** (LOW, Feature Completion)
  - `* XXX This algo is flawed, fragile, but simple.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc#L78)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:79** (LOW, Feature Completion)
  - `* XXX It almost surely would be much better to implement this in`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc#L79)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:111** (LOW, Testing)
  - `* XXX This implementation is kinda-wrong, its very specific`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc#L111)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:218** (LOW, Feature Completion)
  - `* xxxxxxxxx this routine is never called!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc#L218)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:220** (LOW, Feature Completion)
  - `* XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc#L220)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc:285** (LOW, Feature Completion)
  - `(WORD_NODE == soltype) || // XXX get rid of WordNode here, someday.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/question/WordRelQuery.cc#L285)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm:628** (LOW, Documentation)
  - `; XXX but it leaves a DocumentNode with nothing pointing to it.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm#L628)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm:636** (LOW, Feature Completion)
  - `; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm#L636)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm:645** (LOW, Feature Completion)
  - `(define (do-implication x) #t)  ; XXX use cog-bind instead ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm#L645)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm:646** (LOW, Feature Completion)
  - `(define (xxxprocess-rule rule)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/seme/seme-process.scm#L646)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm:37** (LOW, Pattern Matching)
  - `; XXX Support for OrLink in the pattern matcher would simplify this ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm#L37)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm:85** (LOW, Feature Completion)
  - `; questions (who is, what is) XXX need to revisit this.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm#L85)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm:230** (LOW, Feature Completion)
  - `;; XXX someday, this needs to be an or-list of WH- words.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/question-pipeline.scm#L230)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:211** (LOW, Feature Completion)
  - `; XXX FIXME: really, if a or b are vars, then they are WordInstanceNodes.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm#L211)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:212** (LOW, Feature Completion)
  - `; XXX However, to fix this, we will need to modify the varscope code to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm#L212)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:214** (LOW, Feature Completion)
  - `; XXX err, well, no, since b can sometimes be a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm#L214)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm:216** (LOW, Feature Completion)
  - `; XXX SemeNodes can appear here as well.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rule-tools.scm#L216)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:354** (LOW, Feature Completion)
  - `; reject anything with _subj, _obj, etc. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm#L354)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:394** (LOW, Feature Completion)
  - `; XXX FIXME (this is same, similar problem to the other XXX above.)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm#L394)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:404** (LOW, Feature Completion)
  - `; XXX for some reason, this isn't working, not sure why ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm#L404)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm:469** (LOW, Feature Completion)
  - `; XXX FIXME, rule below is just rule above, but without the prep check.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/rules.scm#L469)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/varscope.scm:7** (LOW, Feature Completion)
  - `; XXX This is not used any more, because the perl script is not used any`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/varscope.scm#L7)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-old/triples/varscope.scm:11** (LOW, Feature Completion)
  - `; (or fully added to OpenCog) XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-old/triples/varscope.scm#L11)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/chatscript.scm:40** (LOW, Feature Completion)
  - `; TODO: Parse the reply?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/chatscript.scm#L40)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/contexts.scm:241** (LOW, Feature Completion)
  - `; TODO: May want to check more than time elapsed`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/contexts.scm#L241)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/external-sources.scm:29** (LOW, Feature Completion)
  - `; TODO: Do something better for getting the first sentence of a paragraph, though`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/external-sources.scm#L29)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/pln-contexts.scm:10** (LOW, Feature Completion)
  - `(define must-have-names '("people")) ;; hack: because the answer involves people`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/pln-contexts.scm#L10)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/pln-contexts.scm:16** (LOW, Feature Completion)
  - `; TODO: Replace with what has been assked to be inferred upon.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/pln-contexts.scm#L16)

- [ ] **components/integration/opencog/opencog/nlp/chatbot-psi/random-sentence-generator.scm:1** (LOW, Feature Completion)
  - `; NOTE Temp quick hacks for the upcoming demos`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot-psi/random-sentence-generator.scm#L1)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:41** (LOW, Feature Completion)
  - `;; XXX FIXME -- remove the IRC debug response below.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/bot-api.scm#L41)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:66** (LOW, Feature Completion)
  - `; XXX Use AIML here to say something snarky.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/bot-api.scm#L66)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:72** (LOW, Feature Completion)
  - `; XXX Use AIML here to say something snarky.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/bot-api.scm#L72)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:76** (LOW, Feature Completion)
  - `; XXX Use AIML here to say something snarky.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/bot-api.scm#L76)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/bot-api.scm:108** (LOW, Feature Completion)
  - `; XXX FIXME This also definitely requires change after the backward`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/bot-api.scm#L108)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm:25** (LOW, Feature Completion)
  - `; TODO: Replace these time related utilities with one from TimeMap, when it is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm#L25)

- [ ] **components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm:96** (LOW, Feature Completion)
  - `; TODO use the timeserver when it is ready.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/chatbot/chat-utils.scm#L96)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/Fuzzy.cc:81** (LOW, Feature Completion)
  - `// TODO: Extend to find similar links as well`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/Fuzzy.cc#L81)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/FuzzyMatchBasic.cc:106** (LOW, Feature Completion)
  - `// TODO: May use Truth Value instead`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/FuzzyMatchBasic.cc#L106)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:21** (LOW, Feature Completion)
  - `XXX FIXME This is broken, because it assumes that there is only`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm#L21)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:49** (LOW, Feature Completion)
  - `TODO: May need to filter out some of the contents of the SetLinks`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm#L49)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:112** (LOW, Feature Completion)
  - `XXX FIXME This is broken, because it assumes that there is only`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm#L112)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:144** (LOW, Feature Completion)
  - `XXX FIXME This is broken, because it assumes that there is only`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm#L144)

- [ ] **components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm:157** (LOW, Feature Completion)
  - `; TODO: Subject to change, currently it returns the top ones that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/fuzzy/fuzzy.scm#L157)

- [ ] **components/integration/opencog/opencog/nlp/irc/go-irc.cc:22** (LOW, Feature Completion)
  - `* This is pretty totally a pure hack with little/no design to it.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/irc/go-irc.cc#L22)

- [ ] **components/integration/opencog/opencog/nlp/irc/go-irc.cc:180** (LOW, Feature Completion)
  - `* XXX DANGER DANGER Extreme Caution Advised XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/irc/go-irc.cc#L180)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm:55** (LOW, Feature Completion)
  - `; TODO recognition of "our group" -> "we" and "our cars" -> "they"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm#L55)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm:89** (LOW, Error Handling)
  - `; XXX how to handle "mine", "hers", "theirs", etc?  Seems these will mostly`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm#L89)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm:178** (LOW, Testing)
  - `; XXX might on some special occassion want the supersets?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm#L178)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm:183** (LOW, Feature Completion)
  - `; XXX also accept links that inherit the abstracted version? currently not doing that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora-noun-item.scm#L183)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora-nouns-list.scm:243** (LOW, Feature Completion)
  - `; TODO sometimes it is OK depends on the main subject (current and previous sentence)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora-nouns-list.scm#L243)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora.scm:23** (LOW, Feature Completion)
  - `; TODO also insert anaphora for missing subjects/objects`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora.scm#L23)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/anaphora.scm:46** (LOW, Feature Completion)
  - `; XXX possibly better algorithm for choosing between pronoun vs lexical noun phrase?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/anaphora.scm#L46)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/main.scm:53** (LOW, Feature Completion)
  - `; TODO: Describe what these variables are for.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/main.scm#L53)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/main.scm:164** (LOW, Feature Completion)
  - `; TODO Keep some of the atoms (those that do not`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/main.scm#L164)

- [ ] **components/integration/opencog/opencog/nlp/microplanning/main.scm:439** (LOW, Feature Completion)
  - `; by doing a hacky search for VariableNode in the sentence-form link`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/microplanning/main.scm#L439)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm:81** (LOW, Feature Completion)
  - `(ure-define-add-rule r2l-rules "todo1" todo1 (stv 1 1))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm#L81)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm:83** (LOW, Feature Completion)
  - `(ure-define-add-rule r2l-rules "todo2" todo2 (stv 1 1))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm#L83)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm:85** (LOW, Feature Completion)
  - `(ure-define-add-rule r2l-rules "todo5" todo5 (stv 1 1))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm#L85)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm:87** (LOW, Feature Completion)
  - `(ure-define-add-rule r2l-rules "todo3" todo3 (stv 1 1))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/gen-r2l-en-rulebase.scm#L87)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm:43** (LOW, Feature Completion)
  - `(load "rules/todo1.scm")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm#L43)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm:44** (LOW, Feature Completion)
  - `(load "rules/todo2.scm")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm#L44)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm:45** (LOW, Feature Completion)
  - `(load "rules/todo3.scm")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm#L45)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm:46** (LOW, Feature Completion)
  - `(load "rules/todo5.scm")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/loader/loader.scm#L46)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:26** (LOW, Feature Completion)
  - `; XXX FIXME this method is really bad because for each new type of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L26)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:53** (LOW, Feature Completion)
  - `; XXX FIXME except that we can have (EvaluationLink "not" "run@1234") which`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L53)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:361** (LOW, Feature Completion)
  - `;NOTE using the hacky word-get-r2l-node, bad idea!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L361)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:371** (LOW, Feature Completion)
  - `; XXX what would an abstracted VariableNode be like?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L371)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:386** (LOW, Feature Completion)
  - `;  ; FIXME: Why create a node with new-instance name? Is this for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L386)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm:480** (LOW, Feature Completion)
  - `;FIXME: Why occurence of a node in more than one relation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/post-processing.scm#L480)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/r2l-utilities.scm:53** (LOW, Feature Completion)
  - `XXX TODO: update this when non-instanced R2L node are linked to WordNode`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/r2l-utilities.scm#L53)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/r2l-utilities.scm:89** (LOW, Feature Completion)
  - `TODO: update this when non-instanced R2L node are linked to WordNode`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/r2l-utilities.scm#L89)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/relex2logic.scm:63** (LOW, Feature Completion)
  - `; FIXME: Presently, only a single interpretation is created for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/relex2logic.scm#L63)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:50** (LOW, Feature Completion)
  - `; TODO: do the same for non-instanced node`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L50)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:59** (LOW, Feature Completion)
  - `; TODO: do the same for non-instanced node`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L59)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:510** (LOW, Feature Completion)
  - `; XXX this rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L510)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:516** (LOW, Feature Completion)
  - `; FIXME: this is bad because in SV, SVO type rules the same word is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L516)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:634** (LOW, Feature Completion)
  - `;; XXX FIXME: right now, this says ImplicationScopeLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L634)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:649** (LOW, Error Handling)
  - `; XXX Just to avoid getting the `#<Invalid handle>` error`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L649)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:650** (LOW, Feature Completion)
  - `; TODO Need to add more to the list (e.g. "the") to cover all cases`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L650)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:696** (LOW, Feature Completion)
  - `; XXX this rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L696)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:706** (LOW, Feature Completion)
  - `; XXX this rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L706)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:725** (LOW, Feature Completion)
  - `; XXX FIXME these two are not returned ???`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L725)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:793** (LOW, Feature Completion)
  - `; second clause. XXX FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L793)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1400** (LOW, Feature Completion)
  - `; XXX all-rule is not used anywhere ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1400)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1418** (LOW, Feature Completion)
  - `; XXX this rule is not used anywhere ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1418)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1648** (LOW, Feature Completion)
  - `; XXX that-rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1648)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1674** (LOW, Feature Completion)
  - `; XXX before-after-rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1674)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1692** (LOW, Feature Completion)
  - `; XXX time-rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1692)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1707** (LOW, Feature Completion)
  - `; XXX FIXME: there is no such thing as a "TruthValueGreaterThanLink",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1707)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm:1724** (LOW, Feature Completion)
  - `; XXX on-rule is not used anywhere!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-helpers.scm#L1724)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rule-utils.scm:7** (LOW, Feature Completion)
  - `; XXX why is this public?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rule-utils.scm#L7)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rules/passive.scm:2** (LOW, Feature Completion)
  - `; XXX Fix relex so that we don't have to make such string searches!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rules/passive.scm#L2)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rules/todo1.scm:8** (LOW, Feature Completion)
  - `(define todo1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rules/todo1.scm#L8)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rules/todo2.scm:9** (LOW, Feature Completion)
  - `(define todo2`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rules/todo2.scm#L9)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rules/todo3.scm:3** (LOW, Feature Completion)
  - `(define todo3`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rules/todo3.scm#L3)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/rules/todo5.scm:8** (LOW, Feature Completion)
  - `(define todo5`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/rules/todo5.scm#L8)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/tv-utilities.scm:11** (LOW, Feature Completion)
  - `; TODO: move this to (opencog data) module when it is created.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/tv-utilities.scm#L11)

- [ ] **components/integration/opencog/opencog/nlp/relex2logic/tv-utilities.scm:142** (LOW, Feature Completion)
  - `XXX FIXME ... this does NOT maintain counts in the same way as`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/relex2logic/tv-utilities.scm#L142)

- [ ] **components/integration/opencog/opencog/nlp/scm/oc.scm:20** (LOW, Feature Completion)
  - `; XXX What? nothing else anywhere needs this! FIXME, somethings broke.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/oc.scm#L20)

- [ ] **components/integration/opencog/opencog/nlp/scm/oc/relex-utils.scm:176** (LOW, Feature Completion)
  - `; FIXME: this is a dumb way to get other type`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/oc/relex-utils.scm#L176)

- [ ] **components/integration/opencog/opencog/nlp/scm/parse-rank.scm:174** (LOW, Feature Completion)
  - `; XXX Umm, actually, this routine was intended for a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/parse-rank.scm#L174)

- [ ] **components/integration/opencog/opencog/nlp/scm/type-definitions.scm:8** (LOW, Feature Completion)
  - `; XXX This is currently not used anywhere, but if it was fixed up,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/type-definitions.scm#L8)

- [ ] **components/integration/opencog/opencog/nlp/scm/type-definitions.scm:13** (LOW, Feature Completion)
  - `; XXX This list is *probably* incomplete, and needs to be reviewed! XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/type-definitions.scm#L13)

- [ ] **components/integration/opencog/opencog/nlp/scm/type-definitions.scm:128** (LOW, Feature Completion)
  - `; XXX this list is highly incomplete`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/scm/type-definitions.scm#L128)

- [ ] **components/integration/opencog/opencog/nlp/sureal/SuRealCache.h:38** (LOW, Performance)
  - `* XXX THIS IS A BROKEN DESIGN! -- FIXME! (The fix is easy) The cache`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/sureal/SuRealCache.h#L38)

- [ ] **components/integration/opencog/opencog/nlp/sureal/SuRealCache.h:43** (LOW, Feature Completion)
  - `* FIXME by getting rid of this class!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/sureal/SuRealCache.h#L43)

- [ ] **components/integration/opencog/opencog/nlp/sureal/sureal.scm:3** (LOW, Feature Completion)
  - `; XXX Huh ???`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/sureal/sureal.scm#L3)

- [ ] **components/integration/opencog/opencog/nlp/sureal/surface-realization.scm:291** (LOW, Error Handling)
  - `; FIXME: This results in 'result' being 'Invalid handle' sometimes.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/sureal/surface-realization.scm#L291)

- [ ] **components/integration/opencog/opencog/nlp/wsd-post/collect-stats/disjunct-list.scm:48** (LOW, Feature Completion)
  - `; word order. XXX FIXME -- instead of doing the below, it would be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd-post/collect-stats/disjunct-list.scm#L48)

- [ ] **components/integration/opencog/opencog/nlp/wsd-post/collect-stats/stats-collection.scm:359** (LOW, Documentation)
  - `; XXX The method used here, of tagging documents with "finished"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd-post/collect-stats/stats-collection.scm#L359)

- [ ] **components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.cc:11** (LOW, Feature Completion)
  - `* the word-sense similarities in an opencog persistent store. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.cc#L11)

- [ ] **components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.h:8** (LOW, Feature Completion)
  - `* the word-sense similarities in an opencog persistent store. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd/SenseSimilaritySQL.h#L8)

- [ ] **components/integration/opencog/opencog/nlp/wsd/WordSenseProcessor.cc:6** (LOW, Feature Completion)
  - `* XXX Currently, this is very crude scaffolding to interface`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/nlp/wsd/WordSenseProcessor.cc#L6)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiImplicator.cc:41** (LOW, Feature Completion)
  - `// TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiImplicator.cc#L41)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiImplicator.cc:49** (LOW, Performance)
  - `// TODO: Add cache per atomspace.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiImplicator.cc#L49)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiImplicator.h:44** (LOW, Feature Completion)
  - `// TODO Why would one need to reset during psi-loop?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiImplicator.h#L44)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.cc:68** (LOW, Feature Completion)
  - `// TODO: Test thoroughly, or develop an alternative. See discussion`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.cc#L68)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.cc:101** (LOW, Feature Completion)
  - `// TODO But why make the add_category public then?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.cc#L101)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.h:105** (LOW, Feature Completion)
  - `// TODO:add predicate to check for membership of category.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.h#L105)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiRules.h:136** (LOW, Feature Completion)
  - `// TODO: Using names that are prefixed with "OpenPsi: " might be a bad idea,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiRules.h#L136)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiSCM.cc:95** (LOW, Feature Completion)
  - `// TODO: Add to multiple categories using scheme rest list.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiSCM.cc#L95)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiSatisfier.cc:42** (LOW, Pattern Matching)
  - `// TODO: Saperated component patterns aren't handled by this function`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiSatisfier.cc#L42)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiSatisfier.cc:56** (LOW, Feature Completion)
  - `// TODO: If we are here it means the suggested groundings doesn't have`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiSatisfier.cc#L56)

- [ ] **components/integration/opencog/opencog/openpsi/OpenPsiSatisfier.cc:76** (LOW, Feature Completion)
  - `// TODO: This happens when InitiateSearchCB::no_search has groundings.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/OpenPsiSatisfier.cc#L76)

- [ ] **components/integration/opencog/opencog/openpsi/action-selector.scm:81** (LOW, Feature Completion)
  - `; TODO Why have a cutoff? That is why return a list of rules`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/action-selector.scm#L81)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:49** (LOW, Feature Completion)
  - `; NOTE: This is a hack b/c once the weight of the rules is separated into atom`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L49)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:89** (LOW, Feature Completion)
  - `; FIXME -- can we have a shorter/better name for this method?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L89)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:104** (LOW, Feature Completion)
  - `; FIXME - the long-term design calls for the use of an AtTimeLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L104)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:107** (LOW, Feature Completion)
  - `; FIXME -- if there are multiple names for a rule, this will`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L107)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:110** (LOW, Feature Completion)
  - `; to linger in the atomspace. FIXME -- make sure that a rule can`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L110)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:218** (LOW, Feature Completion)
  - `; TODO: Use categories instead of aliases for categorization`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L218)

- [ ] **components/integration/opencog/opencog/openpsi/control.scm:239** (LOW, Feature Completion)
  - `; TODO: Use categories instead of aliases for categorization`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/control.scm#L239)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/modulator.scm:48** (LOW, Feature Completion)
  - `; todo: cog-chase-link bug? - it is returning the anchor node in the results`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/modulator.scm#L48)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/sec.scm:67** (LOW, Feature Completion)
  - `; Todo: add variable names (?) and add getters for agent-state secs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/sec.scm#L67)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:38** (LOW, Feature Completion)
  - `; TODO Replace verbose & logger variable in openpsi/dyanmics/updater.scm with`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L38)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:47** (LOW, Feature Completion)
  - `; Todo: Move these to a config file`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L47)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:69** (LOW, Feature Completion)
  - `; Todo: implement these tables in the atomspace`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L69)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:112** (LOW, Feature Completion)
  - `; Todo: Add a general callback function that is called once each loop`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L112)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:532** (LOW, Error Handling)
  - `; TODO: handle current value = #f (or not number in general)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L532)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:723** (LOW, Feature Completion)
  - `Todo: Add this to scheme utilities in Atomspace repo?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L723)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/updater.scm:748** (LOW, Feature Completion)
  - `; Todo: Perhaps will want to integrate this into the main OpenPsi loop, but OTOH`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/updater.scm#L748)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/utilities.scm:69** (LOW, Error Handling)
  - `Todo: How to handle non-number returns. #f?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/utilities.scm#L69)

- [ ] **components/integration/opencog/opencog/openpsi/dynamics/utilities.scm:114** (LOW, Error Handling)
  - `; Todo: this will probably need to be changed to handle`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/dynamics/utilities.scm#L114)

- [ ] **components/integration/opencog/opencog/openpsi/main.scm:35** (LOW, Feature Completion)
  - `; TODO: Adding  a component to a category makes no sense`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/main.scm#L35)

- [ ] **components/integration/opencog/opencog/openpsi/main.scm:137** (LOW, Feature Completion)
  - `; all CPU. FIXME -- this is obviously a hack, awaiting some sort`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/main.scm#L137)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:128** (LOW, Feature Completion)
  - `; TODO: Add utilities for declaring custom urge formula.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L128)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:142** (LOW, Feature Completion)
  - `; TODO: Add utilities for declaring custom decrease-urge formula.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L142)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:161** (LOW, Feature Completion)
  - `; TODO: Add utilities for declaring custom increase-urge formula.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L161)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:197** (LOW, Documentation)
  - `; TODO Uncomment after testing with ghost`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L197)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:211** (LOW, Feature Completion)
  - `;; faster if cog-chase-link was used instead. FIXME.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L211)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:223** (LOW, Documentation)
  - `; TODO Uncomment after testing with ghost`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L223)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:240** (LOW, Feature Completion)
  - `;; XXX this is not an efficient way of searching for goals.  If`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L240)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:294** (LOW, Feature Completion)
  - `; XXX FIXME How about actually using a SequentialAndLink?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L294)

- [ ] **components/integration/opencog/opencog/openpsi/rule.scm:296** (LOW, Feature Completion)
  - `; TODO: This calculation can be done in OpenPsiImplicator::grounding or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/rule.scm#L296)

- [ ] **components/integration/opencog/opencog/openpsi/utilities.scm:14** (LOW, Feature Completion)
  - `; XXX TODO: does this really need to be public? change into atom.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/utilities.scm#L14)

- [ ] **components/integration/opencog/opencog/openpsi/utilities.scm:18** (LOW, Feature Completion)
  - `; XXX TODO: does this really need to be public?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/opencog/openpsi/utilities.scm#L18)

- [ ] **components/language/learn/attic/run-poc/export-mi.scm:4** (LOW, Feature Completion)
  - `; The hacky and ugly code below looks for every EvaluationLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-poc/export-mi.scm#L4)

- [ ] **components/language/learn/attic/run-poc/fetch-word-pairs.scm:25** (LOW, Feature Completion)
  - `(set! star-obj (add-pair-stars pair-obj)) ;TODO: Can it be left out??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-poc/fetch-word-pairs.scm#L25)

- [ ] **components/language/learn/attic/run-v3/run-common/export-dictionary.scm:40** (LOW, Feature Completion)
  - `; XXX put INCLUDE-UNKNOWN into the environment.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-v3/run-common/export-dictionary.scm#L40)

- [ ] **components/language/learn/attic/run-v3/run-common/gen-dict.scm:183** (LOW, Feature Completion)
  - `(format port "\n<UNKNOWN-WORD>:  XXXXXX+;\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-v3/run-common/gen-dict.scm#L183)

- [ ] **components/language/learn/attic/run-v3/run-common/marginals-mst-shape.scm:34** (LOW, Feature Completion)
  - `; XXX The current merge code tracks stats that requires the MI`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-v3/run-common/marginals-mst-shape.scm#L34)

- [ ] **components/language/learn/attic/run-v3/run-common/trim-pair.scm:22** (LOW, Feature Completion)
  - `; XXX This includes some ad-hoc constants. See the Sept 2021 diary entry`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/attic/run-v3/run-common/trim-pair.scm#L22)

- [ ] **components/language/learn/fake/zipf.scm:7** (LOW, Feature Completion)
  - `; srfi-194 random number generator. This is a hack. This is only`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/fake/zipf.scm#L7)

- [ ] **components/language/learn/learn-lang-diary/link-type/gifty.scm:31** (LOW, Feature Completion)
  - `; Given a rank, return a probability. This is hacky`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/link-type/gifty.scm#L31)

- [ ] **components/language/learn/learn-lang-diary/p9-tri/pmi.scm:3** (LOW, Feature Completion)
  - `; Hackery for Chapter 9 exploration.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/p9-tri/pmi.scm#L3)

- [ ] **components/language/learn/learn-lang-diary/utils/disjunct-cross.scm:32** (LOW, Feature Completion)
  - `; NOTE: FIXME add-pair-count-api is obsolete; use add-support-api instead.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/disjunct-cross.scm#L32)

- [ ] **components/language/learn/learn-lang-diary/utils/disjunct-stats.scm:39** (LOW, Feature Completion)
  - `; NOTE: FIXME add-pair-count-api is obsolete; use add-support-api instead.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/disjunct-stats.scm#L39)

- [ ] **components/language/learn/learn-lang-diary/utils/ortho-compute.scm:21** (LOW, Feature Completion)
  - `; TODO filter the top lists`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/ortho-compute.scm#L21)

- [ ] **components/language/learn/learn-lang-diary/utils/ortho-compute.scm:57** (LOW, Feature Completion)
  - `; XXX todo store mean-rms on any-node.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/ortho-compute.scm#L57)

- [ ] **components/language/learn/learn-lang-diary/utils/orthogonal-ensemble.scm:52** (LOW, Feature Completion)
  - `; TODO filter the top lists`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/orthogonal-ensemble.scm#L52)

- [ ] **components/language/learn/learn-lang-diary/utils/word-cosines.scm:31** (LOW, Feature Completion)
  - `; XXX FIXME add-pair-count-api is osolete; use add-support-api instead.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/word-cosines.scm#L31)

- [ ] **components/language/learn/learn-lang-diary/utils/word-cosines.scm:86** (LOW, Feature Completion)
  - `XXX TODO remove leading blank.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/utils/word-cosines.scm#L86)

- [ ] **components/language/learn/learn-lang-diary/word-classes/word-classes.scm:9** (LOW, Feature Completion)
  - `; XXX Don't use this; use ((make-gram-class-api) 'fetch-pairs) instead.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/learn-lang-diary/word-classes/word-classes.scm#L9)

- [ ] **components/language/learn/run-common/attic/export-dictionary.scm:40** (LOW, Feature Completion)
  - `; XXX put INCLUDE-UNKNOWN into the environment.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/attic/export-dictionary.scm#L40)

- [ ] **components/language/learn/run-common/attic/gen-dict.scm:183** (LOW, Feature Completion)
  - `(format port "\n<UNKNOWN-WORD>:  XXXXXX+;\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/attic/gen-dict.scm#L183)

- [ ] **components/language/learn/run-common/cogserver-mst.scm:76** (LOW, Feature Completion)
  - `; Reset the parse timer. Yes, this is a hack.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/cogserver-mst.scm#L76)

- [ ] **components/language/learn/run-common/cogserver-pair.scm:44** (LOW, Feature Completion)
  - `; Reset the parse timer. Yes, this is a hack.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/cogserver-pair.scm#L44)

- [ ] **components/language/learn/run-common/cogserver.scm:97** (LOW, Feature Completion)
  - `; XXX Is this needed? Didn't cogserver already get the top?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/cogserver.scm#L97)

- [ ] **components/language/learn/run-common/marginals-mst-shape.scm:35** (LOW, Feature Completion)
  - `; XXX The current merge code tracks stats that requires the MI`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/marginals-mst-shape.scm#L35)

- [ ] **components/language/learn/run-common/trim-pair.scm:23** (LOW, Feature Completion)
  - `; XXX This includes some ad-hoc constants. See the Sept 2021 diary entry`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/run-common/trim-pair.scm#L23)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:385** (LOW, Feature Completion)
  - `; XXX Maybe-FIXME: There's some amount of pointless recomputation of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L385)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:415** (LOW, Feature Completion)
  - `; If only a stub of a word is left, throw it away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L415)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:487** (LOW, Error Handling)
  - `; handle this. XXX TODO.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L487)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:501** (LOW, Feature Completion)
  - `; The new todo list is probably a lot shorter`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L501)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:567** (LOW, Feature Completion)
  - `; XXX There is a user-adjustable parameter used below, `diag-block-size`,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L567)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:586** (LOW, Feature Completion)
  - `; XXX the block sizes are by powers of 2...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L586)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:646** (LOW, Feature Completion)
  - `(define todo-words (remove! is-single? remain-words))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L646)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:660** (LOW, Feature Completion)
  - `(length todo-words))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L660)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:663** (LOW, Feature Completion)
  - `(greedy-grow MERGER CLS-LST singletons done-list todo-words)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L663)

- [ ] **components/language/learn/scm/attic/cluster/agglo-loops.scm:667** (LOW, Feature Completion)
  - `; XXX FIXME ... at the conclusion of this, we have a done list,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-loops.scm#L667)

- [ ] **components/language/learn/scm/attic/cluster/agglo-pairwise.scm:51** (LOW, Feature Completion)
  - `; XXX The below needs routines defined in `agglo-rank.scm``
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/agglo-pairwise.scm#L51)

- [ ] **components/language/learn/scm/attic/cluster/cset-class.scm:148** (LOW, Feature Completion)
  - `; TODO: Evaluate if this function provides meaningful functionality.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/cset-class.scm#L148)

- [ ] **components/language/learn/scm/attic/cluster/cset-merge.scm:90** (LOW, Feature Completion)
  - `; XXX Incomplete, in development.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/cset-merge.scm#L90)

- [ ] **components/language/learn/scm/attic/cluster/cset-merge.scm:149** (LOW, Feature Completion)
  - `; XXX this is maybe-dead code, its only used by `fetch-mergable-sections``
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/cset-merge.scm#L149)

- [ ] **components/language/learn/scm/attic/cluster/gram-fuzz.scm:139** (LOW, Feature Completion)
  - `by `greedy-grow` to ignore the stub of a word`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-fuzz.scm#L139)

- [ ] **components/language/learn/scm/attic/cluster/gram-fuzz.scm:286** (LOW, Feature Completion)
  - `Caution: this has been hacked to assume shapes (the #t flag is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-fuzz.scm#L286)

- [ ] **components/language/learn/scm/attic/cluster/gram-fuzz.scm:340** (LOW, Feature Completion)
  - `Caution: this has been hacked to assume shapes (the #t flag is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-fuzz.scm#L340)

- [ ] **components/language/learn/scm/attic/cluster/gram-fuzz.scm:390** (LOW, Feature Completion)
  - `Caution: this has been hacked to assume shapes (the #t flag is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-fuzz.scm#L390)

- [ ] **components/language/learn/scm/attic/cluster/gram-fuzz.scm:449** (LOW, Feature Completion)
  - `Caution: this has been hacked to assume shapes (the #t flag is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-fuzz.scm#L449)

- [ ] **components/language/learn/scm/attic/cluster/gram-fuzz.scm:517** (LOW, Feature Completion)
  - `Caution: this has been hacked to assume shapes (the #t flag is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-fuzz.scm#L517)

- [ ] **components/language/learn/scm/attic/cluster/gram-pairwise.scm:188** (LOW, Feature Completion)
  - `; TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-pairwise.scm#L188)

- [ ] **components/language/learn/scm/attic/cluster/gram-pairwise.scm:576** (LOW, Feature Completion)
  - `; that. XXX FIXME These need to be cleaned up!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-pairwise.scm#L576)

- [ ] **components/language/learn/scm/attic/cluster/gram-pairwise.scm:592** (LOW, Feature Completion)
  - `; XXX TODO once make-merge-majority is done, this can be reimplemented`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/gram-pairwise.scm#L592)

- [ ] **components/language/learn/scm/attic/cluster/shape-project.scm:265** (LOW, Feature Completion)
  - `; xxxxx`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/shape-project.scm#L265)

- [ ] **components/language/learn/scm/attic/cluster/shape-project.scm:266** (LOW, Feature Completion)
  - `; todo`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/cluster/shape-project.scm#L266)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:19** (LOW, Feature Completion)
  - `;; NOTE hack alert:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L19)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:20** (LOW, Feature Completion)
  - `;; TODO WordClassNode support might be .. funky.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L20)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:164** (LOW, Feature Completion)
  - `; hack -- Note that this "broadens" coverage, as flagged up top.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L164)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:343** (LOW, Feature Completion)
  - `; NOTE This is a temp hack, because the classification code`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L343)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:453** (LOW, Feature Completion)
  - `; work (when dict debugging). The XXXBOGUS+ will not link to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L453)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:454** (LOW, Feature Completion)
  - `; anything. XXX FIXME is this really needed ??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L454)

- [ ] **components/language/learn/scm/attic/lg-export/export-disjuncts.scm:463** (LOW, Feature Completion)
  - `"'<UNKNOWN-WORD>', 'XXXBOGUS+', 0.0);"))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/lg-export/export-disjuncts.scm#L463)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:70** (LOW, Feature Completion)
  - `XXX TODO Make above configurable.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L70)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:72** (LOW, Feature Completion)
  - `; XXX FIXME Both of these are global and stateful in LG and`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L72)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:88** (LOW, Feature Completion)
  - `(define mst-start (AnchorNode "MST Starts")) ; This is a hack for now.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L88)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:89** (LOW, Feature Completion)
  - `(define mst-timeo (AnchorNode "MST Timeouts")) ; This is a hack for now.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L89)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:90** (LOW, Feature Completion)
  - `(define mst-elaps (AnchorNode "MST Elapsed Time Secs")) ; hack for now.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L90)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:120** (LOW, Feature Completion)
  - `(define start (current-time))  ; XXXX temp hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L120)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:121** (LOW, Feature Completion)
  - `(define timeo #f) ; XXXX temp hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L121)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:132** (LOW, Feature Completion)
  - `(count-one-atom mst-start)   ;; NOTE tmp hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L132)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:133** (LOW, Feature Completion)
  - `(count-inc-atom mst-elaps (- (current-time) start)) ; NOTE temp hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L133)

- [ ] **components/language/learn/scm/attic/mpg-parse/lg-parser.scm:134** (LOW, Feature Completion)
  - `(if timeo (count-one-atom mst-timeo)) ; NOTE temp hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/mpg-parse/lg-parser.scm#L134)

- [ ] **components/language/learn/scm/attic/pair-count/clique-pair-count.scm:24** (LOW, Feature Completion)
  - `; instead. This needs a code redesign. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/pair-count/clique-pair-count.scm#L24)

- [ ] **components/language/learn/scm/attic/summary.scm:5** (LOW, Feature Completion)
  - `; XXX This is stale and semi-abandoned and needs to be modernized.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/summary.scm#L5)

- [ ] **components/language/learn/scm/attic/summary.scm:51** (LOW, Feature Completion)
  - `; XXX FIXME work on the singletons API so that we don't`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/attic/summary.scm#L51)

- [ ] **components/language/learn/scm/common.scm:14** (LOW, Feature Completion)
  - `; XXX TODO FIXME All users of the three functions below need to be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/common.scm#L14)

- [ ] **components/language/learn/scm/gram-class/agglo-goe.scm:147** (LOW, Feature Completion)
  - `;xxxxxxxxxx`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/agglo-goe.scm#L147)

- [ ] **components/language/learn/scm/gram-class/agglo-goe.scm:238** (LOW, Feature Completion)
  - `;xxxxxxxxxx`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/agglo-goe.scm#L238)

- [ ] **components/language/learn/scm/gram-class/agglo-goe.scm:266** (LOW, Feature Completion)
  - `TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/agglo-goe.scm#L266)

- [ ] **components/language/learn/scm/gram-class/agglo-mi-rank.scm:45** (LOW, Feature Completion)
  - `; TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/agglo-mi-rank.scm#L45)

- [ ] **components/language/learn/scm/gram-class/agglo-mi-rank.scm:205** (LOW, Feature Completion)
  - `; XXX Is this really needed? Detailed balance means that neither`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/agglo-mi-rank.scm#L205)

- [ ] **components/language/learn/scm/gram-class/goe-similarity.scm:7** (LOW, Feature Completion)
  - `xxxxxxxx`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/goe-similarity.scm#L7)

- [ ] **components/language/learn/scm/gram-class/goe-similarity.scm:22** (LOW, Feature Completion)
  - `; TODO filter the top lists`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/goe-similarity.scm#L22)

- [ ] **components/language/learn/scm/gram-class/goe-similarity.scm:25** (LOW, Feature Completion)
  - `; TODO check if LLOBJ is a similrity object`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/goe-similarity.scm#L25)

- [ ] **components/language/learn/scm/gram-class/goe-similarity.scm:57** (LOW, Feature Completion)
  - `; XXX todo store mean-rms on any-node.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/goe-similarity.scm#L57)

- [ ] **components/language/learn/scm/gram-class/gram-class-api.scm:65** (LOW, Feature Completion)
  - `; XXX FIXME: this won't work for some classes, which store`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-class-api.scm#L65)

- [ ] **components/language/learn/scm/gram-class/gram-majority.scm:95** (LOW, Feature Completion)
  - `XXX TODO: move this so its a method on `add-gram-class-api`.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-majority.scm#L95)

- [ ] **components/language/learn/scm/gram-class/gram-majority.scm:137** (LOW, Feature Completion)
  - `XXX At this time, setting this to #f does not work.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-majority.scm#L137)

- [ ] **components/language/learn/scm/gram-class/gram-optim.scm:48** (LOW, Feature Completion)
  - `; at least one perhaps-hacky-but-linear-time algo:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/gram-optim.scm#L48)

- [ ] **components/language/learn/scm/gram-class/jaccard.scm:62** (LOW, Feature Completion)
  - `variant is hard-coded. The first variant is stubbed out in the code.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/jaccard.scm#L62)

- [ ] **components/language/learn/scm/gram-class/mi-similarity.scm:94** (LOW, Feature Completion)
  - `; Get all the similarities. We're going to just hack this, for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/mi-similarity.scm#L94)

- [ ] **components/language/learn/scm/gram-class/shape-project.scm:369** (LOW, Feature Completion)
  - `; FIXME but how?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/shape-project.scm#L369)

- [ ] **components/language/learn/scm/gram-class/similarity.scm:62** (LOW, Feature Completion)
  - `(define todo-list (atoms-subtract (sms 'left-duals WX) LIGNORE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/similarity.scm#L62)

- [ ] **components/language/learn/scm/gram-class/similarity.scm:67** (LOW, Feature Completion)
  - `todo-list))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/similarity.scm#L67)

- [ ] **components/language/learn/scm/gram-class/singletons.scm:58** (LOW, Feature Completion)
  - `; XXX this is broken`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/singletons.scm#L58)

- [ ] **components/language/learn/scm/gram-class/singletons.scm:61** (LOW, Feature Completion)
  - `; TODO: Implement delete functionality`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/gram-class/singletons.scm#L61)

- [ ] **components/language/learn/scm/learn.scm:15** (LOW, Feature Completion)
  - `(include-from-path "opencog/learn/pipe-count.scm") ; XXX experimental`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/learn.scm#L15)

- [ ] **components/language/learn/scm/lg-compare.scm:365** (LOW, Feature Completion)
  - `; NOTE Temp hack. Currently, the test dicts are missing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/lg-compare.scm#L365)

- [ ] **components/language/learn/scm/pair-count/word-pair-pipe.scm:2** (LOW, Feature Completion)
  - `; word-pair-pipe.scm -- Hack: random word-pair counting via Atomese pipe.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pair-count/word-pair-pipe.scm#L2)

- [ ] **components/language/learn/scm/pair-count/word-pair-pipe.scm:4** (LOW, Feature Completion)
  - `; Hacked backwards-compat replacement for `word-pair-count.scm` for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pair-count/word-pair-pipe.scm#L4)

- [ ] **components/language/learn/scm/pair-count/word-pair-pipe.scm:7** (LOW, Feature Completion)
  - `; This is a "hack" because it provides a backwards-compat shim between`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pair-count/word-pair-pipe.scm#L7)

- [ ] **components/language/learn/scm/pair-count/word-pair-pipe.scm:24** (LOW, Feature Completion)
  - `; TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pair-count/word-pair-pipe.scm#L24)

- [ ] **components/language/learn/scm/pair-count/word-pair-pipe.scm:38** (LOW, Feature Completion)
  - `; uncertain, so for now, assume only one global. FIXME someday, if`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pair-count/word-pair-pipe.scm#L38)

- [ ] **components/language/learn/scm/parse/lg-pipe-parser.scm:17** (LOW, Feature Completion)
  - `; XXX FIXME. The next 30 lines of code are a cut-n-paste of the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/parse/lg-pipe-parser.scm#L17)

- [ ] **components/language/learn/scm/parse/lg-pipe-parser.scm:26** (LOW, Feature Completion)
  - `; uncertain, so for now, assume only one global. FIXME someday, if`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/parse/lg-pipe-parser.scm#L26)

- [ ] **components/language/learn/scm/pipe-parse/pipe-count.scm:39** (LOW, Feature Completion)
  - `; TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pipe-parse/pipe-count.scm#L39)

- [ ] **components/language/learn/scm/pipe-parse/pipe-count.scm:74** (LOW, Feature Completion)
  - `the sensory-agent API. The current structure here is a bit hacky.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pipe-parse/pipe-count.scm#L74)

- [ ] **components/language/learn/scm/pipe-parse/pipe-count.scm:197** (LOW, Feature Completion)
  - `; XXX Need to fetch any-parse, too.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/pipe-parse/pipe-count.scm#L197)

- [ ] **components/language/learn/scm/utils/utilities.scm:55** (LOW, Feature Completion)
  - `; 1) This uses sleep in a hacky manner, to poll for finished`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/utils/utilities.scm#L55)

- [ ] **components/language/learn/scm/utils/utilities.scm:57** (LOW, Feature Completion)
  - `;    hacky, and needs to be replaced by some semaphore.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/learn/scm/utils/utilities.scm#L57)

- [ ] **components/language/lg-atomese/opencog/nlp/lg-dict/LGDictReader.cc:87** (LOW, Feature Completion)
  - `* XXX FIXME -- this gives incorrect results if the word has non-trivial`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/lg-dict/LGDictReader.cc#L87)

- [ ] **components/language/lg-atomese/opencog/nlp/lg-dict/LGDictUtils.cc:53** (LOW, Error Handling)
  - `XXX FIXME -- this currently fails to correctly handle the head-tail`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/lg-dict/LGDictUtils.cc#L53)

- [ ] **components/language/lg-atomese/opencog/nlp/lg-dict/lg-dict.scm:51** (LOW, Feature Completion)
  - `; XXX Argh. This function gets called in par-map in sureal,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/lg-dict/lg-dict.scm#L51)

- [ ] **components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:70** (LOW, Error Handling)
  - `XXX! Caution/error! This implicitly assumes that there is only one`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm#L70)

- [ ] **components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:72** (LOW, Feature Completion)
  - `XXX! You probably want to be using either StateLink or DefineLink`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm#L72)

- [ ] **components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:134** (LOW, Pattern Matching)
  - `; TODO: Consider refactoring this pattern to use GetLink syntax.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm#L134)

- [ ] **components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:195** (LOW, Feature Completion)
  - `XXX fix-me -- might this not be parse-dependent???`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm#L195)

- [ ] **components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm:494** (LOW, Feature Completion)
  - `XXX: Some of these nodes & links are needed by the language generation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/language/lg-atomese/opencog/nlp/scm/attic/nlp-utils.scm#L494)

- [ ] **entelechy/introspector.py:340** (LOW, Feature Completion)
  - `# Check for TODO/FIXME in reasoning systems`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L340)

- [ ] **entelechy/introspector.py:344** (LOW, Feature Completion)
  - `total_markers += markers['TODO'] + markers['FIXME'] + markers['STUB']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L344)

- [ ] **entelechy/introspector.py:477** (LOW, Feature Completion)
  - `- Code quality markers (TODO/FIXME/STUB)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L477)

- [ ] **entelechy/introspector.py:503** (LOW, Feature Completion)
  - `"""Assess code health via TODO/FIXME/STUB markers"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L503)

- [ ] **entelechy/introspector.py:506** (LOW, Feature Completion)
  - `todo_count = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L506)

- [ ] **entelechy/introspector.py:507** (LOW, Feature Completion)
  - `fixme_count = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L507)

- [ ] **entelechy/introspector.py:508** (LOW, Feature Completion)
  - `stub_count = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L508)

- [ ] **entelechy/introspector.py:514** (LOW, Feature Completion)
  - `todo_count += markers['TODO']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L514)

- [ ] **entelechy/introspector.py:515** (LOW, Feature Completion)
  - `fixme_count += markers['FIXME']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L515)

- [ ] **entelechy/introspector.py:516** (LOW, Feature Completion)
  - `stub_count += markers['STUB']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L516)

- [ ] **entelechy/introspector.py:518** (LOW, Feature Completion)
  - `total_markers = todo_count + fixme_count + stub_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L518)

- [ ] **entelechy/introspector.py:523** (LOW, Feature Completion)
  - `self.metrics.todo_count = todo_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L523)

- [ ] **entelechy/introspector.py:524** (LOW, Feature Completion)
  - `self.metrics.fixme_count = fixme_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L524)

- [ ] **entelechy/introspector.py:525** (LOW, Feature Completion)
  - `self.metrics.stub_count = stub_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L525)

- [ ] **entelechy/introspector.py:528** (LOW, Feature Completion)
  - `print(f"    TODO: {todo_count}, FIXME: {fixme_count}, STUB: {stub_count}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L528)

- [ ] **entelechy/introspector.py:531** (LOW, Feature Completion)
  - `'todo_count': todo_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L531)

- [ ] **entelechy/introspector.py:532** (LOW, Feature Completion)
  - `'fixme_count': fixme_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L532)

- [ ] **entelechy/introspector.py:533** (LOW, Feature Completion)
  - `'stub_count': stub_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L533)

- [ ] **entelechy/introspector.py:539** (LOW, Feature Completion)
  - `"""Count TODO/FIXME/STUB markers in a directory"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L539)

- [ ] **entelechy/introspector.py:540** (LOW, Feature Completion)
  - `markers = {'TODO': 0, 'FIXME': 0, 'STUB': 0}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L540)

- [ ] **entelechy/introspector.py:552** (LOW, Feature Completion)
  - `markers['TODO'] += len(re.findall(r'\bTODO\b', content, re.IGNORECASE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L552)

- [ ] **entelechy/introspector.py:553** (LOW, Feature Completion)
  - `markers['FIXME'] += len(re.findall(r'\bFIXME\b', content, re.IGNORECASE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L553)

- [ ] **entelechy/introspector.py:554** (LOW, Feature Completion)
  - `markers['STUB'] += len(re.findall(r'\bSTUB\b', content, re.IGNORECASE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L554)

- [ ] **entelechy/introspector.py:561** (LOW, Feature Completion)
  - `"""Assess implementation completeness vs placeholders"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L561)

- [ ] **entelechy/introspector.py:580** (LOW, Feature Completion)
  - `'small_stub_files': small_files,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L580)

- [ ] **entelechy/introspector.py:660** (LOW, Feature Completion)
  - `fragmentation_type=FragmentationType.PLACEHOLDER_CODE,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L660)

- [ ] **entelechy/introspector.py:666** (LOW, Feature Completion)
  - `"Systematically resolve TODO markers",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L666)

- [ ] **entelechy/introspector.py:667** (LOW, Feature Completion)
  - `"Address FIXME issues",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L667)

- [ ] **entelechy/introspector.py:668** (LOW, Feature Completion)
  - `"Replace STUB implementations with complete code"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L668)

- [ ] **entelechy/introspector.py:768** (LOW, Feature Completion)
  - `'todo_count': self.metrics.todo_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L768)

- [ ] **entelechy/introspector.py:769** (LOW, Feature Completion)
  - `'fixme_count': self.metrics.fixme_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L769)

- [ ] **entelechy/introspector.py:770** (LOW, Feature Completion)
  - `'stub_count': self.metrics.stub_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/introspector.py#L770)

- [ ] **entelechy/optimizer.py:235** (LOW, Feature Completion)
  - `'description': f'Resolve {metrics.total_code_markers} TODO/FIXME/STUB markers',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/optimizer.py#L235)

- [ ] **entelechy/types.py:28** (LOW, Feature Completion)
  - `PLACEHOLDER_CODE = "placeholder_code"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/types.py#L28)

- [ ] **entelechy/types.py:81** (LOW, Feature Completion)
  - `todo_count: int = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/types.py#L81)

- [ ] **entelechy/types.py:82** (LOW, Feature Completion)
  - `fixme_count: int = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/types.py#L82)

- [ ] **entelechy/types.py:83** (LOW, Feature Completion)
  - `stub_count: int = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy/types.py#L83)

- [ ] **entelechy_introspection.py:50** (LOW, Feature Completion)
  - `PLACEHOLDER_CODE = "placeholder_code"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L50)

- [ ] **entelechy_introspection.py:84** (LOW, Feature Completion)
  - `todo_count: int = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L84)

- [ ] **entelechy_introspection.py:85** (LOW, Feature Completion)
  - `fixme_count: int = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L85)

- [ ] **entelechy_introspection.py:86** (LOW, Feature Completion)
  - `stub_count: int = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L86)

- [ ] **entelechy_introspection.py:378** (LOW, Feature Completion)
  - `# Check for TODO/FIXME in reasoning systems`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L378)

- [ ] **entelechy_introspection.py:382** (LOW, Feature Completion)
  - `total_markers += markers['TODO'] + markers['FIXME'] + markers['STUB']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L382)

- [ ] **entelechy_introspection.py:388** (LOW, Feature Completion)
  - `'todo_markers': total_markers,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L388)

- [ ] **entelechy_introspection.py:527** (LOW, Feature Completion)
  - `- Code quality markers (TODO/FIXME/STUB)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L527)

- [ ] **entelechy_introspection.py:554** (LOW, Feature Completion)
  - `"""Assess code health via TODO/FIXME/STUB markers"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L554)

- [ ] **entelechy_introspection.py:557** (LOW, Feature Completion)
  - `todo_count = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L557)

- [ ] **entelechy_introspection.py:558** (LOW, Feature Completion)
  - `fixme_count = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L558)

- [ ] **entelechy_introspection.py:559** (LOW, Feature Completion)
  - `stub_count = 0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L559)

- [ ] **entelechy_introspection.py:565** (LOW, Feature Completion)
  - `todo_count += markers['TODO']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L565)

- [ ] **entelechy_introspection.py:566** (LOW, Feature Completion)
  - `fixme_count += markers['FIXME']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L566)

- [ ] **entelechy_introspection.py:567** (LOW, Feature Completion)
  - `stub_count += markers['STUB']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L567)

- [ ] **entelechy_introspection.py:569** (LOW, Feature Completion)
  - `total_markers = todo_count + fixme_count + stub_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L569)

- [ ] **entelechy_introspection.py:574** (LOW, Feature Completion)
  - `self.metrics.todo_count = todo_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L574)

- [ ] **entelechy_introspection.py:575** (LOW, Feature Completion)
  - `self.metrics.fixme_count = fixme_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L575)

- [ ] **entelechy_introspection.py:576** (LOW, Feature Completion)
  - `self.metrics.stub_count = stub_count`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L576)

- [ ] **entelechy_introspection.py:579** (LOW, Feature Completion)
  - `print(f"    TODO: {todo_count}, FIXME: {fixme_count}, STUB: {stub_count}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L579)

- [ ] **entelechy_introspection.py:582** (LOW, Feature Completion)
  - `'todo_count': todo_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L582)

- [ ] **entelechy_introspection.py:583** (LOW, Feature Completion)
  - `'fixme_count': fixme_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L583)

- [ ] **entelechy_introspection.py:584** (LOW, Feature Completion)
  - `'stub_count': stub_count,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L584)

- [ ] **entelechy_introspection.py:590** (LOW, Feature Completion)
  - `"""Count TODO/FIXME/STUB markers in a directory"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L590)

- [ ] **entelechy_introspection.py:591** (LOW, Feature Completion)
  - `markers = {'TODO': 0, 'FIXME': 0, 'STUB': 0}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L591)

- [ ] **entelechy_introspection.py:603** (LOW, Feature Completion)
  - `markers['TODO'] += len(re.findall(r'\bTODO\b', content, re.IGNORECASE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L603)

- [ ] **entelechy_introspection.py:604** (LOW, Feature Completion)
  - `markers['FIXME'] += len(re.findall(r'\bFIXME\b', content, re.IGNORECASE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L604)

- [ ] **entelechy_introspection.py:605** (LOW, Feature Completion)
  - `markers['STUB'] += len(re.findall(r'\bSTUB\b', content, re.IGNORECASE))`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L605)

- [ ] **entelechy_introspection.py:612** (LOW, Feature Completion)
  - `"""Assess implementation completeness vs placeholders"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L612)

- [ ] **entelechy_introspection.py:613** (LOW, Feature Completion)
  - `# Check for small/stub files`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L613)

- [ ] **entelechy_introspection.py:637** (LOW, Feature Completion)
  - `'small_stub_files': small_files,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L637)

- [ ] **entelechy_introspection.py:680** (LOW, Feature Completion)
  - `# For now, use placeholders that will be filled by actual analysis`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L680)

- [ ] **entelechy_introspection.py:790** (LOW, Feature Completion)
  - `- **TODO Markers:** {self.metrics.todo_count}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L790)

- [ ] **entelechy_introspection.py:791** (LOW, Feature Completion)
  - `- **FIXME Markers:** {self.metrics.fixme_count}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L791)

- [ ] **entelechy_introspection.py:792** (LOW, Feature Completion)
  - `- **STUB Markers:** {self.metrics.stub_count}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L792)

- [ ] **entelechy_introspection.py:900** (LOW, Feature Completion)
  - `- Resolving placeholder implementations`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_introspection.py#L900)

- [ ] **entelechy_marker_analyzer.py:5** (LOW, Feature Completion)
  - `Comprehensive analysis tool for all code markers (TODO, FIXME, STUB, PLACEHOLDER, etc.)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L5)

- [ ] **entelechy_marker_analyzer.py:28** (LOW, Feature Completion)
  - `"""Represents a single code marker (TODO, FIXME, etc.)."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L28)

- [ ] **entelechy_marker_analyzer.py:31** (LOW, Feature Completion)
  - `marker_type: str  # TODO, FIXME, STUB, PLACEHOLDER, etc.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L31)

- [ ] **entelechy_marker_analyzer.py:53** (LOW, Feature Completion)
  - `'TODO': r'(?://|#|;|/\*|\*)\s*TODO[:;\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L53)

- [ ] **entelechy_marker_analyzer.py:54** (LOW, Feature Completion)
  - `'FIXME': r'(?://|#|;|/\*|\*)\s*(?:FIXME|XXX)[:;\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L54)

- [ ] **entelechy_marker_analyzer.py:55** (LOW, Feature Completion)
  - `'STUB': r'(?://|#|;|/\*|\*)\s*STUB[:;\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L55)

- [ ] **entelechy_marker_analyzer.py:56** (LOW, Feature Completion)
  - `'PLACEHOLDER': r'(?://|#|;|/\*|\*)\s*PLACEHOLDER[:;\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L56)

- [ ] **entelechy_marker_analyzer.py:59** (LOW, Feature Completion)
  - `'HACK': r'(?://|#|;|/\*|\*)\s*HACK[:;\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L59)

- [ ] **entelechy_marker_analyzer.py:196** (LOW, Feature Completion)
  - `content = re.sub(r'^[/\*#;\s]*(?:TODO|FIXME|XXX|STUB|PLACEHOLDER|NOT\s+IMPLEMENTED|MOCK|HACK|BUG)[:;...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L196)

- [ ] **entelechy_marker_analyzer.py:212** (LOW, Feature Completion)
  - `'FIXME': 0.85,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L212)

- [ ] **entelechy_marker_analyzer.py:214** (LOW, Feature Completion)
  - `'STUB': 0.75,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L214)

- [ ] **entelechy_marker_analyzer.py:215** (LOW, Feature Completion)
  - `'PLACEHOLDER': 0.70,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L215)

- [ ] **entelechy_marker_analyzer.py:216** (LOW, Feature Completion)
  - `'TODO': 0.60,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L216)

- [ ] **entelechy_marker_analyzer.py:217** (LOW, Feature Completion)
  - `'HACK': 0.65,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L217)

- [ ] **entelechy_marker_analyzer.py:242** (LOW, Feature Completion)
  - `elif 'implement' in content_lower or 'stub' in marker_type.lower() or 'placeholder' in marker_type.l...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L242)

- [ ] **entelechy_marker_analyzer.py:262** (LOW, Feature Completion)
  - `if marker_type in ['STUB', 'PLACEHOLDER', 'NOT_IMPLEMENTED']:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_analyzer.py#L262)

- [ ] **entelechy_marker_resolver.py:135** (LOW, Documentation)
  - `"""Get markers that are easy to resolve (documentation, simple todos)."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_resolver.py#L135)

- [ ] **entelechy_marker_resolver.py:148** (LOW, Feature Completion)
  - `resolution.marker_type == 'TODO' and len(resolution.original_content) < 50:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/entelechy_marker_resolver.py#L148)

- [ ] **find_actual_stubs.py:2** (LOW, Feature Completion)
  - `"""Find actual stub implementations that need fixing"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L2)

- [ ] **find_actual_stubs.py:7** (LOW, Feature Completion)
  - `def find_stub_implementations(repo_root):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L7)

- [ ] **find_actual_stubs.py:9** (LOW, Feature Completion)
  - `stubs = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L9)

- [ ] **find_actual_stubs.py:21** (LOW, Feature Completion)
  - `# Find function definitions followed by stub implementations`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L21)

- [ ] **find_actual_stubs.py:30** (LOW, Pattern Matching)
  - `# Check next few lines for stub patterns`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L30)

- [ ] **find_actual_stubs.py:49** (LOW, Pattern Matching)
  - `# Check for stub patterns`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L49)

- [ ] **find_actual_stubs.py:50** (LOW, Feature Completion)
  - `is_stub = False`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L50)

- [ ] **find_actual_stubs.py:51** (LOW, Feature Completion)
  - `stub_type = None`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L51)

- [ ] **find_actual_stubs.py:54** (LOW, Feature Completion)
  - `is_stub = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L54)

- [ ] **find_actual_stubs.py:55** (LOW, Feature Completion)
  - `stub_type = 'pass'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L55)

- [ ] **find_actual_stubs.py:57** (LOW, Feature Completion)
  - `is_stub = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L57)

- [ ] **find_actual_stubs.py:58** (LOW, Feature Completion)
  - `stub_type = 'ellipsis'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L58)

- [ ] **find_actual_stubs.py:60** (LOW, Feature Completion)
  - `is_stub = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L60)

- [ ] **find_actual_stubs.py:61** (LOW, Error Handling)
  - `stub_type = 'NotImplementedError'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L61)

- [ ] **find_actual_stubs.py:63** (LOW, Feature Completion)
  - `is_stub = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L63)

- [ ] **find_actual_stubs.py:64** (LOW, Feature Completion)
  - `stub_type = 'return_none'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L64)

- [ ] **find_actual_stubs.py:66** (LOW, Feature Completion)
  - `if is_stub:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L66)

- [ ] **find_actual_stubs.py:67** (LOW, Feature Completion)
  - `stubs.append({`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L67)

- [ ] **find_actual_stubs.py:72** (LOW, Feature Completion)
  - `'stub_type': stub_type,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L72)

- [ ] **find_actual_stubs.py:78** (LOW, Feature Completion)
  - `return stubs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L78)

- [ ] **find_actual_stubs.py:82** (LOW, Feature Completion)
  - `print("Scanning for stub implementations...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L82)

- [ ] **find_actual_stubs.py:83** (LOW, Feature Completion)
  - `stubs = find_stub_implementations(repo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L83)

- [ ] **find_actual_stubs.py:85** (LOW, Feature Completion)
  - `print(f"\nFound {len(stubs)} stub implementations:\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L85)

- [ ] **find_actual_stubs.py:89** (LOW, Feature Completion)
  - `for stub in stubs:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L89)

- [ ] **find_actual_stubs.py:90** (LOW, Feature Completion)
  - `if stub['file'] not in by_file:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L90)

- [ ] **find_actual_stubs.py:91** (LOW, Feature Completion)
  - `by_file[stub['file']] = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L91)

- [ ] **find_actual_stubs.py:92** (LOW, Feature Completion)
  - `by_file[stub['file']].append(stub)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L92)

- [ ] **find_actual_stubs.py:94** (LOW, Feature Completion)
  - `for filepath, file_stubs in sorted(by_file.items()):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L94)

- [ ] **find_actual_stubs.py:96** (LOW, Feature Completion)
  - `for stub in file_stubs:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L96)

- [ ] **find_actual_stubs.py:97** (LOW, Feature Completion)
  - `print(f"  Line {stub['line']}: {stub['function']}({stub['params']}) - {stub['stub_type']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L97)

- [ ] **find_actual_stubs.py:101** (LOW, Feature Completion)
  - `with open('stub_implementations.json', 'w') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L101)

- [ ] **find_actual_stubs.py:102** (LOW, Feature Completion)
  - `json.dump(stubs, f, indent=2)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L102)

- [ ] **find_actual_stubs.py:104** (LOW, Feature Completion)
  - `print(f"\n\nDetailed results saved to stub_implementations.json")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_actual_stubs.py#L104)

- [ ] **find_placeholders.py:3** (LOW, Feature Completion)
  - `Comprehensive Placeholder Detection Script`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L3)

- [ ] **find_placeholders.py:4** (LOW, Error Handling)
  - `Finds all TODO, FIXME, XXX, NotImplementedError, stub implementations, and empty functions`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L4)

- [ ] **find_placeholders.py:12** (LOW, Feature Completion)
  - `class PlaceholderDetector:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L12)

- [ ] **find_placeholders.py:15** (LOW, Feature Completion)
  - `self.placeholders = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L15)

- [ ] **find_placeholders.py:19** (LOW, Feature Completion)
  - `'TODO': r'(//|#|;|/\*)\s*TODO[:\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L19)

- [ ] **find_placeholders.py:20** (LOW, Feature Completion)
  - `'FIXME': r'(//|#|;|/\*)\s*(FIXME|XXX)[:\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L20)

- [ ] **find_placeholders.py:24** (LOW, Feature Completion)
  - `'stub': r'(//|#|;)\s*STUB[:\s]',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L24)

- [ ] **find_placeholders.py:28** (LOW, Feature Completion)
  - `"""Scan a single file for placeholders"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L28)

- [ ] **find_placeholders.py:36** (LOW, Documentation)
  - `# Skip if it's in a string or comment about placeholders`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L36)

- [ ] **find_placeholders.py:37** (LOW, Feature Completion)
  - `if 'placeholder' in line.lower() and 'find' in line.lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L37)

- [ ] **find_placeholders.py:40** (LOW, Feature Completion)
  - `self.placeholders.append({`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L40)

- [ ] **find_placeholders.py:51** (LOW, Feature Completion)
  - `"""Get surrounding context for a placeholder"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L51)

- [ ] **find_placeholders.py:68** (LOW, Feature Completion)
  - `"""Categorize placeholders by type"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L68)

- [ ] **find_placeholders.py:70** (LOW, Feature Completion)
  - `for p in self.placeholders:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L70)

- [ ] **find_placeholders.py:75** (LOW, Feature Completion)
  - `"""Categorize placeholders by file"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L75)

- [ ] **find_placeholders.py:77** (LOW, Feature Completion)
  - `for p in self.placeholders:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L77)

- [ ] **find_placeholders.py:87** (LOW, Feature Completion)
  - `'total_placeholders': len(self.placeholders),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L87)

- [ ] **find_placeholders.py:90** (LOW, Feature Completion)
  - `'detailed_placeholders': self.placeholders,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L90)

- [ ] **find_placeholders.py:92** (LOW, Feature Completion)
  - `'files_with_most_placeholders': sorted(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L92)

- [ ] **find_placeholders.py:102** (LOW, Feature Completion)
  - `detector = PlaceholderDetector('/home/ubuntu/opencog-unified')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L102)

- [ ] **find_placeholders.py:103** (LOW, Feature Completion)
  - `print("Scanning repository for placeholders...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L103)

- [ ] **find_placeholders.py:105** (LOW, Feature Completion)
  - `print(f"Found {len(detector.placeholders)} placeholders")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L105)

- [ ] **find_placeholders.py:110** (LOW, Feature Completion)
  - `with open('placeholder_analysis.json', 'w') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L110)

- [ ] **find_placeholders.py:113** (LOW, Feature Completion)
  - `print("\nPlaceholder Summary:")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L113)

- [ ] **find_placeholders.py:114** (LOW, Feature Completion)
  - `print(f"Total: {report['total_placeholders']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L114)

- [ ] **find_placeholders.py:119** (LOW, Feature Completion)
  - `print("\nTop 10 Files with Most Placeholders:")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L119)

- [ ] **find_placeholders.py:120** (LOW, Feature Completion)
  - `for filename, count in report['files_with_most_placeholders'][:10]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/find_placeholders.py#L120)

- [ ] **fixme_easy_wins.py:3** (LOW, Feature Completion)
  - `FIXME Easy Wins Implementation Script`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L3)

- [ ] **fixme_easy_wins.py:5** (LOW, Feature Completion)
  - `This script implements the immediate actions from FIXME-SORTED-CATALOG.md`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L5)

- [ ] **fixme_easy_wins.py:18** (LOW, Feature Completion)
  - `from fixme_resolution_tracker import FIXMEResolutionTracker`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L18)

- [ ] **fixme_easy_wins.py:21** (LOW, Feature Completion)
  - `"""Implements easy FIXME resolutions systematically."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L21)

- [ ] **fixme_easy_wins.py:25** (LOW, Feature Completion)
  - `self.tracker = FIXMEResolutionTracker(repo_root)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L25)

- [ ] **fixme_easy_wins.py:52** (LOW, Feature Completion)
  - `"xxx fixme replace" in item.fixme_text.lower() or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L52)

- [ ] **fixme_easy_wins.py:53** (LOW, Feature Completion)
  - `"temp hack alert" in item.fixme_text.lower() or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L53)

- [ ] **fixme_easy_wins.py:54** (LOW, Feature Completion)
  - `"might no longer exist" in item.fixme_text.lower()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L54)

- [ ] **fixme_easy_wins.py:88** (LOW, Documentation)
  - `if "xxx fixme replace below by real docs" in line_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L88)

- [ ] **fixme_easy_wins.py:89** (LOW, Documentation)
  - `return line.replace("XXX FIXME replace below by real docs.",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L89)

- [ ] **fixme_easy_wins.py:93** (LOW, Feature Completion)
  - `return line.replace("FIXME:", "NOTE:").replace("might no longer exist",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L93)

- [ ] **fixme_easy_wins.py:96** (LOW, Feature Completion)
  - `elif "temp hack alert" in line_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L96)

- [ ] **fixme_easy_wins.py:97** (LOW, Feature Completion)
  - `return line.replace("XXX Temp hack alert.", "NOTE: Legacy compatibility handling.")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L97)

- [ ] **fixme_easy_wins.py:99** (LOW, Documentation)
  - `# For other cases, convert FIXME to descriptive comments`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L99)

- [ ] **fixme_easy_wins.py:100** (LOW, Feature Completion)
  - `elif "xxx fixme" in line_lower or "fixme:" in line_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L100)

- [ ] **fixme_easy_wins.py:101** (LOW, Feature Completion)
  - `# Convert to NOTE or TODO depending on content`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L101)

- [ ] **fixme_easy_wins.py:103** (LOW, Feature Completion)
  - `return line.replace("XXX FIXME", "NOTE:").replace("FIXME:", "NOTE:")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L103)

- [ ] **fixme_easy_wins.py:105** (LOW, Feature Completion)
  - `return line.replace("XXX FIXME", "TODO:").replace("FIXME:", "TODO:")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L105)

- [ ] **fixme_easy_wins.py:130** (LOW, Feature Completion)
  - `report_file = self.repo_root / "FIXME_RESOLUTION_PROGRESS_REPORT.md"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L130)

- [ ] **fixme_easy_wins.py:139** (LOW, Feature Completion)
  - `parser = argparse.ArgumentParser(description="FIXME Easy Wins Implementation")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_easy_wins.py#L139)

- [ ] **fixme_resolution_tracker.py:3** (LOW, Feature Completion)
  - `FIXME Resolution Tracker`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L3)

- [ ] **fixme_resolution_tracker.py:5** (LOW, Feature Completion)
  - `This tool implements the "next steps" from FIXME-SORTED-CATALOG.md by providing:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L5)

- [ ] **fixme_resolution_tracker.py:6** (LOW, Feature Completion)
  - `1. Progress tracking for FIXME resolution`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L6)

- [ ] **fixme_resolution_tracker.py:15** (LOW, Feature Completion)
  - `- Long Term (12+ months): Complete all FIXME items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L15)

- [ ] **fixme_resolution_tracker.py:28** (LOW, Feature Completion)
  - `class FIXMEResolution:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L28)

- [ ] **fixme_resolution_tracker.py:29** (LOW, Feature Completion)
  - `"""Tracks resolution status of a FIXME item."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L29)

- [ ] **fixme_resolution_tracker.py:32** (LOW, Feature Completion)
  - `fixme_text: str`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L32)

- [ ] **fixme_resolution_tracker.py:47** (LOW, Feature Completion)
  - `class FIXMEResolutionTracker:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L47)

- [ ] **fixme_resolution_tracker.py:48** (LOW, Feature Completion)
  - `"""Manages FIXME resolution progress and implements next steps."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L48)

- [ ] **fixme_resolution_tracker.py:52** (LOW, Feature Completion)
  - `self.tracking_file = self.repo_root / "fixme_resolution_progress.json"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L52)

- [ ] **fixme_resolution_tracker.py:53** (LOW, Feature Completion)
  - `self.resolutions: Dict[str, FIXMEResolution] = {}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L53)

- [ ] **fixme_resolution_tracker.py:63** (LOW, Feature Completion)
  - `self.resolutions[key] = FIXMEResolution(**item_data)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L63)

- [ ] **fixme_resolution_tracker.py:64** (LOW, Feature Completion)
  - `print(f"Loaded {len(self.resolutions)} tracked FIXME items")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L64)

- [ ] **fixme_resolution_tracker.py:76** (LOW, Feature Completion)
  - `print(f"Saved progress for {len(self.resolutions)} FIXME items")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L76)

- [ ] **fixme_resolution_tracker.py:79** (LOW, Feature Completion)
  - `"""Import FIXME items from the sorted catalog."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L79)

- [ ] **fixme_resolution_tracker.py:84** (LOW, Feature Completion)
  - `# Parse the catalog markdown file to extract FIXME items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L84)

- [ ] **fixme_resolution_tracker.py:92** (LOW, Feature Completion)
  - `"""Parse the FIXME catalog markdown content."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L92)

- [ ] **fixme_resolution_tracker.py:112** (LOW, Feature Completion)
  - `# Extract FIXME entries`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L112)

- [ ] **fixme_resolution_tracker.py:114** (LOW, Feature Completion)
  - `self._extract_fixme_entry(lines, i, current_difficulty, current_component)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L114)

- [ ] **fixme_resolution_tracker.py:116** (LOW, Feature Completion)
  - `def _extract_fixme_entry(self, lines: List[str], start_idx: int, difficulty: str, component: str):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L116)

- [ ] **fixme_resolution_tracker.py:117** (LOW, Feature Completion)
  - `"""Extract a single FIXME entry from the catalog."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L117)

- [ ] **fixme_resolution_tracker.py:144** (LOW, Feature Completion)
  - `self.resolutions[key] = FIXMEResolution(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L144)

- [ ] **fixme_resolution_tracker.py:147** (LOW, Feature Completion)
  - `fixme_text=issue_text,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L147)

- [ ] **fixme_resolution_tracker.py:154** (LOW, Error Handling)
  - `print(f"Error parsing FIXME entry at line {start_idx}: {e}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L154)

- [ ] **fixme_resolution_tracker.py:156** (LOW, Feature Completion)
  - `def get_easy_wins(self) -> List[FIXMEResolution]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L156)

- [ ] **fixme_resolution_tracker.py:161** (LOW, Feature Completion)
  - `def get_by_component(self, component: str) -> List[FIXMEResolution]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L161)

- [ ] **fixme_resolution_tracker.py:162** (LOW, Testing)
  - `"""Get FIXME items for a specific component."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L162)

- [ ] **fixme_resolution_tracker.py:169** (LOW, Feature Completion)
  - `report.append("# FIXME Resolution Progress Report")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L169)

- [ ] **fixme_resolution_tracker.py:176** (LOW, Feature Completion)
  - `report.append(f"- Total FIXME items: {stats.get('total', 0)}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L176)

- [ ] **fixme_resolution_tracker.py:197** (LOW, Feature Completion)
  - `report.append(f"- `{item.file_path}:{item.line_number}` - {item.fixme_text[:60]}...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L197)

- [ ] **fixme_resolution_tracker.py:250** (LOW, Feature Completion)
  - `"target": "Complete all FIXME items",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L250)

- [ ] **fixme_resolution_tracker.py:280** (LOW, Feature Completion)
  - `recommendations.append("Set up weekly FIXME resolution review meetings")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L280)

- [ ] **fixme_resolution_tracker.py:281** (LOW, Feature Completion)
  - `recommendations.append("Create contributor guidelines to prevent new FIXME accumulation")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L281)

- [ ] **fixme_resolution_tracker.py:286** (LOW, Feature Completion)
  - `"""Mark a FIXME item as in progress."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L286)

- [ ] **fixme_resolution_tracker.py:297** (LOW, Feature Completion)
  - `"""Mark a FIXME item as resolved."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L297)

- [ ] **fixme_resolution_tracker.py:309** (LOW, Feature Completion)
  - `"""Main entry point for FIXME resolution tracking."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L309)

- [ ] **fixme_resolution_tracker.py:312** (LOW, Feature Completion)
  - `parser = argparse.ArgumentParser(description="FIXME Resolution Tracker")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L312)

- [ ] **fixme_resolution_tracker.py:314** (LOW, Feature Completion)
  - `parser.add_argument("--import-catalog", help="Import from FIXME catalog file")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L314)

- [ ] **fixme_resolution_tracker.py:320** (LOW, Feature Completion)
  - `tracker = FIXMEResolutionTracker(args.repo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L320)

- [ ] **fixme_resolution_tracker.py:330** (LOW, Feature Completion)
  - `report_file = Path(args.repo) / "FIXME_RESOLUTION_PROGRESS_REPORT.md"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L330)

- [ ] **fixme_resolution_tracker.py:340** (LOW, Feature Completion)
  - `print(f"    {item.fixme_text[:80]}...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fixme_resolution_tracker.py#L340)

- [ ] **fragmentation_detector.py:6** (LOW, Feature Completion)
  - `by analyzing TODO/FIXME/STUB markers and their context.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L6)

- [ ] **fragmentation_detector.py:22** (LOW, Feature Completion)
  - `marker_type: str  # TODO, FIXME, STUB`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L22)

- [ ] **fragmentation_detector.py:82** (LOW, Feature Completion)
  - `# Check for TODO/FIXME/STUB`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L82)

- [ ] **fragmentation_detector.py:83** (LOW, Feature Completion)
  - `for marker in ['TODO', 'FIXME', 'STUB']:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L83)

- [ ] **fragmentation_detector.py:151** (LOW, Feature Completion)
  - `'FIXME': 0.7,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L151)

- [ ] **fragmentation_detector.py:152** (LOW, Feature Completion)
  - `'TODO': 0.5,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L152)

- [ ] **fragmentation_detector.py:153** (LOW, Feature Completion)
  - `'STUB': 0.6,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/fragmentation_detector.py#L153)

- [ ] **generate_ascii_dashboard.py:140** (LOW, Feature Completion)
  - `todo = data['metrics']['todo_count']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L140)

- [ ] **generate_ascii_dashboard.py:141** (LOW, Feature Completion)
  - `fixme = data['metrics']['fixme_count']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L141)

- [ ] **generate_ascii_dashboard.py:142** (LOW, Feature Completion)
  - `stub = data['metrics']['stub_count']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L142)

- [ ] **generate_ascii_dashboard.py:143** (LOW, Feature Completion)
  - `total_markers = todo + fixme + stub`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L143)

- [ ] **generate_ascii_dashboard.py:145** (LOW, Feature Completion)
  - `report.append(f"│  📋 TODO:   {todo:>5}  {create_bar_chart(todo/total_markers if total_markers > 0 el...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L145)

- [ ] **generate_ascii_dashboard.py:146** (LOW, Feature Completion)
  - `report.append(f"│  🔧 FIXME:  {fixme:>5}  {create_bar_chart(fixme/total_markers if total_markers > 0 ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L146)

- [ ] **generate_ascii_dashboard.py:147** (LOW, Feature Completion)
  - `report.append(f"│  🚧 STUB:   {stub:>5}  {create_bar_chart(stub/total_markers if total_markers > 0 el...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_ascii_dashboard.py#L147)

- [ ] **generate_entelechy_dashboard.py:179** (LOW, Feature Completion)
  - `categories = ['TODO', 'FIXME', 'STUB']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_entelechy_dashboard.py#L179)

- [ ] **generate_entelechy_dashboard.py:181** (LOW, Feature Completion)
  - `data['metrics']['todo_count'],`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_entelechy_dashboard.py#L181)

- [ ] **generate_entelechy_dashboard.py:182** (LOW, Feature Completion)
  - `data['metrics']['fixme_count'],`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_entelechy_dashboard.py#L182)

- [ ] **generate_entelechy_dashboard.py:183** (LOW, Feature Completion)
  - `data['metrics']['stub_count'],`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_entelechy_dashboard.py#L183)

- [ ] **generate_fixme_catalog.py:3** (LOW, Feature Completion)
  - `Generate a comprehensive sorted FIXME catalog by difficulty`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L3)

- [ ] **generate_fixme_catalog.py:9** (LOW, Feature Completion)
  - `def generate_fixme_catalog():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L9)

- [ ] **generate_fixme_catalog.py:10** (LOW, Documentation)
  - `"""Generate the sorted FIXME catalog markdown document."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L10)

- [ ] **generate_fixme_catalog.py:13** (LOW, Feature Completion)
  - `with open('fixme_analysis_report.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L13)

- [ ] **generate_fixme_catalog.py:16** (LOW, Feature Completion)
  - `markdown_content = """# OpenCog Unified FIXME Implementation Catalog`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L16)

- [ ] **generate_fixme_catalog.py:20** (LOW, Documentation)
  - `This document provides a comprehensive categorization of all **{total_instances} FIXME instances** f...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L20)

- [ ] **generate_fixme_catalog.py:136** (LOW, Feature Completion)
  - `- Stub replacements`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L136)

- [ ] **generate_fixme_catalog.py:200** (LOW, Feature Completion)
  - `- Complete all FIXME items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L200)

- [ ] **generate_fixme_catalog.py:206** (LOW, Feature Completion)
  - `*This catalog was generated automatically by analyzing {total_instances} FIXME instances across {fil...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L206)

- [ ] **generate_fixme_catalog.py:215** (LOW, Documentation)
  - `"""Format a single FIXME instance for the markdown document."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L215)

- [ ] **generate_fixme_catalog.py:217** (LOW, Feature Completion)
  - `# Truncate long FIXME text`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L217)

- [ ] **generate_fixme_catalog.py:218** (LOW, Feature Completion)
  - `fixme_text = instance['fixme_text']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L218)

- [ ] **generate_fixme_catalog.py:219** (LOW, Feature Completion)
  - `if len(fixme_text) > 100:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L219)

- [ ] **generate_fixme_catalog.py:220** (LOW, Feature Completion)
  - `fixme_text = fixme_text[:97] + "..."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L220)

- [ ] **generate_fixme_catalog.py:222** (LOW, Feature Completion)
  - `# Clean up the FIXME text for display`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L222)

- [ ] **generate_fixme_catalog.py:223** (LOW, Feature Completion)
  - `fixme_display = fixme_text.replace('// ', '').replace('/// ', '').replace('; ', '').replace('# ', ''...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L223)

- [ ] **generate_fixme_catalog.py:228** (LOW, Feature Completion)
  - `**Issue:** {fixme_display}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L228)

- [ ] **generate_fixme_catalog.py:245** (LOW, Feature Completion)
  - `catalog_content = generate_fixme_catalog()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L245)

- [ ] **generate_fixme_catalog.py:247** (LOW, Feature Completion)
  - `with open('FIXME-SORTED-CATALOG.md', 'w') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L247)

- [ ] **generate_fixme_catalog.py:250** (LOW, Feature Completion)
  - `print("Generated FIXME-SORTED-CATALOG.md")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L250)

- [ ] **generate_fixme_catalog.py:251** (LOW, Documentation)
  - `print("Document contains comprehensive categorization of all FIXME instances")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_fixme_catalog.py#L251)

- [ ] **generate_progress_report.py:2** (LOW, Feature Completion)
  - `"""Generate a progress report on placeholder implementation"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L2)

- [ ] **generate_progress_report.py:12** (LOW, Feature Completion)
  - `with open(repo_root / 'placeholder_analysis.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L12)

- [ ] **generate_progress_report.py:14** (LOW, Feature Completion)
  - `with open(repo_root / 'stub_implementations.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L14)

- [ ] **generate_progress_report.py:15** (LOW, Feature Completion)
  - `stubs = json.load(f)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L15)

- [ ] **generate_progress_report.py:25** (LOW, Feature Completion)
  - `total_placeholders=analysis['total_placeholders']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L25)

- [ ] **generate_progress_report.py:26** (LOW, Feature Completion)
  - `fixme_count=analysis['by_type'].get('FIXME', 0)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L26)

- [ ] **generate_progress_report.py:27** (LOW, Feature Completion)
  - `todo_count=analysis['by_type'].get('TODO', 0)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L27)

- [ ] **generate_progress_report.py:28** (LOW, Documentation)
  - `stub_comment_count=analysis['by_type'].get('stub', 0)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L28)

- [ ] **generate_progress_report.py:30** (LOW, Feature Completion)
  - `stub_func_count=len(stubs)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L30)

- [ ] **generate_progress_report.py:40** (LOW, Feature Completion)
  - `# Progress Report: Placeholder Implementation in opencog-unified`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L40)

- [ ] **generate_progress_report.py:47** (LOW, Feature Completion)
  - `This report details the progress made in identifying and resolving placeholder implementations withi...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L47)

- [ ] **generate_progress_report.py:49** (LOW, Documentation)
  - `A total of **{total_fixes}** placeholders have been successfully addressed, primarily focusing on do...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L49)

- [ ] **generate_progress_report.py:53** (LOW, Feature Completion)
  - `The automated analysis identified the following distribution of placeholders:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L53)

- [ ] **generate_progress_report.py:55** (LOW, Feature Completion)
  - `| Placeholder Type      | Count |`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L55)

- [ ] **generate_progress_report.py:57** (LOW, Feature Completion)
  - `| **FIXME**             | {fixme_count} |`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L57)

- [ ] **generate_progress_report.py:58** (LOW, Feature Completion)
  - `| **TODO**              | {todo_count}  |`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L58)

- [ ] **generate_progress_report.py:59** (LOW, Documentation)
  - `| **Stub Comments**     | {stub_comment_count} |`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L59)

- [ ] **generate_progress_report.py:61** (LOW, Feature Completion)
  - `| **Total**             | **{total_placeholders}** |`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L61)

- [ ] **generate_progress_report.py:63** (LOW, Feature Completion)
  - `Additionally, **{stub_func_count}** actual stub functions (with `pass` or similar) were found, one o...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L63)

- [ ] **generate_progress_report.py:69** (LOW, Feature Completion)
  - `### 3.1. Stub Function Implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L69)

- [ ] **generate_progress_report.py:77** (LOW, Documentation)
  - `{obsolete_fixes_count} `FIXME` and `TODO` comments referring to obsolete or deprecated code were upd...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L77)

- [ ] **generate_progress_report.py:84** (LOW, Documentation)
  - `{clarification_fixes_count} `TODO` items requesting clarification were addressed by adding comments ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L84)

- [ ] **generate_progress_report.py:107** (LOW, Feature Completion)
  - `1.  **Address Actionable `TODO`s:** Focus on the **{actionable_count}** identified `TODO` items that...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L107)

- [ ] **generate_progress_report.py:109** (LOW, Feature Completion)
  - `3.  **Categorize and Prioritize `FIXME`s:** Begin a more detailed analysis and categorization of the...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L109)

- [ ] **generate_progress_report.py:114** (LOW, Feature Completion)
  - `This initial phase of work has successfully addressed a number of low-hanging fruit, improving the o...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/generate_progress_report.py#L114)

- [ ] **implement_fixes.py:3** (LOW, Feature Completion)
  - `Implement fixes for placeholder code`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L3)

- [ ] **implement_fixes.py:4** (LOW, Documentation)
  - `Focuses on easy wins: removing temporary hacks, cleaning up code, adding documentation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L4)

- [ ] **implement_fixes.py:12** (LOW, Feature Completion)
  - `class PlaceholderFixer:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L12)

- [ ] **implement_fixes.py:18** (LOW, Feature Completion)
  - `def apply_fix(self, placeholder):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L18)

- [ ] **implement_fixes.py:19** (LOW, Testing)
  - `"""Apply a fix to a specific placeholder"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L19)

- [ ] **implement_fixes.py:20** (LOW, Feature Completion)
  - `filepath = self.repo_root / placeholder['file']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L20)

- [ ] **implement_fixes.py:24** (LOW, Feature Completion)
  - `'placeholder': placeholder,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L24)

- [ ] **implement_fixes.py:33** (LOW, Feature Completion)
  - `line_idx = placeholder['line'] - 1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L33)

- [ ] **implement_fixes.py:36** (LOW, Feature Completion)
  - `'placeholder': placeholder,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L36)

- [ ] **implement_fixes.py:42** (LOW, Feature Completion)
  - `fixed_line = self._fix_line(original_line, placeholder)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L42)

- [ ] **implement_fixes.py:57** (LOW, Feature Completion)
  - `'line': placeholder['line'],`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L57)

- [ ] **implement_fixes.py:60** (LOW, Feature Completion)
  - `'type': placeholder['type']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L60)

- [ ] **implement_fixes.py:65** (LOW, Feature Completion)
  - `'placeholder': placeholder,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L65)

- [ ] **implement_fixes.py:72** (LOW, Feature Completion)
  - `'placeholder': placeholder,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L72)

- [ ] **implement_fixes.py:77** (LOW, Feature Completion)
  - `def _fix_line(self, line, placeholder):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L77)

- [ ] **implement_fixes.py:81** (LOW, Documentation)
  - `# Remove temporary hacks/workarounds - comment them out instead of deleting`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L81)

- [ ] **implement_fixes.py:82** (LOW, Feature Completion)
  - `if any(keyword in content_lower for keyword in ['xxx remove', 'todo remove', 'fixme remove']):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L82)

- [ ] **implement_fixes.py:86** (LOW, Feature Completion)
  - `return line.replace('XXX', 'ARCHIVED').replace('TODO', 'ARCHIVED').replace('FIXME', 'ARCHIVED')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L86)

- [ ] **implement_fixes.py:88** (LOW, Feature Completion)
  - `return line.replace('XXX', 'ARCHIVED').replace('TODO', 'ARCHIVED').replace('FIXME', 'ARCHIVED')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L88)

- [ ] **implement_fixes.py:90** (LOW, Feature Completion)
  - `return line.replace('XXX', 'ARCHIVED').replace('TODO', 'ARCHIVED').replace('FIXME', 'ARCHIVED')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L90)

- [ ] **implement_fixes.py:92** (LOW, Documentation)
  - `# Convert XXX/FIXME to NOTE for informational comments`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L92)

- [ ] **implement_fixes.py:93** (LOW, Feature Completion)
  - `if 'xxx' in content_lower and ('hack' in content_lower or 'temp' in content_lower):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L93)

- [ ] **implement_fixes.py:94** (LOW, Feature Completion)
  - `line = re.sub(r'XXX\s+FIXME', 'NOTE', line, flags=re.IGNORECASE)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L94)

- [ ] **implement_fixes.py:95** (LOW, Feature Completion)
  - `line = re.sub(r'XXX', 'NOTE', line, flags=re.IGNORECASE)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L95)

- [ ] **implement_fixes.py:96** (LOW, Feature Completion)
  - `line = re.sub(r'FIXME', 'NOTE', line, flags=re.IGNORECASE)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L96)

- [ ] **implement_fixes.py:99** (LOW, Testing)
  - `# Convert TODO to more specific action items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L99)

- [ ] **implement_fixes.py:100** (LOW, Feature Completion)
  - `if 'todo' in content_lower and 'remove' in content_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L100)

- [ ] **implement_fixes.py:101** (LOW, Feature Completion)
  - `return line.replace('TODO', 'DEPRECATED').replace('todo', 'DEPRECATED')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L101)

- [ ] **implement_fixes.py:104** (LOW, Feature Completion)
  - `if 'temp' in content_lower and ('hack' in content_lower or 'quick' in content_lower):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L104)

- [ ] **implement_fixes.py:105** (LOW, Feature Completion)
  - `line = re.sub(r'XXX\s+Temp', 'Legacy', line, flags=re.IGNORECASE)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L105)

- [ ] **implement_fixes.py:106** (LOW, Feature Completion)
  - `line = re.sub(r'TODO:', 'Legacy:', line, flags=re.IGNORECASE)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L106)

- [ ] **implement_fixes.py:123** (LOW, Feature Completion)
  - `# Load categorized placeholders`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L123)

- [ ] **implement_fixes.py:124** (LOW, Feature Completion)
  - `with open('placeholder_categorization.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L124)

- [ ] **implement_fixes.py:133** (LOW, Feature Completion)
  - `fixer = PlaceholderFixer('/home/ubuntu/opencog-unified')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L133)

- [ ] **implement_fixes.py:135** (LOW, Feature Completion)
  - `for i, placeholder in enumerate(easy_fixes[:30], 1):  # Start with first 30`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L135)

- [ ] **implement_fixes.py:136** (LOW, Feature Completion)
  - `print(f"Processing {i}/{min(30, len(easy_fixes))}: {placeholder['file']}:{placeholder['line']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L136)

- [ ] **implement_fixes.py:137** (LOW, Feature Completion)
  - `fixer.apply_fix(placeholder)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_fixes.py#L137)

- [ ] **implement_functional_fixes.py:3** (LOW, Feature Completion)
  - `Implement functional fixes for placeholder code`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L3)

- [ ] **implement_functional_fixes.py:29** (LOW, Error Handling)
  - `if 'NotImplementedError' in lines[line_idx] or 'not implemented' in lines[line_idx].lower():`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L29)

- [ ] **implement_functional_fixes.py:42** (LOW, Feature Completion)
  - `new_impl = ' ' * indent + f'# TODO: Implement {func_name} functionality\n'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L42)

- [ ] **implement_functional_fixes.py:44** (LOW, Feature Completion)
  - `new_impl += ' ' * indent + 'return None  # Placeholder return\n'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L44)

- [ ] **implement_functional_fixes.py:74** (LOW, Feature Completion)
  - `"""Add input validation where TODOs mention checking or validating"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L74)

- [ ] **implement_functional_fixes.py:146** (LOW, Feature Completion)
  - `# Load placeholder analysis`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L146)

- [ ] **implement_functional_fixes.py:147** (LOW, Feature Completion)
  - `with open('placeholder_analysis.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L147)

- [ ] **implement_functional_fixes.py:150** (LOW, Feature Completion)
  - `placeholders = data['detailed_placeholders']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L150)

- [ ] **implement_functional_fixes.py:152** (LOW, Error Handling)
  - `# Filter for NotImplementedError and validation TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L152)

- [ ] **implement_functional_fixes.py:153** (LOW, Error Handling)
  - `not_implemented = [p for p in placeholders if p['type'] == 'NotImplementedError']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L153)

- [ ] **implement_functional_fixes.py:154** (LOW, Feature Completion)
  - `validation_todos = [p for p in placeholders`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L154)

- [ ] **implement_functional_fixes.py:155** (LOW, Feature Completion)
  - `if p['type'] == 'TODO' and`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L155)

- [ ] **implement_functional_fixes.py:159** (LOW, Feature Completion)
  - `print(f"Found {len(validation_todos)} validation TODOs")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L159)

- [ ] **implement_functional_fixes.py:165** (LOW, Feature Completion)
  - `for i, placeholder in enumerate(not_implemented[:5], 1):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L165)

- [ ] **implement_functional_fixes.py:166** (LOW, Feature Completion)
  - `filepath = fixer.repo_root / placeholder['file']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L166)

- [ ] **implement_functional_fixes.py:167** (LOW, Feature Completion)
  - `print(f"  {i}/{min(5, len(not_implemented))}: {placeholder['file']}:{placeholder['line']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L167)

- [ ] **implement_functional_fixes.py:168** (LOW, Error Handling)
  - `fixer.implement_error_handling(filepath, placeholder['line'], placeholder['context'])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L168)

- [ ] **implement_functional_fixes.py:170** (LOW, Feature Completion)
  - `# Process validation TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L170)

- [ ] **implement_functional_fixes.py:171** (LOW, Feature Completion)
  - `print("\nProcessing validation TODOs...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L171)

- [ ] **implement_functional_fixes.py:172** (LOW, Feature Completion)
  - `for i, placeholder in enumerate(validation_todos[:5], 1):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L172)

- [ ] **implement_functional_fixes.py:173** (LOW, Feature Completion)
  - `filepath = fixer.repo_root / placeholder['file']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L173)

- [ ] **implement_functional_fixes.py:174** (LOW, Feature Completion)
  - `print(f"  {i}/{min(5, len(validation_todos))}: {placeholder['file']}:{placeholder['line']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L174)

- [ ] **implement_functional_fixes.py:175** (LOW, Feature Completion)
  - `fixer.add_validation_checks(filepath, placeholder['line'], placeholder['context'])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_functional_fixes.py#L175)

- [ ] **implement_missing_features.py:3** (LOW, Feature Completion)
  - `Implement missing features identified in TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_missing_features.py#L3)

- [ ] **implement_missing_features.py:25** (LOW, Feature Completion)
  - `# Check if this is the timestamp TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_missing_features.py#L25)

- [ ] **implement_missing_features.py:51** (LOW, Feature Completion)
  - `# Replace TODO with implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_missing_features.py#L51)

- [ ] **implement_placeholders.py:3** (LOW, Feature Completion)
  - `Implement fixes for placeholder code`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L3)

- [ ] **implement_placeholders.py:10** (LOW, Feature Completion)
  - `class PlaceholderImplementer:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L10)

- [ ] **implement_placeholders.py:28** (LOW, Feature Completion)
  - `# Replace FIXME/TODO about obsolete code with NOTE`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L28)

- [ ] **implement_placeholders.py:30** (LOW, Feature Completion)
  - `# Change FIXME/TODO to NOTE`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L30)

- [ ] **implement_placeholders.py:31** (LOW, Feature Completion)
  - `new_line = re.sub(r'(//|#|;)\s*(FIXME|TODO|XXX)\s*', r'\1 NOTE: ', original_line)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L31)

- [ ] **implement_placeholders.py:64** (LOW, Documentation)
  - `"""Add clarifying comments where TODOs mention clarification"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L64)

- [ ] **implement_placeholders.py:78** (LOW, Feature Completion)
  - `# Keep the TODO but add a clarification note`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L78)

- [ ] **implement_placeholders.py:136** (LOW, Feature Completion)
  - `implementer = PlaceholderImplementer('/home/ubuntu/opencog-unified')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L136)

- [ ] **implement_placeholders.py:146** (LOW, Feature Completion)
  - `# Process clarification TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_placeholders.py#L146)

- [ ] **implement_proper_functions.py:3** (LOW, Feature Completion)
  - `Implement proper functional code for stub functions`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L3)

- [ ] **implement_proper_functions.py:169** (LOW, Feature Completion)
  - `# Load placeholder analysis`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L169)

- [ ] **implement_proper_functions.py:170** (LOW, Feature Completion)
  - `with open('placeholder_analysis.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L170)

- [ ] **implement_proper_functions.py:173** (LOW, Feature Completion)
  - `# Find question rule stubs in rule-helpers.scm`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L173)

- [ ] **implement_proper_functions.py:174** (LOW, Feature Completion)
  - `question_stubs = [p for p in data['detailed_placeholders']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L174)

- [ ] **implement_proper_functions.py:178** (LOW, Feature Completion)
  - `print(f"Found {len(question_stubs)} question rule stubs to implement")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L178)

- [ ] **implement_proper_functions.py:183** (LOW, Feature Completion)
  - `for i, placeholder in enumerate(question_stubs, 1):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L183)

- [ ] **implement_proper_functions.py:184** (LOW, Feature Completion)
  - `filepath = implementer.repo_root / placeholder['file']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L184)

- [ ] **implement_proper_functions.py:187** (LOW, Pattern Matching)
  - `match = re.search(r'\(define(?:-public)?\s+\((\w+)([^)]*)\)', placeholder['context'])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L187)

- [ ] **implement_proper_functions.py:193** (LOW, Feature Completion)
  - `print(f"  {i}/{len(question_stubs)}: {func_name}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L193)

- [ ] **implement_proper_functions.py:194** (LOW, Feature Completion)
  - `implementer.implement_question_rule(filepath, placeholder['line'], func_name, params)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_proper_functions.py#L194)

- [ ] **implement_scheme_stubs.py:3** (LOW, Feature Completion)
  - `Implement stub functions in Scheme files`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L3)

- [ ] **implement_scheme_stubs.py:11** (LOW, Feature Completion)
  - `class SchemeStubImplementer:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L11)

- [ ] **implement_scheme_stubs.py:17** (LOW, Feature Completion)
  - `def implement_stub(self, filepath, line_num):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L17)

- [ ] **implement_scheme_stubs.py:18** (LOW, Feature Completion)
  - `"""Implement a stub function that throws 'not-implemented"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L18)

- [ ] **implement_scheme_stubs.py:54** (LOW, Feature Completion)
  - `backup_path = filepath.with_suffix(filepath.suffix + '.stub_bak')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L54)

- [ ] **implement_scheme_stubs.py:89** (LOW, Feature Completion)
  - `impl = indent_str + '; TODO: Implement full question handling logic\n'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L89)

- [ ] **implement_scheme_stubs.py:96** (LOW, Feature Completion)
  - `impl = indent_str + '; TODO: Implement full SVO/SVIO relationship logic\n'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L96)

- [ ] **implement_scheme_stubs.py:103** (LOW, Feature Completion)
  - `impl = indent_str + '; TODO: Implement prepositional object handling\n'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L103)

- [ ] **implement_scheme_stubs.py:110** (LOW, Feature Completion)
  - `impl = indent_str + '; TODO: Implement ' + func_name + ' functionality\n'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L110)

- [ ] **implement_scheme_stubs.py:127** (LOW, Feature Completion)
  - `# Load placeholder analysis`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L127)

- [ ] **implement_scheme_stubs.py:128** (LOW, Feature Completion)
  - `with open('placeholder_analysis.json', 'r') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L128)

- [ ] **implement_scheme_stubs.py:132** (LOW, Feature Completion)
  - `not_implemented = [p for p in data['detailed_placeholders']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L132)

- [ ] **implement_scheme_stubs.py:136** (LOW, Feature Completion)
  - `print(f"Found {len(not_implemented)} not-implemented stubs")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L136)

- [ ] **implement_scheme_stubs.py:138** (LOW, Feature Completion)
  - `implementer = SchemeStubImplementer('/home/ubuntu/opencog-unified')`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L138)

- [ ] **implement_scheme_stubs.py:140** (LOW, Feature Completion)
  - `print("\nImplementing stubs...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L140)

- [ ] **implement_scheme_stubs.py:141** (LOW, Feature Completion)
  - `for i, placeholder in enumerate(not_implemented, 1):`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L141)

- [ ] **implement_scheme_stubs.py:142** (LOW, Feature Completion)
  - `filepath = implementer.repo_root / placeholder['file']`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L142)

- [ ] **implement_scheme_stubs.py:143** (LOW, Feature Completion)
  - `print(f"  {i}/{len(not_implemented)}: {placeholder['file']}:{placeholder['line']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L143)

- [ ] **implement_scheme_stubs.py:144** (LOW, Feature Completion)
  - `implementer.implement_stub(filepath, placeholder['line'])`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L144)

- [ ] **implement_scheme_stubs.py:149** (LOW, Feature Completion)
  - `with open('scheme_stubs_report.json', 'w') as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L149)

- [ ] **implement_scheme_stubs.py:152** (LOW, Feature Completion)
  - `print(f"\n\nStubs Implemented: {report['summary']['total_implemented']}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/implement_scheme_stubs.py#L152)

- [ ] **language-learning/src/common/fileconfman.py:43** (LOW, Feature Completion)
  - `print("save_config() is not implemented.")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/common/fileconfman.py#L43)

- [ ] **language-learning/src/grammar_learner/category_learner.py:32** (LOW, Feature Completion)
  - `log = OrderedDict()  # FIXME: log » response`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/category_learner.py#L32)

- [ ] **language-learning/src/grammar_learner/category_learner.py:53** (LOW, Feature Completion)
  - `except:  # FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/category_learner.py#L53)

- [ ] **language-learning/src/grammar_learner/category_learner.py:170** (LOW, Feature Completion)
  - `# 81231 cleanup after upstream merge and conflicts resolution (FIXME: 2nd check)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/category_learner.py#L170)

- [ ] **language-learning/src/grammar_learner/clustering.py:361** (LOW, Feature Completion)
  - `# TODO: n_clusters ⇒ best_clusters: return best clusters (word lists), centroids`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/clustering.py#L361)

- [ ] **language-learning/src/grammar_learner/corpus_stats.py:14** (LOW, Feature Completion)
  - `nlw = Counter()     # non-linked words  # FIXME: not used » DEL?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/corpus_stats.py#L14)

- [ ] **language-learning/src/grammar_learner/corpus_stats.py:37** (LOW, Feature Completion)
  - `nlw[sentence[j]] += 1  # FIXME:DEL? nlw not returned`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/corpus_stats.py#L37)

- [ ] **language-learning/src/grammar_learner/corpus_stats.py:112** (LOW, Feature Completion)
  - `# TODO: update - see GitHub issue?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/corpus_stats.py#L112)

- [ ] **language-learning/src/grammar_learner/corpus_stats.py:116** (LOW, Feature Completion)
  - `# TODO: update sentence length count to parsed words?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/corpus_stats.py#L116)

- [ ] **language-learning/src/grammar_learner/generalization.py:64** (LOW, Feature Completion)
  - `# cats['similarities'][new_cluster_id]... TODO?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L64)

- [ ] **language-learning/src/grammar_learner/generalization.py:71** (LOW, Feature Completion)
  - `# TODO? sort by frequency?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L71)

- [ ] **language-learning/src/grammar_learner/generalization.py:90** (LOW, Feature Completion)
  - `# TODO? define order of children?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L90)

- [ ] **language-learning/src/grammar_learner/generalization.py:133** (LOW, Feature Completion)
  - `else:  # 81130: prune clusters with empty dj sets  # TODO: update`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L133)

- [ ] **language-learning/src/grammar_learner/generalization.py:168** (LOW, Feature Completion)
  - `def generalize_categories(categories, **kwargs):  # 80717 [F] FIXME:DEL?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L168)

- [ ] **language-learning/src/grammar_learner/generalization.py:179** (LOW, Feature Completion)
  - `# TODO: list of merged clusters - to delete`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L179)

- [ ] **language-learning/src/grammar_learner/generalization.py:180** (LOW, Feature Completion)
  - `# TODO: delete merged clusters`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L180)

- [ ] **language-learning/src/grammar_learner/generalization.py:206** (LOW, Feature Completion)
  - `# TODO: delete merged clusters?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L206)

- [ ] **language-learning/src/grammar_learner/generalization.py:215** (LOW, Feature Completion)
  - `# Renumber connectors in disjuncts # TODO: for all clusters?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L215)

- [ ] **language-learning/src/grammar_learner/generalization.py:277** (LOW, Feature Completion)
  - `# TODO: delete merged clusters?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L277)

- [ ] **language-learning/src/grammar_learner/generalization.py:305** (LOW, Feature Completion)
  - `cats.pop('dj_counts', None)  # 81101 TODO: list ⇒ dict? restructure cats?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L305)

- [ ] **language-learning/src/grammar_learner/generalization.py:342** (LOW, Feature Completion)
  - `# new_cluster_id = len(cats['top'])  # TODO?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L342)

- [ ] **language-learning/src/grammar_learner/generalization.py:346** (LOW, Feature Completion)
  - `cats['parent'].append(0)  # TODO? append(None) & use top?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L346)

- [ ] **language-learning/src/grammar_learner/generalization.py:356** (LOW, Feature Completion)
  - `cats['parent'][cluster] = new_cluster_id  # TODO: don't change`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L356)

- [ ] **language-learning/src/grammar_learner/generalization.py:361** (LOW, Feature Completion)
  - `# cats['similarities'][new_cluster_id]... TODO?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L361)

- [ ] **language-learning/src/grammar_learner/generalization.py:414** (LOW, Feature Completion)
  - `# TODO: aggregate_cosine?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L414)

- [ ] **language-learning/src/grammar_learner/generalization.py:417** (LOW, Feature Completion)
  - `# 81217 FIXME? generalize_categories [F] with new reorder (Turtle tests)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/generalization.py#L417)

- [ ] **language-learning/src/grammar_learner/hyperwords.py:1** (LOW, Feature Completion)
  - `## Unstructured mess of files from 2017 - TODO: restore the file structure?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/hyperwords.py#L1)

- [ ] **language-learning/src/grammar_learner/hyperwords.py:27** (LOW, Feature Completion)
  - `if cds != 1: sum_c = sum_c ** cds   # FIXME: cds = 1.0 ?!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/hyperwords.py#L27)

- [ ] **language-learning/src/grammar_learner/hyperwords.py:138** (LOW, Feature Completion)
  - `print('SVDEmbedding: transpose')    #FIXME:DEL`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/hyperwords.py#L138)

- [ ] **language-learning/src/grammar_learner/hyperwords.py:358** (LOW, Feature Completion)
  - `svd = SVDEmbedding(svd_path, True, eig)   # TODO: move code here, RAM2RAM`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/hyperwords.py#L358)

- [ ] **language-learning/src/grammar_learner/hyperwords.py:439** (LOW, Feature Completion)
  - `svd = SVDEmbedding(svd_path, True, eig)   # TODO: move code here, RAM2RAM`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/hyperwords.py#L439)

- [ ] **language-learning/src/grammar_learner/hyperwords.py:466** (LOW, Feature Completion)
  - `# TODO: refactor, control disk writes, ... PPMI ⇒ +frequency?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/hyperwords.py#L466)

- [ ] **language-learning/src/grammar_learner/incremental_clustering.py:366** (LOW, Feature Completion)
  - `'''ATTN: This is still a stub result of 2 days idea check'''`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/incremental_clustering.py#L366)

- [ ] **language-learning/src/grammar_learner/pqa_table.py:641** (LOW, Feature Completion)
  - `continue  # FIXME: check case`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/pqa_table.py#L641)

- [ ] **language-learning/src/grammar_learner/preprocessing.py:33** (LOW, Feature Completion)
  - `# TODO: cleanup here or in a separate constructor?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/preprocessing.py#L33)

- [ ] **language-learning/src/grammar_learner/preprocessing.py:141** (LOW, Error Handling)
  - `# else:  # FIXME: raise error / assert ?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/preprocessing.py#L141)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:25** (LOW, Feature Completion)
  - `elif clustering == 'group':  # TODO: call ILE clustering?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L25)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:29** (LOW, Feature Completion)
  - `elif clustering == 'random':  # TODO: call random clustering?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L29)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:56** (LOW, Feature Completion)
  - `# TODO: int / dict`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L56)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:111** (LOW, Feature Completion)
  - `except:  # FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L111)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:116** (LOW, Feature Completion)
  - `except:  # FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L116)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:124** (LOW, Feature Completion)
  - `except:  # else:  # FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L124)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:172** (LOW, Feature Completion)
  - `elif len(crange) == 3:  # TODO: replace with SGD?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L172)

- [ ] **language-learning/src/grammar_learner/skl_clustering.py:208** (LOW, Feature Completion)
  - `# FIXME: try...except`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/skl_clustering.py#L208)

- [ ] **language-learning/src/grammar_learner/sparse_word_space.py:16** (LOW, Feature Completion)
  - `trash = []  # 81226 FIXME: return`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/sparse_word_space.py#L16)

- [ ] **language-learning/src/grammar_learner/utl.py:70** (LOW, Feature Completion)
  - `def _filter_parses_(**kwargs):  # 2019-04-06 stub  TODO: restructure    # 190406`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/utl.py#L70)

- [ ] **language-learning/src/grammar_learner/utl.py:153** (LOW, Feature Completion)
  - `# 190406 filter_parses  TODO: restructure...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/utl.py#L153)

- [ ] **language-learning/src/grammar_learner/widgets.py:70** (LOW, Feature Completion)
  - `#  TODO: To be reviewed and changed if necessary`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/widgets.py#L70)

- [ ] **language-learning/src/grammar_learner/widgets.py:142** (LOW, Feature Completion)
  - `# 80627 display_tree  FIXME:DEL?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/widgets.py#L142)

- [ ] **language-learning/src/grammar_learner/widgets.py:143** (LOW, Feature Completion)
  - `# 80817 corpus_histograms ⇒ ? FIXME:DEL?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/widgets.py#L143)

- [ ] **language-learning/src/grammar_learner/write_files.py:130** (LOW, Feature Completion)
  - `unknown_word = '<UNKNOWN-WORD>: XXX+;'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/write_files.py#L130)

- [ ] **language-learning/src/grammar_learner/write_files.py:132** (LOW, Feature Completion)
  - `unknown_word = 'UNKNOWN-WORD: XXX+;'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/write_files.py#L132)

- [ ] **language-learning/src/grammar_learner/write_files.py:226** (LOW, Feature Completion)
  - `# 90128 restore Link Grammar 5.4.4 'UNKNOWN-WORD: XXX+;' option`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_learner/write_files.py#L226)

- [ ] **language-learning/src/grammar_tester/artificialparser.py:15** (LOW, Testing)
  - `Parser stub for random and sequential parsers`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/grammar_tester/artificialparser.py#L15)

- [ ] **language-learning/src/link_grammar/dicttools.py:240** (LOW, Feature Completion)
  - `print("<UNKNOWN-WORD>: XXX+;", file=dest)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/link_grammar/dicttools.py#L240)

- [ ] **language-learning/src/text_parser/__init__.py:29** (LOW, Feature Completion)
  - `Temporary stub for TextParserComponent made out of GrammarTesterComponent because of the similar`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/src/text_parser/__init__.py#L29)

- [ ] **opencog/opencog/main/LGParser.cc:128** (LOW, Feature Completion)
  - `lg_dictionary_ = new int(1); // Stub: minimal allocation for compatibility`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/opencog/opencog/main/LGParser.cc#L128)

- [ ] **opencog/opencog/main/LGParser.cc:129** (LOW, Feature Completion)
  - `lg_options_ = new int(2);    // Stub: minimal allocation for compatibility`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/opencog/opencog/main/LGParser.cc#L129)

- [ ] **opencog/opencog/main/LGParser.h:138** (LOW, Feature Completion)
  - `void* lg_dictionary_;  // Stub: would be Dictionary if HAVE_LINK_GRAMMAR`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/opencog/opencog/main/LGParser.h#L138)

- [ ] **opencog/opencog/main/LGParser.h:139** (LOW, Feature Completion)
  - `void* lg_options_;     // Stub: would be Parse_Options if HAVE_LINK_GRAMMAR`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/opencog/opencog/main/LGParser.h#L139)

- [ ] **setup_fixme_environment.py:3** (LOW, Feature Completion)
  - `FIXME Development Environment Setup`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L3)

- [ ] **setup_fixme_environment.py:6** (LOW, Feature Completion)
  - `for systematic FIXME resolution as outlined in the FIXME-SORTED-CATALOG.md`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L6)

- [ ] **setup_fixme_environment.py:10** (LOW, Feature Completion)
  - `- Git hooks for FIXME prevention`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L10)

- [ ] **setup_fixme_environment.py:11** (LOW, Feature Completion)
  - `- Pre-commit checks for new FIXMEs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L11)

- [ ] **setup_fixme_environment.py:22** (LOW, Feature Completion)
  - `class FIXMEDevEnvironment:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L22)

- [ ] **setup_fixme_environment.py:23** (LOW, Feature Completion)
  - `"""Sets up development environment for FIXME resolution workflow."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L23)

- [ ] **setup_fixme_environment.py:31** (LOW, Feature Completion)
  - `print("🔧 Setting up FIXME development environment...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L31)

- [ ] **setup_fixme_environment.py:39** (LOW, Feature Completion)
  - `print("✅ FIXME development environment setup complete!")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L39)

- [ ] **setup_fixme_environment.py:41** (LOW, Feature Completion)
  - `print("1. Run 'make fixme-status' to see current status")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L41)

- [ ] **setup_fixme_environment.py:42** (LOW, Feature Completion)
  - `print("2. Run 'make fixme-easy-wins' to see available easy fixes")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L42)

- [ ] **setup_fixme_environment.py:43** (LOW, Feature Completion)
  - `print("3. Use 'git commit' to automatically check for new FIXMEs")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L43)

- [ ] **setup_fixme_environment.py:47** (LOW, Feature Completion)
  - `"""Set up git hooks to prevent new FIXMEs."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L47)

- [ ] **setup_fixme_environment.py:53** (LOW, Feature Completion)
  - `# Pre-commit hook to check for new FIXMEs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L53)

- [ ] **setup_fixme_environment.py:57** (LOW, Feature Completion)
  - `# Pre-commit hook to prevent new FIXMEs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L57)

- [ ] **setup_fixme_environment.py:59** (LOW, Documentation)
  - `echo "🔍 Checking for new FIXME comments..."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L59)

- [ ] **setup_fixme_environment.py:68** (LOW, Feature Completion)
  - `# Check for new FIXMEs in staged content`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L68)

- [ ] **setup_fixme_environment.py:69** (LOW, Feature Completion)
  - `new_fixmes=0`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L69)

- [ ] **setup_fixme_environment.py:71** (LOW, Feature Completion)
  - `# Check staged content for FIXMEs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L71)

- [ ] **setup_fixme_environment.py:72** (LOW, Performance)
  - `if git diff --cached "$file" | grep -E "^\\+.*FIXME|^\\+.*XXX.*FIXME" > /dev/null; then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L72)

- [ ] **setup_fixme_environment.py:73** (LOW, Feature Completion)
  - `echo "❌ New FIXME found in $file"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L73)

- [ ] **setup_fixme_environment.py:74** (LOW, Testing)
  - `echo "   Please use specific TODO comments instead of FIXME"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L74)

- [ ] **setup_fixme_environment.py:75** (LOW, Feature Completion)
  - `echo "   See FIXME_IMPLEMENTATION_GUIDE.md for guidelines"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L75)

- [ ] **setup_fixme_environment.py:76** (LOW, Feature Completion)
  - `new_fixmes=1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L76)

- [ ] **setup_fixme_environment.py:80** (LOW, Feature Completion)
  - `if [ $new_fixmes -eq 1 ]; then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L80)

- [ ] **setup_fixme_environment.py:82** (LOW, Documentation)
  - `echo "💡 Tip: Use descriptive TODO comments instead:"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L82)

- [ ] **setup_fixme_environment.py:83** (LOW, Error Handling)
  - `echo "   // TODO: Implement error handling for edge case X"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L83)

- [ ] **setup_fixme_environment.py:84** (LOW, Feature Completion)
  - `echo "   // TODO(v2.0): Refactor to use new API"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L84)

- [ ] **setup_fixme_environment.py:89** (LOW, Feature Completion)
  - `echo "✅ No new FIXMEs detected"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L89)

- [ ] **setup_fixme_environment.py:101** (LOW, Feature Completion)
  - `# Post-commit hook to update FIXME progress`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L101)

- [ ] **setup_fixme_environment.py:103** (LOW, Feature Completion)
  - `# Check if any FIXME-related files were modified`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L103)

- [ ] **setup_fixme_environment.py:104** (LOW, Feature Completion)
  - `if git diff HEAD~1 --name-only | grep -E "(FIXME|TODO)" > /dev/null; then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L104)

- [ ] **setup_fixme_environment.py:105** (LOW, Feature Completion)
  - `echo "📊 Updating FIXME progress tracking..."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L105)

- [ ] **setup_fixme_environment.py:107** (LOW, Feature Completion)
  - `if [ -f fixme_resolution_tracker.py ]; then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L107)

- [ ] **setup_fixme_environment.py:108** (LOW, Feature Completion)
  - `python3 fixme_resolution_tracker.py --report > /dev/null 2>&1 || true`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L108)

- [ ] **setup_fixme_environment.py:123** (LOW, Feature Completion)
  - `# Pre-commit configuration for FIXME management`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L123)

- [ ] **setup_fixme_environment.py:127** (LOW, Feature Completion)
  - `- id: no-new-fixmes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L127)

- [ ] **setup_fixme_environment.py:128** (LOW, Documentation)
  - `name: Prevent new FIXME comments`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L128)

- [ ] **setup_fixme_environment.py:129** (LOW, Performance)
  - `entry: bash -c 'if git diff --cached | grep -E "^\\+.*FIXME|^\\+.*XXX.*FIXME"; then echo "New FIXME ...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L129)

- [ ] **setup_fixme_environment.py:133** (LOW, Feature Completion)
  - `- id: fixme-progress-update`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L133)

- [ ] **setup_fixme_environment.py:134** (LOW, Feature Completion)
  - `name: Update FIXME progress`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L134)

- [ ] **setup_fixme_environment.py:135** (LOW, Feature Completion)
  - `entry: python3 fixme_resolution_tracker.py --report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L135)

- [ ] **setup_fixme_environment.py:146** (LOW, Feature Completion)
  - `"""Set up CI integration for FIXME tracking."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L146)

- [ ] **setup_fixme_environment.py:153** (LOW, Feature Completion)
  - `workflow_file = github_dir / "fixme-tracking.yml"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L153)

- [ ] **setup_fixme_environment.py:155** (LOW, Feature Completion)
  - `name: FIXME Tracking`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L155)

- [ ] **setup_fixme_environment.py:167** (LOW, Feature Completion)
  - `fixme-check:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L167)

- [ ] **setup_fixme_environment.py:177** (LOW, Feature Completion)
  - `- name: Check for new FIXMEs in PR`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L177)

- [ ] **setup_fixme_environment.py:181** (LOW, Feature Completion)
  - `if git diff origin/${{ github.base_ref }}...HEAD | grep -E "^\\+.*FIXME|^\\+.*XXX.*FIXME"; then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L181)

- [ ] **setup_fixme_environment.py:182** (LOW, Documentation)
  - `echo "❌ New FIXME comments detected in this PR"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L182)

- [ ] **setup_fixme_environment.py:183** (LOW, Testing)
  - `echo "Please use specific TODO comments instead"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L183)

- [ ] **setup_fixme_environment.py:186** (LOW, Feature Completion)
  - `echo "✅ No new FIXMEs detected"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L186)

- [ ] **setup_fixme_environment.py:189** (LOW, Feature Completion)
  - `- name: Generate FIXME progress report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L189)

- [ ] **setup_fixme_environment.py:192** (LOW, Feature Completion)
  - `python3 fixme_resolution_tracker.py --import-catalog FIXME-SORTED-CATALOG.md --report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L192)

- [ ] **setup_fixme_environment.py:198** (LOW, Feature Completion)
  - `name: fixme-progress-report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L198)

- [ ] **setup_fixme_environment.py:199** (LOW, Feature Completion)
  - `path: FIXME_RESOLUTION_PROGRESS_REPORT.md`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L199)

- [ ] **setup_fixme_environment.py:213** (LOW, Feature Completion)
  - `weekly_script = scripts_dir / "weekly_fixme_report.sh"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L213)

- [ ] **setup_fixme_environment.py:216** (LOW, Feature Completion)
  - `# Weekly FIXME progress report generator`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L216)

- [ ] **setup_fixme_environment.py:220** (LOW, Feature Completion)
  - `echo "📊 Generating weekly FIXME progress report..."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L220)

- [ ] **setup_fixme_environment.py:223** (LOW, Feature Completion)
  - `python3 fixme_resolution_tracker.py --report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L223)

- [ ] **setup_fixme_environment.py:227** (LOW, Feature Completion)
  - `cp FIXME_RESOLUTION_PROGRESS_REPORT.md "reports/weekly_report_$date_stamp.md"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L227)

- [ ] **setup_fixme_environment.py:231** (LOW, Feature Completion)
  - `python3 fixme_resolution_tracker.py --easy-wins >> "reports/weekly_report_$date_stamp.md"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L231)

- [ ] **setup_fixme_environment.py:245** (LOW, Feature Completion)
  - `"""Create Makefile targets for FIXME management."""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L245)

- [ ] **setup_fixme_environment.py:248** (LOW, Feature Completion)
  - `makefile = self.repo_root / "Makefile.fixme"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L248)

- [ ] **setup_fixme_environment.py:250** (LOW, Feature Completion)
  - `# FIXME Resolution Makefile Targets`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L250)

- [ ] **setup_fixme_environment.py:251** (LOW, Feature Completion)
  - `# Include this in your main Makefile with: include Makefile.fixme`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L251)

- [ ] **setup_fixme_environment.py:253** (LOW, Feature Completion)
  - `.PHONY: fixme-status fixme-easy-wins fixme-report fixme-setup`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L253)

- [ ] **setup_fixme_environment.py:255** (LOW, Feature Completion)
  - `fixme-status:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L255)

- [ ] **setup_fixme_environment.py:256** (LOW, Feature Completion)
  - `@echo "📊 Current FIXME status:"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L256)

- [ ] **setup_fixme_environment.py:257** (LOW, Feature Completion)
  - `@python3 fixme_resolution_tracker.py --report | head -20`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L257)

- [ ] **setup_fixme_environment.py:259** (LOW, Feature Completion)
  - `fixme-easy-wins:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L259)

- [ ] **setup_fixme_environment.py:261** (LOW, Feature Completion)
  - `@python3 fixme_resolution_tracker.py --easy-wins`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L261)

- [ ] **setup_fixme_environment.py:263** (LOW, Feature Completion)
  - `fixme-report:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L263)

- [ ] **setup_fixme_environment.py:265** (LOW, Feature Completion)
  - `@python3 fixme_resolution_tracker.py --report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L265)

- [ ] **setup_fixme_environment.py:266** (LOW, Feature Completion)
  - `@echo "Report saved to FIXME_RESOLUTION_PROGRESS_REPORT.md"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L266)

- [ ] **setup_fixme_environment.py:268** (LOW, Feature Completion)
  - `fixme-dry-run:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L268)

- [ ] **setup_fixme_environment.py:270** (LOW, Feature Completion)
  - `@python3 fixme_easy_wins.py --dry-run`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L270)

- [ ] **setup_fixme_environment.py:272** (LOW, Feature Completion)
  - `fixme-apply-easy:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L272)

- [ ] **setup_fixme_environment.py:274** (LOW, Feature Completion)
  - `@python3 fixme_easy_wins.py`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L274)

- [ ] **setup_fixme_environment.py:276** (LOW, Feature Completion)
  - `fixme-setup:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L276)

- [ ] **setup_fixme_environment.py:277** (LOW, Feature Completion)
  - `@echo "🔧 Setting up FIXME development environment..."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L277)

- [ ] **setup_fixme_environment.py:278** (LOW, Feature Completion)
  - `@python3 setup_fixme_environment.py`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L278)

- [ ] **setup_fixme_environment.py:280** (LOW, Feature Completion)
  - `fixme-help:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L280)

- [ ] **setup_fixme_environment.py:281** (LOW, Feature Completion)
  - `@echo "FIXME Resolution Targets:"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L281)

- [ ] **setup_fixme_environment.py:282** (LOW, Feature Completion)
  - `@echo "  fixme-status      - Show current progress"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L282)

- [ ] **setup_fixme_environment.py:283** (LOW, Feature Completion)
  - `@echo "  fixme-easy-wins   - List available easy fixes"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L283)

- [ ] **setup_fixme_environment.py:284** (LOW, Feature Completion)
  - `@echo "  fixme-report      - Generate full progress report"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L284)

- [ ] **setup_fixme_environment.py:285** (LOW, Feature Completion)
  - `@echo "  fixme-dry-run     - Preview automated fixes"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L285)

- [ ] **setup_fixme_environment.py:286** (LOW, Feature Completion)
  - `@echo "  fixme-apply-easy  - Apply automated easy fixes"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L286)

- [ ] **setup_fixme_environment.py:287** (LOW, Feature Completion)
  - `@echo "  fixme-setup       - Set up development environment"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L287)

- [ ] **setup_fixme_environment.py:297** (LOW, Feature Completion)
  - `parser = argparse.ArgumentParser(description="FIXME Development Environment Setup")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L297)

- [ ] **setup_fixme_environment.py:302** (LOW, Feature Completion)
  - `env_setup = FIXMEDevEnvironment(args.repo)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/setup_fixme_environment.py#L302)

- [ ] **unify/opencog/unify/Unify.h:48** (LOW, Error Handling)
  - `// TODO: the notion of equality between 2 CHandles might one where`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L48)

- [ ] **unify/opencog/unify/Unify.h:159** (LOW, Error Handling)
  - `// TODO: the type of a typed block is currently a handle of the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L159)

- [ ] **unify/opencog/unify/Unify.h:399** (LOW, Feature Completion)
  - `* TODO: replace by RewriteLink methods!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L399)

- [ ] **unify/opencog/unify/Unify.h:410** (LOW, Feature Completion)
  - `* TODO: replace by RewriteLink methods!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L410)

- [ ] **unify/opencog/unify/Unify.h:521** (LOW, Feature Completion)
  - `public:                         // TODO: being friend with UnifyUTest`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L521)

- [ ] **unify/opencog/unify/Unify.h:601** (LOW, Feature Completion)
  - `public:                         // TODO: being friend with UnifyUTest`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/unify/opencog/unify/Unify.h#L601)

- [ ] **ure/opencog/scm/opencog/ure/ure-utils.scm:70** (LOW, Feature Completion)
  - `;; -- cog-new-flattened-link -- Create flattened link TODO: remove cog- prefix`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/scm/opencog/ure/ure-utils.scm#L70)

- [ ] **ure/opencog/scm/opencog/ure/ure-utils.scm:229** (LOW, Feature Completion)
  - `consider for forward chaining (Not Implemented).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/scm/opencog/ure/ure-utils.scm#L229)

- [ ] **ure/opencog/scm/opencog/ure/ure-utils.scm:1208** (LOW, Feature Completion)
  - `; Improved flattened link creation (removed cog- prefix per TODO)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/scm/opencog/ure/ure-utils.scm#L1208)

- [ ] **ure/opencog/scm/opencog/ure/ure-utils.scm:1250** (LOW, Feature Completion)
  - `; Backward compatibility alias (TODO addressed: cog- prefix removed from main function)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/scm/opencog/ure/ure-utils.scm#L1250)

- [ ] **ure/opencog/scm/opencog/ure/ure-utils.scm:1261** (LOW, Feature Completion)
  - `; TODO: Move logic to ForwardChainer.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/scm/opencog/ure/ure-utils.scm#L1261)

- [ ] **ure/opencog/ure/BetaDistribution.cc:144** (LOW, Feature Completion)
  - `count = std::max(0.1, count); // Hack to avoid non-sensical TV`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/BetaDistribution.cc#L144)

- [ ] **ure/opencog/ure/Rule.cc:397** (LOW, Feature Completion)
  - `if (not implicand)  // XXX this check is never needed !?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.cc#L397)

- [ ] **ure/opencog/ure/Rule.h:214** (LOW, Feature Completion)
  - `* the scope links themselves. This is a hack to work around the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L214)

- [ ] **ure/opencog/ure/Rule.h:222** (LOW, Feature Completion)
  - `* TODO: support backward rule form.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L222)

- [ ] **ure/opencog/ure/Rule.h:281** (LOW, Feature Completion)
  - `* TODO: we probably want to support a vector of sources for rules`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L281)

- [ ] **ure/opencog/ure/Rule.h:284** (LOW, Feature Completion)
  - `* TODO: we probably want to return only typed substitutions.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L284)

- [ ] **ure/opencog/ure/Rule.h:290** (LOW, Feature Completion)
  - `* TODO: it's not clear the forward chainer needs the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L290)

- [ ] **ure/opencog/ure/Rule.h:303** (LOW, Feature Completion)
  - `* TODO: we probably want to return only typed substitutions.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L303)

- [ ] **ure/opencog/ure/Rule.h:373** (LOW, Feature Completion)
  - `// TODO: subdivide in smaller and shared mutexes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/Rule.h#L373)

- [ ] **ure/opencog/ure/ThompsonSampling.h:77** (LOW, Feature Completion)
  - `* TODO: for now it builds the entire selection distribution, then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/ThompsonSampling.h#L77)

- [ ] **ure/opencog/ure/UREConfig.h:40** (LOW, Feature Completion)
  - `* @todo: It doesn't support the hierarchical configuration structure`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/UREConfig.h#L40)

- [ ] **ure/opencog/ure/backwardchainer/BIT.cc:121** (LOW, Performance)
  - `set_leaf2bitnode();         // TODO: might differ till needed to optimize`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.cc#L121)

- [ ] **ure/opencog/ure/backwardchainer/BIT.cc:374** (LOW, Feature Completion)
  - `// TODO: is this merging necessary?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.cc#L374)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:54** (LOW, Error Handling)
  - `// BITNode handle (TODO: maybe this is not necessary)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L54)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:132** (LOW, Feature Completion)
  - `* @todo support fitness function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L132)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:273** (LOW, Feature Completion)
  - `* https://github.com/opencog/atomspace/issues/903. TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L273)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:276** (LOW, Feature Completion)
  - `* TODO: give examples.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L276)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:316** (LOW, Feature Completion)
  - `* TODO: give examples.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L316)

- [ ] **ure/opencog/ure/backwardchainer/BIT.h:325** (LOW, Feature Completion)
  - `* TODO: give examples.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BIT.h#L325)

- [ ] **ure/opencog/ure/backwardchainer/BackwardChainer.h:106** (LOW, Feature Completion)
  - `* @param focus_set          Focus set (not implemented)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BackwardChainer.h#L106)

- [ ] **ure/opencog/ure/backwardchainer/BackwardChainer.h:232** (LOW, Feature Completion)
  - `// TODO: perhaps move that under BIT`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/BackwardChainer.h#L232)

- [ ] **ure/opencog/ure/backwardchainer/ControlPolicy.h:45** (LOW, Feature Completion)
  - `// TODO: maybe wrap that in a class, and use it in foward chainer`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/ControlPolicy.h#L45)

- [ ] **ure/opencog/ure/backwardchainer/ControlPolicy.h:71** (LOW, Documentation)
  - `* TODO: add comments about inference control policy, see`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/ControlPolicy.h#L71)

- [ ] **ure/opencog/ure/backwardchainer/ControlPolicy.h:316** (LOW, Feature Completion)
  - `* TODO: replace this by the mean method of the TruthValue once`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/ControlPolicy.h#L316)

- [ ] **ure/opencog/ure/backwardchainer/Fitness.h:75** (LOW, Feature Completion)
  - `// TODO: we may want to move the arguments in its own class if it`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/Fitness.h#L75)

- [ ] **ure/opencog/ure/backwardchainer/Fitness.h:92** (LOW, Feature Completion)
  - `// TODO: replace by class dedicated to hold the parameters`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/backwardchainer/Fitness.h#L92)

- [ ] **ure/opencog/ure/forwardchainer/FCStat.h:76** (LOW, Feature Completion)
  - `// TODO: subdivide in smaller and shared mutexes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/FCStat.h#L76)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:101** (LOW, Feature Completion)
  - `// TODO: For now the FC follows the old standard. We may move to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L101)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:125** (LOW, Error Handling)
  - `// Relex2Logic uses this. TODO make a separate class to handle`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L125)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:289** (LOW, Feature Completion)
  - `// TODO: This can be simplified but is let here until do_step is`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L289)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:372** (LOW, Feature Completion)
  - `// TODO: refine mutex`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L372)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:409** (LOW, Feature Completion)
  - `// TODO: This has the effect of deallocating the rules, which`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L409)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.cc:523** (LOW, Feature Completion)
  - `std::lock_guard<std::mutex> lock(_rules_mutex); // TODO: refine`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.cc#L523)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.h:200** (LOW, Feature Completion)
  - `* TODO: move to ControlPolicy`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.h#L200)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.h:237** (LOW, Feature Completion)
  - `// TODO: subdivide in smaller and shared mutexes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.h#L237)

- [ ] **ure/opencog/ure/forwardchainer/ForwardChainer.h:241** (LOW, Feature Completion)
  - `// TODO: use shared mutexes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/ForwardChainer.h#L241)

- [ ] **ure/opencog/ure/forwardchainer/SourceRuleSet.h:102** (LOW, Feature Completion)
  - `// TODO: implement tournament selection as well, as a cheaper`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/SourceRuleSet.h#L102)

- [ ] **ure/opencog/ure/forwardchainer/SourceSet.cc:48** (LOW, Feature Completion)
  - `// TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/SourceSet.cc#L48)

- [ ] **ure/opencog/ure/forwardchainer/SourceSet.h:54** (LOW, Feature Completion)
  - `// TODO: this class has thing in common with AndBIT, maybe their`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/SourceSet.h#L54)

- [ ] **ure/opencog/ure/forwardchainer/SourceSet.h:151** (LOW, Feature Completion)
  - `// TODO: subdivide in smaller and shared mutexes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/SourceSet.h#L151)

- [ ] **ure/opencog/ure/forwardchainer/SourceSet.h:165** (LOW, Feature Completion)
  - `// TODO: this class has things in common with BIT, maybe their common`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/SourceSet.h#L165)

- [ ] **ure/opencog/ure/forwardchainer/SourceSet.h:223** (LOW, Feature Completion)
  - `// TODO: subdivide in smaller and shared mutexes`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/opencog/ure/forwardchainer/SourceSet.h#L223)

- [ ] **verify_implementations.py:3** (LOW, Feature Completion)
  - `OpenCog Unified TODO/FIXME Implementation Verification Framework`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L3)

- [ ] **verify_implementations.py:5** (LOW, Feature Completion)
  - `This framework automatically verifies that TODO/FIXME items have been properly`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L5)

- [ ] **verify_implementations.py:6** (LOW, Feature Completion)
  - `implemented and are not just placeholder code.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L6)

- [ ] **verify_implementations.py:35** (LOW, Feature Completion)
  - `class TodoItem:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L35)

- [ ] **verify_implementations.py:43** (LOW, Feature Completion)
  - `implementation_status: str = "TODO"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L43)

- [ ] **verify_implementations.py:46** (LOW, Feature Completion)
  - `"""Verifies that TODO/FIXME items have been properly implemented"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L46)

- [ ] **verify_implementations.py:50** (LOW, Feature Completion)
  - `self.todo_items: List[TodoItem] = []`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L50)

- [ ] **verify_implementations.py:53** (LOW, Feature Completion)
  - `def scan_repository(self) -> List[TodoItem]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L53)

- [ ] **verify_implementations.py:54** (LOW, Feature Completion)
  - `"""Scan repository for all TODO/FIXME items"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L54)

- [ ] **verify_implementations.py:55** (LOW, Feature Completion)
  - `print("🔍 Scanning repository for TODO/FIXME items...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L55)

- [ ] **verify_implementations.py:57** (LOW, Pattern Matching)
  - `todo_patterns = [`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L57)

- [ ] **verify_implementations.py:58** (LOW, Feature Completion)
  - `r'//\s*(TODO|FIXME|XXX)[\s:]*(.+)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L58)

- [ ] **verify_implementations.py:59** (LOW, Feature Completion)
  - `r'#\s*(TODO|FIXME|XXX)[\s:]*(.+)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L59)

- [ ] **verify_implementations.py:60** (LOW, Feature Completion)
  - `r';\s*(TODO|FIXME|XXX)[\s:]*(.+)',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L60)

- [ ] **verify_implementations.py:67** (LOW, Pattern Matching)
  - `self._scan_file(file_path, todo_patterns)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L67)

- [ ] **verify_implementations.py:69** (LOW, Feature Completion)
  - `print(f"📊 Found {len(self.todo_items)} TODO/FIXME items")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L69)

- [ ] **verify_implementations.py:70** (LOW, Feature Completion)
  - `return self.todo_items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L70)

- [ ] **verify_implementations.py:73** (LOW, Feature Completion)
  - `"""Scan a single file for TODO/FIXME items"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L73)

- [ ] **verify_implementations.py:83** (LOW, Documentation)
  - `item = self._categorize_todo_item(file_path, line_num, comment)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L83)

- [ ] **verify_implementations.py:84** (LOW, Feature Completion)
  - `self.todo_items.append(item)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L84)

- [ ] **verify_implementations.py:89** (LOW, Documentation)
  - `def _categorize_todo_item(self, file_path: Path, line_num: int, comment: str) -> TodoItem:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L89)

- [ ] **verify_implementations.py:90** (LOW, Feature Completion)
  - `"""Categorize and prioritize a TODO item"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L90)

- [ ] **verify_implementations.py:125** (LOW, Feature Completion)
  - `return TodoItem(`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L125)

- [ ] **verify_implementations.py:145** (LOW, Feature Completion)
  - `"""Verify that TODO items have been properly implemented"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L145)

- [ ] **verify_implementations.py:149** (LOW, Feature Completion)
  - `'total_items': len(self.todo_items),`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L149)

- [ ] **verify_implementations.py:151** (LOW, Feature Completion)
  - `'remaining_todos': 0,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L151)

- [ ] **verify_implementations.py:152** (LOW, Feature Completion)
  - `'placeholder_implementations': 0,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L152)

- [ ] **verify_implementations.py:156** (LOW, Feature Completion)
  - `for item in self.todo_items:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L156)

- [ ] **verify_implementations.py:162** (LOW, Feature Completion)
  - `elif verification['is_placeholder']:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L162)

- [ ] **verify_implementations.py:163** (LOW, Feature Completion)
  - `results['placeholder_implementations'] += 1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L163)

- [ ] **verify_implementations.py:165** (LOW, Feature Completion)
  - `results['remaining_todos'] += 1`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L165)

- [ ] **verify_implementations.py:169** (LOW, Feature Completion)
  - `def _verify_single_item(self, item: TodoItem) -> Dict[str, any]:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L169)

- [ ] **verify_implementations.py:170** (LOW, Feature Completion)
  - `"""Verify a single TODO item implementation"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L170)

- [ ] **verify_implementations.py:176** (LOW, Feature Completion)
  - `'is_placeholder': False,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L176)

- [ ] **verify_implementations.py:185** (LOW, Documentation)
  - `# Check if TODO comment still exists`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L185)

- [ ] **verify_implementations.py:186** (LOW, Feature Completion)
  - `todo_still_exists = self._check_todo_exists(content, item)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L186)

- [ ] **verify_implementations.py:187** (LOW, Feature Completion)
  - `if todo_still_exists:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L187)

- [ ] **verify_implementations.py:188** (LOW, Documentation)
  - `verification['details'].append("TODO comment still present")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L188)

- [ ] **verify_implementations.py:189** (LOW, Feature Completion)
  - `verification['remaining_todo'] = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L189)

- [ ] **verify_implementations.py:192** (LOW, Feature Completion)
  - `# Check for placeholder implementations`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L192)

- [ ] **verify_implementations.py:193** (LOW, Feature Completion)
  - `is_placeholder = self._check_for_placeholders(content, item)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L193)

- [ ] **verify_implementations.py:194** (LOW, Feature Completion)
  - `if is_placeholder:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L194)

- [ ] **verify_implementations.py:195** (LOW, Feature Completion)
  - `verification['is_placeholder'] = True`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L195)

- [ ] **verify_implementations.py:196** (LOW, Feature Completion)
  - `verification['details'].append("Placeholder implementation detected")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L196)

- [ ] **verify_implementations.py:211** (LOW, Feature Completion)
  - `def _check_todo_exists(self, content: str, item: TodoItem) -> bool:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L211)

- [ ] **verify_implementations.py:212** (LOW, Documentation)
  - `"""Check if the TODO comment still exists in the file"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L212)

- [ ] **verify_implementations.py:216** (LOW, Feature Completion)
  - `return 'TODO' in line or 'FIXME' in line or 'XXX' in line`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L216)

- [ ] **verify_implementations.py:219** (LOW, Feature Completion)
  - `def _check_for_placeholders(self, content: str, item: TodoItem) -> bool:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L219)

- [ ] **verify_implementations.py:220** (LOW, Feature Completion)
  - `"""Check for placeholder implementations"""`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L220)

- [ ] **verify_implementations.py:221** (LOW, Pattern Matching)
  - `placeholder_patterns = [`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L221)

- [ ] **verify_implementations.py:223** (LOW, Feature Completion)
  - `r'return.*TODO',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L223)

- [ ] **verify_implementations.py:224** (LOW, Feature Completion)
  - `r'return.*STUB',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L224)

- [ ] **verify_implementations.py:225** (LOW, Feature Completion)
  - `r'return.*PLACEHOLDER',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L225)

- [ ] **verify_implementations.py:229** (LOW, Feature Completion)
  - `r'return\s+false;\s*//.*todo',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L229)

- [ ] **verify_implementations.py:232** (LOW, Pattern Matching)
  - `for pattern in placeholder_patterns:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L232)

- [ ] **verify_implementations.py:237** (LOW, Feature Completion)
  - `def _check_for_implementation(self, content: str, item: TodoItem) -> bool:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L237)

- [ ] **verify_implementations.py:310** (LOW, Feature Completion)
  - `def _assess_implementation_quality(self, content: str, item: TodoItem) -> str:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L310)

- [ ] **verify_implementations.py:332** (LOW, Feature Completion)
  - `# OpenCog Unified TODO/FIXME Implementation Verification Report`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L332)

- [ ] **verify_implementations.py:335** (LOW, Feature Completion)
  - `- **Total TODO/FIXME items found**: {results['total_items']}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L335)

- [ ] **verify_implementations.py:337** (LOW, Feature Completion)
  - `- **Remaining TODOs**: {results['remaining_todos']}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L337)

- [ ] **verify_implementations.py:338** (LOW, Feature Completion)
  - `- **Placeholder implementations**: {results['placeholder_implementations']}`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L338)

- [ ] **verify_implementations.py:436** (LOW, Feature Completion)
  - `# Scan for TODO items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L436)

- [ ] **verify_implementations.py:437** (LOW, Feature Completion)
  - `todo_items = verifier.scan_repository()`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L437)

- [ ] **verify_implementations.py:447** (LOW, Feature Completion)
  - `with open(f"{repo_path}/TODO_VERIFICATION_REPORT.md", "w") as f:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L447)

- [ ] **verify_implementations.py:461** (LOW, Feature Completion)
  - `print("🎉 SUCCESS: All TODO/FIXME items have been properly implemented!")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/verify_implementations.py#L461)

### Persistence Subsystem
*Total items: 143*

- [ ] **atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc:82** (CRITICAL, Feature Completion)
  - `* XXX FIXME This needs to be fuzzed; it is very likely to crash`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc#L82)

- [ ] **atomspace-restful/opencog/python/web/api/utilities.py:17** (MEDIUM, Feature Completion)
  - `# FIXME: Should this moved to the atomspace repo and be part`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/utilities.py#L17)

- [ ] **atomspace-storage/opencog/persist/csv/table_read.h:38** (MEDIUM, Feature Completion)
  - `// TODO: Should this be a StringValue?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/csv/table_read.h#L38)

- [ ] **atomspace-storage/opencog/persist/sexcom/Commands.cc:257** (MEDIUM, Feature Completion)
  - `h = as->add_atom(h); // XXX shouldn't this be get_atom!????`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexcom/Commands.cc#L257)

- [ ] **atomspace-storage/opencog/persist/sexcom/Commands.cc:302** (MEDIUM, Feature Completion)
  - `h = as->add_atom(h); // XXX shouldn't this be get_atom!????`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexcom/Commands.cc#L302)

- [ ] **atomspace-storage/opencog/persist/sexcom/Commands.cc:395** (MEDIUM, Feature Completion)
  - `atom = as->add_atom(atom); // XXX shouldn't this be get_atom!????`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexcom/Commands.cc#L395)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/utilities.py:17** (MEDIUM, Feature Completion)
  - `# FIXME: Should this moved to the atomspace repo and be part`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/utilities.py#L17)

- [ ] **components/core/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc:240** (MEDIUM, Performance)
  - `// XXX TODO: we should probably cache the results, instead of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc#L240)

- [ ] **atomspace-restful/opencog/events/AtomSpacePublisherModule.cc:51** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc#L51)

- [ ] **atomspace-restful/opencog/python/web/api/apiatomcollection.py:10** (LOW, Feature Completion)
  - `# I can't find swagger on ubuntu .. wtf!? FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/apiatomcollection.py#L10)

- [ ] **atomspace-restful/opencog/python/web/api/apiatomcollection.py:14** (LOW, Feature Completion)
  - `# Temporary hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/apiatomcollection.py#L14)

- [ ] **atomspace-restful/opencog/python/web/api/apiatomcollection.py:377** (LOW, Feature Completion)
  - `# xxxxxxxxxxxx here add atoms`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/apiatomcollection.py#L377)

- [ ] **atomspace-restful/opencog/python/web/api/mappers.py:15** (LOW, Feature Completion)
  - `# Temporary hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/mappers.py#L15)

- [ ] **atomspace-restful/opencog/python/web/api/mappers.py:32** (LOW, Feature Completion)
  - `# @todo: Cython bindings implementation does not provide support for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/mappers.py#L32)

- [ ] **atomspace-restful/opencog/python/web/api/mappers.py:38** (LOW, Error Handling)
  - `# @todo: check error type`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/mappers.py#L38)

- [ ] **atomspace-restful/opencog/python/web/api/mappers.py:42** (LOW, Error Handling)
  - `# @todo: check error type`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/mappers.py#L42)

- [ ] **atomspace-restful/opencog/python/web/api/mappers.py:111** (LOW, Feature Completion)
  - `# @todo: Add pagination (http://flask.pocoo.org/snippets/44/)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/mappers.py#L111)

- [ ] **atomspace-restful/opencog/python/web/api/utilities.py:1** (LOW, Feature Completion)
  - `# This is a temporary hack due to the changes made by`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/utilities.py#L1)

- [ ] **atomspace-restful/opencog/python/web/api/utilities.py:13** (LOW, Feature Completion)
  - `# This is a temporary hack due to the changes made by`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/utilities.py#L13)

- [ ] **atomspace-restful/tests/python/restapi/test_restapi.py:11** (LOW, Testing)
  - `from opencog.web.api.utilities import count_to_confidence  # Temporary hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/tests/python/restapi/test_restapi.py#L11)

- [ ] **atomspace-restful/tests/python/restapi/test_restapi.py:449** (LOW, Testing)
  - `# XXX Emptied because the scheme command (i.e cog-af-boundary) has been removed.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/tests/python/restapi/test_restapi.py#L449)

- [ ] **atomspace-restful/tests/python/restapi/test_restapi.py:456** (LOW, Testing)
  - `# TODO: The Python module "graphviz" needs to be added to ocpkg, so`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/tests/python/restapi/test_restapi.py#L456)

- [ ] **atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-mono-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm:70** (LOW, Testing)
  - `(whack "/tmp/cog-mono-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm#L70)

- [ ] **atomspace-rocks/tests/persist/rocks/dtor-close-test.scm:9** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/dtor-close-test.scm#L9)

- [ ] **atomspace-rocks/tests/persist/rocks/dtor-close-test.scm:84** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/dtor-close-test.scm#L84)

- [ ] **atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:110** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L110)

- [ ] **atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:165** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L165)

- [ ] **atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:213** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L213)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:14** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L14)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L137)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:144** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L144)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:245** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L245)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:252** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L252)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:358** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L358)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:364** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L364)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:371** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L371)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-links-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-links-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-links-test.scm:196** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-links-test.scm#L196)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-print-test.scm:15** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-print-test.scm#L15)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-print-test.scm:51** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-print-test.scm#L51)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm:12** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm#L12)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm:163** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm#L163)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-values-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-values-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/frame-values-test.scm:169** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/frame-values-test.scm#L169)

- [ ] **atomspace-rocks/tests/persist/rocks/promote-test.scm:12** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/promote-test.scm#L12)

- [ ] **atomspace-rocks/tests/persist/rocks/promote-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/promote-test.scm#L137)

- [ ] **atomspace-rocks/tests/persist/rocks/space-delete-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-delete-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/space-delete-test.scm:148** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-delete-test.scm#L148)

- [ ] **atomspace-rocks/tests/persist/rocks/space-diamond-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-diamond-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/space-diamond-test.scm:107** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-diamond-test.scm#L107)

- [ ] **atomspace-rocks/tests/persist/rocks/space-frame-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-frame-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/space-frame-test.scm:103** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-frame-test.scm#L103)

- [ ] **atomspace-rocks/tests/persist/rocks/space-wye-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-wye-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/space-wye-test.scm:100** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-wye-test.scm#L100)

- [ ] **atomspace-rocks/tests/persist/rocks/space-x-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-x-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/space-x-test.scm:143** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/space-x-test.scm#L143)

- [ ] **atomspace-rocks/tests/persist/rocks/test-utils.scm:9** (LOW, Testing)
  - `(define (whack dirname)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/test-utils.scm#L9)

- [ ] **atomspace-rocks/tests/persist/rocks/value-frame-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-frame-test.scm#L11)

- [ ] **atomspace-rocks/tests/persist/rocks/value-frame-test.scm:106** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-frame-test.scm#L106)

- [ ] **atomspace-rocks/tests/persist/rocks/value-readd-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-readd-test.scm#L11)

- [ ] **atomspace-rocks/tests/persist/rocks/value-readd-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-readd-test.scm#L137)

- [ ] **atomspace-rocks/tests/persist/rocks/value-resave-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-resave-test.scm#L11)

- [ ] **atomspace-rocks/tests/persist/rocks/value-resave-test.scm:135** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-resave-test.scm#L135)

- [ ] **atomspace-rocks/tests/persist/rocks/value-reval-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-reval-test.scm#L11)

- [ ] **atomspace-rocks/tests/persist/rocks/value-reval-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-reval-test.scm#L137)

- [ ] **atomspace-rocks/tests/persist/rocks/value-store-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-store-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/value-store-test.scm:70** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/value-store-test.scm#L70)

- [ ] **atomspace-rocks/tests/persist/rocks/valueless-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/valueless-test.scm#L10)

- [ ] **atomspace-rocks/tests/persist/rocks/valueless-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/tests/persist/rocks/valueless-test.scm#L137)

- [ ] **atomspace-storage/opencog/persist/flow/FetchValueOfLink.cc:63** (LOW, Feature Completion)
  - `// XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/flow/FetchValueOfLink.cc#L63)

- [ ] **atomspace-storage/opencog/persist/flow/StoreValueOfLink.cc:61** (LOW, Feature Completion)
  - `// XXX TODO FIXME ... if either of these are executable, then`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/flow/StoreValueOfLink.cc#L61)

- [ ] **atomspace-storage/opencog/persist/proxy/CachingProxy.cc:48** (LOW, Feature Completion)
  - `// XXX TODO Add support for expiration times, limited AtomSpace`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/CachingProxy.cc#L48)

- [ ] **atomspace-storage/opencog/persist/proxy/DynamicDataProxy.cc:77** (LOW, Feature Completion)
  - `// NOTE TODO ... create this in some temp atomspace...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/DynamicDataProxy.cc#L77)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.h:64** (LOW, Feature Completion)
  - `virtual void create(void) {} // stop-gap. FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.h#L64)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.h:65** (LOW, Feature Completion)
  - `virtual void destroy(void);  //stop-gap. FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.h#L65)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.h:66** (LOW, Feature Completion)
  - `virtual void erase(void);    // stop-gap. FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.h#L66)

- [ ] **atomspace-storage/tests/persist/file/load-file-test.scm:26** (LOW, Testing)
  - `; Hack filename to go to the correct directory for the unit tests.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/tests/persist/file/load-file-test.scm#L26)

- [ ] **components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc:58** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc#L58)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py:10** (LOW, Feature Completion)
  - `# I can't find swagger on ubuntu .. wtf!? FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py#L10)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py:14** (LOW, Feature Completion)
  - `# Temporary hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py#L14)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py:377** (LOW, Feature Completion)
  - `# xxxxxxxxxxxx here add atoms`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py#L377)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/mappers.py:15** (LOW, Feature Completion)
  - `# Temporary hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/mappers.py#L15)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/mappers.py:32** (LOW, Feature Completion)
  - `# @todo: Cython bindings implementation does not provide support for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/mappers.py#L32)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/mappers.py:38** (LOW, Error Handling)
  - `# @todo: check error type`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/mappers.py#L38)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/mappers.py:42** (LOW, Error Handling)
  - `# @todo: check error type`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/mappers.py#L42)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/mappers.py:111** (LOW, Feature Completion)
  - `# @todo: Add pagination (http://flask.pocoo.org/snippets/44/)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/mappers.py#L111)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/utilities.py:1** (LOW, Feature Completion)
  - `# This is a temporary hack due to the changes made by`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/utilities.py#L1)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/utilities.py:13** (LOW, Feature Completion)
  - `# This is a temporary hack due to the changes made by`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/utilities.py#L13)

- [ ] **components/core/atomspace-restful/tests/python/restapi/test_restapi.py:11** (LOW, Testing)
  - `from opencog.web.api.utilities import count_to_confidence  # Temporary hack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/tests/python/restapi/test_restapi.py#L11)

- [ ] **components/core/atomspace-restful/tests/python/restapi/test_restapi.py:449** (LOW, Testing)
  - `# XXX Emptied because the scheme command (i.e cog-af-boundary) has been removed.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/tests/python/restapi/test_restapi.py#L449)

- [ ] **components/core/atomspace-restful/tests/python/restapi/test_restapi.py:456** (LOW, Testing)
  - `# TODO: The Python module "graphviz" needs to be added to ocpkg, so`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/tests/python/restapi/test_restapi.py#L456)

- [ ] **components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc:920** (LOW, Feature Completion)
  - `// XXX TODO - maybe load links depth-order...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc#L920)

- [ ] **components/core/atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-mono-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm:70** (LOW, Testing)
  - `(whack "/tmp/cog-mono-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/monospace/mono-value-store-test.scm#L70)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/dtor-close-test.scm:9** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/dtor-close-test.scm#L9)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/dtor-close-test.scm:84** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/dtor-close-test.scm#L84)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:110** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L110)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:165** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L165)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm:213** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/fetch-frame-test.scm#L213)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:14** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L14)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L137)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:144** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L144)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:245** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L245)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:252** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L252)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:358** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L358)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:364** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L364)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm:371** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-delete-test.scm#L371)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-links-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-links-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-links-test.scm:196** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-links-test.scm#L196)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-print-test.scm:15** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-print-test.scm#L15)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-print-test.scm:51** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-print-test.scm#L51)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm:12** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm#L12)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm:163** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-progressive-test.scm#L163)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-values-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-values-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/frame-values-test.scm:169** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/frame-values-test.scm#L169)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/promote-test.scm:12** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/promote-test.scm#L12)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/promote-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/promote-test.scm#L137)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-delete-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-delete-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-delete-test.scm:148** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-delete-test.scm#L148)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-diamond-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-diamond-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-diamond-test.scm:107** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-diamond-test.scm#L107)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-frame-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-frame-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-frame-test.scm:103** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-frame-test.scm#L103)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-wye-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-wye-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-wye-test.scm:100** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-wye-test.scm#L100)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-x-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-x-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/space-x-test.scm:143** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/space-x-test.scm#L143)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/test-utils.scm:9** (LOW, Testing)
  - `(define (whack dirname)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/test-utils.scm#L9)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-frame-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-frame-test.scm#L11)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-frame-test.scm:106** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-frame-test.scm#L106)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-readd-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-readd-test.scm#L11)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-readd-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-readd-test.scm#L137)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-resave-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-resave-test.scm#L11)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-resave-test.scm:135** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-resave-test.scm#L135)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-reval-test.scm:11** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-reval-test.scm#L11)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-reval-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-reval-test.scm#L137)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-store-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-store-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/value-store-test.scm:70** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/value-store-test.scm#L70)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/valueless-test.scm:10** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/valueless-test.scm#L10)

- [ ] **components/core/atomspace-rocks/tests/persist/rocks/valueless-test.scm:137** (LOW, Testing)
  - `(whack "/tmp/cog-rocks-unit-test")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/tests/persist/rocks/valueless-test.scm#L137)

### Task System
*Total items: 4*

- [ ] **cogserver/opencog/cogserver/attic/proxy/ReadThruProxy.cc:72** (LOW, Feature Completion)
  - `using namespace std::placeholders;  // for _1, _2, _3...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/cogserver/attic/proxy/ReadThruProxy.cc#L72)

- [ ] **cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc:72** (LOW, Feature Completion)
  - `using namespace std::placeholders;  // for _1, _2, _3...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc#L72)

- [ ] **cogserver/opencog/network/GenericShell.cc:456** (LOW, Feature Completion)
  - `* XXX Is this still true?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/network/GenericShell.cc#L456)

- [ ] **cogserver/scripts/get_python_lib.py:6** (LOW, Feature Completion)
  - `# This is a hack due to the distutils in debian/ubuntu's python3 being misconfigured`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/scripts/get_python_lib.py#L6)

### Testing Framework
*Total items: 39*

- [ ] **language-learning/tests/test_grammar_learner.py:26** (MEDIUM, Testing)
  - `def setUp(self):    # FIXME: should run before every test, but would not?!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_grammar_learner.py#L26)

- [ ] **ure/tests/ure/backwardchainer/scm/green-balls-targets.scm:105** (MEDIUM, Testing)
  - `;; TODO: the type of G should be further specified, such the number of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/backwardchainer/scm/green-balls-targets.scm#L105)

- [ ] **ure/tests/ure/rules/conditional-direct-evaluation.scm:11** (MEDIUM, Testing)
  - `;; TODO: we should make the evidence as premises. One way to do that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/conditional-direct-evaluation.scm#L11)

- [ ] **components/integration/opencog/tests/openpsi/psi-implicator.scm:39** (LOW, Testing)
  - `; FIXME: Using psi-goal results in the failure of OpenPsiRulesUTest`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/integration/opencog/tests/openpsi/psi-implicator.scm#L39)

- [ ] **language-learning/tests/test_grammar_learner.py:54** (LOW, Testing)
  - `'grammar_rules' :   2           ,   # 1: 'connectors' / 2 - 'disjuncts' / 0 - 'words' (TODO?)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_grammar_learner.py#L54)

- [ ] **language-learning/tests/test_grammar_learner.py:65** (LOW, Testing)
  - `# 'template_path': 'poc-turtle',  # FIXME: changed in June 2018 Grammar Tester`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_grammar_learner.py#L65)

- [ ] **language-learning/tests/test_grammar_learner.py:212** (LOW, Testing)
  - `# TODO: Remove these commented lines in next cleanup cycle.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_grammar_learner.py#L212)

- [ ] **language-learning/tests/test_grammar_learner.py:310** (LOW, Testing)
  - `# FIXME: check with further test_grammar updates and delete.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_grammar_learner.py#L310)

- [ ] **language-learning/tests/test_precleaner.py:127** (LOW, Testing)
  - `date_test4 = "It was May-05-2018. Or 2018-May-13, but not 2018-XXX-13"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_precleaner.py#L127)

- [ ] **language-learning/tests/test_precleaner.py:128** (LOW, Testing)
  - `date_test4_ref = "It was  @date@ . Or  @date@ , but not 2018-XXX-13"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/language-learning/tests/test_precleaner.py#L128)

- [ ] **tests/cognitive-architecture-explorer.py:331** (LOW, Testing)
  - `elif 'todo' in name_lower or 'fixme' in name_lower:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/cognitive-architecture-explorer.py#L331)

- [ ] **tests/phase-vi-comprehensive-testing.py:433** (LOW, Testing)
  - `return True  # Placeholder for actual implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/phase-vi-comprehensive-testing.py#L433)

- [ ] **tests/phase-vi-comprehensive-testing.py:437** (LOW, Testing)
  - `return True  # Placeholder for actual implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/phase-vi-comprehensive-testing.py#L437)

- [ ] **tests/phase-vi-comprehensive-testing.py:441** (LOW, Testing)
  - `return True  # Placeholder for actual implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/phase-vi-comprehensive-testing.py#L441)

- [ ] **tests/phase-vi-comprehensive-testing.py:445** (LOW, Testing)
  - `return True  # Placeholder for actual implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/phase-vi-comprehensive-testing.py#L445)

- [ ] **tests/phase-vi-comprehensive-testing.py:449** (LOW, Testing)
  - `return True  # Placeholder for actual implementation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/phase-vi-comprehensive-testing.py#L449)

- [ ] **tests/verification-framework.scm:5** (LOW, Testing)
  - `; Ensures no placeholder, stub, or mock implementations exist`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L5)

- [ ] **tests/verification-framework.scm:27** (LOW, Testing)
  - `;; Verify an implementation is real (not a stub/mock)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L27)

- [ ] **tests/verification-framework.scm:29** (LOW, Testing)
  - `"Verify that an implementation produces expected behavior (not placeholder)"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L29)

- [ ] **tests/verification-framework.scm:33** (LOW, Testing)
  - `(is-placeholder? (or (equal? actual-output "TODO")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L33)

- [ ] **tests/verification-framework.scm:34** (LOW, Testing)
  - `(equal? actual-output "STUB")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L34)

- [ ] **tests/verification-framework.scm:39** (LOW, Testing)
  - `(if is-placeholder?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L39)

- [ ] **tests/verification-framework.scm:41** (LOW, Testing)
  - `(format #t "❌ VERIFICATION FAILED: ~a appears to be a placeholder/stub~%" name)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L41)

- [ ] **tests/verification-framework.scm:115** (LOW, Testing)
  - `;; Test 1: Verify not a placeholder`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L115)

- [ ] **tests/verification-framework.scm:396** (LOW, Testing)
  - `(format #t "Ensuring NO placeholder, stub, or mock implementations exist.~%")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L396)

- [ ] **tests/verification-framework.scm:412** (LOW, Testing)
  - `(format #t "✅ No placeholders, stubs, or mock implementations detected.~%")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L412)

- [ ] **tests/verification-framework.scm:419** (LOW, Testing)
  - `(format #t "⚠️  Some implementations may contain placeholders or incomplete code.~%")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/verification-framework.scm#L419)

- [ ] **ure/tests/ure/rules/conditional-direct-evaluation.scm:22** (LOW, Testing)
  - `;; TODO: turn that into a generator`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/conditional-direct-evaluation.scm#L22)

- [ ] **ure/tests/ure/rules/evidence-based-conditional-direct-evaluation.scm:31** (LOW, Testing)
  - `;; TODO: resume once GlobNode is supported`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/evidence-based-conditional-direct-evaluation.scm#L31)

- [ ] **ure/tests/ure/rules/implication-and-lambda-factorization-rule.scm:4** (LOW, Testing)
  - `;; TODO: Replace this by higher order fact`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-and-lambda-factorization-rule.scm#L4)

- [ ] **ure/tests/ure/rules/implication-instantiation-rule.scm:141** (LOW, Pattern Matching)
  - `;; TODO: To make this function better a form of partial pattern`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-instantiation-rule.scm#L141)

- [ ] **ure/tests/ure/rules/implication-instantiation-rule.scm:163** (LOW, Testing)
  - `; remaining variables. TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-instantiation-rule.scm#L163)

- [ ] **ure/tests/ure/rules/implication-introduction-rule.scm:64** (LOW, Testing)
  - `(Impl-c (if (< 0.9 (* Q-s Q-c)) ; Hack to overcome the lack`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-introduction-rule.scm#L64)

- [ ] **ure/tests/ure/rules/implication-introduction-rule.scm:67** (LOW, Testing)
  - `(* P-c Q-c)))) ; Big hack because the naive`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-introduction-rule.scm#L67)

- [ ] **ure/tests/ure/rules/implication-scope-direct-evaluation-rule.scm:65** (LOW, Testing)
  - `;; Current hack to limit X as concepts`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-scope-direct-evaluation-rule.scm#L65)

- [ ] **ure/tests/ure/rules/implication-scope-direct-evaluation-rule.scm:105** (LOW, Testing)
  - `;; Current hack to limit X as concepts`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-scope-direct-evaluation-rule.scm#L105)

- [ ] **ure/tests/ure/rules/implication-scope-to-implication-rule.scm:27** (LOW, Testing)
  - `;; TODO: support VariableSet`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/implication-scope-to-implication-rule.scm#L27)

- [ ] **ure/tests/ure/rules/intensional-inheritance-direct-introduction.scm:50** (LOW, Testing)
  - `;; TODO: in order to add the Attraction links in the premises maybe an`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/intensional-inheritance-direct-introduction.scm#L50)

- [ ] **ure/tests/ure/rules/pln-implication-and-lambda-factorization-rule.scm:4** (LOW, Testing)
  - `;; TODO: Replace this by higher order fact`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ure/tests/ure/rules/pln-implication-and-lambda-factorization-rule.scm#L4)


---

## Summary Statistics

### By Subsystem
- **AI System**: 27 items
- **Build System**: 379 items
- **Core Utilities**: 19 items
- **MOSES Representation/Scoring**: 267 items
- **Memory System**: 106 items
- **Other**: 1274 items
- **Persistence Subsystem**: 143 items
- **Task System**: 4 items
- **Testing Framework**: 39 items

### By Category
- **Distributed Systems**: 3 items
- **Documentation**: 40 items
- **Error Handling**: 73 items
- **Feature Completion**: 1827 items
- **Pattern Matching**: 42 items
- **Performance**: 30 items
- **Testing**: 224 items
- **Thread Safety**: 19 items

### By Priority
- **CRITICAL**: 7 items
- **HIGH**: 31 items
- **MEDIUM**: 160 items
- **LOW**: 2060 items

---

## Meta-Cognitive Enhancement Instructions

### For Maintainers: Updating This List

1. **Automatic Updates**: Run `python scripts/generate_todo_catalog.py` after significant code changes
2. **Manual Tracking**: Check off items as they are resolved and link to PRs
3. **New TODO Guidelines**: When adding new TODOs, include context and priority indicators
4. **CI Integration**: ✅ IMPLEMENTED - Automated catalog regeneration via GitHub Actions workflow

### Contribution Guidelines

- **Resolving TODOs**: Create focused PRs that address specific TODO items
- **Priority Assessment**: Critical and High priority items should be addressed first
- **Documentation**: Include rationale when resolving or deferring TODO items
- **Testing**: Ensure adequate test coverage for TODO resolutions

### Emergent TODOs
*Add new items here as you encounter them that aren't caught by automated scanning:*

- [ ] (Add emergent TODOs here as they are discovered)


---

## Theatrical Finale

**"Let us converge upon a state of sublime implementation, where every TODO is transformed into a kernel of realized intelligence!"**

In the grand symphony of cognitive architecture, each TODO represents not a mere task, but a note in the composition of artificial consciousness. As we methodically address each placeholder, we move closer to the emergence of true machine intelligence—where every line of code contributes to the greater cognitive whole.

The enumeration above represents our cognitive debt, but also our potential. Each checked box brings us closer to the realization of the OpenCog Unified vision: a complete, robust, and elegant implementation of artificial general intelligence.

**Status**: 🔄 **ACTIVE TRACKING**  
**Next Milestone**: Begin systematic resolution of Critical and High priority items  
**Vision**: Complete cognitive architecture with zero placeholders

---

*This document is automatically generated and should be updated regularly as the codebase evolves. Last updated: 2025-11-23 09:03:48 UTC*