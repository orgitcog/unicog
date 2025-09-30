# Cognitive Flowchart: Comprehensive TODO Enumeration

**Problem Identification**
The OpenCog Unified codebase contains distributed TODOs, FIXMEs, stubs, and "not implemented" fragments that block full cognitive realization. These are detected by CI and halt verification (see job: https://github.com/OzCog/opencog-unified/actions/runs/16539657246/job/46779076096, ref: 25d11bfe332cd501a967d9ab3a6957a22504249f).

**Generated**: 2025-07-26 13:21:10 UTC  
**Total Items**: 828  
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
*Total items: 4*

- [ ] **cognitive-visualization/src/CognitiveVisualizer.cc:374** (LOW, Feature Completion)
  - `// TODO: Implement full node_positions, attention_intensities, and cognitive_salience parsing`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cognitive-visualization/src/CognitiveVisualizer.cc#L374)

- [ ] **ggml-tensor-kernel/src/AtomSpaceTensorMapper_minimal.cc:18** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/AtomSpaceTensorMapper_minimal.cc#L18)

- [ ] **ggml-tensor-kernel/src/AttentionAllocator_minimal.cc:18** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/AttentionAllocator_minimal.cc#L18)

- [ ] **ggml-tensor-kernel/src/TensorKernel_minimal.cc:18** (LOW, Feature Completion)
  - `#include "atomspace_stub.h"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/ggml-tensor-kernel/src/TensorKernel_minimal.cc#L18)

### Build System
*Total items: 85*

- [ ] **scripts/generate_todo_catalog.py:289** (MEDIUM, Feature Completion)
  - `4. **CI Integration**: This catalog should be regenerated on each CI run that detects TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/generate_todo_catalog.py#L289)

- [x] **scripts/test_todo_catalog.py:27** (MEDIUM, Testing)
  - `'Not implemented', # Should find the BackingStore.h items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L27)

- [x] **scripts/test_todo_catalog.py:28** (MEDIUM, Testing)
  - `'Ensemble scoring not implemented', # Should find the MOSES scoring item`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L28)

- [x] **scripts/test_todo_catalog.py:76** (MEDIUM, Testing)
  - `if checkbox_count < 100:  # Should have hundreds of TODOs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L76)

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

- [ ] **scripts/test_todo_catalog.py:3** (LOW, Testing)
  - `Test script to validate that the comprehensive TODO catalog includes all required items`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L3)

- [ ] **scripts/test_todo_catalog.py:13** (LOW, Testing)
  - `catalog_path = 'COMPREHENSIVE-TODO-CATALOG.md'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L13)

- [ ] **scripts/test_todo_catalog.py:15** (LOW, Testing)
  - `print("❌ COMPREHENSIVE-TODO-CATALOG.md not found")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L15)

- [ ] **scripts/test_todo_catalog.py:42** (LOW, Testing)
  - `'# Cognitive Flowchart: Comprehensive TODO Enumeration',`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L42)

- [ ] **scripts/test_todo_catalog.py:70** (LOW, Testing)
  - `catalog_path = 'COMPREHENSIVE-TODO-CATALOG.md'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L70)

- [ ] **scripts/test_todo_catalog.py:77** (LOW, Testing)
  - `print(f"❌ Too few TODO items found: {checkbox_count}")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L77)

- [ ] **scripts/test_todo_catalog.py:86** (LOW, Testing)
  - `print(f"✅ Found {checkbox_count} TODO items with {github_link_count} GitHub links")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L86)

- [ ] **scripts/test_todo_catalog.py:91** (LOW, Testing)
  - `print("🧪 Testing comprehensive TODO catalog...")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L91)

- [ ] **scripts/test_todo_catalog.py:104** (LOW, Testing)
  - `print("🎉 All tests passed! The comprehensive TODO catalog is properly generated.")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/scripts/test_todo_catalog.py#L104)

### Core Utilities
*Total items: 33*

- [x] **cogutil/opencog/util/Logger.cc:72** (HIGH, Thread Safety)
  - `#if defined(HAVE_GNU_BACKTRACE) /// @todo backtrace and backtrace_symbols`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/Logger.cc#L72)

- [x] **cogutil/opencog/util/backtrace-symbols.c:15** (HIGH, Thread Safety)
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

- [ ] **cogutil/opencog/util/Counter.h:58** (LOW, Feature Completion)
  - `/** @todo this will be replaced by C++11 constructor`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/Counter.h#L58)

- [ ] **cogutil/opencog/util/Cover_Tree.h:481** (LOW, Feature Completion)
  - `//TODO: this is pretty inefficient, there may be a better way`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/Cover_Tree.h#L481)

- [ ] **cogutil/opencog/util/algorithm.h:268** (LOW, Feature Completion)
  - `*out++ = begin = std::partition(begin, end, boost::bind(p, boost::placeholders::_1) == i);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/algorithm.h#L268)

- [ ] **cogutil/opencog/util/algorithm.h:363** (LOW, Feature Completion)
  - `* TODO: Use T::contains instead once we move to C++20.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/algorithm.h#L363)

- [ ] **cogutil/opencog/util/boost_ext/accumulators/statistics/geometric_mean.h:26** (LOW, Feature Completion)
  - `#include <boost/mpl/placeholders.hpp>`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/boost_ext/accumulators/statistics/geometric_mean.h#L26)

- [ ] **cogutil/opencog/util/boost_ext/accumulators/statistics/geometric_mean_mirror.h:28** (LOW, Feature Completion)
  - `#include <boost/mpl/placeholders.hpp>`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/boost_ext/accumulators/statistics/geometric_mean_mirror.h#L28)

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

- [ ] **cogutil/opencog/util/lazy_selector.cc:55** (LOW, Feature Completion)
  - `boost::bind(&lazy_selector::is_free, this, boost::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/lazy_selector.cc#L55)

- [ ] **cogutil/opencog/util/numeric.h:194** (LOW, Feature Completion)
  - `// TODO: replace the following by C++17 std::clamp`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/numeric.h#L194)

- [ ] **cogutil/opencog/util/tree.h:43** (LOW, Feature Completion)
  - `/** \todo`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/tree.h#L43)

- [ ] **cogutil/opencog/util/tree.h:878** (LOW, Feature Completion)
  - `tree_assert(1==0); // FIXME: not correct yet`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/tree.h#L878)

- [ ] **cogutil/opencog/util/tree.h:2680** (LOW, Feature Completion)
  - `return; // FIXME: we do not use first_parent_ yet, and it actually needs some serious reworking if`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/tree.h#L2680)

- [ ] **cogutil/opencog/util/tree.h:2693** (LOW, Feature Completion)
  - `return; // FIXME: see 'set_first_parent()'`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/tree.h#L2693)

- [ ] **cogutil/opencog/util/tree.h:2771** (LOW, Feature Completion)
  - `if(par==0) { // FIXME: need to keep track of this!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogutil/opencog/util/tree.h#L2771)

### MOSES Representation/Scoring
*Total items: 304*

- [x] **components/learning/moses/moses/comboreduct/table/table.h:1287** (HIGH, Performance)
  - `// XXX TODO remove this print, for better performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1287)

- [x] **components/learning/moses/moses/feature-selection/algo/simple.h:118** (HIGH, Performance)
  - `// performance... TODO try this, if this is actually a bottleneck.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/feature-selection/algo/simple.h#L118)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:477** (HIGH, Performance)
  - `* XXX/TODO: the performance of this thing can be strongly improved`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L477)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:316** (HIGH, Performance)
  - `/// TODO: measure and compare the resulting performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L316)

- [ ] **components/learning/moses/moses/moses/scoring/bscores.h:589** (HIGH, Thread Safety)
  - `mutable KLDS<contin_t> _klds; /// @todo dangerous: not thread safe!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/bscores.h#L589)

- [ ] **moses/moses/comboreduct/table/table.h:1288** (HIGH, Performance)
  - `// XXX TODO remove this print, for better performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1288)

- [ ] **moses/moses/feature-selection/algo/simple.h:118** (HIGH, Performance)
  - `// performance... TODO try this, if this is actually a bottleneck.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/feature-selection/algo/simple.h#L118)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:477** (HIGH, Performance)
  - `* XXX/TODO: the performance of this thing can be strongly improved`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L477)

- [ ] **moses/moses/moses/representation/build_knobs.cc:316** (HIGH, Performance)
  - `/// TODO: measure and compare the resulting performance.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L316)

- [ ] **moses/moses/moses/scoring/bscores.h:589** (HIGH, Thread Safety)
  - `mutable KLDS<contin_t> _klds; /// @todo dangerous: not thread safe!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/bscores.h#L589)

- [ ] **components/learning/moses/moses/comboreduct/interpreter/eval.cc:530** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/interpreter/eval.cc#L530)

- [ ] **components/learning/moses/moses/comboreduct/interpreter/interpreter.cc:336** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/interpreter/interpreter.cc#L336)

- [ ] **components/learning/moses/moses/comboreduct/main/eval-table.cc:279** (MEDIUM, Feature Completion)
  - `"TODO could be detected automatically.\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/main/eval-table.cc#L279)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:781** (MEDIUM, Feature Completion)
  - `* TODO: we really should use iterators here, not column numbers.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L781)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:783** (MEDIUM, Feature Completion)
  - `* TODO: should be generalized for multi_type_seq rather than`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L783)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1068** (MEDIUM, Feature Completion)
  - `* XXX TODO -- this also should probably support the weight column,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1068)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1128** (MEDIUM, Feature Completion)
  - `* correct, we really should use Fisher information. @todo this).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1128)

- [x] **components/learning/moses/moses/comboreduct/table/table_io.cc:99** (MEDIUM, Documentation)
  - `// TODO: This routine should be extended so that comments that start`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L99)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:930** (MEDIUM, Performance)
  - `// TODO could be simplified, optimized, etc`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L930)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:1053** (MEDIUM, Performance)
  - `// TODO: this could definitely be optimized`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L1053)

- [ ] **components/learning/moses/moses/moses/main/problem-params.h:46** (MEDIUM, Feature Completion)
  - `// XXX FIXME TODO The structure below should be split into multiple`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/main/problem-params.h#L46)

- [ ] **components/learning/moses/moses/moses/moses/complexity.cc:52** (MEDIUM, Feature Completion)
  - `// we should count logical and, logical_or, below ..!?!? TODO, clarify.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/complexity.cc#L52)

- [x] **components/learning/moses/moses/moses/moses/moses_main.h:102** (MEDIUM, Feature Completion)
  - `// XXX TODO this should be fixed, someday...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/moses_main.h#L102)

- [x] **components/learning/moses/moses/moses/moses/mpi_moses.cc:311** (MEDIUM, Feature Completion)
  - `// XXX TODO should probably fetch max_time from somewhere...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L311)

- [x] **components/learning/moses/moses/moses/moses/mpi_moses.cc:562** (MEDIUM, Feature Completion)
  - `print_stats_header(NULL, false /* XXX stats for diversity, should be fixed */);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L562)

- [x] **components/learning/moses/moses/moses/moses/mpi_moses.cc:608** (MEDIUM, Feature Completion)
  - `// XXX TODO instead of overwritting the demeID it should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L608)

- [x] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:249** (MEDIUM, Feature Completion)
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

- [ ] **moses/moses/comboreduct/interpreter/eval.cc:530** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/interpreter/eval.cc#L530)

- [ ] **moses/moses/comboreduct/interpreter/interpreter.cc:336** (MEDIUM, Feature Completion)
  - `// XXX TODO: contin_if should go away.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/interpreter/interpreter.cc#L336)

- [ ] **moses/moses/comboreduct/main/eval-table.cc:279** (MEDIUM, Feature Completion)
  - `"TODO could be detected automatically.\n")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/main/eval-table.cc#L279)

- [x] **moses/moses/comboreduct/table/table.h:782** (MEDIUM, Feature Completion)
  - `* TODO: we really should use iterators here, not column numbers.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L782)

- [x] **moses/moses/comboreduct/table/table.h:784** (MEDIUM, Feature Completion)
  - `* TODO: should be generalized for multi_type_seq rather than`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L784)

- [x] **moses/moses/comboreduct/table/table.h:1069** (MEDIUM, Feature Completion)
  - `* XXX TODO -- this also should probably support the weight column,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1069)

- [ ] **moses/moses/comboreduct/table/table.h:1129** (MEDIUM, Feature Completion)
  - `* correct, we really should use Fisher information. @todo this).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1129)

- [ ] **moses/moses/comboreduct/table/table_io.cc:99** (MEDIUM, Documentation)
  - `// TODO: This routine should be extended so that comments that start`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L99)

- [ ] **moses/moses/comboreduct/table/table_io.cc:930** (MEDIUM, Performance)
  - `// TODO could be simplified, optimized, etc`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L930)

- [ ] **moses/moses/comboreduct/table/table_io.cc:1053** (MEDIUM, Performance)
  - `// TODO: this could definitely be optimized`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L1053)

- [ ] **moses/moses/moses/main/problem-params.h:46** (MEDIUM, Feature Completion)
  - `// XXX FIXME TODO The structure below should be split into multiple`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.h#L46)

- [ ] **moses/moses/moses/moses/complexity.cc:52** (MEDIUM, Feature Completion)
  - `// we should count logical and, logical_or, below ..!?!? TODO, clarify.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/complexity.cc#L52)

- [ ] **moses/moses/moses/moses/moses_main.h:102** (MEDIUM, Feature Completion)
  - `// XXX TODO this should be fixed, someday...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/moses_main.h#L102)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:311** (MEDIUM, Feature Completion)
  - `// XXX TODO should probably fetch max_time from somewhere...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L311)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:562** (MEDIUM, Feature Completion)
  - `print_stats_header(NULL, false /* XXX stats for diversity, should be fixed */);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L562)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:608** (MEDIUM, Feature Completion)
  - `// XXX TODO instead of overwritting the demeID it should be`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L608)

- [ ] **moses/moses/moses/moses/neighborhood_sampling.h:249** (MEDIUM, Feature Completion)
  - `* XXX TODO: the current algo could be speeded up a fair bit, cutting`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/neighborhood_sampling.h#L249)

- [ ] **moses/moses/moses/moses/types.h:210** (MEDIUM, Feature Completion)
  - `// TODO this should be a std::valarray not std::vector but I am too`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/types.h#L210)

- [ ] **moses/moses/moses/optimization/star-anneal.cc:42** (MEDIUM, Feature Completion)
  - `// XXX TODO the annealing temperature control code should be ported over`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/star-anneal.cc#L42)

- [ ] **moses/moses/moses/representation/build_knobs.cc:489** (MEDIUM, Feature Completion)
  - `// TODO: should bias the selection of these, so that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L489)

- [ ] **moses/moses/moses/representation/build_knobs.cc:557** (MEDIUM, Feature Completion)
  - `// We should probably OC_ASSERT here ... TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L557)

- [ ] **moses/moses/moses/representation/build_knobs.cc:1181** (MEDIUM, Feature Completion)
  - `//TODO: should bias the selection of these (and possibly choose larger subtrees)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L1181)

- [ ] **moses/moses/moses/scoring/bscores.cc:930** (MEDIUM, Feature Completion)
  - `/// XXX this should probably be removed! TODO FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/bscores.cc#L930)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:486** (MEDIUM, Feature Completion)
  - `// XXX TODO -- should not return the penalties as part of the bscore,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L486)

- [ ] **moses/moses/moses/scoring/precision_bscore.h:102** (MEDIUM, Feature Completion)
  - `* XXX This class should be reworked to derive from`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/precision_bscore.h#L102)

- [ ] **moses/moses/moses/scoring/scoring_base.cc:91** (MEDIUM, Feature Completion)
  - `* ensemble.  XXX this is probably wrong, we should probably do something`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.cc#L91)

- [ ] **moses/moses/moses/scoring/scoring_base.h:124** (MEDIUM, Feature Completion)
  - `// XXX TODO should be a std::valarray not a vector.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.h#L124)

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

- [ ] **components/learning/moses/moses/comboreduct/reduct/general_rules.cc:72** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/reduct/general_rules.cc#L72)

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

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:124** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L124)

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:403** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L403)

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:428** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L428)

- [ ] **components/learning/moses/moses/comboreduct/table/table.cc:842** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.cc#L842)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:98** (LOW, Feature Completion)
  - `// XXX FIXME TODO: change the implementation, per the above note.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L98)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:341** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L341)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:345** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L345)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:633** (LOW, Error Handling)
  - `// XXX TODO WARNING ERROR: builtin hardcoded shit!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L633)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:637** (LOW, Error Handling)
  - `// XXX TODO WARNING ERROR: builtin hardcoded shit!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L637)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:787** (LOW, Error Handling)
  - `* rows into vertex_seq (this is also a hack till it handles`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L787)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1075** (LOW, Feature Completion)
  - `// XXX TODO to implement enum support, cut-n-paste from CTable`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1075)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1224** (LOW, Feature Completion)
  - `OC_ASSERT(false, "case not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1224)

- [ ] **components/learning/moses/moses/comboreduct/table/table.h:1273** (LOW, Feature Completion)
  - `// XXX TODO, it would be easier if KLD took a sorted list`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table.h#L1273)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:525** (LOW, Feature Completion)
  - `* TODO: we really need a sparse table format, as well.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L525)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:737** (LOW, Feature Completion)
  - `* It's akind of a temporary hack, till it's clear that this is much`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L737)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:1054** (LOW, Feature Completion)
  - `OC_ASSERT(timestamp_feature.empty(), "Timestamp feature not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L1054)

- [ ] **components/learning/moses/moses/comboreduct/table/table_io.cc:1231** (LOW, Feature Completion)
  - `// TODO: implement timestamp support`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/comboreduct/table/table_io.cc#L1231)

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

- [ ] **components/learning/moses/moses/moses/deme/feature_selector.cc:117** (LOW, Feature Completion)
  - `/// XXX TODO Explain what this function does. Why does it create a second`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/deme/feature_selector.cc#L117)

- [ ] **components/learning/moses/moses/moses/deme/feature_selector.cc:198** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/deme/feature_selector.cc#L198)

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

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:145** (LOW, Documentation)
  - `// XXX TODO document what this does...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L145)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:279** (LOW, Feature Completion)
  - `// XXX TODO this is unclear, explain what is being accumulated where.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L279)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:285** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L285)

- [ ] **components/learning/moses/moses/moses/eda/local_structure.h:302** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/eda/local_structure.h#L302)

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
  - `// XXX TODO fix the cap so its more sensitive to the size of`
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

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.cc:557** (LOW, Feature Completion)
  - `// OC_ASSERT(false, "TODO: understand what is the role source=0 exactly");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.cc#L557)

- [ ] **components/learning/moses/moses/moses/moses/mpi_moses.h:51** (LOW, Feature Completion)
  - `// this just right now. XXX TODO: do this, someday.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/mpi_moses.h#L51)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:102** (LOW, Feature Completion)
  - `* @todo: in order to better approximate the real-number metric, we`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L102)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:122** (LOW, Feature Completion)
  - `* @todo: term algebra fields are ignored for now`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L122)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:252** (LOW, Feature Completion)
  - `* @todo: term algebra is ignored for the moment.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L252)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:327** (LOW, Feature Completion)
  - `// XXX TODO, unroll the last tail call, just like the single-bit`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L327)

- [ ] **components/learning/moses/moses/moses/moses/neighborhood_sampling.h:478** (LOW, Feature Completion)
  - `* by looping on the tail-call, just as in the xxx routine...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/moses/neighborhood_sampling.h#L478)

- [ ] **components/learning/moses/moses/moses/moses/partial.cc:96** (LOW, Feature Completion)
  - `// XXX TODO: we need to get the actual number of gens run, back`
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

- [ ] **components/learning/moses/moses/moses/optimization/univariate.cc:89** (LOW, Feature Completion)
  - `"Trunction selection not implemented."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/optimization/univariate.cc#L89)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:211** (LOW, Documentation)
  - `* XXX TODO: see comments on disc_probe() below.  This method is a real`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/build_knobs.cc#L211)

- [ ] **components/learning/moses/moses/moses/representation/build_knobs.cc:581** (LOW, Feature Completion)
  - `// XXX TODO clarify actual breakeven on range of problems...`
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

- [ ] **components/learning/moses/moses/moses/representation/knobs.h:208** (LOW, Feature Completion)
  - `idx = 0;  // interpreted as "absent", as used below. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/knobs.h#L208)

- [ ] **components/learning/moses/moses/moses/representation/representation.cc:51** (LOW, Feature Completion)
  - `// XXX TODO: One might think that varying the stepsize, i.e. shrinking`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/representation.cc#L51)

- [ ] **components/learning/moses/moses/moses/representation/representation.cc:236** (LOW, Feature Completion)
  - `// XXX TODO need to add support for "term algebra" knobs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/representation/representation.cc#L236)

- [ ] **components/learning/moses/moses/moses/scoring/bscores.cc:570** (LOW, Feature Completion)
  - `// TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/bscores.cc#L570)

- [ ] **components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:620** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/discriminating_bscore.cc#L620)

- [ ] **components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:629** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/discriminating_bscore.cc#L629)

- [ ] **components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:681** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/discriminating_bscore.cc#L681)

- [ ] **components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:688** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/discriminating_bscore.cc#L688)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:75** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Ensemble scoring not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L75)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:115** (LOW, Error Handling)
  - `OC_ASSERT(false, "bscore error not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L115)

- [ ] **components/learning/moses/moses/moses/scoring/scoring_base.cc:130** (LOW, Error Handling)
  - `OC_ASSERT(false, "tree error not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/scoring_base.cc#L130)

- [ ] **components/learning/moses/moses/moses/scoring/ss_bscore.cc:146** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/ss_bscore.cc#L146)

- [ ] **components/learning/moses/moses/moses/scoring/ss_bscore.cc:150** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/learning/moses/moses/moses/scoring/ss_bscore.cc#L150)

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

- [ ] **moses/moses/comboreduct/interpreter/eval.cc:514** (LOW, Feature Completion)
  - `OC_ASSERT(false, "apply() is not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/interpreter/eval.cc#L514)

- [ ] **moses/moses/comboreduct/main/action-reductor.cc:93** (LOW, Feature Completion)
  - `// TODO -- replace this by cond`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/main/action-reductor.cc#L93)

- [ ] **moses/moses/comboreduct/main/eval-table.cc:165** (LOW, Feature Completion)
  - `"Timestamp feature not implemented. "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/main/eval-table.cc#L165)

- [ ] **moses/moses/comboreduct/reduct/contin_rules.cc:489** (LOW, Feature Completion)
  - `return; //@todo: maybe the other`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/contin_rules.cc#L489)

- [ ] **moses/moses/comboreduct/reduct/contin_rules.cc:963** (LOW, Feature Completion)
  - `// TODO:  sin(*(-1 x)) -> -sin(x)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/contin_rules.cc#L963)

- [ ] **moses/moses/comboreduct/reduct/flat_normal_form.cc:31** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/flat_normal_form.cc#L31)

- [ ] **moses/moses/comboreduct/reduct/flat_normal_form.cc:54** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/flat_normal_form.cc#L54)

- [ ] **moses/moses/comboreduct/reduct/general_rules.cc:72** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/general_rules.cc#L72)

- [ ] **moses/moses/comboreduct/reduct/logical_rules.cc:100** (LOW, Feature Completion)
  - `// XXX TODO: I don't understand why this is not damaging contin_if  !??`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/logical_rules.cc#L100)

- [ ] **moses/moses/comboreduct/reduct/logical_rules.cc:289** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/logical_rules.cc#L289)

- [ ] **moses/moses/comboreduct/reduct/logical_rules.cc:341** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/logical_rules.cc#L341)

- [ ] **moses/moses/comboreduct/reduct/mixed_rules.cc:959** (LOW, Feature Completion)
  - `// maybe TODO : 0<sum x_i -> true if exist i 0<x_i->true and forall other i 0<=x_i`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/mixed_rules.cc#L959)

- [ ] **moses/moses/comboreduct/reduct/mixed_rules.cc:1228** (LOW, Feature Completion)
  - `//check if 0<-(y+pi) -> false //TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/reduct/mixed_rules.cc#L1228)

- [ ] **moses/moses/comboreduct/table/table.cc:124** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L124)

- [ ] **moses/moses/comboreduct/table/table.cc:403** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L403)

- [ ] **moses/moses/comboreduct/table/table.cc:428** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L428)

- [ ] **moses/moses/comboreduct/table/table.cc:842** (LOW, Feature Completion)
  - `// XXX TODO replace this by the util p_norm function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.cc#L842)

- [ ] **moses/moses/comboreduct/table/table.h:99** (LOW, Feature Completion)
  - `// XXX FIXME TODO: change the implementation, per the above note.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L99)

- [ ] **moses/moses/comboreduct/table/table.h:342** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L342)

- [ ] **moses/moses/comboreduct/table/table.h:346** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L346)

- [ ] **moses/moses/comboreduct/table/table.h:634** (LOW, Error Handling)
  - `// XXX TODO WARNING ERROR: builtin hardcoded shit!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L634)

- [ ] **moses/moses/comboreduct/table/table.h:638** (LOW, Error Handling)
  - `// XXX TODO WARNING ERROR: builtin hardcoded shit!!!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L638)

- [ ] **moses/moses/comboreduct/table/table.h:788** (LOW, Error Handling)
  - `* rows into vertex_seq (this is also a hack till it handles`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L788)

- [ ] **moses/moses/comboreduct/table/table.h:1076** (LOW, Feature Completion)
  - `// XXX TODO to implement enum support, cut-n-paste from CTable`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1076)

- [ ] **moses/moses/comboreduct/table/table.h:1225** (LOW, Feature Completion)
  - `OC_ASSERT(false, "case not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1225)

- [ ] **moses/moses/comboreduct/table/table.h:1274** (LOW, Feature Completion)
  - `// XXX TODO, it would be easier if KLD took a sorted list`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table.h#L1274)

- [ ] **moses/moses/comboreduct/table/table_io.cc:525** (LOW, Feature Completion)
  - `* TODO: we really need a sparse table format, as well.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L525)

- [ ] **moses/moses/comboreduct/table/table_io.cc:737** (LOW, Feature Completion)
  - `* It's akind of a temporary hack, till it's clear that this is much`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L737)

- [ ] **moses/moses/comboreduct/table/table_io.cc:1054** (LOW, Feature Completion)
  - `OC_ASSERT(timestamp_feature.empty(), "Timestamp feature not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L1054)

- [ ] **moses/moses/comboreduct/table/table_io.cc:1231** (LOW, Feature Completion)
  - `// TODO: implement timestamp support`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.cc#L1231)

- [ ] **moses/moses/comboreduct/table/table_io.h:137** (LOW, Feature Completion)
  - `// TODO: reimplement loadITable with the same model of loadTable and`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/table/table_io.h#L137)

- [ ] **moses/moses/comboreduct/type_checker/type_tree.cc:40** (LOW, Error Handling)
  - `logger().error() << "default value for " << tn << " not implemented";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/type_checker/type_tree.cc#L40)

- [ ] **moses/moses/comboreduct/type_checker/type_tree.cc:599** (LOW, Feature Completion)
  - `// XXX TODO the code below was modified to allow arg lists of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/comboreduct/type_checker/type_tree.cc#L599)

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

- [ ] **moses/moses/moses/deme/feature_selector.cc:117** (LOW, Feature Completion)
  - `/// XXX TODO Explain what this function does. Why does it create a second`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/deme/feature_selector.cc#L117)

- [ ] **moses/moses/moses/deme/feature_selector.cc:198** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/deme/feature_selector.cc#L198)

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

- [ ] **moses/moses/moses/eda/local_structure.h:145** (LOW, Documentation)
  - `// XXX TODO document what this does...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L145)

- [ ] **moses/moses/moses/eda/local_structure.h:279** (LOW, Feature Completion)
  - `// XXX TODO this is unclear, explain what is being accumulated where.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L279)

- [ ] **moses/moses/moses/eda/local_structure.h:285** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L285)

- [ ] **moses/moses/moses/eda/local_structure.h:302** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/local_structure.h#L302)

- [ ] **moses/moses/moses/eda/optimize.h:120** (LOW, Feature Completion)
  - `std::bind(std::cref(score), std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/optimize.h#L120)

- [ ] **moses/moses/moses/eda/optimize.h:167** (LOW, Feature Completion)
  - `std::bind(std::cref(score), std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/optimize.h#L167)

- [ ] **moses/moses/moses/eda/replacement.h:62** (LOW, Feature Completion)
  - `// TODO: I think it might be a little more efficent to use the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/eda/replacement.h#L62)

- [ ] **moses/moses/moses/main/eval-candidate-likelihood.cc:273** (LOW, Feature Completion)
  - `OC_ASSERT(false, "likelihood for problem %s is not implemented",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/eval-candidate-likelihood.cc#L273)

- [ ] **moses/moses/moses/main/problem-params.cc:166** (LOW, Feature Completion)
  - `// XXX TODO: make this print correctly, instead of using brackets.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L166)

- [ ] **moses/moses/moses/main/problem-params.cc:633** (LOW, Feature Completion)
  - `// The remaining options (TODO organize this)`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L633)

- [ ] **moses/moses/moses/main/problem-params.cc:901** (LOW, Feature Completion)
  - `"default, only a single deme is created. (XXX Does this "`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L901)

- [ ] **moses/moses/moses/main/problem-params.cc:1419** (LOW, Feature Completion)
  - `ss << "Granularity " << time_bscore_granularity_str << " not implemented";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/main/problem-params.cc#L1419)

- [ ] **moses/moses/moses/metapopulation/ensemble.h:55** (LOW, Feature Completion)
  - `* XXX FIXME: right now, the ensemble is attached to the metapop, its`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/metapopulation/ensemble.h#L55)

- [ ] **moses/moses/moses/metapopulation/merging.cc:404** (LOW, Feature Completion)
  - `// XXX TODO fix the cap so its more sensitive to the size of`
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

- [ ] **moses/moses/moses/moses/mpi_moses.cc:201** (LOW, Feature Completion)
  - `// XXX TODO -- trim the deme down, before sending, by using the worst acceptable score.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L201)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:304** (LOW, Feature Completion)
  - `if (!dex.create_demes(exemplar, 0 /* TODO replace with the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L304)

- [ ] **moses/moses/moses/moses/mpi_moses.cc:557** (LOW, Feature Completion)
  - `// OC_ASSERT(false, "TODO: understand what is the role source=0 exactly");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/mpi_moses.cc#L557)

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
  - `// XXX TODO: we need to get the actual number of gens run, back`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/moses/partial.cc#L96)

- [ ] **moses/moses/moses/optimization/hill-climbing.cc:54** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/hill-climbing.cc#L54)

- [ ] **moses/moses/moses/optimization/hill-climbing.h:110** (LOW, Feature Completion)
  - `// XXX TODO make sure this value is appropriately updated.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/hill-climbing.h#L110)

- [ ] **moses/moses/moses/optimization/particle-swarm.cc:52** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.cc#L52)

- [ ] **moses/moses/moses/optimization/particle-swarm.cc:197** (LOW, Feature Completion)
  - `// TODO: work in a better way to identify convergence.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.cc#L197)

- [ ] **moses/moses/moses/optimization/particle-swarm.cc:236** (LOW, Feature Completion)
  - `// TODO: Explanation`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/particle-swarm.cc#L236)

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

- [ ] **moses/moses/moses/optimization/star-anneal.cc:61** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/star-anneal.cc#L61)

- [ ] **moses/moses/moses/optimization/star-anneal.h:125** (LOW, Feature Completion)
  - `* distance.  @todo: it may be better to have the distance`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/star-anneal.h#L125)

- [ ] **moses/moses/moses/optimization/univariate.cc:89** (LOW, Feature Completion)
  - `"Trunction selection not implemented."`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/optimization/univariate.cc#L89)

- [ ] **moses/moses/moses/representation/build_knobs.cc:211** (LOW, Documentation)
  - `* XXX TODO: see comments on disc_probe() below.  This method is a real`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L211)

- [ ] **moses/moses/moses/representation/build_knobs.cc:581** (LOW, Feature Completion)
  - `// XXX TODO clarify actual breakeven on range of problems...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L581)

- [ ] **moses/moses/moses/representation/build_knobs.cc:690** (LOW, Feature Completion)
  - `// XXX TODO: Is this really optimal?  The below adds an entire copy`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L690)

- [ ] **moses/moses/moses/representation/build_knobs.cc:725** (LOW, Feature Completion)
  - `using namespace boost::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L725)

- [ ] **moses/moses/moses/representation/build_knobs.cc:914** (LOW, Error Handling)
  - `logger().warn("TODO: handle case where it = id::times in build_knobs::rec_canonize");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L914)

- [ ] **moses/moses/moses/representation/build_knobs.cc:1011** (LOW, Feature Completion)
  - `// TODO: implement support for enumerated types in the input.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L1011)

- [ ] **moses/moses/moses/representation/build_knobs.cc:1283** (LOW, Feature Completion)
  - `// XXX TODO this below is clearly unfinished, broken, etc.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/build_knobs.cc#L1283)

- [ ] **moses/moses/moses/representation/knobs.h:208** (LOW, Feature Completion)
  - `idx = 0;  // interpreted as "absent", as used below. XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/knobs.h#L208)

- [ ] **moses/moses/moses/representation/representation.cc:51** (LOW, Feature Completion)
  - `// XXX TODO: One might think that varying the stepsize, i.e. shrinking`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/representation.cc#L51)

- [ ] **moses/moses/moses/representation/representation.cc:236** (LOW, Feature Completion)
  - `// XXX TODO need to add support for "term algebra" knobs`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/representation/representation.cc#L236)

- [ ] **moses/moses/moses/scoring/bscores.cc:570** (LOW, Feature Completion)
  - `// TODO`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/bscores.cc#L570)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:620** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L620)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:629** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L629)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:681** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L681)

- [ ] **moses/moses/moses/scoring/discriminating_bscore.cc:688** (LOW, Feature Completion)
  - `// XXX TODO FIXME is this really correct?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/discriminating_bscore.cc#L688)

- [ ] **moses/moses/moses/scoring/scoring_base.cc:75** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Ensemble scoring not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.cc#L75)

- [ ] **moses/moses/moses/scoring/scoring_base.cc:115** (LOW, Error Handling)
  - `OC_ASSERT(false, "bscore error not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.cc#L115)

- [ ] **moses/moses/moses/scoring/scoring_base.cc:130** (LOW, Error Handling)
  - `OC_ASSERT(false, "tree error not implemented for bscorer %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/scoring_base.cc#L130)

- [ ] **moses/moses/moses/scoring/ss_bscore.cc:146** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/ss_bscore.cc#L146)

- [ ] **moses/moses/moses/scoring/ss_bscore.cc:150** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented yet");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/ss_bscore.cc#L150)

- [ ] **moses/moses/moses/scoring/time_dispersion.cc:43** (LOW, Feature Completion)
  - `// TODO multipler other than 1 is not supported yet`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/time_dispersion.cc#L43)

- [ ] **moses/moses/moses/scoring/time_dispersion.cc:69** (LOW, Feature Completion)
  - `ss << "Case " << static_cast<size_t>(_granularity) << " not implemented";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/time_dispersion.cc#L69)

- [ ] **moses/moses/moses/scoring/time_dispersion.h:61** (LOW, Feature Completion)
  - `* TODO: multiplier other than 1 are not supported at the moment`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/moses/moses/scoring/time_dispersion.h#L61)

- [ ] **moses/tests/moses/moses-framework.h:42** (LOW, Testing)
  - `char tempfile[] = "/tmp/mosesUTestXXXXXX";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/moses/tests/moses/moses-framework.h#L42)

### Memory System
*Total items: 139*

- [x] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:59** (CRITICAL, Feature Completion)
  - `/// XXX TODO: We could have a non-blocking version of this atom. We`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L59)

- [ ] **atomspace/opencog/atoms/core/TypeUtils.cc:323** (HIGH, Thread Safety)
  - `throw RuntimeException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeUtils.cc#L323)

- [ ] **atomspace/opencog/atoms/flow/SplitLink.cc:59** (HIGH, Thread Safety)
  - `throw RuntimeException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/SplitLink.cc#L59)

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:172** (HIGH, Thread Safety)
  - `std::thread([as, silent, todo_list, qvp, finished_count, nthreads = _nthreads]() {`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L172)

- [ ] **atomspace/opencog/atomspace/TypeIndex.h:73** (HIGH, Thread Safety)
  - `* @todo The iterator is NOT thread-safe against the insertion or`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/TypeIndex.h#L73)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:1504** (HIGH, Thread Safety)
  - `throw RuntimeException(TRACE_INFO, "Not implemented!!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L1504)

- [ ] **atomspace/opencog/query/SatisfyMixin.cc:178** (HIGH, Performance)
  - `* XXX FIXME: A major performance optimization is possible, to handle`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/SatisfyMixin.cc#L178)

- [ ] **atomspace/opencog/scm/opencog/base/debug-trace.scm:9** (HIGH, Thread Safety)
  - `; problem, we have a quick hack here: just dump trace messages to a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/debug-trace.scm#L9)

- [ ] **atomspace/examples/c++-guile/PrimitiveExample.cc:87** (MEDIUM, Error Handling)
  - `printf("XXX ERROR XXX: an error should have been thrown, but wasn't!\n");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/c++-guile/PrimitiveExample.cc#L87)

- [x] **atomspace/opencog/atoms/base/Valuation.h:48** (MEDIUM, Feature Completion)
  - `ValuePtr _value;  // XXX TODO should this be  vector?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/base/Valuation.h#L48)

- [x] **atomspace/opencog/atoms/core/PrenexLink.cc:207** (MEDIUM, Feature Completion)
  - `// TODO: this step could be simplified by using final_variables`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/PrenexLink.cc#L207)

- [x] **atomspace/opencog/atoms/core/PutLink.cc:375** (MEDIUM, Feature Completion)
  - `// XXX TODO we should perform a type-check on the function.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/PutLink.cc#L375)

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

- [x] **atomspace/opencog/guile/SchemeEval.cc:1065** (MEDIUM, Error Handling)
  - `// TODO: it would be nice to pass exceptions on through, but`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeEval.cc#L1065)

- [ ] **atomspace/opencog/haskell/AtomSpace_CWrapper.h:112** (MEDIUM, Error Handling)
  - `* XXX FIXME no one should be using Handle's to work with atoms,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/haskell/AtomSpace_CWrapper.h#L112)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:169** (MEDIUM, Feature Completion)
  - `// XXX TODO We could start inside an evaluatable, but it would`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L169)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:1158** (MEDIUM, Feature Completion)
  - `// XXX TODO The logic here should be updated to resemble that`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L1158)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:1501** (MEDIUM, Feature Completion)
  - `// its definition. XXX TODO. Hmm. Should we do this at runtime,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L1501)

- [ ] **atomspace/opencog/query/TermMatchMixin.cc:539** (MEDIUM, Feature Completion)
  - `// XXX TODO as discussed on the mailing list, we should perhaps first`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/TermMatchMixin.cc#L539)

- [ ] **atomspace/opencog/scm/opencog/base/utilities.scm:749** (MEDIUM, Feature Completion)
  - `; XXX This should probably be made obsolete.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/utilities.scm#L749)

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

- [ ] **atomspace/examples/atomspace/queue.scm:7** (LOW, Feature Completion)
  - `; XXX FIXME, this example is not yet complete and does not yet work...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/atomspace/queue.scm#L7)

- [ ] **atomspace/examples/atomspace/queue.scm:37** (LOW, Feature Completion)
  - `; XXX This is incorrect, because instead of calling SetValue,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/atomspace/queue.scm#L37)

- [ ] **atomspace/examples/pattern-matcher/deduction-engine.scm:9** (LOW, Feature Completion)
  - `;; XXX under construction, incomplete. The correct fix is to remove`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/pattern-matcher/deduction-engine.scm#L9)

- [ ] **atomspace/examples/pattern-matcher/deduction-engine.scm:237** (LOW, Feature Completion)
  - `;; TODO: x is undefined`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/pattern-matcher/deduction-engine.scm#L237)

- [ ] **atomspace/examples/pattern-matcher/recognizer.scm:448** (LOW, Feature Completion)
  - `; polluting it with sentences.  The push and pop here is a hack;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/examples/pattern-matcher/recognizer.scm#L448)

- [ ] **atomspace/opencog/atoms/atom_types/NameServer.cc:57** (LOW, Feature Completion)
  - `* This is a strange kind of hack to allow the cogserver unit`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/atom_types/NameServer.cc#L57)

- [ ] **atomspace/opencog/atoms/base/Valuation.cc:50** (LOW, Feature Completion)
  - `// XXX TODO -- C++ smart pointers are not atomic; we really`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/base/Valuation.cc#L50)

- [ ] **atomspace/opencog/atoms/core/Checkers.cc:143** (LOW, Feature Completion)
  - `// TODO - look up the schema, and make sure its numeric, also.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/Checkers.cc#L143)

- [ ] **atomspace/opencog/atoms/core/FindUtils.h:146** (LOW, Feature Completion)
  - `* XXX FIXME: what if it appears quoted in one place, and unquoted`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/FindUtils.h#L146)

- [ ] **atomspace/opencog/atoms/core/PutLink.cc:337** (LOW, Error Handling)
  - `* an undefined handle is returned (?? XXX really? or is it a throw?).`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/PutLink.cc#L337)

- [ ] **atomspace/opencog/atoms/core/RandomChoice.cc:143** (LOW, Feature Completion)
  - `// XXX TODO if execute() above returns FloatValue, use that!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RandomChoice.cc#L143)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.cc:234** (LOW, Feature Completion)
  - `OC_ASSERT(false, "Not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.cc#L234)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.cc:276** (LOW, Feature Completion)
  - `// TODO: the following has no unit test!!! Yet it introduces a`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.cc#L276)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.cc:320** (LOW, Feature Completion)
  - `// TODO: generalize with when Unquote and Quote are apart`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.cc#L320)

- [ ] **atomspace/opencog/atoms/core/RewriteLink.h:224** (LOW, Feature Completion)
  - `// TODO: we probably want to`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/RewriteLink.h#L224)

- [ ] **atomspace/opencog/atoms/core/TypeIntersectionLink.cc:180** (LOW, Feature Completion)
  - `"Intersection of deep types not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeIntersectionLink.cc#L180)

- [ ] **atomspace/opencog/atoms/core/TypeIntersectionLink.cc:188** (LOW, Feature Completion)
  - `"Intersection of signatures or type constants not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeIntersectionLink.cc#L188)

- [ ] **atomspace/opencog/atoms/core/TypeNode.h:90** (LOW, Feature Completion)
  - `// XXX TODO ... Some types are defined. In this case,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeNode.h#L90)

- [ ] **atomspace/opencog/atoms/core/TypeUtils.cc:105** (LOW, Feature Completion)
  - `"Not implemented! TODO XXX FIXME");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeUtils.cc#L105)

- [ ] **atomspace/opencog/atoms/core/TypeUtils.cc:167** (LOW, Feature Completion)
  - `* The implementation below feels awfully hacky and bug-prone,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeUtils.cc#L167)

- [ ] **atomspace/opencog/atoms/core/TypeUtils.cc:300** (LOW, Feature Completion)
  - `"Not implemented! TODO XXX FIXME");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypeUtils.cc#L300)

- [ ] **atomspace/opencog/atoms/core/TypedVariableLink.cc:56** (LOW, Feature Completion)
  - `VARIABLE_NODE != dtype and // XXX FIXME this is wrong; URE-bug`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/TypedVariableLink.cc#L56)

- [ ] **atomspace/opencog/atoms/core/Variables.cc:284** (LOW, Error Handling)
  - `* XXX TODO this does not currently handle type equations, as outlined`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/core/Variables.cc#L284)

- [ ] **atomspace/opencog/atoms/execution/EvaluationLink.cc:668** (LOW, Feature Completion)
  - `"Either incorrect or not implemented yet (crisp). Cannot evaluate %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/EvaluationLink.cc#L668)

- [ ] **atomspace/opencog/atoms/execution/EvaluationLink.cc:951** (LOW, Feature Completion)
  - `"Either incorrect or not implemented yet. Cannot evaluate %s",`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/EvaluationLink.cc#L951)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.cc:288** (LOW, Feature Completion)
  - `// TODO: what about globs?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.cc#L288)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.cc:378** (LOW, Feature Completion)
  - `// TODO: copy values`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.cc#L378)

- [ ] **atomspace/opencog/atoms/execution/Instantiator.h:88** (LOW, Feature Completion)
  - `* TODO: maybe this can eliminate the need for`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/execution/Instantiator.h#L88)

- [ ] **atomspace/opencog/atoms/flow/FilterLink.cc:211** (LOW, Feature Completion)
  - `"Globbing for Values not implemented! FIXME!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/FilterLink.cc#L211)

- [ ] **atomspace/opencog/atoms/flow/FilterLink.cc:604** (LOW, Feature Completion)
  - `// XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/flow/FilterLink.cc#L604)

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

- [ ] **atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:194** (LOW, Error Handling)
  - `// TODO: Consider adding error reporting mechanism to QueueValue`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc#L194)

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

- [ ] **atomspace/opencog/atoms/reduct/BoolOpLink.cc:44** (LOW, Feature Completion)
  - `// XXX TODO we can relax this, and accept simple truth values, too.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/reduct/BoolOpLink.cc#L44)

- [ ] **atomspace/opencog/atoms/truthvalue/SimpleTruthValue.cc:135** (LOW, Feature Completion)
  - `"SimpleTruthValue::merge: case not implemented");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atoms/truthvalue/SimpleTruthValue.cc#L135)

- [ ] **atomspace/opencog/atomspace/AtomTable.cc:60** (LOW, Feature Completion)
  - `std::bind(&AtomSpace::typeAdded, this, std::placeholders::_1));`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/AtomTable.cc#L60)

- [ ] **atomspace/opencog/atomspace/TypeIndex.h:54** (LOW, Feature Completion)
  - `#if HAVE_FOLLY_XXX`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/atomspace/TypeIndex.h#L54)

- [ ] **atomspace/opencog/cython/PythonEval.cc:81** (LOW, Feature Completion)
  - `* @todo When can we remove the singleton instance? Answer: not sure.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/PythonEval.cc#L81)

- [ ] **atomspace/opencog/cython/PythonEval.cc:1078** (LOW, Error Handling)
  - `* assorted hacks to handle the different argument type.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/PythonEval.cc#L1078)

- [ ] **atomspace/opencog/cython/PythonEval.cc:1479** (LOW, Feature Completion)
  - `_result += "PythonEval: interrupt not implemented!\n";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/PythonEval.cc#L1479)

- [ ] **atomspace/opencog/cython/PythonEval.cc:1481** (LOW, Feature Completion)
  - `logger().warn("[PythonEval] interrupt not implemented!\n");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/cython/PythonEval.cc#L1481)

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

- [ ] **atomspace/opencog/guile/SchemeSmob.cc:49** (LOW, Feature Completion)
  - `* XXX TODO:`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmob.cc#L49)

- [ ] **atomspace/opencog/guile/SchemeSmob.h:208** (LOW, Feature Completion)
  - `// Logger XXX TODO these do not belong here, they`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmob.h#L208)

- [ ] **atomspace/opencog/guile/SchemeSmobAS.cc:210** (LOW, Feature Completion)
  - `* then set it on the current atomspace.  XXX This is a temporary hack,`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmobAS.cc#L210)

- [ ] **atomspace/opencog/guile/SchemeSmobAS.cc:211** (LOW, Feature Completion)
  - `* until a better permission system is invented. XXX FIXME.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmobAS.cc#L211)

- [ ] **atomspace/opencog/guile/SchemeSmobAtom.cc:210** (LOW, Feature Completion)
  - `printf("XXX FIXME Bad string %s\nconverted to %s\n", (char *) data, wbuf);`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmobAtom.cc#L210)

- [ ] **atomspace/opencog/guile/SchemeSmobValue.cc:242** (LOW, Feature Completion)
  - `* XXX FIXME Clearly, a factory for values is called for.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/guile/SchemeSmobValue.cc#L242)

- [ ] **atomspace/opencog/haskell/PatternMatcher_CWrapper.h:8** (LOW, Feature Completion)
  - `* XXX FIXME: atoms must never be accessed by UUID except by the`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/haskell/PatternMatcher_CWrapper.h#L8)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:380** (LOW, Feature Completion)
  - `// TODO -- weed out duplicates!`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L380)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:549** (LOW, Feature Completion)
  - `return choice_loop(pmc, "xxxxxxxxxx neighbor_search xxxxxxxxxx");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L549)

- [ ] **atomspace/opencog/query/InitiateSearchMixin.cc:576** (LOW, Feature Completion)
  - `return choice_loop(pmc, "xxxxxxxxxx neighbor_search xxxxxxxxxx");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/InitiateSearchMixin.cc#L576)

- [ ] **atomspace/opencog/query/NextSearchMixin.cc:167** (LOW, Feature Completion)
  - `// XXX TODO ... Rather than counting the number of variables, we`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/NextSearchMixin.cc#L167)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2052** (LOW, Feature Completion)
  - `// XXX TODO FIXME. The ptm needs to be decomposed into connected`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2052)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2137** (LOW, Feature Completion)
  - `OC_ASSERT(not term->hasGlobbyVar(), "Buggy or not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2137)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2429** (LOW, Feature Completion)
  - `// XXX TODO make sure that all variables in the clause have`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2429)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:2728** (LOW, Feature Completion)
  - `* XXX TODO -- if the algo is working correctly, then all`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L2728)

- [ ] **atomspace/opencog/query/PatternMatchEngine.cc:3116** (LOW, Feature Completion)
  - `* TODO: The implementation here is minimal - very simple, very basic.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/PatternMatchEngine.cc#L3116)

- [ ] **atomspace/opencog/query/Recognizer.cc:126** (LOW, Feature Completion)
  - `// TODO: Change to something better if possible...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/Recognizer.cc#L126)

- [ ] **atomspace/opencog/query/TermMatchMixin.cc:202** (LOW, Feature Completion)
  - `"Not implemented! Need to implement a stack, here.");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/query/TermMatchMixin.cc#L202)

- [ ] **atomspace/opencog/scm/opencog.scm:22** (LOW, Feature Completion)
  - `; hairy hack needed to be able to run unit tests without installing.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog.scm#L22)

- [ ] **atomspace/opencog/scm/opencog.scm:122** (LOW, Feature Completion)
  - `; FIXME: Both of the above-described problems might no longer exist.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog.scm#L122)

- [ ] **atomspace/opencog/scm/opencog/base/atom-docs.scm:41** (LOW, Documentation)
  - `; XXX FIXME replace below by real docs.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/atom-docs.scm#L41)

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

- [ ] **atomspace/opencog/scm/opencog/base/utilities.scm:1080** (LOW, Feature Completion)
  - `; XXX FIXME -- this is a stunningly slow and sloppy random-string`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace/opencog/scm/opencog/base/utilities.scm#L1080)

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
*Total items: 65*

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
*Total items: 162*

- [x] **atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc:82** (CRITICAL, Feature Completion)
  - `* XXX FIXME This needs to be fuzzed; it is very likely to crash`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc#L82)

- [ ] **atomspace-storage/opencog/persist/api/BackingStore.h:129** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L129)

- [x] **atomspace-storage/opencog/persist/api/BackingStore.h:175** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L175)

- [x] **atomspace-storage/opencog/persist/api/BackingStore.h:193** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L193)

- [x] **atomspace-storage/opencog/persist/api/BackingStore.h:205** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L205)

- [x] **atomspace-storage/opencog/persist/api/BackingStore.h:277** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L277)

- [x] **atomspace-storage/opencog/persist/api/BackingStore.h:286** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L286)

- [ ] **atomspace-storage/opencog/persist/api/BackingStore.h:294** (HIGH, Thread Safety)
  - `throw IOException(TRACE_INFO, "Not implemented!");`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/api/BackingStore.h#L294)

- [ ] **atomspace-restful/opencog/python/web/api/utilities.py:17** (MEDIUM, Feature Completion)
  - `# FIXME: Should this moved to the atomspace repo and be part`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/python/web/api/utilities.py#L17)

- [ ] **atomspace-rocks/opencog/persist/rocks/RocksDAG.cc:240** (MEDIUM, Performance)
  - `// XXX TODO: we should probably cache the results, instead of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc#L240)

- [ ] **atomspace-storage/opencog/persist/csv/table_read.cc:91** (MEDIUM, Documentation)
  - `// TODO: This routine should be extended so that comments that start`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/csv/table_read.cc#L91)

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

- [x] **atomspace-storage/opencog/persist/sexcom/Commands.cc:483** (MEDIUM, Feature Completion)
  - `// TODO: In principle, we should be selective, and only pass`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexcom/Commands.cc#L483)

- [ ] **components/core/atomspace-restful/opencog/python/web/api/utilities.py:17** (MEDIUM, Feature Completion)
  - `# FIXME: Should this moved to the atomspace repo and be part`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/python/web/api/utilities.py#L17)

- [ ] **components/core/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc:240** (MEDIUM, Performance)
  - `// XXX TODO: we should probably cache the results, instead of`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/opencog/persist/rocks/RocksDAG.cc#L240)

- [ ] **atomspace-restful/opencog/events/AtomSpacePublisherModule.cc:51** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc#L51)

- [ ] **atomspace-restful/opencog/events/AtomSpacePublisherModule.h:170** (LOW, Feature Completion)
  - `// TODO: add protoatom to JSON functionality`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-restful/opencog/events/AtomSpacePublisherModule.h#L170)

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

- [ ] **atomspace-rocks/opencog/persist/monospace/MonoIO.cc:920** (LOW, Feature Completion)
  - `// XXX TODO - maybe load links depth-order...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/opencog/persist/monospace/MonoIO.cc#L920)

- [ ] **atomspace-rocks/opencog/persist/monospace/MonoStorage.h:108** (LOW, Feature Completion)
  - `void destroy(void) { kill_data(); /* TODO also delete the db */ }`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/opencog/persist/monospace/MonoStorage.h#L108)

- [ ] **atomspace-rocks/opencog/persist/rocks/RocksStorage.h:137** (LOW, Feature Completion)
  - `void destroy(void) { kill_data(); /* TODO also delete the db */ }`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-rocks/opencog/persist/rocks/RocksStorage.h#L137)

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
  - `// XXX TODO ... create this in some temp atomspace...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/DynamicDataProxy.cc#L77)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.cc:79** (LOW, Feature Completion)
  - `rpt += "Monitoring not implemented for ";`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.cc#L79)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.h:64** (LOW, Feature Completion)
  - `virtual void create(void) {} // stop-gap. FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.h#L64)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.h:65** (LOW, Feature Completion)
  - `virtual void destroy(void);  //stop-gap. FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.h#L65)

- [ ] **atomspace-storage/opencog/persist/proxy/ProxyNode.h:66** (LOW, Feature Completion)
  - `virtual void erase(void);    // stop-gap. FIXME`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/proxy/ProxyNode.h#L66)

- [ ] **atomspace-storage/opencog/persist/sexcom/Dispatcher.cc:49** (LOW, Feature Completion)
  - `using namespace std::placeholders;  // for _1, _2, _3...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/opencog/persist/sexcom/Dispatcher.cc#L49)

- [ ] **atomspace-storage/tests/persist/file/load-file-test.scm:26** (LOW, Testing)
  - `; Hack filename to go to the correct directory for the unit tests.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/atomspace-storage/tests/persist/file/load-file-test.scm#L26)

- [ ] **components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc:51** (LOW, Feature Completion)
  - `using namespace std::placeholders;`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.cc#L51)

- [ ] **components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.h:170** (LOW, Feature Completion)
  - `// TODO: add protoatom to JSON functionality`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-restful/opencog/events/AtomSpacePublisherModule.h#L170)

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

- [ ] **components/core/atomspace-rocks/opencog/persist/monospace/MonoStorage.h:108** (LOW, Feature Completion)
  - `void destroy(void) { kill_data(); /* TODO also delete the db */ }`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/opencog/persist/monospace/MonoStorage.h#L108)

- [ ] **components/core/atomspace-rocks/opencog/persist/rocks/RocksStorage.h:137** (LOW, Feature Completion)
  - `void destroy(void) { kill_data(); /* TODO also delete the db */ }`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/components/core/atomspace-rocks/opencog/persist/rocks/RocksStorage.h#L137)

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
*Total items: 7*

- [ ] **cogserver/opencog/network/ServerSocket.cc:162** (HIGH, Thread Safety)
  - `// TODO: should use std::jthread, once c++20 is widely available.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/network/ServerSocket.cc#L162)

- [ ] **cogserver/opencog/cogserver/attic/proxy/ReadThruProxy.cc:72** (LOW, Feature Completion)
  - `using namespace std::placeholders;  // for _1, _2, _3...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/cogserver/attic/proxy/ReadThruProxy.cc#L72)

- [ ] **cogserver/opencog/cogserver/attic/proxy/ThruCommands.cc:76** (LOW, Feature Completion)
  - `// TODO: check if the StorageNode is read-only.`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/cogserver/attic/proxy/ThruCommands.cc#L76)

- [ ] **cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc:72** (LOW, Feature Completion)
  - `using namespace std::placeholders;  // for _1, _2, _3...`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc#L72)

- [ ] **cogserver/opencog/network/GenericShell.cc:456** (LOW, Feature Completion)
  - `* XXX Is this still true?`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/network/GenericShell.cc#L456)

- [ ] **cogserver/opencog/network/WebSocket.cc:235** (LOW, Feature Completion)
  - `Send("HTTP/1.1 501 Not Implemented\r\n"`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/opencog/network/WebSocket.cc#L235)

- [ ] **cogserver/scripts/get_python_lib.py:6** (LOW, Feature Completion)
  - `# This is a hack due to the distutils in debian/ubuntu's python3 being misconfigured`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/cogserver/scripts/get_python_lib.py#L6)

### Testing Framework
*Total items: 29*

- [ ] **tests/integration/test_atomspace-restful.py:23** (LOW, Testing)
  - `# TODO: Implement actual import test`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-restful.py#L23)

- [ ] **tests/integration/test_atomspace-restful.py:24** (LOW, Testing)
  - `self.assertTrue(True, "atomspace-restful import test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-restful.py#L24)

- [ ] **tests/integration/test_atomspace-restful.py:28** (LOW, Testing)
  - `# TODO: Implement basic functionality test`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-restful.py#L28)

- [ ] **tests/integration/test_atomspace-restful.py:29** (LOW, Testing)
  - `self.assertTrue(True, "atomspace-restful functionality test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-restful.py#L29)

- [ ] **tests/integration/test_atomspace-restful.py:33** (LOW, Testing)
  - `# TODO: Test dependency integration`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-restful.py#L33)

- [ ] **tests/integration/test_atomspace-restful.py:34** (LOW, Testing)
  - `self.assertTrue(True, "atomspace-restful dependency test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-restful.py#L34)

- [ ] **tests/integration/test_atomspace-rocks.py:23** (LOW, Testing)
  - `# TODO: Implement actual import test`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-rocks.py#L23)

- [ ] **tests/integration/test_atomspace-rocks.py:24** (LOW, Testing)
  - `self.assertTrue(True, "atomspace-rocks import test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-rocks.py#L24)

- [ ] **tests/integration/test_atomspace-rocks.py:28** (LOW, Testing)
  - `# TODO: Implement basic functionality test`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-rocks.py#L28)

- [ ] **tests/integration/test_atomspace-rocks.py:29** (LOW, Testing)
  - `self.assertTrue(True, "atomspace-rocks functionality test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-rocks.py#L29)

- [ ] **tests/integration/test_atomspace-rocks.py:33** (LOW, Testing)
  - `# TODO: Test dependency integration`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-rocks.py#L33)

- [ ] **tests/integration/test_atomspace-rocks.py:34** (LOW, Testing)
  - `self.assertTrue(True, "atomspace-rocks dependency test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_atomspace-rocks.py#L34)

- [ ] **tests/integration/test_moses.py:23** (LOW, Testing)
  - `# TODO: Implement actual import test`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_moses.py#L23)

- [ ] **tests/integration/test_moses.py:24** (LOW, Testing)
  - `self.assertTrue(True, "moses import test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_moses.py#L24)

- [ ] **tests/integration/test_moses.py:28** (LOW, Testing)
  - `# TODO: Implement basic functionality test`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_moses.py#L28)

- [ ] **tests/integration/test_moses.py:29** (LOW, Testing)
  - `self.assertTrue(True, "moses functionality test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_moses.py#L29)

- [ ] **tests/integration/test_moses.py:33** (LOW, Testing)
  - `# TODO: Test dependency integration`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_moses.py#L33)

- [ ] **tests/integration/test_moses.py:34** (LOW, Testing)
  - `self.assertTrue(True, "moses dependency test placeholder")`
  - [Code reference](https://github.com/OzCog/opencog-unified/blob/25d11bfe332cd501a967d9ab3a6957a22504249f/tests/integration/test_moses.py#L34)

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


---

## Summary Statistics

### By Subsystem
- **AI System**: 4 items
- **Build System**: 85 items
- **Core Utilities**: 33 items
- **MOSES Representation/Scoring**: 304 items
- **Memory System**: 139 items
- **Other**: 65 items
- **Persistence Subsystem**: 162 items
- **Task System**: 7 items
- **Testing Framework**: 29 items

### By Category
- **Distributed Systems**: 2 items
- **Documentation**: 14 items
- **Error Handling**: 35 items
- **Feature Completion**: 567 items
- **Pattern Matching**: 9 items
- **Performance**: 15 items
- **Testing**: 167 items
- **Thread Safety**: 19 items

### By Priority
- **CRITICAL**: 2 items
- **HIGH**: 27 items
- **MEDIUM**: 86 items
- **LOW**: 713 items

---



## 🔄 Recursive Resolution Progress

**Current Iteration:** 2  
**Last Run:** 2025-07-26T13:23:42.301371  
**TODOs Resolved:** 0  
**TODOs In Progress:** 5  
**Total Remaining:** 820

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 3  
**Last Run:** 2025-07-26T13:28:32.632984  
**TODOs Resolved:** 0  
**TODOs In Progress:** 10  
**Total Remaining:** 815

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 4  
**Last Run:** 2025-07-28T03:10:49.262638  
**TODOs Resolved:** 1  
**TODOs In Progress:** 14  
**Total Remaining:** 810

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 5  
**Last Run:** 2025-07-28T03:14:16.756681  
**TODOs Resolved:** 2  
**TODOs In Progress:** 18  
**Total Remaining:** 805

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 6  
**Last Run:** 2025-07-28T03:20:44.171297  
**TODOs Resolved:** 2  
**TODOs In Progress:** 23  
**Total Remaining:** 800

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 7  
**Last Run:** 2025-07-28T03:21:47.751584  
**TODOs Resolved:** 2  
**TODOs In Progress:** 28  
**Total Remaining:** 795

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 9  
**Last Run:** 2025-08-16T19:36:50.604896  
**TODOs Resolved:** 23  
**TODOs In Progress:** 27  
**Total Remaining:** 790

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 10  
**Last Run:** 2025-08-16T19:37:05.635913  
**TODOs Resolved:** 23  
**TODOs In Progress:** 32  
**Total Remaining:** 785

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 11  
**Last Run:** 2025-08-16T19:40:57.527603  
**TODOs Resolved:** 27  
**TODOs In Progress:** 33  
**Total Remaining:** 780

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 12  
**Last Run:** 2025-08-16T19:41:14.688356  
**TODOs Resolved:** 27  
**TODOs In Progress:** 38  
**Total Remaining:** 775

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 13  
**Last Run:** 2025-08-16T19:43:42.600639  
**TODOs Resolved:** 29  
**TODOs In Progress:** 41  
**Total Remaining:** 770

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 14  
**Last Run:** 2025-08-16T19:43:59.269305  
**TODOs Resolved:** 29  
**TODOs In Progress:** 46  
**Total Remaining:** 765

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 16  
**Last Run:** 2025-08-17T03:52:23.876597  
**TODOs Resolved:** 35  
**TODOs In Progress:** 50  
**Total Remaining:** 763

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 17  
**Last Run:** 2025-08-17T08:26:05.166069  
**TODOs Resolved:** 35  
**TODOs In Progress:** 55  
**Total Remaining:** 758

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 18  
**Last Run:** 2025-08-17T08:27:23.267482  
**TODOs Resolved:** 35  
**TODOs In Progress:** 60  
**Total Remaining:** 753

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 19  
**Last Run:** 2025-08-17T08:31:47.878157  
**TODOs Resolved:** 37  
**TODOs In Progress:** 63  
**Total Remaining:** 748

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 20  
**Last Run:** 2025-08-17T08:36:02.047580  
**TODOs Resolved:** 38  
**TODOs In Progress:** 66  
**Total Remaining:** 744

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 21  
**Last Run:** 2025-08-17T08:37:34.016490  
**TODOs Resolved:** 38  
**TODOs In Progress:** 69  
**Total Remaining:** 741

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 22  
**Last Run:** 2025-08-17T08:38:28.111959  
**TODOs Resolved:** 38  
**TODOs In Progress:** 73  
**Total Remaining:** 737

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 23  
**Last Run:** 2025-09-30T05:49:28.016059  
**TODOs Resolved:** 39  
**TODOs In Progress:** 76  
**Total Remaining:** 733

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 24  
**Last Run:** 2025-09-30T05:56:03.195674  
**TODOs Resolved:** 43  
**TODOs In Progress:** 75  
**Total Remaining:** 730

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---


## 🔄 Recursive Resolution Progress

**Current Iteration:** 25  
**Last Run:** 2025-09-30T05:58:07.850947  
**TODOs Resolved:** 44  
**TODOs In Progress:** 78  
**Total Remaining:** 726

*Use `python scripts/recursive_todo_resolver.py --next-batch` to continue resolution*

---
## Meta-Cognitive Enhancement Instructions

### For Maintainers: Updating This List

1. **Automatic Updates**: Run `python scripts/generate_todo_catalog.py` after significant code changes
2. **Manual Tracking**: Check off items as they are resolved and link to PRs
3. **New TODO Guidelines**: When adding new TODOs, include context and priority indicators
4. **CI Integration**: This catalog should be regenerated on each CI run that detects TODOs

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

*This document is automatically generated and should be updated regularly as the codebase evolves. Last updated: 2025-07-26 13:21:10 UTC*