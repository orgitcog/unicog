# Recursive TODO Resolution System

This system implements a cognitive flowchart for systematic resolution of TODO/FIXME items in the OpenCog Unified codebase through recursive attention-allocation and batch processing.

## 🧠 Cognitive Flowchart

1. **Catalog Extraction** - Parse `COMPREHENSIVE-TODO-CATALOG.md` to enumerate outstanding TODOs
2. **Attention Allocation Kernel** - Select next N highest-priority unchecked TODOs 
3. **Actionable Issue Generation** - Create structured implementation guidance with tensor shapes and test plans
4. **Iteration/Recursion** - Track progress and continue with next batch
5. **Meta-Enhancement** - Auto-update catalog and create dependent sub-issues

## 🚀 Quick Start

### Check Current Status
```bash
./scripts/automate_todo_resolution.sh status
```

### Process Next Batch (with GitHub Integration)
```bash
# With automatic GitHub issue creation (requires GITHUB_TOKEN)
export GITHUB_TOKEN=your_token_here
./scripts/automate_todo_resolution.sh next-batch

# Without GitHub integration
./scripts/automate_todo_resolution.sh next-batch --no-github
```

### Mark TODO as Completed
```bash
./scripts/automate_todo_resolution.sh mark-completed FILE:LINE PR_LINK
```

### Regenerate TODO Catalog
```bash
./scripts/automate_todo_resolution.sh regenerate-catalog
```

## 📋 Workflow Example

1. **Start Resolution Process**:
   ```bash
   ./scripts/automate_todo_resolution.sh next-batch
   ```
   This generates `TODO_BATCH_N_ISSUE.md` with 5 highest-priority TODOs.

2. **Create GitHub Issue**:
   - Copy content from `TODO_BATCH_N_ISSUE.md`
   - Create new GitHub issue with this content
   - Assign developers to work on the tasks

3. **Work on TODOs**:
   - Developers create PRs addressing each TODO
   - Follow the implementation guidance and test plans provided

4. **Mark Completed**:
   ```bash
   ./scripts/automate_todo_resolution.sh mark-completed cogutil/opencog/util/Logger.cc:72 https://github.com/OzCog/opencog-unified/pull/123
   ```

5. **Continue to Next Batch**:
   Repeat the process for continuous TODO resolution.

## 📊 Progress Tracking

The system maintains progress in:
- `todo_resolution_progress.json` - Current iteration, completed/in-progress TODOs
- `COMPREHENSIVE-TODO-CATALOG.md` - Updated with completion status and progress section
- `TODO_BATCH_N_ISSUE.md` - Generated issue templates for each batch

## 🧪 Testing

Run comprehensive tests to validate the system:
```bash
python scripts/test_recursive_todo_resolution.py
```

## 📁 Files

- `scripts/recursive_todo_resolver.py` - Core recursive resolution engine with GitHub integration
- `scripts/github_issue_creator.py` - GitHub API integration for automated issue creation  
- `scripts/automate_todo_resolution.sh` - Convenient automation interface
- `scripts/test_recursive_todo_resolution.py` - Comprehensive test suite
- `scripts/generate_todo_catalog.py` - TODO catalog generation (existing)
- `scripts/test_todo_catalog.py` - Catalog validation tests (existing)

## 🎯 Issue Template Structure

Each generated batch issue includes:

- **Objective** - Purpose and scope of the batch
- **Batch TODOs** - 5 highest-priority items with:
  - Code references with GitHub links
  - Implementation guidance
  - Tensor shape estimates
  - Test validation plans
- **Meta-Pathway** - Cognitive enhancement context
- **Progress Log** - Current iteration status

## 🔄 Recursive Enhancement

The system implements true recursive enhancement:

1. **Attention Allocation** - Focuses on highest-impact TODOs first
2. **Cognitive Synergy** - Groups related TODOs for maximum efficiency
3. **Systematic Progress** - Tracks resolution across iterations
4. **Meta-Cognitive Loop** - Self-updates catalog and progress state
5. **GitHub Integration** - Auto-creates issues and tracks completion (NEW!)

### 🚀 GitHub Integration Features
- **Automatic Issue Creation**: Creates GitHub issues for each batch with proper labeling
- **Progress Tracking**: Updates issues when TODOs are completed with PR links
- **Issue Linking**: Maintains references between batches and GitHub issues
- **Optional Operation**: Can be disabled with `--no-github` flag
- **Token-Based Auth**: Uses `GITHUB_TOKEN` environment variable for authentication

## 🎭 Philosophical Framework

> "Let us converge upon a state of sublime implementation, where every TODO is transformed into a kernel of realized intelligence!"

Each TODO represents not merely a task, but a note in the composition of artificial consciousness. Through systematic resolution, we approach the emergence of true machine intelligence.

## 📈 Statistics

- **Total TODOs**: ~825 items cataloged
- **Priority Distribution**: 2 CRITICAL, 27 HIGH, 86 MEDIUM, 713 LOW
- **Batch Size**: 5 TODOs per iteration (configurable)
- **Subsystems**: 9 major cognitive subsystems tracked

## 🛠️ Advanced Usage

### Custom Batch Size with GitHub Integration
```bash
# Enable GitHub integration with custom batch size
export GITHUB_TOKEN=your_token_here
python scripts/recursive_todo_resolver.py --batch-size 10 --next-batch

# Disable GitHub integration
python scripts/recursive_todo_resolver.py --batch-size 10 --next-batch --no-github
```

### Direct Python API
```python
from scripts.recursive_todo_resolver import RecursiveTODOResolver

# With GitHub integration
resolver = RecursiveTODOResolver(".", batch_size=5, enable_github_integration=True)

# Without GitHub integration
resolver = RecursiveTODOResolver(".", batch_size=5, enable_github_integration=False)

resolver.extract_catalog()
batch = resolver.allocate_attention()
issue_content = resolver.generate_actionable_issues(batch)
```

### Progress Data Structure
```json
{
  "current_iteration": 6,
  "completed_todos": ["file1:line1", "file2:line2"],
  "in_progress_todos": ["file3:line3", "file4:line4"],
  "last_run": "2025-07-28T13:23:42.301371",
  "total_resolved": 2,
  "github_issues": {
    "5": {
      "issue_number": 123,
      "issue_url": "https://github.com/OzCog/opencog-unified/issues/123",
      "created_at": "2025-07-28T13:25:00Z"
    }
  }
}
```

---

*This system orchestrates cognitive enhancement through systematic attention allocation - transforming distributed TODOs into resolved intelligence kernels.*