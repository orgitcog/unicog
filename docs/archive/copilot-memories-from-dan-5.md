# Copilot Memories

Hey copilot it's me dan you tried to store these memories but it failed so i saved them here for you :-)

---

Now let me store this knowledge for future sessions and commit the documentation update:

Call to store_memory
Input
{
  "subject": "GitHub Actions workflow patterns",
  "fact": "Always checkout main repository before external repos when scripts are needed",
  "citations": "File: .github/workflows/cognitive-orchestration.yml (lines 71-80, 237-246) - Added explicit main repo checkout before external repo checkout to fix \"directory not found\" errors when accessing scripts/auto_fix.py",
  "reason": "This pattern is critical for multi-repository workflows. Future workflow modifications must ensure the main repository is checked out before any steps that reference its files. Without this, workflows fail with \"directory not found\" errors. This will help avoid regression of the same issue and guide future workflow development.",
  "category": "general"
}
Failed to store memory with error: Resource not found

Call to store_memory
Input
{
  "subject": "GitHub Actions path references",
  "fact": "Use ${{ github.workspace }} instead of hardcoded paths like /home/runner/work/...",
  "citations": "File: .github/workflows/cognitive-orchestration.yml (lines 99, 112, 283, 296) - Replaced hardcoded path /home/runner/work/opencog-unified/opencog-unified with ${{ github.workspace }} for portability",
  "reason": "Hardcoded paths break portability and make workflows fragile. Using ${{ github.workspace }} ensures workflows work in any runner environment. This is a GitHub Actions best practice that should be followed in all workflow files to ensure maintainability and prevent environment-specific failures.",
  "category": "general"
}
Failed to store memory with error: Resource not found

Call to store_memory
Input
{
  "subject": "workflow validation",
  "fact": "Use .github/validate_workflows.py to check workflow files before committing",
  "citations": "File: .github/validate_workflows.py - Created validation script that checks for proper checkout patterns, hardcoded paths, and checkout order in all workflow YAML files",
  "reason": "This validation tool prevents regressions and enforces best practices for workflow development. Running it before committing workflow changes will catch common issues early. This tool should be integrated into the development process and potentially added to pre-commit hooks or CI checks.",
  "category": "general"
}
Failed to store memory with error: Resource not found

The memory storage failed, but that's okay. Let me commit the documentation update and finalize
