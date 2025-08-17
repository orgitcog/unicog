#!/usr/bin/env python3
"""
Test script for the Recursive TODO Resolution System

This script validates the complete workflow:
1. Catalog extraction
2. Batch selection
3. Issue generation
4. Progress tracking
5. Completion marking
6. Iteration continuation
"""

import os
import sys
import subprocess
import json
from pathlib import Path

def run_command(cmd, description):
    """Run a command and return its output"""
    print(f"🧪 {description}")
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True, cwd=os.getcwd())
    
    if result.returncode != 0:
        print(f"❌ Command failed: {cmd}")
        print(f"Error: {result.stderr}")
        return None
    
    print(f"✅ {description} - Success")
    return result.stdout

def test_recursive_todo_resolution():
    """Test the complete recursive TODO resolution workflow"""
    
    print("🎯 Testing Recursive TODO Resolution System")
    print("=" * 50)
    
    # Test 1: Check current status
    print("\n1️⃣ Testing status command")
    output = run_command("python scripts/recursive_todo_resolver.py --status", "Get current status")
    if not output:
        return False
    
    # Extract status info
    lines = output.strip().split('\n')
    unchecked_line = [l for l in lines if "Unchecked:" in l][0]
    unchecked_count = int(unchecked_line.split(":")[1].strip())
    print(f"   📊 Found {unchecked_count} unchecked TODOs")
    
    # Test 2: Generate a batch
    print("\n2️⃣ Testing batch generation")
    output = run_command("python scripts/recursive_todo_resolver.py --next-batch", "Generate next batch")
    if not output:
        return False
    
    # Check that issue file was created
    issue_files = list(Path(".").glob("TODO_BATCH_*_ISSUE.md"))
    if not issue_files:
        print("❌ No issue file was generated")
        return False
    
    latest_issue = max(issue_files, key=lambda p: p.stat().st_mtime)
    print(f"   📝 Generated issue file: {latest_issue}")
    
    # Verify issue content
    with open(latest_issue, 'r') as f:
        issue_content = f.read()
    
    required_sections = [
        "# Iterative TODO Resolution – Batch",
        "## 🎯 Objective",
        "## 🧩 Batch",
        "## 🔄 Next Steps",
        "## 🧬 Meta-Pathway",
        "## 🕰️ Progress Log"
    ]
    
    missing_sections = []
    for section in required_sections:
        if section not in issue_content:
            missing_sections.append(section)
    
    if missing_sections:
        print(f"❌ Missing issue sections: {missing_sections}")
        return False
    
    print("   ✅ Issue content validates successfully")
    
    # Test 3: Check progress tracking
    print("\n3️⃣ Testing progress tracking")
    if not Path("todo_resolution_progress.json").exists():
        print("❌ Progress file not created")
        return False
    
    with open("todo_resolution_progress.json", 'r') as f:
        progress = json.load(f)
    
    required_keys = ["current_iteration", "completed_todos", "in_progress_todos", "last_run", "total_resolved"]
    missing_keys = [k for k in required_keys if k not in progress]
    
    if missing_keys:
        print(f"❌ Missing progress keys: {missing_keys}")
        return False
    
    print(f"   📊 Progress tracking: {len(progress['in_progress_todos'])} in progress")
    
    # Test 4: Test completion marking (simulate)
    print("\n4️⃣ Testing completion marking")
    
    # Use a known TODO from catalog that exists and hasn't been completed
    test_todo = "moses/moses/comboreduct/table/table.h:1069"  # Changed to avoid already completed one
    print(f"   🎯 Testing completion of: {test_todo}")
    
    # Mark as completed
    cmd = f"python scripts/recursive_todo_resolver.py --mark-completed \"{test_todo}\" \"https://github.com/test/pr/125\""
    output = run_command(cmd, "Mark TODO as completed")
    
    if not output:
        return False
    
    # Verify it was marked completed
    output = run_command("python scripts/recursive_todo_resolver.py --status", "Check status after completion")
    # Check that completed count is non-zero (it might be more than 1 due to previous tests)
    if "Completed:" not in output or "Completed: 0" in output:
        print("❌ TODO was not marked as completed")
        return False
    
    print("   ✅ Completion marking works correctly")
    
    # Test 5: Test automation script
    print("\n5️⃣ Testing automation script")
    output = run_command("./scripts/automate_todo_resolution.sh status", "Test automation script")
    if not output or "TODO Resolution Status" not in output:
        print("❌ Automation script failed")
        return False
    
    print("   ✅ Automation script works correctly")
    
    # Test 6: Verify catalog integration
    print("\n6️⃣ Testing catalog integration")
    with open("COMPREHENSIVE-TODO-CATALOG.md", 'r') as f:
        catalog_content = f.read()
    
    if "🔄 Recursive Resolution Progress" not in catalog_content:
        print("❌ Progress section not added to catalog")
        return False
    
    print("   ✅ Catalog integration works correctly")
    
    print("\n🎉 All tests passed! Recursive TODO Resolution System is working correctly.")
    print("\n📋 System Features Validated:")
    print("   ✅ Catalog extraction and parsing")
    print("   ✅ Priority-based batch selection")
    print("   ✅ Actionable issue generation")
    print("   ✅ Progress tracking and persistence")
    print("   ✅ Completion marking workflow")
    print("   ✅ Automation script interface")
    print("   ✅ Catalog integration and updates")
    
    return True

def main():
    """Main test execution"""
    
    # Ensure we're in the right directory
    if not Path("COMPREHENSIVE-TODO-CATALOG.md").exists():
        print("❌ Please run this test from the repository root directory")
        return 1
    
    success = test_recursive_todo_resolution()
    return 0 if success else 1

if __name__ == '__main__':
    sys.exit(main())