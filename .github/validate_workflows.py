#!/usr/bin/env python3
"""
Validate GitHub Actions workflow files for proper repository checkout.

This script checks that workflows properly check out repositories before
attempting to access files, and use proper path variables.
"""

import yaml
import sys
from pathlib import Path


def validate_workflow(workflow_path):
    """Validate a single workflow file."""
    print(f"\n🔍 Validating: {workflow_path.name}")
    
    with open(workflow_path) as f:
        try:
            workflow = yaml.safe_load(f)
        except yaml.YAMLError as e:
            print(f"  ❌ YAML parsing error: {e}")
            return False
    
    if not workflow or 'jobs' not in workflow:
        print(f"  ⚠️  No jobs found in workflow")
        return True
    
    issues_found = []
    warnings = []
    
    for job_name, job_config in workflow['jobs'].items():
        if 'steps' not in job_config:
            continue
            
        steps = job_config['steps']
        has_main_checkout = False
        has_external_checkout = False
        checkout_order = []
        
        for idx, step in enumerate(steps):
            if not isinstance(step, dict):
                continue
                
            # Check for checkout actions
            if step.get('uses', '').startswith('actions/checkout'):
                if 'with' in step and 'repository' in step['with']:
                    # External repository checkout
                    has_external_checkout = True
                    checkout_order.append(('external', idx, step['with']['repository']))
                else:
                    # Main repository checkout
                    has_main_checkout = True
                    checkout_order.append(('main', idx, 'main-repo'))
            
            # Check for hardcoded paths
            if 'run' in step:
                run_script = step['run']
                if '/home/runner/work/' in run_script:
                    issues_found.append(
                        f"  ❌ Job '{job_name}', step {idx}: Contains hardcoded path "
                        f"'/home/runner/work/' - use ${{{{ github.workspace }}}} instead"
                    )
        
        # Validate checkout order
        if has_external_checkout and not has_main_checkout:
            warnings.append(
                f"  ⚠️  Job '{job_name}': Has external checkout but no main repo checkout. "
                f"This is OK if main repo files are not needed."
            )
        
        if has_main_checkout and has_external_checkout:
            # Check that main comes before external
            main_idx = next(idx for typ, idx, _ in checkout_order if typ == 'main')
            external_indices = [idx for typ, idx, _ in checkout_order if typ == 'external']
            
            if any(ext_idx < main_idx for ext_idx in external_indices):
                issues_found.append(
                    f"  ❌ Job '{job_name}': External repository checked out before main repo. "
                    f"This may cause issues if main repo files are needed."
                )
            else:
                print(f"  ✅ Job '{job_name}': Proper checkout order (main → external)")
    
    # Report findings
    if issues_found:
        print(f"  ❌ ISSUES FOUND:")
        for issue in issues_found:
            print(issue)
    
    if warnings:
        print(f"  ⚠️  WARNINGS:")
        for warning in warnings:
            print(warning)
    
    if not issues_found and not warnings:
        print(f"  ✅ No issues found")
    
    return len(issues_found) == 0


def main():
    """Validate all workflow files."""
    workflows_dir = Path('.github/workflows')
    
    if not workflows_dir.exists():
        print(f"❌ Workflows directory not found: {workflows_dir}")
        return 1
    
    workflow_files = list(workflows_dir.glob('*.yml')) + list(workflows_dir.glob('*.yaml'))
    
    if not workflow_files:
        print(f"❌ No workflow files found in {workflows_dir}")
        return 1
    
    print(f"🔍 Validating {len(workflow_files)} workflow files...")
    
    all_valid = True
    for workflow_file in sorted(workflow_files):
        if not validate_workflow(workflow_file):
            all_valid = False
    
    print("\n" + "="*60)
    if all_valid:
        print("✅ All workflows validated successfully!")
        return 0
    else:
        print("❌ Some workflows have issues that need to be fixed")
        return 1


if __name__ == '__main__':
    sys.exit(main())
