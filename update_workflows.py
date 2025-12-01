#!/usr/bin/env python3
"""
Update all GitHub Actions workflows with best practices
"""

import os
import yaml
from pathlib import Path

def update_workflow(filepath):
    """Add concurrency control and timeout to workflow"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    lines = content.split('\n')
    updated_lines = []
    
    # Track if we've already added these
    has_concurrency = 'concurrency:' in content
    has_timeout = 'timeout-minutes:' in content
    
    in_jobs = False
    in_job_def = False
    job_indent = 0
    
    for i, line in enumerate(lines):
        # Add concurrency control after 'on:' block
        if line.strip().startswith('on:') and not has_concurrency:
            # Find the end of the 'on:' block
            j = i + 1
            while j < len(lines) and (lines[j].startswith('  ') or lines[j].strip() == ''):
                j += 1
            
            # Insert concurrency after 'on:' block
            if i == len(updated_lines):
                updated_lines.append(line)
                # Skip to end of 'on:' block
                while i < j - 1:
                    i += 1
                    updated_lines.append(lines[i])
                
                # Add concurrency
                updated_lines.append('')
                updated_lines.append('concurrency:')
                updated_lines.append('  group: ${{ github.workflow }}-${{ github.ref }}')
                updated_lines.append('  cancel-in-progress: true')
                continue
        
        # Add timeout to jobs
        if line.strip().startswith('jobs:'):
            in_jobs = True
        
        if in_jobs and line.strip().endswith(':') and not line.strip().startswith('#'):
            # This is a job definition
            job_indent = len(line) - len(line.lstrip())
            updated_lines.append(line)
            
            # Look ahead to see if timeout already exists
            has_job_timeout = False
            for k in range(i+1, min(i+10, len(lines))):
                if 'timeout-minutes:' in lines[k]:
                    has_job_timeout = True
                    break
            
            if not has_job_timeout and not has_timeout:
                # Add timeout after job name
                updated_lines.append(' ' * (job_indent + 2) + 'timeout-minutes: 30')
            continue
        
        updated_lines.append(line)
    
    # Write back
    with open(filepath, 'w') as f:
        f.write('\n'.join(updated_lines))
    
    print(f"Updated: {filepath}")

# Update all workflows
workflow_dir = Path('/home/ubuntu/opencog-unified/.github/workflows')
for workflow_file in workflow_dir.glob('*.yml'):
    if 'enhanced' not in workflow_file.name:
        try:
            update_workflow(workflow_file)
        except Exception as e:
            print(f"Error updating {workflow_file}: {e}")

print("\nWorkflow updates complete!")
