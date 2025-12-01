#!/usr/bin/env python3
"""
Comprehensive Analysis Script for OpenCog Unified Repository
Identifies improvements in GitHub Actions workflows and codebase
"""

import os
import json
import re
from pathlib import Path
from collections import defaultdict

class RepositoryAnalyzer:
    def __init__(self, repo_path):
        self.repo_path = Path(repo_path)
        self.improvements = {
            "workflow_improvements": [],
            "build_optimizations": [],
            "code_quality_issues": [],
            "placeholder_resolutions": [],
            "architecture_enhancements": []
        }
    
    def analyze_workflows(self):
        """Analyze GitHub Actions workflows for improvements"""
        workflow_dir = self.repo_path / ".github" / "workflows"
        
        for workflow_file in workflow_dir.glob("*.yml"):
            with open(workflow_file, 'r') as f:
                content = f.read()
                
            issues = []
            
            # Check for outdated actions
            if "actions/checkout@v3" in content or "actions/checkout@v2" in content:
                issues.append("Outdated checkout action (should use @v4)")
            
            # Check for missing caching
            if "cmake" in content.lower() and "cache" not in content.lower():
                issues.append("Missing CMake build cache")
            
            # Check for missing dependency caching
            if "pip install" in content and "actions/cache" not in content:
                issues.append("Missing pip dependency cache")
            
            # Check for missing timeout
            if "timeout-minutes" not in content:
                issues.append("Missing job timeout configuration")
            
            # Check for missing concurrency control
            if "concurrency:" not in content:
                issues.append("Missing concurrency control to prevent duplicate runs")
            
            # Check for hardcoded versions
            if re.search(r'python-version.*3\.\d+', content):
                issues.append("Consider using matrix strategy for multiple Python versions")
            
            # Check for missing artifact retention
            if "actions/upload-artifact" in content and "retention-days" not in content:
                issues.append("Missing artifact retention policy")
            
            # Check for missing error handling
            if "continue-on-error" not in content and "if: failure()" not in content:
                issues.append("Consider adding error handling strategies")
                
            if issues:
                self.improvements["workflow_improvements"].append({
                    "file": str(workflow_file.relative_to(self.repo_path)),
                    "issues": issues
                })
    
    def analyze_build_system(self):
        """Analyze CMake build system for optimizations"""
        cmake_files = list(self.repo_path.rglob("CMakeLists.txt"))
        
        for cmake_file in cmake_files[:20]:  # Limit to first 20
            try:
                with open(cmake_file, 'r') as f:
                    content = f.read()
                
                issues = []
                
                # Check for missing modern CMake practices
                if "cmake_minimum_required" in content:
                    version_match = re.search(r'cmake_minimum_required\(VERSION (\d+\.\d+)', content)
                    if version_match and float(version_match.group(1)) < 3.15:
                        issues.append("CMake version too old (recommend 3.15+)")
                
                # Check for missing target properties
                if "add_library" in content or "add_executable" in content:
                    if "target_include_directories" not in content:
                        issues.append("Consider using target_include_directories for better encapsulation")
                
                # Check for global settings
                if "include_directories" in content:
                    issues.append("Prefer target_include_directories over global include_directories")
                
                if issues:
                    self.improvements["build_optimizations"].append({
                        "file": str(cmake_file.relative_to(self.repo_path)),
                        "issues": issues
                    })
            except Exception as e:
                pass
    
    def analyze_placeholders(self):
        """Analyze placeholder issues in code"""
        patterns = {
            "TODO": r'//\s*TODO[:\s]+(.*?)(?:\n|$)',
            "FIXME": r'//\s*(?:XXX\s+)?FIXME[:\s]+(.*?)(?:\n|$)',
            "HACK": r'//\s*HACK[:\s]+(.*?)(?:\n|$)',
            "NotImplementedError": r'raise\s+NotImplementedError\((.*?)\)',
            "stub": r'def\s+(\w+).*?:\s*(?:pass|\.\.\.)\s*(?:#.*stub)?'
        }
        
        file_extensions = ['.cpp', '.h', '.cc', '.hpp', '.py', '.scm']
        
        for ext in file_extensions:
            for code_file in self.repo_path.rglob(f"*{ext}"):
                if '.git' in str(code_file):
                    continue
                    
                try:
                    with open(code_file, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    for pattern_type, pattern in patterns.items():
                        matches = re.finditer(pattern, content, re.MULTILINE)
                        for match in matches:
                            self.improvements["placeholder_resolutions"].append({
                                "file": str(code_file.relative_to(self.repo_path)),
                                "type": pattern_type,
                                "line": content[:match.start()].count('\n') + 1,
                                "description": match.group(1) if match.lastindex else match.group(0)
                            })
                except Exception as e:
                    pass
    
    def generate_report(self):
        """Generate comprehensive improvement report"""
        report = {
            "summary": {
                "workflow_issues": len(self.improvements["workflow_improvements"]),
                "build_issues": len(self.improvements["build_optimizations"]),
                "placeholder_count": len(self.improvements["placeholder_resolutions"]),
            },
            "details": self.improvements
        }
        
        output_file = self.repo_path / "improvement_analysis.json"
        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"Analysis complete. Report saved to {output_file}")
        print(f"\nSummary:")
        print(f"  Workflow improvements needed: {report['summary']['workflow_issues']}")
        print(f"  Build optimizations needed: {report['summary']['build_issues']}")
        print(f"  Placeholders to resolve: {report['summary']['placeholder_count']}")
        
        return report

if __name__ == "__main__":
    analyzer = RepositoryAnalyzer("/home/ubuntu/opencog-unified")
    
    print("Analyzing GitHub Actions workflows...")
    analyzer.analyze_workflows()
    
    print("Analyzing build system...")
    analyzer.analyze_build_system()
    
    print("Analyzing placeholder issues...")
    analyzer.analyze_placeholders()
    
    print("\nGenerating report...")
    analyzer.generate_report()
