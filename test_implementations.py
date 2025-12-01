#!/usr/bin/env python3
"""
Comprehensive Test Suite for All Implementations
Tests workflows, cognitive architecture, and placeholder resolutions
"""

import os
import json
import subprocess
from pathlib import Path
from typing import Dict, List

class ImplementationTester:
    def __init__(self, repo_path):
        self.repo_path = Path(repo_path)
        self.test_results = {
            "workflow_tests": [],
            "architecture_tests": [],
            "scheme_tests": [],
            "cmake_tests": []
        }
    
    def test_workflows(self):
        """Test GitHub Actions workflows for syntax and completeness"""
        print("\n" + "="*60)
        print("TESTING GITHUB ACTIONS WORKFLOWS")
        print("="*60)
        
        workflow_dir = self.repo_path / ".github" / "workflows"
        
        for workflow_file in workflow_dir.glob("*.yml"):
            try:
                with open(workflow_file, 'r') as f:
                    content = f.read()
                
                # Check for required elements
                checks = {
                    "has_concurrency": "concurrency:" in content,
                    "has_timeout": "timeout-minutes:" in content,
                    "has_checkout": "actions/checkout" in content,
                    "has_jobs": "jobs:" in content
                }
                
                passed = all(checks.values())
                
                self.test_results["workflow_tests"].append({
                    "file": workflow_file.name,
                    "passed": passed,
                    "checks": checks
                })
                
                status = "✓ PASS" if passed else "✗ FAIL"
                print(f"{status}: {workflow_file.name}")
                
            except Exception as e:
                print(f"✗ ERROR: {workflow_file.name} - {e}")
                self.test_results["workflow_tests"].append({
                    "file": workflow_file.name,
                    "passed": False,
                    "error": str(e)
                })
    
    def test_cognitive_architecture(self):
        """Test cognitive architecture implementation"""
        print("\n" + "="*60)
        print("TESTING COGNITIVE ARCHITECTURE")
        print("="*60)
        
        state_file = self.repo_path / "cognitive_architecture_state.json"
        
        if state_file.exists():
            with open(state_file, 'r') as f:
                state = json.load(f)
            
            checks = {
                "has_3_engines": state.get("num_engines") == 3,
                "has_looms": state.get("num_looms") > 0,
                "has_fibers": state.get("num_fibers") > 0,
                "completed_iterations": state.get("iteration_count") > 0
            }
            
            passed = all(checks.values())
            
            self.test_results["architecture_tests"].append({
                "component": "cognitive_architecture",
                "passed": passed,
                "checks": checks,
                "state": state
            })
            
            status = "✓ PASS" if passed else "✗ FAIL"
            print(f"{status}: Cognitive Architecture")
            print(f"  - Engines: {state.get('num_engines')}")
            print(f"  - Looms: {state.get('num_looms')}")
            print(f"  - Fibers: {state.get('num_fibers')}")
            print(f"  - Iterations: {state.get('iteration_count')}")
        else:
            print("✗ FAIL: Cognitive architecture state file not found")
            self.test_results["architecture_tests"].append({
                "component": "cognitive_architecture",
                "passed": False,
                "error": "State file not found"
            })
    
    def test_scheme_implementations(self):
        """Test Scheme implementations"""
        print("\n" + "="*60)
        print("TESTING SCHEME IMPLEMENTATIONS")
        print("="*60)
        
        scheme_dir = self.repo_path / "opencog" / "scm" / "cognitive-core"
        
        if scheme_dir.exists():
            scheme_files = list(scheme_dir.glob("*.scm"))
            
            for scheme_file in scheme_files:
                try:
                    with open(scheme_file, 'r') as f:
                        content = f.read()
                    
                    # Check for Scheme syntax elements
                    checks = {
                        "has_define": "define" in content,
                        "has_export": "export" in content or "define-public" in content,
                        "has_documentation": '"""' in content or ";;;" in content,
                        "has_functions": "lambda" in content or "define (" in content
                    }
                    
                    passed = sum(checks.values()) >= 3  # At least 3 out of 4
                    
                    self.test_results["scheme_tests"].append({
                        "file": scheme_file.name,
                        "passed": passed,
                        "checks": checks
                    })
                    
                    status = "✓ PASS" if passed else "✗ FAIL"
                    print(f"{status}: {scheme_file.name}")
                    
                except Exception as e:
                    print(f"✗ ERROR: {scheme_file.name} - {e}")
        else:
            print("✗ FAIL: Scheme directory not found")
    
    def test_cmake_optimizations(self):
        """Test CMake optimization module"""
        print("\n" + "="*60)
        print("TESTING CMAKE OPTIMIZATIONS")
        print("="*60)
        
        cmake_file = self.repo_path / "cmake" / "OptimizedBuild.cmake"
        
        if cmake_file.exists():
            with open(cmake_file, 'r') as f:
                content = f.read()
            
            checks = {
                "has_ccache": "ccache" in content,
                "has_lto": "INTERPROCEDURAL_OPTIMIZATION" in content,
                "has_parallel": "ProcessorCount" in content,
                "has_precompiled_headers": "target_precompile_headers" in content,
                "has_unity_build": "UNITY_BUILD" in content
            }
            
            passed = all(checks.values())
            
            self.test_results["cmake_tests"].append({
                "file": "OptimizedBuild.cmake",
                "passed": passed,
                "checks": checks
            })
            
            status = "✓ PASS" if passed else "✗ FAIL"
            print(f"{status}: OptimizedBuild.cmake")
            for check, result in checks.items():
                check_status = "✓" if result else "✗"
                print(f"  {check_status} {check}")
        else:
            print("✗ FAIL: OptimizedBuild.cmake not found")
    
    def generate_test_report(self):
        """Generate comprehensive test report"""
        print("\n" + "="*60)
        print("TEST SUMMARY")
        print("="*60)
        
        total_tests = 0
        passed_tests = 0
        
        for category, tests in self.test_results.items():
            category_passed = sum(1 for t in tests if t.get("passed", False))
            category_total = len(tests)
            total_tests += category_total
            passed_tests += category_passed
            
            print(f"\n{category}: {category_passed}/{category_total} passed")
        
        print(f"\n{'='*60}")
        print(f"OVERALL: {passed_tests}/{total_tests} tests passed")
        print(f"Success Rate: {(passed_tests/total_tests*100) if total_tests > 0 else 0:.1f}%")
        print(f"{'='*60}")
        
        # Save report
        report_file = self.repo_path / "test_results.json"
        with open(report_file, 'w') as f:
            json.dump({
                "summary": {
                    "total_tests": total_tests,
                    "passed_tests": passed_tests,
                    "success_rate": (passed_tests/total_tests*100) if total_tests > 0 else 0
                },
                "details": self.test_results
            }, f, indent=2)
        
        print(f"\nDetailed results saved to: {report_file}")
        
        return self.test_results

if __name__ == "__main__":
    tester = ImplementationTester("/home/ubuntu/opencog-unified")
    
    tester.test_workflows()
    tester.test_cognitive_architecture()
    tester.test_scheme_implementations()
    tester.test_cmake_optimizations()
    
    tester.generate_test_report()
