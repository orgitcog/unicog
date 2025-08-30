#!/usr/bin/env python3
"""
Test script to validate attention module dependency declarations
"""

import os
import sys
import subprocess
from pathlib import Path

def test_attention_cmake_dependencies():
    """Test that attention module properly declares its dependencies in CMake"""
    
    attention_cmake = Path(__file__).parent / "CMakeLists.txt"
    
    if not attention_cmake.exists():
        print("ERROR: CMakeLists.txt not found")
        return False
    
    with open(attention_cmake, 'r') as f:
        content = f.read()
    
    # Check for proper find_package calls
    required_packages = [
        "find_package(AtomSpace CONFIG)",
        "find_package(CogServer CONFIG)",
        "find_package(CogUtil CONFIG)"
    ]
    
    for package in required_packages:
        if package not in content:
            print(f"ERROR: Missing {package}")
            return False
        else:
            print(f"✓ Found: {package}")
    
    # Check for dependency documentation
    if "atomspace: For knowledge representation" in content:
        print("✓ Found: Dependency documentation for atomspace")
    else:
        print("ERROR: Missing dependency documentation for atomspace")
        return False
        
    if "cogserver: For distributed server integration" in content:
        print("✓ Found: Dependency documentation for cogserver")
    else:
        print("ERROR: Missing dependency documentation for cogserver")
        return False
    
    # Check for proper linking logic
    if "target_link_libraries(attention ${ATTENTION_LINK_LIBRARIES})" in content:
        print("✓ Found: Proper target linking")
    else:
        print("ERROR: Missing proper target linking")
        return False
    
    print("✓ All attention module dependency checks passed")
    return True

def test_cmake_configuration():
    """Test that CMake configuration works"""
    
    test_dir = Path(__file__).parent / "test_cmake"
    test_dir.mkdir(exist_ok=True)
    
    try:
        # Run cmake configuration
        result = subprocess.run(
            ["cmake", "..", "-DCMAKE_BUILD_TYPE=Release"], 
            cwd=test_dir, 
            capture_output=True, 
            text=True,
            timeout=30
        )
        
        if result.returncode == 0:
            print("✓ CMake configuration successful")
            
            # Check for dependency messages
            if "AtomSpace not found, using relative path" in result.stderr:
                print("✓ AtomSpace fallback working")
            if "CogServer not found, using relative path" in result.stderr:
                print("✓ CogServer fallback working")
                
            return True
        else:
            print(f"ERROR: CMake configuration failed: {result.stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        print("ERROR: CMake configuration timed out")
        return False
    except Exception as e:
        print(f"ERROR: CMake test failed: {e}")
        return False
    finally:
        # Clean up
        import shutil
        if test_dir.exists():
            shutil.rmtree(test_dir)

if __name__ == "__main__":
    print("Testing Attention Module Dependencies...")
    print("=" * 50)
    
    success = True
    
    success &= test_attention_cmake_dependencies()
    print()
    success &= test_cmake_configuration()
    
    print()
    print("=" * 50)
    if success:
        print("✓ All tests passed!")
        sys.exit(0)
    else:
        print("✗ Some tests failed!")
        sys.exit(1)