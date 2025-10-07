#!/usr/bin/env python3
"""
Test script to validate opencog component dependencies are properly declared
"""

import os
import re

def test_opencog_dependencies():
    """Test that opencog CMakeLists.txt includes all required dependencies"""
    
    opencog_cmake = "/home/runner/work/opencog-unified/opencog-unified/opencog/CMakeLists.txt"
    
    print("🧪 Testing OpenCog Component Dependencies")
    print("=" * 50)
    
    if not os.path.exists(opencog_cmake):
        print("❌ FAIL: opencog/CMakeLists.txt not found")
        return False
    
    with open(opencog_cmake, 'r') as f:
        content = f.read()
    
    # Required dependencies according to the issue
    required_deps = [
        'AtomSpace',  # Already required
        'CogServer',
        'AttentionModule', 
        'URE',
        'LGAtomese'
    ]
    
    required_targets = [
        'atomspace',
        'cogserver', 
        'attention',
        'ure',
        'lg-atomese'
    ]
    
    print("✅ Testing find_package declarations:")
    for dep in required_deps:
        if f'find_package({dep}' in content:
            print(f"  ✅ {dep}: found")
        else:
            print(f"  ❌ {dep}: missing")
            return False
    
    print("\n✅ Testing target linking:")
    # atomspace is required, so it's linked directly via ATOMSPACE_LIBRARY
    if '${ATOMSPACE_LIBRARY}' in content:
        print(f"  ✅ atomspace: required dependency linking found")
    else:
        print(f"  ❌ atomspace: missing required dependency linking")
        return False
    
    # Other dependencies are conditional
    conditional_targets = ['cogserver', 'attention', 'ure', 'lg-atomese']
    for target in conditional_targets:
        if f'TARGET {target}' in content:
            print(f"  ✅ {target}: conditional linking found")
        else:
            print(f"  ❌ {target}: missing conditional linking")
            return False
    
    print("\n✅ Testing alias target:")
    if 'add_library(opencog ALIAS opencog-main)' in content:
        print("  ✅ opencog alias target: found")
    else:
        print("  ❌ opencog alias target: missing")
        return False
    
    print("\n✅ Testing documentation:")
    required_docs = [
        '# The opencog module requires:',
        'atomspace: For knowledge representation',
        'cogserver: For distributed server integration',
        'attention: For ECAN attention allocation',
        'ure: For unified rule engine',
        'lg-atomese: For Link Grammar parsing'
    ]
    
    for doc in required_docs:
        if doc in content:
            print(f"  ✅ Documentation: '{doc[:40]}...' found")
        else:
            print(f"  ❌ Documentation: '{doc[:40]}...' missing")
            return False
    
    print("\n🎉 ALL TESTS PASSED!")
    print("✅ OpenCog component dependencies properly implemented")
    return True

if __name__ == "__main__":
    success = test_opencog_dependencies()
    exit(0 if success else 1)
