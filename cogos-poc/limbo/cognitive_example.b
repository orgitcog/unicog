implement CognitiveExample;

#
# cognitive_example.b
#
# Proof-of-Concept: Limbo Example Using Cognitive Syscalls
#
# This example demonstrates how user-space applications can interact
# with the cognitive kernel through the new syscalls and /cog namespace.
#
# Author: Manus AI
# Date: December 13, 2025
# License: MIT
#

include "sys.m";
    sys: Sys;
    print: import sys;

include "draw.m";

include "cogspace.m";
    cogspace: CogSpace;
    Atom, AtomSpace: import cogspace;

CognitiveExample: module
{
    init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
    sys = load Sys Sys->PATH;
    cogspace = load CogSpace CogSpace->PATH;
    
    if (cogspace == nil) {
        print("Error: Cannot load CogSpace module\n");
        return;
    }
    
    cogspace->init();
    
    print("=== CogOS Cognitive Example ===\n\n");
    
    # Example 1: Create atoms in the AtomSpace
    example_create_atoms();
    
    # Example 2: Query atoms
    example_query_atoms();
    
    # Example 3: Attention allocation
    example_attention();
    
    # Example 4: Pattern matching
    example_pattern_matching();
    
    print("\n=== Example Complete ===\n");
}

#
# Example 1: Create atoms in the AtomSpace
#
example_create_atoms()
{
    print("Example 1: Creating atoms...\n");
    
    # Create concept nodes
    cat := cogspace->create_node("ConceptNode", "cat");
    animal := cogspace->create_node("ConceptNode", "animal");
    mammal := cogspace->create_node("ConceptNode", "mammal");
    
    if (cat == nil || animal == nil || mammal == nil) {
        print("Error: Failed to create atoms\n");
        return;
    }
    
    print("  Created atom: cat (ID: %d)\n", cat.id);
    print("  Created atom: animal (ID: %d)\n", animal.id);
    print("  Created atom: mammal (ID: %d)\n", mammal.id);
    
    # Create inheritance links
    link1 := cogspace->create_link("InheritanceLink", array[] of {cat, mammal});
    link2 := cogspace->create_link("InheritanceLink", array[] of {mammal, animal});
    
    if (link1 == nil || link2 == nil) {
        print("Error: Failed to create links\n");
        return;
    }
    
    print("  Created link: cat -> mammal (ID: %d)\n", link1.id);
    print("  Created link: mammal -> animal (ID: %d)\n", link2.id);
    
    # Get AtomSpace statistics
    stats := cogspace->get_stats();
    print("  AtomSpace now contains %d atoms\n", stats.atom_count);
}

#
# Example 2: Query atoms from the AtomSpace
#
example_query_atoms()
{
    print("\nExample 2: Querying atoms...\n");
    
    # Query by name
    cat := cogspace->query_by_name("cat");
    if (cat == nil) {
        print("Error: Atom 'cat' not found\n");
        return;
    }
    
    print("  Found atom: %s (ID: %d)\n", cat.name, cat.id);
    print("    STI: %.2f\n", cat.sti);
    print("    LTI: %.2f\n", cat.lti);
    
    # Query by type
    concepts := cogspace->query_by_type("ConceptNode");
    print("  Found %d ConceptNodes\n", len concepts);
    
    for (i := 0; i < len concepts; i++) {
        print("    - %s (ID: %d)\n", concepts[i].name, concepts[i].id);
    }
}

#
# Example 3: Attention allocation
#
example_attention()
{
    print("\nExample 3: Attention allocation...\n");
    
    # Get the 'cat' atom
    cat := cogspace->query_by_name("cat");
    if (cat == nil) {
        print("Error: Atom 'cat' not found\n");
        return;
    }
    
    print("  Initial STI of 'cat': %.2f\n", cat.sti);
    
    # Allocate attention to the atom
    cogspace->attention_allocate(array[] of {cat}, 50.0);
    
    # Query again to see updated STI
    cat = cogspace->query_by_name("cat");
    print("  STI after allocation: %.2f\n", cat.sti);
    
    # Focus attention on the atom
    cogspace->attention_focus(cat);
    print("  Focused attention on 'cat'\n");
    
    # Get attentional focus
    focus := cogspace->attention_get_focus();
    print("  Attentional focus contains %d atoms:\n", len focus);
    for (i := 0; i < len focus; i++) {
        print("    - %s (STI: %.2f)\n", focus[i].name, focus[i].sti);
    }
    
    # Spread attention
    cogspace->attention_spread(array[] of {cat});
    print("  Spread attention from 'cat' to neighbors\n");
}

#
# Example 4: Pattern matching
#
example_pattern_matching()
{
    print("\nExample 4: Pattern matching...\n");
    
    # Create a pattern: (InheritanceLink $X mammal)
    # This pattern matches all atoms that inherit from 'mammal'
    
    mammal := cogspace->query_by_name("mammal");
    if (mammal == nil) {
        print("Error: Atom 'mammal' not found\n");
        return;
    }
    
    # Create variable node for pattern
    var_x := cogspace->create_variable("$X");
    
    # Create pattern
    pattern := cogspace->create_pattern("InheritanceLink", 
                                        array[] of {var_x, mammal});
    
    if (pattern == nil) {
        print("Error: Failed to create pattern\n");
        return;
    }
    
    print("  Created pattern: (InheritanceLink $X mammal)\n");
    
    # Match pattern against AtomSpace
    matches := cogspace->pattern_match(pattern);
    
    print("  Found %d matches:\n", len matches);
    for (i := 0; i < len matches; i++) {
        print("    - %s\n", matches[i].name);
    }
}
