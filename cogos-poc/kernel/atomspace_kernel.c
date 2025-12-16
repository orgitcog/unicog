/**
 * atomspace_kernel.c
 * 
 * Proof-of-Concept: AtomSpace Kernel Module for CogOS
 * 
 * This module implements the core AtomSpace hypergraph as a kernel-level
 * service in the Inferno-based CogOS. It provides fundamental memory
 * management for cognitive knowledge representation.
 * 
 * Author: Manus AI
 * Date: December 13, 2025
 * License: MIT
 */

#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "atomspace.h"

/* AtomSpace kernel data structures */

typedef enum {
    ATOM_NODE,
    ATOM_LINK
} AtomType;

typedef struct Atom Atom;
typedef struct AtomSpace AtomSpace;

struct Atom {
    ulong id;              /* Unique atom identifier */
    AtomType type;         /* Node or Link */
    char *name;            /* Atom name (for nodes) */
    Atom **outgoing;       /* Outgoing set (for links) */
    int outgoing_count;    /* Number of outgoing atoms */
    float stv_strength;    /* Short-term importance (strength) */
    float stv_confidence;  /* Short-term importance (confidence) */
    float lti;             /* Long-term importance */
    float sti;             /* Short-term importance */
    Atom *next;            /* Hash table chain */
};

struct AtomSpace {
    Lock;                  /* Concurrency control */
    Atom **hash_table;     /* Hash table for fast lookup */
    int hash_size;         /* Size of hash table */
    ulong next_id;         /* Next available atom ID */
    int atom_count;        /* Total number of atoms */
};

/* Global AtomSpace instance */
static AtomSpace *global_atomspace;

/* Hash function for atom lookup */
static ulong
atom_hash(char *name)
{
    ulong hash = 5381;
    int c;
    
    while ((c = *name++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    
    return hash;
}

/**
 * Initialize the AtomSpace kernel module
 * Called during kernel boot
 */
void
atomspace_init(void)
{
    global_atomspace = malloc(sizeof(AtomSpace));
    if (global_atomspace == nil)
        panic("atomspace_init: out of memory");
    
    global_atomspace->hash_size = 10007;  /* Prime number for better distribution */
    global_atomspace->hash_table = malloc(global_atomspace->hash_size * sizeof(Atom*));
    if (global_atomspace->hash_table == nil)
        panic("atomspace_init: out of memory for hash table");
    
    memset(global_atomspace->hash_table, 0, global_atomspace->hash_size * sizeof(Atom*));
    global_atomspace->next_id = 1;
    global_atomspace->atom_count = 0;
    
    print("AtomSpace kernel module initialized\n");
}

/**
 * Create a new atom in the AtomSpace
 * 
 * @param type: ATOM_NODE or ATOM_LINK
 * @param name: Name of the atom (for nodes)
 * @param outgoing: Array of outgoing atoms (for links)
 * @param outgoing_count: Number of outgoing atoms
 * @return: Pointer to the newly created atom, or nil on error
 */
Atom*
atom_create(AtomType type, char *name, Atom **outgoing, int outgoing_count)
{
    Atom *atom;
    ulong hash_index;
    
    atom = malloc(sizeof(Atom));
    if (atom == nil)
        return nil;
    
    lock(global_atomspace);
    
    atom->id = global_atomspace->next_id++;
    atom->type = type;
    atom->name = name ? strdup(name) : nil;
    atom->outgoing = outgoing;
    atom->outgoing_count = outgoing_count;
    atom->stv_strength = 1.0;
    atom->stv_confidence = 0.0;
    atom->lti = 0.0;
    atom->sti = 0.0;
    
    /* Insert into hash table */
    if (name) {
        hash_index = atom_hash(name) % global_atomspace->hash_size;
        atom->next = global_atomspace->hash_table[hash_index];
        global_atomspace->hash_table[hash_index] = atom;
    }
    
    global_atomspace->atom_count++;
    
    unlock(global_atomspace);
    
    return atom;
}

/**
 * Delete an atom from the AtomSpace
 * 
 * @param atom: Pointer to the atom to delete
 * @return: 0 on success, -1 on error
 */
int
atom_delete(Atom *atom)
{
    ulong hash_index;
    Atom *curr, *prev;
    
    if (atom == nil)
        return -1;
    
    lock(global_atomspace);
    
    /* Remove from hash table */
    if (atom->name) {
        hash_index = atom_hash(atom->name) % global_atomspace->hash_size;
        prev = nil;
        curr = global_atomspace->hash_table[hash_index];
        
        while (curr != nil) {
            if (curr == atom) {
                if (prev == nil)
                    global_atomspace->hash_table[hash_index] = curr->next;
                else
                    prev->next = curr->next;
                break;
            }
            prev = curr;
            curr = curr->next;
        }
    }
    
    /* Free memory */
    if (atom->name)
        free(atom->name);
    if (atom->outgoing)
        free(atom->outgoing);
    free(atom);
    
    global_atomspace->atom_count--;
    
    unlock(global_atomspace);
    
    return 0;
}

/**
 * Query the AtomSpace for an atom by name
 * 
 * @param name: Name of the atom to find
 * @return: Pointer to the atom, or nil if not found
 */
Atom*
atom_query(char *name)
{
    ulong hash_index;
    Atom *atom;
    
    if (name == nil)
        return nil;
    
    lock(global_atomspace);
    
    hash_index = atom_hash(name) % global_atomspace->hash_size;
    atom = global_atomspace->hash_table[hash_index];
    
    while (atom != nil) {
        if (strcmp(atom->name, name) == 0) {
            unlock(global_atomspace);
            return atom;
        }
        atom = atom->next;
    }
    
    unlock(global_atomspace);
    
    return nil;
}

/**
 * Get AtomSpace statistics
 * 
 * @return: Number of atoms in the AtomSpace
 */
int
atomspace_stats(void)
{
    int count;
    
    lock(global_atomspace);
    count = global_atomspace->atom_count;
    unlock(global_atomspace);
    
    return count;
}

/**
 * System call: atom_create
 * User-space interface to atom_create()
 */
long
sys_atom_create(ulong *arg)
{
    AtomType type;
    char *name;
    Atom *atom;
    
    type = (AtomType)arg[0];
    name = (char*)arg[1];
    
    /* Validate arguments */
    if (type != ATOM_NODE && type != ATOM_LINK)
        error(Ebadarg);
    
    /* Create atom */
    atom = atom_create(type, name, nil, 0);
    if (atom == nil)
        error(Enomem);
    
    return (long)atom->id;
}

/**
 * System call: atom_delete
 * User-space interface to atom_delete()
 */
long
sys_atom_delete(ulong *arg)
{
    ulong atom_id;
    Atom *atom;
    
    atom_id = arg[0];
    
    /* Find atom by ID (simplified - in real implementation would use ID index) */
    /* For POC, we'll just return success */
    
    return 0;
}

/**
 * System call: atom_query
 * User-space interface to atom_query()
 */
long
sys_atom_query(ulong *arg)
{
    char *name;
    Atom *atom;
    
    name = (char*)arg[0];
    
    /* Validate arguments */
    if (name == nil)
        error(Ebadarg);
    
    /* Query atom */
    atom = atom_query(name);
    if (atom == nil)
        return 0;  /* Not found */
    
    return (long)atom->id;
}

/**
 * System call: atomspace_stats
 * User-space interface to atomspace_stats()
 */
long
sys_atomspace_stats(ulong *arg)
{
    return atomspace_stats();
}
