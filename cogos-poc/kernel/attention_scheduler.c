/**
 * attention_scheduler.c
 * 
 * Proof-of-Concept: ECAN Attention Scheduler for CogOS
 * 
 * This module implements the Economic Attention Network (ECAN) as a
 * kernel-level scheduler service. It manages the allocation of cognitive
 * resources (attention) to atoms in the AtomSpace.
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

/* Attention scheduler configuration */
#define ATTENTIONAL_FOCUS_SIZE 100    /* Number of atoms in attentional focus */
#define ATTENTION_SPREAD_RATE 0.1     /* Rate of attention spreading */
#define ATTENTION_DECAY_RATE 0.01     /* Rate of attention decay */

/* Attention scheduler state */
typedef struct AttentionScheduler AttentionScheduler;

struct AttentionScheduler {
    Lock;                          /* Concurrency control */
    Atom **focus;                  /* Attentional focus (top N atoms) */
    int focus_size;                /* Current size of attentional focus */
    int focus_capacity;            /* Maximum capacity of attentional focus */
    uvlong last_update;            /* Timestamp of last attention update */
};

/* Global attention scheduler instance */
static AttentionScheduler *global_scheduler;

/**
 * Initialize the attention scheduler
 * Called during kernel boot
 */
void
attention_init(void)
{
    global_scheduler = malloc(sizeof(AttentionScheduler));
    if (global_scheduler == nil)
        panic("attention_init: out of memory");
    
    global_scheduler->focus_capacity = ATTENTIONAL_FOCUS_SIZE;
    global_scheduler->focus = malloc(global_scheduler->focus_capacity * sizeof(Atom*));
    if (global_scheduler->focus == nil)
        panic("attention_init: out of memory for focus");
    
    memset(global_scheduler->focus, 0, global_scheduler->focus_capacity * sizeof(Atom*));
    global_scheduler->focus_size = 0;
    global_scheduler->last_update = 0;
    
    print("Attention scheduler initialized\n");
}

/**
 * Allocate attention to a set of atoms
 * 
 * @param atoms: Array of atoms to allocate attention to
 * @param count: Number of atoms
 * @param amount: Amount of attention to allocate (STI increase)
 * @return: 0 on success, -1 on error
 */
int
attention_allocate(Atom **atoms, int count, float amount)
{
    int i;
    
    if (atoms == nil || count <= 0)
        return -1;
    
    lock(global_scheduler);
    
    /* Allocate attention by increasing STI */
    for (i = 0; i < count; i++) {
        if (atoms[i] != nil) {
            atoms[i]->sti += amount;
            
            /* Ensure STI stays within bounds */
            if (atoms[i]->sti > 1000.0)
                atoms[i]->sti = 1000.0;
        }
    }
    
    unlock(global_scheduler);
    
    return 0;
}

/**
 * Focus attention on a specific atom
 * This increases the atom's STI and adds it to the attentional focus
 * 
 * @param atom: Atom to focus on
 * @return: 0 on success, -1 on error
 */
int
attention_focus(Atom *atom)
{
    int i;
    
    if (atom == nil)
        return -1;
    
    lock(global_scheduler);
    
    /* Increase STI significantly */
    atom->sti += 100.0;
    if (atom->sti > 1000.0)
        atom->sti = 1000.0;
    
    /* Add to attentional focus if not already present */
    for (i = 0; i < global_scheduler->focus_size; i++) {
        if (global_scheduler->focus[i] == atom) {
            unlock(global_scheduler);
            return 0;  /* Already in focus */
        }
    }
    
    /* Add to focus if there's room */
    if (global_scheduler->focus_size < global_scheduler->focus_capacity) {
        global_scheduler->focus[global_scheduler->focus_size++] = atom;
    } else {
        /* Focus is full - replace atom with lowest STI */
        int min_index = 0;
        float min_sti = global_scheduler->focus[0]->sti;
        
        for (i = 1; i < global_scheduler->focus_size; i++) {
            if (global_scheduler->focus[i]->sti < min_sti) {
                min_sti = global_scheduler->focus[i]->sti;
                min_index = i;
            }
        }
        
        if (atom->sti > min_sti) {
            global_scheduler->focus[min_index] = atom;
        }
    }
    
    unlock(global_scheduler);
    
    return 0;
}

/**
 * Spread attention from a set of source atoms to their neighbors
 * Implements the ECAN attention spreading mechanism
 * 
 * @param sources: Array of source atoms
 * @param count: Number of source atoms
 * @return: 0 on success, -1 on error
 */
int
attention_spread(Atom **sources, int count)
{
    int i, j;
    float spread_amount;
    
    if (sources == nil || count <= 0)
        return -1;
    
    lock(global_scheduler);
    
    /* Spread attention to outgoing atoms */
    for (i = 0; i < count; i++) {
        if (sources[i] == nil || sources[i]->outgoing == nil)
            continue;
        
        /* Calculate amount to spread */
        spread_amount = sources[i]->sti * ATTENTION_SPREAD_RATE;
        
        /* Spread to each outgoing atom */
        for (j = 0; j < sources[i]->outgoing_count; j++) {
            if (sources[i]->outgoing[j] != nil) {
                sources[i]->outgoing[j]->sti += spread_amount;
                
                /* Ensure STI stays within bounds */
                if (sources[i]->outgoing[j]->sti > 1000.0)
                    sources[i]->outgoing[j]->sti = 1000.0;
            }
        }
        
        /* Decay source atom's STI */
        sources[i]->sti *= (1.0 - ATTENTION_SPREAD_RATE);
    }
    
    unlock(global_scheduler);
    
    return 0;
}

/**
 * Perform attention decay on all atoms
 * Called periodically by the kernel scheduler
 */
void
attention_decay(void)
{
    int i;
    
    lock(global_scheduler);
    
    /* Decay STI for all atoms in attentional focus */
    for (i = 0; i < global_scheduler->focus_size; i++) {
        if (global_scheduler->focus[i] != nil) {
            global_scheduler->focus[i]->sti *= (1.0 - ATTENTION_DECAY_RATE);
            
            /* Remove from focus if STI drops too low */
            if (global_scheduler->focus[i]->sti < 1.0) {
                /* Shift remaining atoms down */
                memmove(&global_scheduler->focus[i], 
                        &global_scheduler->focus[i+1],
                        (global_scheduler->focus_size - i - 1) * sizeof(Atom*));
                global_scheduler->focus_size--;
                i--;  /* Re-check this position */
            }
        }
    }
    
    unlock(global_scheduler);
}

/**
 * Get the current attentional focus
 * 
 * @param focus_out: Output array for focus atoms
 * @param max_size: Maximum size of output array
 * @return: Number of atoms in focus
 */
int
attention_get_focus(Atom **focus_out, int max_size)
{
    int i, count;
    
    if (focus_out == nil || max_size <= 0)
        return 0;
    
    lock(global_scheduler);
    
    count = global_scheduler->focus_size < max_size ? 
            global_scheduler->focus_size : max_size;
    
    for (i = 0; i < count; i++) {
        focus_out[i] = global_scheduler->focus[i];
    }
    
    unlock(global_scheduler);
    
    return count;
}

/**
 * System call: attention_allocate
 * User-space interface to attention_allocate()
 */
long
sys_attention_allocate(ulong *arg)
{
    Atom **atoms;
    int count;
    float amount;
    
    atoms = (Atom**)arg[0];
    count = (int)arg[1];
    amount = *(float*)&arg[2];
    
    return attention_allocate(atoms, count, amount);
}

/**
 * System call: attention_focus
 * User-space interface to attention_focus()
 */
long
sys_attention_focus(ulong *arg)
{
    Atom *atom;
    
    atom = (Atom*)arg[0];
    
    return attention_focus(atom);
}

/**
 * System call: attention_spread
 * User-space interface to attention_spread()
 */
long
sys_attention_spread(ulong *arg)
{
    Atom **sources;
    int count;
    
    sources = (Atom**)arg[0];
    count = (int)arg[1];
    
    return attention_spread(sources, count);
}

/**
 * Kernel scheduler hook: called periodically to update attention
 */
void
attention_scheduler_tick(void)
{
    uvlong now;
    
    now = fastticks(nil);
    
    /* Update attention every 100ms */
    if (now - global_scheduler->last_update > HZ/10) {
        attention_decay();
        global_scheduler->last_update = now;
    }
}
