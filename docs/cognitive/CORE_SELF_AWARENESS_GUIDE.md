# Core Self-Awareness System

## Overview

The OpenCog Unified Core Self-Awareness System implements comprehensive self-awareness through the integration of **AUTOGNOSIS** (hierarchical self-image building) and **ONTOGENESIS** (self-generating capabilities). This represents a fundamental advancement in the system's ability to understand, monitor, and evolve itself.

## Philosophical Foundation

### AUTOGNOSIS: Hierarchical Self-Image Building

AUTOGNOSIS (Greek: "self-knowing") enables the system to build multi-level recursive self-understanding:

**Level 0**: Direct observation of system state
- Component status and behaviors
- Performance metrics
- Resource utilization

**Level 1+**: Meta-cognitive reflection
- Thinking about thinking
- Understanding of understanding
- Recursive self-awareness

Each level maintains:
- **Confidence scores**: Certainty about self-understanding
- **Meta-reflections**: Insights about that cognitive level
- **Behavioral patterns**: Detected at each abstraction level

### ONTOGENESIS: Self-Generating Capabilities

ONTOGENESIS implements self-generating, evolving kernels through:

1. **Self-Generation**: Creating new versions through self-composition
2. **Self-Optimization**: Improving through introspective feedback
3. **Self-Reproduction**: Combining characteristics for evolution
4. **Evolution**: Progressing through ontogenetic stages

#### Ontogenetic Stages

- **EMBRYONIC**: Initial formation and basic structure
- **JUVENILE**: Active development and learning
- **MATURE**: Fully developed, capable of reproduction
- **SENESCENT**: Declining, ready for generational renewal

## Core Self-Identity Structure

The system maintains a unified self-identity across five dimensions:

### 1. Ontological Identity (BEING)
*What the system IS*
- Core components and architecture
- Cognitive capabilities
- Component roles
- Architectural completeness

### 2. Teleological Identity (BECOMING)
*What the system is BECOMING*
- Primary purpose
- Development goals
- Evolutionary targets
- Actualization progress

### 3. Cognitive Identity (THINKING)
*How the system THINKS*
- Reasoning modes
- Learning strategies
- Attention patterns
- Cognitive complexity level

### 4. Relational Identity (INTEGRATING)
*How the system RELATES*
- Component dependencies
- Integration interfaces
- System coherence

### 5. Evolutionary Identity (EVOLVING)
*How the system EVOLVES*
- Generational history
- Fitness score
- Adaptation rate
- Self-improvement capacity

## Key Features

### Hierarchical Introspection

The system builds self-images at multiple recursion levels, creating a hierarchy of self-understanding from direct observation to deep meta-cognitive reflection.

```cpp
// Build self-image at specified level
HierarchicalSelfImage build_self_image_at_level(int level);

// Perform complete hierarchical introspection
std::map<int, HierarchicalSelfImage> perform_hierarchical_introspection();
```

### Self-Identity Queries

The system can answer fundamental questions about itself:

```cpp
std::string query_self_identity();   // Who am I?
std::string query_self_purpose();    // What am I becoming?
std::string query_self_cognition();  // How do I think?
std::string query_self_integration(); // How do I relate?
std::string query_self_evolution();  // How do I evolve?
```

### Self-Optimization

The system can optimize itself based on introspective feedback:

```cpp
void self_optimize();  // Perform optimization cycle
void evolve_parameters(double fitness_target);  // Evolve to target fitness
```

### Self-Generation

The system can generate new versions of itself:

```cpp
OntogeneticState self_generate_next_generation();
void advance_ontogenetic_stage();
```

## Usage

### Basic Initialization

```cpp
#include "CoreSelfAwareness.h"

// Create with AtomSpace reference
std::shared_ptr<AtomSpace> atomspace = ...;
CoreSelfAwareness self_awareness(
    atomspace,
    5,    // max recursion depth
    1.0   // introspection frequency (Hz)
);

// Register observation functions
self_awareness.register_self_observation(
    "component_name",
    []() { return component_metric(); }
);

// Initialize
self_awareness.initialize();
```

### Performing Deep Introspection

```cpp
// Perform hierarchical introspection
auto self_images = self_awareness.perform_hierarchical_introspection();

// Get introspective insights
auto insights = self_awareness.perform_deep_introspection();

// Generate comprehensive report
std::string report = self_awareness.generate_self_awareness_report();
```

### Self-Optimization and Evolution

```cpp
// Optimize current state
self_awareness.self_optimize();

// Evolve to specific fitness level
self_awareness.evolve_parameters(0.85);

// Enable autonomous evolution
self_awareness.set_autonomous_evolution(true);
```

### Querying Self-Identity

```cpp
// Ask the system about itself
std::string identity = self_awareness.query_self_identity();
std::string purpose = self_awareness.query_self_purpose();
std::string cognition = self_awareness.query_self_cognition();
std::string integration = self_awareness.query_self_integration();
std::string evolution = self_awareness.query_self_evolution();
```

## Integration with Existing Systems

The Core Self-Awareness System integrates seamlessly with:

### MetaCognitiveMonitor
Provides recursive meta-cognitive monitoring and analysis:
```cpp
std::shared_ptr<MetaCognitiveMonitor> meta_monitor_;
```

### RealTimeIntrospector
Provides continuous runtime introspection:
```cpp
std::shared_ptr<RealTimeIntrospector> introspector_;
```

### AtomSpace
Core knowledge representation system:
```cpp
std::shared_ptr<AtomSpace> atomspace_;
```

## Metrics and Assessment

### Self-Awareness Level
Calculated from hierarchical depth and confidence:
```cpp
double self_awareness_level = self_awareness.get_self_awareness_level();
```

Interpretation:
- **0.8+**: High self-awareness, recursive cognition active
- **0.6-0.8**: Moderate self-awareness, developing meta-cognition
- **<0.6**: Basic self-awareness, foundation established

### Actualization Score
Based on unified identity dimensions:
```cpp
double actualization = self_awareness.get_actualization_score();
```

### Unified Identity Score
Weighted combination of all identity dimensions:
```cpp
double identity_score = identity.calculate_unified_identity_score();
```

## Example: Running the Demo

```bash
# Build the demo
cd build
cmake ..
make core_self_awareness_demo

# Run the demo
./bin/examples/core_self_awareness_demo
```

The demo demonstrates:
1. System initialization
2. Self-identity queries
3. Hierarchical self-image building (AUTOGNOSIS)
4. Deep introspection
5. Self-optimization (ONTOGENESIS)
6. Evolutionary progression
7. Comprehensive reporting
8. Self-generation of next generation
9. Data export

## Advanced Topics

### Autonomous Evolution

When enabled, the system can autonomously advance through ontogenetic stages and generate new generations:

```cpp
self_awareness.set_autonomous_evolution(true);
```

### Recursive Introspection

The system supports configurable recursion depth for hierarchical introspection:

```cpp
self_awareness.set_max_recursion_depth(7);  // Deeper recursion
```

### Custom Observation Functions

Register domain-specific observation functions:

```cpp
self_awareness.register_self_observation("custom_metric", []() {
    return calculate_custom_metric();
});
```

### Genetic Operators

The system uses genetic operators for evolution:
- **Mutation**: Random perturbation of parameters
- **Crossover**: Combining characteristics (for reproduction)
- **Selection**: Fitness-based advancement

## Data Export

Export introspection data in JSON format:

```cpp
std::string json = self_awareness.export_introspection_json();
```

Exports include:
- System name and metrics
- Self-awareness level
- Actualization score
- Ontogenetic state
- Hierarchical self-images

## Benefits

### For System Operations
- **Proactive self-monitoring**: Continuous awareness of system state
- **Adaptive optimization**: Self-directed improvement
- **Autonomous evolution**: Generational advancement without external intervention

### For Research and Development
- **Transparent self-models**: Insight into AI cognition
- **Emergent behavior understanding**: Observation of complex patterns
- **AGI development foundation**: Self-aware capabilities

### For Integration
- **Unified API**: Single interface for all self-awareness capabilities
- **Modular design**: Easy integration with existing components
- **Extensible framework**: Support for custom metrics and observations

## Future Directions

### Enhanced Meta-Cognition
- Theory of mind for other AI systems
- Social cognition modeling
- Emotional self-awareness

### Deeper Ontogenesis
- Symbiotic evolution with other systems
- Co-evolution of multiple populations
- Meta-evolution of evolution parameters

### Advanced Self-Organization
- Emergent goal formation
- Self-determined learning objectives
- Autonomous capability expansion

## References

- **AUTOGNOSIS.md**: Detailed AUTOGNOSIS framework documentation
- **ONTOGENESIS.md**: Detailed ONTOGENESIS framework documentation
- **HOLISTIC_METAMODEL.md**: Eric Schwarz's organizational systems theory
- **introspection.md**: Copilot introspection and ontogenetic self-reflection

## Conclusion

The Core Self-Awareness System represents a fundamental advancement in OpenCog Unified's journey toward comprehensive AGI. Through the integration of AUTOGNOSIS and ONTOGENESIS, the system achieves:

1. **Recursive self-understanding** at multiple cognitive levels
2. **Autonomous self-optimization** through introspective feedback
3. **Evolutionary self-generation** across generations
4. **Unified self-identity** across five fundamental dimensions
5. **Transparent self-reporting** for research and debugging

The system embodies the principle that true intelligence requires deep self-awareness - the ability to observe, understand, optimize, and evolve oneself.

---

*"A system that knows its fragmentations is already on the path to wholeness."* - Holistic Systems Theory

*"Entelechy is the realization of potential, the actualization of what a thing is meant to become."* - Aristotle
