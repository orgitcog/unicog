# OpenCog Pattern Miner

## Overview

The Pattern Miner is an advanced component of the OpenCog Unified framework designed to discover and extract meaningful patterns from large knowledge graphs stored in the AtomSpace. It implements sophisticated algorithms for unsupervised pattern mining, enabling the system to identify recurring structures and relationships that may represent important conceptual patterns.

## Features

- **Unsupervised Pattern Discovery**: Automatically identifies frequent and interesting patterns without predefined templates
- **Scalable Mining**: Efficiently processes large-scale knowledge graphs using optimized algorithms
- **Surprisingness Metrics**: Evaluates patterns based on statistical significance and information-theoretic measures
- **Integration with URE**: Seamlessly integrates with the Unified Rule Engine for pattern-based reasoning
- **Configurable Parameters**: Fine-tune mining behavior through comprehensive configuration options

## Architecture

The Pattern Miner operates through several key components:

1. **Pattern Extractor**: Traverses the AtomSpace to identify candidate patterns
2. **Frequency Counter**: Efficiently counts pattern occurrences using specialized data structures
3. **Surprisingness Evaluator**: Computes statistical measures to rank pattern interestingness
4. **Pattern Specializer**: Generates specialized versions of abstract patterns
5. **Result Manager**: Stores and organizes discovered patterns for downstream processing

## Dependencies

- **cogutil**: Core utilities and data structures
- **atomspace**: Knowledge representation and storage
- **ure**: Unified Rule Engine for pattern-based inference

## Building

The Pattern Miner is built as part of the OpenCog Unified monorepo:

```bash
cd /path/to/opencog-unified
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --target miner
```

## Usage

### Basic Pattern Mining

```scheme
(use-modules (opencog) (opencog miner))

; Configure mining parameters
(cog-miner-configure
  #:minimum-support 5
  #:maximum-iterations 100
  #:surprisingness-measure 'nisurp)

; Run pattern mining on current AtomSpace
(cog-mine)

; Retrieve discovered patterns
(cog-miner-results)
```

### Advanced Configuration

```scheme
; Set specific pattern constraints
(cog-miner-set-constraint
  #:maximum-pattern-size 4
  #:minimum-frequency 0.01
  #:enable-specialization #t)

; Mine with custom initial patterns
(cog-mine-with-seeds
  (List
    (Inheritance (Variable "$X") (Concept "Animal"))
    (Evaluation (Predicate "has-property") (List (Variable "$X") (Variable "$Y")))))
```

## Pattern Evaluation Metrics

The miner supports several metrics for evaluating pattern interestingness:

- **Frequency**: Raw occurrence count
- **Support**: Normalized frequency relative to dataset size
- **Nisurp (Normalized I-Surprisingness)**: Information-theoretic measure of unexpectedness
- **Jsurp (Jensen-Shannon Surprisingness)**: Divergence-based surprisingness metric

## Integration with Cognitive Architecture

The Pattern Miner plays a crucial role in the cognitive architecture:

1. **Knowledge Compression**: Identifies recurring patterns to create more compact representations
2. **Concept Formation**: Discovered patterns can seed new concept definitions
3. **Inference Acceleration**: Frequent patterns become reusable inference templates
4. **Attention Allocation**: Surprising patterns attract cognitive resources

## Performance Considerations

- **Memory Usage**: Pattern mining can be memory-intensive for large AtomSpaces
- **Computational Complexity**: Mining time scales with AtomSpace size and pattern complexity
- **Optimization Strategies**: Use minimum support thresholds to prune search space

## Testing

Run the test suite:

```bash
cd build/miner
ctest --output-on-failure
```

## References

- Nil Geisweiller, Hedra Seid, "Pattern Mining for the OpenCog AtomSpace", AGI-2019
- OpenCog Pattern Miner Documentation: https://wiki.opencog.org/w/Pattern_Miner

## Contributing

Contributions are welcome! Please ensure:

- Code follows OpenCog style guidelines
- New features include comprehensive tests
- Documentation is updated accordingly
- Performance implications are considered

## License

This component is part of OpenCog Unified and is licensed under the AGPL-3.0 license.

## Contact

- GitHub Issues: https://github.com/OzCog/opencog-unified/issues
- Mailing List: opencog@googlegroups.com
- Wiki: https://wiki.opencog.org
