# Data Directory

This directory contains generated data files, analysis results, test outputs, and tracking data for the OpenCog Unified system.

## Structure

- **`analysis-results/`**: Analysis reports, metrics, and generated artifacts
- **`entelechy/`**: Entelechy framework tracking data and markers
- **`todo-fixme/`**: TODO/FIXME tracking data and resolution progress
- **`test-results/`**: Test execution results and logs
- **`cognitive-state/`**: Cognitive system state snapshots

## Important Notes

### Git Tracking
Most files in this directory are **generated artifacts** and are excluded from git via `.gitignore`. Only essential tracking files and configuration are committed.

### File Lifecycle
- Analysis results are regenerated on demand
- Test results are ephemeral (generated during CI/testing)
- Tracking data persists to maintain development history
- State snapshots capture cognitive system states at specific points

## Regenerating Data

To regenerate analysis data:
```bash
# Run analysis scripts from repository root
python3 scripts/analysis/analyze_placeholders.py
python3 scripts/analysis/analyze_cpp_fixmes.py
python3 scripts/analysis/dependency_analyzer.py
```

To regenerate test data:
```bash
# Run test suites
./scripts/testing/comprehensive-integration-test.sh
```

## Cleanup

To clean generated data:
```bash
rm -rf data/test-results/*.log data/test-results/*.json
rm -rf data/analysis-results/*.json
```
