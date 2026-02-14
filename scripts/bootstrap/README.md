# Bootstrap and Integration Scripts

This directory contains scripts for bootstrapping the development environment and integrating OpenCog components.

## Main Scripts

### integrate-components.sh
**Primary integration script** for cloning and integrating OpenCog components into the unified monorepo.

Usage:
```bash
./integrate-components.sh all         # Integrate all components
./integrate-components.sh 1           # Integrate Phase 1 only
./integrate-components.sh 2           # Integrate Phase 2 only
./integrate-components.sh all build   # Integrate and build
```

**Note**: A symlink exists in the repository root for convenience.

### init-unified-repo.sh
Initialize a new unified OpenCog repository with all necessary configuration.

### init-cognitive-repo.sh
Initialize cognitive-specific repository configuration.

### bootstrap-unified-cognitive.sh
Bootstrap the complete unified cognitive architecture development environment.

### bootstrap-unified-cognitive_Version2.sh
Alternative bootstrap script with enhanced features.

## Integration Phases

Refer to `DEVELOPMENT-ROADMAP.md` in the repository root for details on the 5-phase integration plan.

## Usage

Run these scripts from the repository root. They set up the development environment and integrate components according to the phase-based roadmap.
