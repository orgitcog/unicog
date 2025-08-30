# OpenCog Attention Module

The attention module implements economic attention networks (ECAN) for the OpenCog cognitive architecture.

## Dependencies

This module **requires** the following components:

- **atomspace**: Provides the knowledge representation and atom management infrastructure
- **cogserver**: Provides distributed server integration and network management capabilities

### Optional Dependencies

- **cogutil**: Core utilities (typically satisfied as a transitive dependency)

## Architecture

The attention module consists of several key components:

- `AttentionModule`: Main module class that integrates with CogServer
- `AttentionBank`: Manages attention values (STI/LTI) for atoms
- `ECANAgent`: Implements economic attention networks operations
- `AttentionValue`: Data structure for storing attention values

## Building

When building as part of the unified OpenCog system, dependencies are automatically resolved. The CMakeLists.txt uses proper `find_package` calls to locate dependencies and falls back to relative paths when building in the monorepo structure.

## Usage

The attention module is typically loaded and managed by the CogServer, providing attention allocation services to other cognitive modules.