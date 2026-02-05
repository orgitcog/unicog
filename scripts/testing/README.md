# Testing Scripts

This directory contains test scripts for validating the OpenCog Unified system across all integration phases.

## Phase Test Scripts

### Phase II - Logic Systems
- `test-phase-ii.sh` - Basic Phase II tests
- `test-phase-ii-logic-systems.sh` - Logic system validation
- `test-phase-ii-comprehensive.sh` - Comprehensive Phase II testing

### Phase III - Cognitive Systems
- `test-phase-iii-attention.sh` - Attention (ECAN) system tests
- `test-phase-iii-spacetime.sh` - Spacetime reasoning tests
- `test-phase-iii-cognitive-integration.sh` - Cognitive integration tests
- `test-phase-iii-validation.sh` - Comprehensive Phase III validation

### Phase IV - Advanced Systems
- `test-phase-iv-pln.sh` - Probabilistic Logic Networks tests
- `test-phase-iv-v-structure.sh` - Phase IV/V structure validation
- `test-phase-iv-comprehensive.sh` - Comprehensive Phase IV testing

### Phase V - Language & Integration
- `test-phase-v-functionality.sh` - Phase V functionality tests
- `test-phase-v-comprehensive.sh` - Comprehensive Phase V testing
- `test-phase-5-meta-cognition.sh` - Meta-cognitive capabilities

### Phase VI - Complete System
- `test-phase-vi-comprehensive.sh` - Full system integration tests

## Component-Specific Tests

- `test_atomspace_rocks_integration.py` - RocksDB persistence tests
- `test_attention_fixes.sh` - ECAN attention fixes validation
- `test_core_self_awareness.py` - Self-awareness framework tests
- `test_multi_agent_framework.py` - Multi-agent coordination tests
- `test_placeholder_fixes.sh` - Placeholder resolution validation

## Specialized Tests

- `test-ggml-tensor-kernels.sh` - GGML tensor kernel tests
- `test-neural-symbolic-integration.sh` - Neural-symbolic bridge tests
- `test-documentation-framework.sh` - Documentation system tests
- `test_implementations.py` - General implementation validation
- `verify_implementations.py` - Implementation verification

## Manual Tests

- `test_rocks_manual.scm` - Manual Scheme tests for RocksDB integration

## Comprehensive Testing

- `comprehensive-integration-test.sh` - Complete system integration testing

## Usage

Run phase tests in sequence to validate progressive integration. Most scripts should be run from the repository root.
