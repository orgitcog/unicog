# Top 3 Immediate Action Items: Cognitive Kernel Engineer

**Date:** December 13, 2025  
**Source:** NEXT_PRIORITIES_IMPLEMENTATION_PLAN.md

This document outlines the top 3 immediate action items for the Cognitive Kernel Engineer role, extracted from the comprehensive implementation plan. These priorities are designed to deliver maximum impact in the first 5 weeks of the project.

---

## Priority 1: Establish Testing Foundation (Weeks 1-2)

### Objective

Create a robust, unified testing framework to ensure the stability and correctness of all kernel-level cognitive components. This is the foundational step for all subsequent development.

### Key Tasks

1.  **Develop Standardized Test Templates:**
    *   Create CxxTest templates for all new C++ kernel modules.
    *   Establish best practices for unit testing cognitive services, focusing on isolation and mock objects.
    *   Document all testing conventions in `TESTING_GUIDE.md`.

2.  **Set Up Code Coverage Infrastructure:**
    *   Configure `gcov/lcov` for C++ code coverage analysis.
    *   Integrate coverage reporting into the GitHub Actions CI/CD pipeline.
    *   Create a workflow (`.github/workflows/coverage-report.yml`) to generate and publish coverage reports on every pull request.
    *   Establish the initial baseline coverage metrics for the existing codebase.

### Success Metrics

*   A `tests/templates/` directory containing reusable CxxTest templates is committed to the repository.
*   A comprehensive `TESTING_GUIDE.md` is available for all developers.
*   Code coverage reports are automatically generated and viewable for all pull requests.

---

## Priority 2: Implement Unit Tests for `ggml-tensor-kernel` (Weeks 3-5)

### Objective

Achieve 60% test coverage for the `ggml-tensor-kernel` component, which is the core computational engine for neural-symbolic integration. This is the highest priority among the 14 critical components with 0% test coverage.

### Key Tasks

1.  **Analyze and Prioritize:**
    *   Analyze the 33 source files in `ggml-tensor-kernel` to identify the most critical functions for testing.
    *   Prioritize testing of core tensor operations (e.g., matrix multiplication, vector operations) and GGML integration points.

2.  **Develop Unit Tests:**
    *   Create a `tests` directory within the `ggml-tensor-kernel` component.
    *   Write comprehensive unit tests using the CxxTest framework to validate the correctness of all core tensor operations.
    *   Implement tests for edge cases, including empty tensors, mismatched dimensions, and numerical stability.

3.  **Integrate and Validate:**
    *   Add the new tests to the CMake build system.
    *   Integrate the tests into the CI/CD pipeline to run automatically.
    *   Verify that the test suite achieves the 60% code coverage target.

### Success Metrics

*   A comprehensive suite of unit tests for `ggml-tensor-kernel` is committed and passing.
*   Code coverage for the component increases from 0% to at least 60%.
*   All tests are integrated into the continuous integration pipeline.

---

## Priority 3: Implement Unit Tests for `attention` Component (Weeks 3-5)

### Objective

Achieve 70% test coverage for the `attention` component, which is critical for the cognitive architecture's resource allocation and reasoning processes. This is the second-highest priority critical component.

### Key Tasks

1.  **Analyze and Prioritize:**
    *   Analyze the 16 source files in the `attention` component.
    *   Prioritize testing of the core Economic Attention Network (ECAN) algorithms, including attention allocation, spreading, and decay functions.

2.  **Develop Unit Tests:**
    *   Create a `tests` directory within the `attention` component.
    *   Write unit tests to validate the correctness of the ECAN implementation, including the mathematical formulas for Short-Term Importance (STI) and Long-Term Importance (LTI).
    *   Implement tests for the dynamic behavior of the attention scheduler, ensuring it correctly prioritizes and de-prioritizes atoms.

3.  **Integrate and Validate:**
    *   Add the new tests to the CMake build system.
    *   Integrate the tests into the CI/CD pipeline.
    *   Verify that the test suite achieves the 70% code coverage target.

### Success Metrics

*   A comprehensive suite of unit tests for the `attention` component is committed and passing.
*   Code coverage for the component increases from 0% to at least 70%.
*   The correctness of the ECAN implementation is validated through automated testing.

---

## Implementation Notes

*   These three priorities are designed to be worked on in parallel where possible. The testing foundation (Priority 1) should be completed first, as it enables the work on Priorities 2 and 3.
*   The Cognitive Kernel Engineer should work closely with the Tools & Testing Team to ensure the testing framework meets the unique needs of kernel-level cognitive components.
*   All code and documentation should be committed to the `main` branch of the `OzCog/opencog-unified` repository upon completion.` repository` repository upon completion.
