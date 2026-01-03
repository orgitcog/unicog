;;
;; Scheme Test Template for OpenCog Unified Components
;;
;; This template provides a standardized structure for creating Scheme unit tests
;; for OpenCog components that have Guile/Scheme bindings.
;;
;; Usage:
;;   1. Copy this file to your component's tests/scm/ directory
;;   2. Rename to your-component-test.scm
;;   3. Replace placeholders with your component-specific code
;;   4. Run with: guile -l your-component-test.scm
;;
;; Copyright (C) 2025-2026 OpenCog Foundation
;; SPDX-License-Identifier: AGPL-3.0-or-later
;;

;; Load required modules
(use-modules (opencog) (opencog exec))
;; (use-modules (opencog your-component))  ; Uncomment when available

;; Test utilities
(define test-count 0)
(define test-passed 0)
(define test-failed 0)

(define (test-header name)
  "Print a test header."
  (display "=== Test: ")
  (display name)
  (display " ===\n"))

(define (test-assert name condition)
  "Assert that condition is true."
  (set! test-count (+ test-count 1))
  (if condition
      (begin
        (set! test-passed (+ test-passed 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! test-failed (+ test-failed 1))
        (display "  FAIL: ")
        (display name)
        (newline))))

(define (test-equal name expected actual)
  "Assert that expected equals actual."
  (test-assert name (equal? expected actual)))

(define (test-not-null name value)
  "Assert that value is not null."
  (test-assert name (not (null? value))))

(define (test-summary)
  "Print test summary."
  (newline)
  (display "================================\n")
  (display "Test Summary:\n")
  (display "  Total:  ") (display test-count) (newline)
  (display "  Passed: ") (display test-passed) (newline)
  (display "  Failed: ") (display test-failed) (newline)
  (display "================================\n")
  (if (= test-failed 0)
      (display "All tests passed!\n")
      (display "Some tests failed!\n")))

;; -----------------
;; Actual Tests
;; -----------------

(define (test-atomspace-creation)
  "Test basic AtomSpace operations."
  (test-header "AtomSpace Creation")

  ;; Create an AtomSpace
  (define as (cog-new-atomspace))
  (test-not-null "AtomSpace created" as)

  ;; Create some nodes
  (define concept (Concept "test-concept"))
  (test-not-null "ConceptNode created" concept)

  ;; Create a link
  (define link (List concept (Concept "another")))
  (test-not-null "ListLink created" link))

(define (test-truth-values)
  "Test TruthValue operations."
  (test-header "TruthValue Operations")

  ;; Create a node with truth value
  (define node (Concept "tv-test" (stv 0.8 0.9)))
  (test-not-null "Node with TV created" node)

  ;; Get and check truth value
  (define tv (cog-tv node))
  (test-not-null "TruthValue retrieved" tv)

  ;; Check TV components (approximately)
  (define strength (cog-tv-mean tv))
  (test-assert "Strength correct" (< (abs (- strength 0.8)) 0.01))

  (define confidence (cog-tv-confidence tv))
  (test-assert "Confidence correct" (< (abs (- confidence 0.9)) 0.01)))

(define (test-pattern-matching)
  "Test pattern matching operations."
  (test-header "Pattern Matching")

  ;; Create some atoms for matching
  (Inheritance (Concept "cat") (Concept "animal"))
  (Inheritance (Concept "dog") (Concept "animal"))
  (Inheritance (Concept "bird") (Concept "animal"))

  ;; Create a pattern to match
  (define pattern
    (Get
      (TypedVariable (Variable "$X") (Type "ConceptNode"))
      (Inheritance (Variable "$X") (Concept "animal"))))

  ;; Execute the pattern
  (define results (cog-execute! pattern))
  (test-not-null "Pattern results obtained" results)

  ;; Check result count
  (define result-list (cog-outgoing-set results))
  (test-assert "Found 3 animals" (= (length result-list) 3)))

(define (test-your-component)
  "Test YourComponent specific functionality."
  (test-header "YourComponent")

  ;; Placeholder tests - replace with actual component tests
  ;; (define component (your-component-create))
  ;; (test-not-null "Component created" component)
  ;; (define result (your-component-operation component input))
  ;; (test-equal "Operation result correct" expected result)

  (test-assert "Placeholder test" #t))

;; -----------------
;; Run Tests
;; -----------------

(display "\n========================================\n")
(display "OpenCog Unified Component Tests\n")
(display "========================================\n\n")

(test-atomspace-creation)
(test-truth-values)
(test-pattern-matching)
(test-your-component)

(test-summary)

;; Exit with appropriate code
(exit (if (= test-failed 0) 0 1))
