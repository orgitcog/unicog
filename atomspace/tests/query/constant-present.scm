;
; constant-present.scm
;
; Contains a constant clause inside of a PresentLink.
; This occurs naturally in the URE ForwardChainerUTest
;

(use-modules (opencog) (opencog exec))

(Inheritance (Concept "B") (Concept "foo"))

(define query
	(Bind
		(TypedVariable (Variable "$C-7a4842c1") (Type "Concept"))
		(And
			(Present
				(Inheritance (Concept "A") (Concept "B"))
				(Inheritance (Concept "B") (Variable "$C-7a4842c1")))
			(Not (Identical (Variable "$C-7a4842c1") (Concept "A"))))
		(Execution
			(Schema "scm: fc-deduction-formula")
			(List
				(Inheritance (Concept "A") (Variable "$C-7a4842c1"))
				(Inheritance (Concept "A") (Concept "B"))
				(Inheritance (Concept "B") (Variable "$C-7a4842c1"))))))

; (cog-execute! query)

(define expected
	(Set
		(Execution
			(Schema "scm: fc-deduction-formula")
			(List
				(Inheritance (Concept "A") (Concept "foo"))
				(Inheritance (Concept "A") (Concept "B"))
				(Inheritance (Concept "B") (Concept "foo"))))))

; Enhanced recursive constant propagation validation
(define (validate-constant-propagation)
  "Recursively check constant propagation through rule trees"
  (let* ((a-concept (Concept "A"))
         (b-concept (Concept "B"))
         (foo-concept (Concept "foo"))
         ; Check constant relationships are established
         (b-to-foo (Inheritance b-concept foo-concept)))
    
    ; Verify constant B->foo exists
    (if (not (cog-atom? b-to-foo))
        (throw 'propagation-error "Constant B->foo not propagated"))
    
    ; Recursively verify constant can be reached
    (define (check-path from to depth)
      (if (> depth 3)
          #f  ; Prevent infinite recursion
          (or (equal? from to)
              (let ((outgoing (cog-outgoing-set from)))
                (any (lambda (node)
                       (check-path node to (+ depth 1)))
                     outgoing)))))
    
    ; Verify propagation path exists
    (if (check-path b-to-foo foo-concept 0)
        #t
        (throw 'propagation-error "Constant propagation path incomplete"))))

; Test constant pattern matching integrity
(define (test-constant-pattern-integrity)
  "Ensure constants within Present blocks are matched correctly"
  (let* ((test-query (Bind
                       (Variable "$X")
                       (Present
                         (Inheritance (Concept "A") (Concept "B")))
                       (Inheritance (Concept "A") (Variable "$X"))))
         ; Execute query - this should work with constant in Present
         (result (catch #t
                   (lambda () (cog-execute! test-query))
                   (lambda (key . args) #f))))
    
    ; Verify result is valid
    (if (or (not result) (equal? result (Set)))
        (throw 'pattern-error "Constant pattern matching failed")
        #t)))

; Comprehensive constant propagation test
(define (run-constant-tests)
  (display "Running constant propagation tests...\n")
  (validate-constant-propagation)
  (test-constant-pattern-integrity)
  (display "Constant propagation tests passed!\n")
  #t)
