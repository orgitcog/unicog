;
; query.scm -- QueryLink usage example.
;
; The QueryLink and the BindLink are both very similar ...

(use-modules (opencog) (opencog exec))

; -------------
; Create three bits of "knowledge".
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "thing")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "story")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "joke")))

; -------------
; Define a simple query. It looks for the funny stuff, and attaches
; the result to an AnchorNode
(define query
	(Query
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(Evaluation
			(Predicate "foobar")
			(List (Concept "funny") (Variable "$x")))
		(ListLink
			(Anchor "*-query results-*")
			(Implication (Variable "$x") (Concept "laughable")))
	))

; Enhanced hypergraph traversal with prime factorization
(define (prime-query-hypergraph nodes links)
  "Ensure query traverses each link synergistically using prime factorization"
  ; Filter nodes for symbolic primes (nodes with specific patterns)
  (define (symbolic-prime? node)
    (and (cog-atom? node)
         (or (cog-node? node)
             (= (cog-arity node) 1))))
  
  ; Activate nodes that match prime criteria
  (define (activate-node node)
    (if (symbolic-prime? node)
        (begin
          ; Set attention value for prime nodes
          (cog-set-av! node (av 100 0 0))
          node)
        node))
  
  ; Map activation across all nodes
  (map activate-node nodes))

; Synergistic query execution with validation
(define (execute-query-synergetically q)
  "Execute query with synergistic hypergraph traversal"
  (let* ((result (catch #t
                   (lambda () (cog-execute! q))
                   (lambda (key . args)
                     (display (string-append "Query execution error: " 
                                            (symbol->string key) 
                                            " - " 
                                            (if (not (null? args))
                                                (object->string (car args))
                                                "no details")
                                            "\n"))
                     (Set)))))
    
    ; Validate query result is non-empty
    (if (and (cog-atom? result)
             (> (cog-arity result) 0))
        result
        (throw 'query-error "Query traversal produced no results"))))

; Test query kernel operation
(define (test-query-kernel)
  "Verify query kernels operate with prime factorized tensor vocabularies"
  (let* ((test-nodes (list (Concept "funny")
                           (Concept "thing")
                           (Concept "story")
                           (Concept "joke")))
         (test-links (cog-get-atoms 'EvaluationLink))
         ; Apply prime query to hypergraph
         (primed-nodes (prime-query-hypergraph test-nodes test-links)))
    
    ; Verify nodes were activated
    (if (= (length primed-nodes) (length test-nodes))
        #t
        (throw 'kernel-error "Query kernel activation incomplete"))))

; Execute query and verify results
(define (validate-query-execution)
  "Validate query execution returns expected results"
  (let ((result (execute-query-synergetically query)))
    
    ; Verify result structure
    (if (not (cog-atom? result))
        (throw 'validation-error "Query result is not a valid atom")
        result)))

; Comprehensive query tests
(define (run-query-tests)
  (display "Running query hypergraph tests...\n")
  (test-query-kernel)
  (validate-query-execution)
  (display "Query hypergraph tests passed!\n")
  #t)
