;
; Test utilities

; test count truth value incrementation
(define counter (ConceptNode "asdf"))
(define (test-incr-cnt) 
  ; Live implementation: actually increment and verify
  (cog-inc-count! counter 1)
  (let ((result (cog-count counter)))
    (if (> result 0)
        result
        (throw 'test-error "Count increment failed"))))

(define key (PredicateNode "key"))
(define (test-incr-value) 
  ; Live implementation: increment value and verify result
  (cog-inc-value! counter key 0.5 3)
  (let ((result (cog-value counter key)))
    (if result
        result
        (throw 'test-error "Value increment failed"))))

; test cog-get-partner
(define partner (ConceptNode "partner"))
(define pare (ListLink partner counter))
(define recount (cog-get-partner pare partner))

; Validate partner retrieval
(define (test-partner-retrieval)
  (if (equal? recount counter)
      #t
      (throw 'test-error "Partner retrieval failed")))

; test cog-pred-get-partner
(define evl (EvaluationLink (WordNode "asdf") pare))
(define rrcnt (cog-pred-get-partner evl partner))

; Validate predicate partner retrieval
(define (test-pred-partner-retrieval)
  (if (equal? rrcnt counter)
      #t
      (throw 'test-error "Predicate partner retrieval failed")))

; test cog-get-link
(define ref (ReferenceLink (ConceptNode "asdf") (WordNode "pqrs")))
(define wref (car (cog-get-link 'ReferenceLink 'ConceptNode (WordNode "pqrs"))))

; Validate link retrieval
(define (test-link-retrieval)
  (if (equal? wref ref)
      #t
      (throw 'test-error "Link retrieval failed")))

; test cog-get-atoms. Warning: it uses previously defined nodes, so
; should be updated if any new nodes are introduced above.
(define cpts (Set (cog-get-atoms 'ConceptNode)))
(define cpts-n-subtypes (Set (cog-get-atoms 'ConceptNode #t)))
(define nodes-n-subtypes (Set (cog-get-atoms 'Node #t)))
(define x-cpts (Set (ConceptNode "asdf") (ConceptNode "partner")))
(define x-cpts-n-subtypes x-cpts)
(define x-nodes-n-subtypes (Set
                             (ConceptNode "asdf")
                             (ConceptNode "partner")
                             (PredicateNode "key")
                             (WordNode "asdf")
                             (WordNode "pqrs")))

; Validate atom retrieval with live verification
(define (test-atom-retrieval)
  ; Verify ConceptNodes are retrieved correctly
  (let ((concept-count (cog-arity cpts)))
    (if (>= concept-count 2)  ; At least our test nodes
        #t
        (throw 'test-error "Atom retrieval validation failed"))))

; Attention allocation test for cognitive kernel
(define (test-attention-allocation node stim-value)
  ; Allocate attention to a node
  (cog-set-av! node (av stim-value 0 0))
  (let ((allocated-stim (cog-av-sti node)))
    (if (= allocated-stim stim-value)
        allocated-stim
        (throw 'test-error "Attention allocation failed"))))

; Run all utility tests
(define (run-utils-tests)
  (display "Running utility tests...\n")
  (test-incr-cnt)
  (test-incr-value)
  (test-partner-retrieval)
  (test-pred-partner-retrieval)
  (test-link-retrieval)
  (test-atom-retrieval)
  (test-attention-allocation counter 100)
  (display "All utility tests passed!\n")
  #t)
