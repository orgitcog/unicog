;;; Level 0: Object-Level Cognitive Processing
;;; Direct interaction with cognitive content

(define (object-level-perceive input)
  "Process perceptual input at object level"
  (let ((atoms (parse-perceptual-input input)))
    (map (lambda (atom)
           (add-to-atomspace atom)
           (allocate-initial-attention atom))
         atoms)))

(define (object-level-reason context)
  "Perform reasoning at object level"
  (let ((relevant-atoms (query-atomspace context)))
    (apply-inference-rules relevant-atoms)
    (generate-conclusions relevant-atoms)))

(define (object-level-act plan)
  "Execute actions at object level"
  (let ((action-sequence (decompose-plan plan)))
    (map execute-primitive-action action-sequence)))

;; Self-reference: Object level knows it exists
(define object-level-identity
  (ConceptNode "ObjectLevel"))

(InheritanceLink
  (ConceptNode "self")
  object-level-identity)
