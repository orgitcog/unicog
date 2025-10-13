;
; emergent-patterns.scm
;
; Phase II.2: Emergent Pattern Encoding (Hypergraph Synergy)
; Pattern extraction routines for self-reflexive learning
; Enhanced with emergent phenomena observation hooks
;

(use-modules (opencog))
(use-modules (opencog exec))

; Load emergent phenomena observation hooks
(load "../../../documentation/hooks/emergent-phenomena-hooks.scm")

; Define cognitive schema for emergent pattern encoding
(define cognitive-pattern-schema
  '(schema
     (module emergent-patterns (role "hypergraph-synergy-processor"))
     (features "pattern-extraction" "self-reflexive-learning" "recursive-reification")
     (recursive-pathway (detected-patterns reified-links perception-feedback))))

; Emergent pattern detection in hypergraph structures
(define (detect-emergent-patterns hypergraph-structure)
  "Detect recurring and emergent patterns in hypergraph structures"
  (let* ((nodes (cog-get-atoms 'ConceptNode))
         (links (cog-get-atoms 'Link))
         (detected-patterns '()))
    
    ; Find structural similarities between link patterns
    (for-each
      (lambda (link1)
        (for-each
          (lambda (link2)
            (when (and (not (equal? link1 link2))
                      (equal? (cog-type link1) (cog-type link2)))
              (let ((similarity (calculate-hypergraph-similarity link1 link2)))
                (when (> similarity 0.7)
                  (let ((pattern-descriptor (create-pattern-descriptor link1 link2 similarity)))
                    (set! detected-patterns
                          (cons pattern-descriptor detected-patterns))
                    
                    ; OBSERVATION HOOK: Document emergent pattern
                    (observe-pattern-emergence
                      (string-append "pattern-" (number->string (random 10000)))
                      (list (cog-name link1) (cog-name link2))
                      similarity
                      "hypergraph-similarity-detection"))))))
          links))
      links)
    
    detected-patterns))

; Calculate structural similarity between hypergraph components
(define (calculate-hypergraph-similarity link1 link2)
  "Calculate structural similarity between two hypergraph links"
  (let* ((outgoing1 (cog-outgoing-set link1))
         (outgoing2 (cog-outgoing-set link2))
         (size1 (length outgoing1))
         (size2 (length outgoing2)))
    
    (if (= size1 size2)
        ; Count matching structural elements
        (let ((matches 0))
          (for-each
            (lambda (atom1)
              (when (any (lambda (atom2) 
                          (or (equal? atom1 atom2)
                              (equal? (cog-type atom1) (cog-type atom2))))
                        outgoing2)
                (set! matches (+ matches 1))))
            outgoing1)
          (/ matches size1))
        0.0)))

; Create pattern descriptor for detected similarities
(define (create-pattern-descriptor link1 link2 similarity)
  "Create a structured descriptor for a detected pattern"
  (Evaluation
    (Predicate "emergent-pattern")
    (List
      (Concept (string-append "pattern-" 
                             (number->string (random 10000))))
      (List
        (Concept "source-link-1") link1
        (Concept "source-link-2") link2
        (Concept "similarity-score") (Number similarity))
      (Inheritance
        (Concept "pattern-type")
        (Concept (symbol->string (cog-type link1)))))))

; Reify detected patterns as new hypergraph links
(define (reify-patterns-as-links detected-patterns)
  "Reify detected patterns into new hypergraph link structures"
  (let ((reified-patterns
         (map (lambda (pattern)
                (let* ((pattern-id (cog-name (cadr (cog-outgoing-set pattern))))
                       (reified-link
                        (Inheritance
                          (Concept "reified-pattern")
                          (List
                            (Concept pattern-id)
                            pattern
                            ; Recursive self-reference: pattern points to itself
                            (Inheritance
                              (Concept "recursive-pattern-reference")
                              (Variable "$self"))))))
                  
                  ; Add temporal and frequency tracking
                  (Evaluation
                    (Predicate "pattern-reification-time")
                    (List
                      reified-link
                      (Number (current-time))))
                  
                  ; Update pattern frequency
                  (State
                    (Concept (string-append pattern-id "-frequency"))
                    (Number (+ 1 (get-pattern-frequency pattern-id))))
                  
                  ; OBSERVATION HOOK: Document recursive behavior of pattern reification
                  (observe-recursive-behavior
                    (string-append "reification-" pattern-id)
                    "pattern-to-link-conversion"
                    `(("stability-score" . 0.8)
                      ("recursion-depth" . 1.0)
                      ("self-reference-strength" . 0.9)))
                  
                  reified-link))
              detected-patterns)))
    
    ; OBSERVATION HOOK: Document meta-cognitive insight about reification process
    (observe-meta-cognitive-insight
      (format #f "Reified ~a patterns into hypergraph links, enabling recursive pattern detection"
              (length detected-patterns))
      "tactical"
      0.85)
    
    reified-patterns))

; Self-reflexive learning for pattern detection adaptation
(define (self-reflexive-pattern-adaptation detection-history)
  "Adapt pattern detection parameters based on historical performance"
  (let* ((avg-quality (calculate-average-detection-quality detection-history))
         (pattern-diversity (length (cog-get-atoms 'Concept)))
         (current-threshold (get-detection-threshold))
         (new-threshold current-threshold))
    
    ; Adapt threshold based on detection quality
    (cond 
      ((< avg-quality 0.5)
       (set! new-threshold (* current-threshold 0.9)))  ; Lower threshold
      ((> avg-quality 0.8)
       (set! new-threshold (* current-threshold 1.1))))  ; Raise threshold
    
    ; Clamp threshold to reasonable bounds
    (set! new-threshold (max 0.3 (min 0.9 new-threshold)))
    
    ; Update detection threshold
    (State
      (Concept "pattern-detection-threshold")
      (Number new-threshold))
    
    ; OBSERVATION HOOK: Document meta-cognitive insight about self-adaptation
    (observe-meta-cognitive-insight
      (format #f "Self-reflexive adaptation: threshold changed from ~a to ~a based on quality ~a"
              current-threshold new-threshold avg-quality)
      "operational"
      0.9)
    
    ; Self-reflexive feedback with meta-cognitive integration
    (let ((adaptation-result
           (Evaluation
             (Predicate "self-reflexive-adaptation")
             (List
               (Concept "old-threshold") (Number current-threshold)
               (Concept "new-threshold") (Number new-threshold)
               (Concept "avg-quality") (Number avg-quality)
               (Concept "pattern-diversity") (Number pattern-diversity))))
          (meta-cognitive-feedback
           (Evaluation
             (Predicate "meta-cognitive-pattern-adaptation")
             (List
               (Concept "adaptation-effectiveness") (Number (- new-threshold current-threshold))
               (Concept "quality-improvement") (Number (max 0 (- avg-quality 0.5)))
               (Concept "system-awareness") (Number 0.8)))))
      
      ; OBSERVATION HOOK: Document recursive behavior of self-adaptation
      (observe-recursive-behavior
        "self-reflexive-threshold-adaptation"
        "parameter-self-modification"
        `(("stability-score" . ,(abs (- 1.0 (abs (- new-threshold current-threshold)))))
          ("recursion-depth" . 2.0)
          ("adaptation-magnitude" . ,(abs (- new-threshold current-threshold)))))
      
      adaptation-result)))

; Generate feedback signals for perception loop
(define (generate-pattern-feedback reified-patterns)
  "Generate feedback signals to influence future perceptual processing"
  (map (lambda (reified-pattern)
         (let* ((pattern-novelty (calculate-pattern-novelty reified-pattern))
                (pattern-complexity (calculate-pattern-complexity reified-pattern))
                (feedback-strength (* pattern-novelty pattern-complexity)))
           
           ; Create feedback link that influences attention allocation
           (Evaluation
             (Predicate "pattern-feedback")
             (List
               reified-pattern
               (Number feedback-strength)
               ; Recursive pathway: feedback influences future perception
               (Inheritance
                 (Concept "feedback-to-perception")
                 (Concept "attention-allocation-modifier"))))))
       reified-patterns))

; Recursive pattern processing cycle
(define (recursive-pattern-cycle hypergraph-input)
  "Complete recursive cycle: detect → reify → feedback → enhance perception"
  (let* (; Step 1: Detect emergent patterns
         (detected-patterns (detect-emergent-patterns hypergraph-input))
         
         ; Step 2: Reify patterns as new hypergraph links
         (reified-patterns (reify-patterns-as-links detected-patterns))
         
         ; Step 3: Generate feedback for perception loop
         (pattern-feedback (generate-pattern-feedback reified-patterns))
         
         ; Step 4: Create enhanced hypergraph with new patterns
         (enhanced-hypergraph
          (List
            hypergraph-input
            reified-patterns
            pattern-feedback
            ; Recursive self-reference
            (Inheritance
              (Concept "recursive-enhancement")
              (List
                (Concept "previous-cycle")
                (Concept "current-cycle")
                (Concept "next-cycle"))))))
    
    ; Return complete recursive processing result
    (List
      detected-patterns
      reified-patterns
      pattern-feedback
      enhanced-hypergraph)))

; Utility functions
(define (get-pattern-frequency pattern-id)
  "Get current frequency count for a pattern"
  (let ((freq-state (cog-link 'State (Concept (string-append pattern-id "-frequency")))))
    (if freq-state
        (cog-number (cog-outgoing-atom freq-state 1))
        0)))

(define (get-detection-threshold)
  "Get current pattern detection threshold"
  (let ((threshold-state (cog-link 'State (Concept "pattern-detection-threshold"))))
    (if threshold-state
        (cog-number (cog-outgoing-atom threshold-state 1))
        0.7)))

(define (calculate-average-detection-quality history)
  "Calculate average quality score from detection history"
  (if (null? history)
      0.5
      (/ (apply + (map cdr history)) (length history))))

(define (calculate-pattern-novelty pattern)
  "Calculate novelty score based on pattern frequency"
  (let ((frequency (get-pattern-frequency (cog-name pattern))))
    (/ 1.0 (+ 1.0 frequency))))

(define (calculate-pattern-complexity pattern)
  "Calculate complexity score based on pattern structure"
  (let ((outgoing-count (length (cog-outgoing-set pattern)))
        (depth (calculate-pattern-depth pattern)))
    (min 1.0 (/ (+ outgoing-count depth) 20.0))))

(define (calculate-pattern-depth pattern)
  "Calculate structural depth of a pattern"
  (define (depth-helper atom current-depth)
    (if (cog-link? atom)
        (+ 1 (apply max (map (lambda (child) (depth-helper child (+ current-depth 1)))
                            (cog-outgoing-set atom))))
        current-depth))
  (depth-helper pattern 0))

; Demonstration function
(define (demonstrate-emergent-pattern-encoding)
  "Demonstrate the complete emergent pattern encoding system"
  (let* ((sample-hypergraph
          (List
            ; Create some sample structures for pattern detection
            (Inheritance (Concept "A") (Concept "B"))
            (Inheritance (Concept "C") (Concept "D"))
            (Evaluation (Predicate "similar") (List (Concept "A") (Concept "C")))
            (Evaluation (Predicate "similar") (List (Concept "B") (Concept "D")))))
         
         (recursive-result (recursive-pattern-cycle sample-hypergraph)))
    
    (format #t "~%=== Emergent Pattern Encoding Demonstration ===~%")
    (format #t "Input hypergraph nodes: ~a~%" (length (cog-get-atoms 'ConceptNode)))
    (format #t "Detected patterns: ~a~%" (length (car recursive-result)))
    (format #t "Reified patterns: ~a~%" (length (cadr recursive-result)))
    (format #t "Feedback signals: ~a~%" (length (caddr recursive-result)))
    
    recursive-result))

; Export key functions
(export detect-emergent-patterns
        reify-patterns-as-links
        self-reflexive-pattern-adaptation
        recursive-pattern-cycle
        demonstrate-emergent-pattern-encoding)