;
; aprfe-integration.scm
;
; Scheme interface for Adaptive Pattern-Recognition Fusion Engine (APRFE)
; Provides high-level cognitive pattern recognition capabilities
;

(use-modules (opencog))
(use-modules (opencog exec))

; Load emergent phenomena observation hooks
(load "../../../documentation/hooks/emergent-phenomena-hooks.scm")

;; APRFE Scheme Interface Functions

(define (aprfe-recognize-patterns input-atoms target-pattern)
  "High-level Scheme interface for APRFE pattern recognition"
  (let* ((aprfe-instance (get-aprfe-instance))
         (recognition-result (aprfe-recognize aprfe-instance input-atoms target-pattern)))
    
    ; Log pattern recognition for emergent phenomena tracking
    (when (and recognition-result (> (get-accuracy recognition-result) 0.7))
      (observe-pattern-emergence
        (string-append "aprfe-pattern-" (number->string (random 10000)))
        (map cog-name (get-matched-patterns recognition-result))
        (get-accuracy recognition-result)
        "adaptive-fusion-recognition"))
    
    recognition-result))

(define (aprfe-cross-modal-recognition task-description)
  "Cross-modal pattern recognition with full modality integration"
  (let* ((aprfe-instance (get-aprfe-instance))
         (cross-modal-result (aprfe-cross-modal aprfe-instance task-description)))
    
    ; Document cross-modal integration insights
    (when cross-modal-result
      (observe-meta-cognitive-insight
        (format #f "Cross-modal integration for '~a' achieved ~a symbolic, ~a neural, ~a attention contributions"
                task-description
                (get-symbolic-contributions cross-modal-result)
                (get-neural-contributions cross-modal-result)
                (get-attention-contributions cross-modal-result))
        "integrative"
        0.85))
    
    cross-modal-result))

(define (aprfe-adaptive-strategy-selection context-atoms urgency-factor)
  "Select optimal recognition strategy based on current context"
  (let* ((aprfe-instance (get-aprfe-instance))
         (selected-strategy (aprfe-select-strategy aprfe-instance context-atoms urgency-factor)))
    
    ; Log strategy selection for adaptive behavior tracking
    (observe-recursive-behavior
      (string-append "strategy-selection-" (symbol->string selected-strategy))
      "adaptive-strategy-selection"
      `(("context-size" . ,(length context-atoms))
        ("urgency-factor" . ,urgency-factor)
        ("cognitive-load" . ,(aprfe-get-cognitive-load aprfe-instance))))
    
    selected-strategy))

;; Cognitive Load Management

(define (aprfe-update-cognitive-load new-load)
  "Update system cognitive load and trigger adaptation if needed"
  (let ((aprfe-instance (get-aprfe-instance)))
    (aprfe-set-cognitive-load aprfe-instance new-load)
    
    ; Document cognitive load changes
    (when (> (abs (- new-load (aprfe-get-previous-load aprfe-instance))) 0.2)
      (observe-meta-cognitive-insight
        (format #f "Significant cognitive load change: ~a -> ~a"
                (aprfe-get-previous-load aprfe-instance) new-load)
        "operational"
        0.8))
    
    new-load))

(define (aprfe-monitor-performance)
  "Monitor APRFE performance and trigger adaptations"
  (let* ((aprfe-instance (get-aprfe-instance))
         (performance-metrics (aprfe-get-performance-metrics aprfe-instance))
         (accuracy (assoc-ref performance-metrics "cumulative_accuracy"))
         (cognitive-load (assoc-ref performance-metrics "current_cognitive_load")))
    
    ; Trigger adaptation if performance degrades
    (when (< accuracy 0.6)
      (aprfe-trigger-adaptation aprfe-instance 
                               `(("low_accuracy" . ,accuracy)
                                 ("cognitive_load" . ,cognitive-load))))
    
    performance-metrics))

;; Advanced Pattern Recognition Workflows

(define (aprfe-hierarchical-pattern-recognition atoms max-depth)
  "Hierarchical pattern recognition with recursive depth control"
  (define (recursive-recognition current-atoms current-depth)
    (if (>= current-depth max-depth)
        '()
        (let* ((patterns (aprfe-recognize-patterns current-atoms (Undefined)))
               (matched-atoms (if patterns (get-matched-patterns patterns) '()))
               (next-level-patterns (if (> (length matched-atoms) 1)
                                      (recursive-recognition matched-atoms (+ current-depth 1))
                                      '())))
          
          ; Document recursive pattern discovery
          (when (and patterns (> (length matched-atoms) 0))
            (observe-recursive-behavior
              (string-append "hierarchical-recognition-depth-" (number->string current-depth))
              "hierarchical-pattern-discovery"
              `(("depth" . ,current-depth)
                ("patterns-found" . ,(length matched-atoms))
                ("accuracy" . ,(if patterns (get-accuracy patterns) 0.0)))))
          
          (append (if patterns (list patterns) '()) next-level-patterns))))
  
  (recursive-recognition atoms 0))

(define (aprfe-emergent-pattern-synthesis input-patterns)
  "Synthesize new emergent patterns from existing pattern set"
  (let* ((aprfe-instance (get-aprfe-instance))
         (emergent-patterns (aprfe-detect-emergent-patterns aprfe-instance)))
    
    ; Process each emergent pattern discovered
    (for-each
      (lambda (emergent-pattern)
        (let ((pattern-novelty (calculate-pattern-novelty emergent-pattern)))
          (when (> pattern-novelty 0.8)
            (observe-pattern-emergence
              (cog-name emergent-pattern)
              (list (cog-name emergent-pattern))
              pattern-novelty
              "emergent-synthesis"))))
      emergent-patterns)
    
    ; Return synthesized patterns
    emergent-patterns))

(define (aprfe-cognitive-load-adaptation atoms task-complexity)
  "Adapt recognition strategy based on cognitive load and task complexity"
  (let* ((current-load (aprfe-get-cognitive-load (get-aprfe-instance)))
         (adapted-load (+ current-load (* task-complexity 0.1)))
         (strategy (cond
                     ((> adapted-load 0.9) 'attention_guided)
                     ((> adapted-load 0.7) 'neural_only)
                     ((> adapted-load 0.5) 'symbolic_only)
                     (else 'hybrid_fusion))))
    
    ; Update cognitive load
    (aprfe-update-cognitive-load adapted-load)
    
    ; Apply adapted strategy
    (aprfe-set-preferred-strategy (get-aprfe-instance) strategy)
    
    ; Document adaptation decision
    (observe-recursive-behavior
      "cognitive-load-adaptation"
      "load-based-strategy-adaptation"
      `(("original-load" . ,current-load)
        ("adapted-load" . ,adapted-load)
        ("task-complexity" . ,task-complexity)
        ("selected-strategy" . ,(symbol->string strategy))))
    
    strategy))

;; Learning and Self-Improvement

(define (aprfe-cross-modal-learning symbolic-result neural-result attention-result)
  "Learn correlations between different recognition modalities"
  (let* ((aprfe-instance (get-aprfe-instance))
         (correlations (aprfe-learn-correlations aprfe-instance 
                                                symbolic-result 
                                                neural-result 
                                                attention-result)))
    
    ; Document cross-modal learning insights
    (observe-meta-cognitive-insight
      (format #f "Cross-modal learning discovered correlations: ~a"
              correlations)
      "strategic"
      0.9)
    
    correlations))

(define (aprfe-performance-feedback recognition-results expected-accuracy)
  "Provide performance feedback to APRFE for continuous improvement"
  (let* ((aprfe-instance (get-aprfe-instance))
         (actual-accuracy (if recognition-results 
                            (get-accuracy recognition-results) 
                            0.0))
         (performance-gap (- expected-accuracy actual-accuracy))
         (feedback-metrics `(("accuracy-gap" . ,performance-gap)
                           ("expected-accuracy" . ,expected-accuracy)
                           ("actual-accuracy" . ,actual-accuracy))))
    
    ; Apply feedback for adaptation
    (aprfe-adapt-strategies aprfe-instance feedback-metrics)
    
    ; Document performance feedback loop
    (observe-feedback-loop
      "aprfe-performance-feedback"
      '("recognition" "evaluation" "adaptation")
      "continuous-improvement"
      (abs performance-gap))
    
    performance-gap))

(define (aprfe-emergent-behavior-detection)
  "Detect and document emergent cognitive behaviors in APRFE"
  (let* ((aprfe-instance (get-aprfe-instance))
         (performance-history (aprfe-get-performance-history aprfe-instance))
         (emergent-behaviors '()))
    
    ; Analyze performance trends for emergent behaviors
    (when (>= (length performance-history) 10)
      (let* ((recent-performance (take performance-history 10))
             (accuracy-trend (map get-accuracy recent-performance))
             (improvement-rate (- (car accuracy-trend) (last accuracy-trend))))
        
        ; Detect learning behavior
        (when (> improvement-rate 0.1)
          (set! emergent-behaviors 
                (cons "self-improving-accuracy" emergent-behaviors))
          (observe-meta-cognitive-insight
            "APRFE demonstrating emergent self-improvement behavior"
            "emergent"
            0.95))))
    
    ; Detect strategy adaptation patterns
    (let ((strategy-switches (aprfe-get-strategy-switches aprfe-instance)))
      (when (> (length strategy-switches) 5)
        (set! emergent-behaviors 
              (cons "adaptive-strategy-switching" emergent-behaviors))
        (observe-recursive-behavior
          "emergent-adaptive-switching"
          "strategy-adaptation-emergence"
          `(("switch-count" . ,(length strategy-switches))
            ("adaptation-frequency" . ,(/ (length strategy-switches) 
                                         (length performance-history)))))))
    
    emergent-behaviors))

;; Demonstration and Testing Functions

(define (aprfe-comprehensive-demo)
  "Comprehensive demonstration of APRFE capabilities"
  (let* ((test-atoms (create-test-atom-set 50))
         (target-pattern (Inheritance (Concept "test-concept") (Concept "demo-target")))
         (task-description "demonstrate adaptive pattern recognition with cross-modal integration"))
    
    (format #t "~%=== APRFE Comprehensive Demonstration ===~%")
    
    ; 1. Basic pattern recognition
    (format #t "1. Basic Pattern Recognition~%")
    (let ((basic-result (aprfe-recognize-patterns test-atoms target-pattern)))
      (format #t "   Found ~a patterns with accuracy ~a~%"
              (length (get-matched-patterns basic-result))
              (get-accuracy basic-result)))
    
    ; 2. Cross-modal recognition
    (format #t "2. Cross-Modal Recognition~%")
    (let ((cross-modal-result (aprfe-cross-modal-recognition task-description)))
      (format #t "   Integrated ~a patterns across modalities~%"
              (length (get-integrated-patterns cross-modal-result))))
    
    ; 3. Adaptive strategy selection
    (format #t "3. Adaptive Strategy Selection~%")
    (let ((selected-strategy (aprfe-adaptive-strategy-selection test-atoms 0.7)))
      (format #t "   Selected strategy: ~a~%" selected-strategy))
    
    ; 4. Hierarchical pattern recognition
    (format #t "4. Hierarchical Pattern Recognition~%")
    (let ((hierarchical-results (aprfe-hierarchical-pattern-recognition test-atoms 3)))
      (format #t "   Discovered ~a hierarchical levels~%" (length hierarchical-results)))
    
    ; 5. Emergent behavior detection
    (format #t "5. Emergent Behavior Detection~%")
    (let ((emergent-behaviors (aprfe-emergent-behavior-detection)))
      (format #t "   Detected emergent behaviors: ~a~%" emergent-behaviors))
    
    ; 6. Performance monitoring
    (format #t "6. Performance Monitoring~%")
    (let ((performance-metrics (aprfe-monitor-performance)))
      (format #t "   System accuracy: ~a, Cognitive load: ~a~%"
              (assoc-ref performance-metrics "cumulative_accuracy")
              (assoc-ref performance-metrics "current_cognitive_load")))
    
    (format #t "=== APRFE Demonstration Complete ===~%")))

(define (aprfe-cognitive-stress-test max-load-levels)
  "Test APRFE performance under increasing cognitive load"
  (let* ((test-atoms (create-test-atom-set 100))
         (load-levels (map (lambda (i) (/ i max-load-levels)) 
                          (iota max-load-levels 1)))
         (performance-results '()))
    
    (format #t "~%=== APRFE Cognitive Stress Test ===~%")
    
    (for-each
      (lambda (load-level)
        (format #t "Testing load level: ~a~%" load-level)
        
        ; Update cognitive load
        (aprfe-update-cognitive-load load-level)
        
        ; Perform recognition under load
        (let* ((start-time (current-time))
               (recognition-result (aprfe-recognize-patterns test-atoms (Undefined)))
               (end-time (current-time))
               (processing-time (- end-time start-time))
               (accuracy (if recognition-result (get-accuracy recognition-result) 0.0)))
          
          ; Record performance data
          (set! performance-results
                (cons `((load-level . ,load-level)
                       (accuracy . ,accuracy)
                       (processing-time . ,processing-time)
                       (patterns-found . ,(if recognition-result 
                                             (length (get-matched-patterns recognition-result)) 
                                             0)))
                      performance-results))
          
          (format #t "   Accuracy: ~a, Time: ~a, Patterns: ~a~%"
                  accuracy processing-time 
                  (if recognition-result (length (get-matched-patterns recognition-result)) 0))))
      load-levels)
    
    (format #t "=== Cognitive Stress Test Complete ===~%")
    (reverse performance-results)))

;; Helper Functions

(define (create-test-atom-set size)
  "Create a set of test atoms for demonstration purposes"
  (let ((atoms '()))
    (do ((i 0 (+ i 1)))
        ((>= i size))
      (let ((concept-name (string-append "test-concept-" (number->string i))))
        (set! atoms (cons (Concept concept-name) atoms))
        ; Add some relationships
        (when (> i 0)
          (set! atoms (cons (Inheritance (Concept concept-name) 
                                        (Concept (string-append "test-concept-" 
                                                               (number->string (- i 1)))))
                           atoms)))))
    atoms))

(define *aprfe-instance-cache* #f)
(define *aprfe-performance-data* '())

(define (get-aprfe-instance)
  "Get or create APRFE instance with proper initialization"
  (if (not *aprfe-instance-cache*)
      (begin
        (set! *aprfe-instance-cache* 
              (make-hash-table))
        (hash-set! *aprfe-instance-cache* 'initialized #t)
        (hash-set! *aprfe-instance-cache* 'cognitive-load 0.5)
        (hash-set! *aprfe-instance-cache* 'previous-load 0.5)
        (hash-set! *aprfe-instance-cache* 'strategy 'hybrid_fusion)
        (hash-set! *aprfe-instance-cache* 'performance-history '())
        (hash-set! *aprfe-instance-cache* 'strategy-switches '())))
  *aprfe-instance-cache*)

;; Real accessor functions for pattern recognition results
(define (get-matched-patterns result) 
  (if (and result (cog-atom? result))
      (cog-outgoing-set result)
      (if (list? result) result '())))

(define (get-accuracy result)
  "Calculate actual accuracy based on pattern matching quality"
  (if (not result)
      0.0
      (let* ((matched (get-matched-patterns result))
             (match-count (length matched))
             (confidence-sum (fold (lambda (atom acc)
                                    (+ acc (if (cog-atom? atom)
                                             (cog-tv-confidence (cog-tv atom))
                                             0.5)))
                                  0.0 matched)))
        (if (> match-count 0)
            (min 1.0 (/ confidence-sum match-count))
            0.0))))

(define (get-symbolic-contributions result)
  "Calculate symbolic reasoning contribution to pattern recognition"
  (if (not result) 0.0
      (let* ((matched (get-matched-patterns result))
             (symbolic-atoms (filter (lambda (a) 
                                      (and (cog-atom? a)
                                           (or (eq? (cog-type a) 'ConceptNode)
                                               (eq? (cog-type a) 'PredicateNode)
                                               (eq? (cog-type a) 'InheritanceLink))))
                                    matched)))
        (if (> (length matched) 0)
            (/ (length symbolic-atoms) (length matched))
            0.0))))

(define (get-neural-contributions result)
  "Calculate neural network contribution to pattern recognition"
  (if (not result) 0.0
      (let* ((matched (get-matched-patterns result))
             (neural-score (fold (lambda (atom acc)
                                  (+ acc (if (cog-atom? atom)
                                           (cog-tv-mean (cog-tv atom))
                                           0.0)))
                                0.0 matched)))
        (if (> (length matched) 0)
            (min 1.0 (/ neural-score (length matched)))
            0.0))))

(define (get-attention-contributions result)
  "Calculate attention mechanism contribution to pattern recognition"
  (if (not result) 0.0
      (let* ((matched (get-matched-patterns result))
             (attention-sum (fold (lambda (atom acc)
                                   (+ acc (if (cog-atom? atom)
                                            (let ((av (cog-av atom)))
                                              (if av (/ (cog-av-sti av) 100.0) 0.0))
                                            0.0)))
                                 0.0 matched)))
        (if (> (length matched) 0)
            (min 1.0 (/ attention-sum (length matched)))
            0.0))))

(define (get-integrated-patterns result)
  "Extract integrated patterns from cross-modal recognition"
  (if (not result) '()
      (let ((matched (get-matched-patterns result)))
        (filter (lambda (atom)
                 (and (cog-atom? atom)
                      (cog-link? atom)
                      (> (length (cog-outgoing-set atom)) 1)))
               matched))))

(define (calculate-pattern-novelty pattern)
  "Calculate pattern novelty based on historical pattern database"
  (if (not (cog-atom? pattern))
      0.5
      (let* ((pattern-type (cog-type pattern))
             (pattern-tv (cog-tv pattern))
             (pattern-confidence (cog-tv-confidence pattern-tv))
             (pattern-strength (cog-tv-mean pattern-tv))
             (incoming-count (length (cog-incoming-set pattern)))
             ;; Novelty increases with low confidence and few connections
             (novelty-score (- 1.0 (* pattern-confidence 
                                     (min 1.0 (/ incoming-count 10.0))))))
        ;; Boost novelty for high-strength but low-confidence patterns
        (+ (* novelty-score 0.7) 
           (* pattern-strength (- 1.0 pattern-confidence) 0.3))))

;; Export key functions
(export aprfe-recognize-patterns
        aprfe-cross-modal-recognition
        aprfe-adaptive-strategy-selection
        aprfe-hierarchical-pattern-recognition
        aprfe-emergent-pattern-synthesis
        aprfe-cognitive-load-adaptation
        aprfe-cross-modal-learning
        aprfe-performance-feedback
        aprfe-emergent-behavior-detection
        aprfe-comprehensive-demo
        aprfe-cognitive-stress-test)

; Initialize APRFE logging
(format #t "APRFE Scheme integration loaded successfully~%")