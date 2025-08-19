;; cognitive-grammar-adapter.scm
;;
;; Scheme Cognitive Grammar Microservices for Agentic Grammar AtomSpace Integration
;; Implements modular scheme adapters with round-trip translation capabilities
;;
;; Part of Phase 1: Cognitive Primitives & Foundational Hypergraph Encoding

(define-module (opencog tensor cognitive-grammar))

(use-modules (opencog)
             (opencog tensor-kernel)
             (opencog exec)
             (opencog persist)
             (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 match))

;; ====================================================================
;; COGNITIVE PRIMITIVE TENSOR DEFINITIONS
;; ====================================================================

;; Cognitive primitive tensor signature as per Phase 1 requirements
;; Cognitive_Primitive_Tensor[5] = {
;;   modality: [visual, auditory, textual, symbolic],
;;   depth: [surface, semantic, pragmatic], 
;;   context: [local, global, temporal],
;;   salience: [0.0, 1.0],
;;   autonomy_index: [0.0, 1.0]
;; }

(define cognitive-primitive-shape '(4 3 3 1 1))

(define modality-types '(visual auditory textual symbolic))
(define depth-types '(surface semantic pragmatic))
(define context-types '(local global temporal))

;; ====================================================================
;; COGNITIVE PRIMITIVE RECORD TYPE
;; ====================================================================

(define-record-type <cognitive-primitive>
  (make-cognitive-primitive name modality depth context salience autonomy)
  cognitive-primitive?
  (name cognitive-primitive-name)
  (modality cognitive-primitive-modality)
  (depth cognitive-primitive-depth)
  (context cognitive-primitive-context)
  (salience cognitive-primitive-salience)
  (autonomy cognitive-primitive-autonomy))

;; ====================================================================
;; SCHEME TO HYPERGRAPH ENCODING ADAPTER
;; ====================================================================

(define (scheme-primitive-to-hypergraph primitive)
  "Convert cognitive primitive to hypergraph node structure with tensor signatures"
  (let* ((name (cognitive-primitive-name primitive))
         (concept-node (Concept name))
         (tensor-signature-node (Concept (string-append name "-tensor-signature"))))
    
    ;; Create hypergraph structure for the cognitive primitive
    (List
      ;; Main primitive concept
      concept-node
      
      ;; Tensor signature with 5D encoding
      (Evaluation
        (Predicate "tensor-signature")
        (List
          concept-node
          tensor-signature-node))
      
      ;; Modality encoding
      (Evaluation
        (Predicate "modality")
        (List
          concept-node
          (Concept (symbol->string (cognitive-primitive-modality primitive)))))
      
      ;; Depth encoding  
      (Evaluation
        (Predicate "depth")
        (List
          concept-node
          (Concept (symbol->string (cognitive-primitive-depth primitive)))))
      
      ;; Context encoding
      (Evaluation
        (Predicate "context")
        (List
          concept-node
          (Concept (symbol->string (cognitive-primitive-context primitive)))))
      
      ;; Salience encoding (numerical)
      (Evaluation
        (Predicate "salience")
        (List
          concept-node
          (Number (cognitive-primitive-salience primitive))))
      
      ;; Autonomy index encoding (numerical)
      (Evaluation
        (Predicate "autonomy-index")
        (List
          concept-node
          (Number (cognitive-primitive-autonomy primitive))))
      
      ;; Prime factorization mapping
      (Evaluation
        (Predicate "prime-factorization")
        (List
          concept-node
          (List 
            (Number (modality-to-prime (cognitive-primitive-modality primitive)))
            (Number (depth-to-prime (cognitive-primitive-depth primitive)))
            (Number (context-to-prime (cognitive-primitive-context primitive)))
            (Number (+ 31 (* 10 (cognitive-primitive-salience primitive))))
            (Number (+ 41 (* 10 (cognitive-primitive-autonomy primitive))))))))))

;; ====================================================================
;; HYPERGRAPH TO SCHEME DECODING ADAPTER  
;; ====================================================================

(define (hypergraph-to-scheme-primitive hypergraph-atoms)
  "Convert hypergraph atom structure back to cognitive primitive"
  (let ((concept-node #f)
        (modality 'symbolic)
        (depth 'semantic)
        (context 'local)
        (salience 0.5)
        (autonomy 0.5)
        (name "unnamed"))
    
    ;; Extract information from hypergraph atoms
    (for-each
      (lambda (atom)
        (match atom
          ;; Extract concept node name
          (('Concept name-str)
           (unless (string-contains name-str "tensor-signature")
             (set! name name-str)
             (set! concept-node atom)))
          
          ;; Extract modality
          (('Evaluation ('Predicate "modality") ('List _ ('Concept mod-str)))
           (set! modality (string->symbol mod-str)))
          
          ;; Extract depth
          (('Evaluation ('Predicate "depth") ('List _ ('Concept depth-str)))
           (set! depth (string->symbol depth-str)))
          
          ;; Extract context
          (('Evaluation ('Predicate "context") ('List _ ('Concept ctx-str)))
           (set! context (string->symbol ctx-str)))
          
          ;; Extract salience
          (('Evaluation ('Predicate "salience") ('List _ ('Number sal-val)))
           (set! salience sal-val))
          
          ;; Extract autonomy
          (('Evaluation ('Predicate "autonomy-index") ('List _ ('Number auto-val)))
           (set! autonomy auto-val))
          
          ;; Ignore other patterns
          (_ #f)))
      hypergraph-atoms)
    
    ;; Create cognitive primitive record
    (make-cognitive-primitive name modality depth context salience autonomy)))

;; ====================================================================
;; ROUND-TRIP TRANSLATION FUNCTIONS
;; ====================================================================

(define (cognitive-primitive-round-trip primitive)
  "Perform round-trip translation: primitive -> hypergraph -> primitive"
  (let* ((hypergraph (scheme-primitive-to-hypergraph primitive))
         (hypergraph-atoms (if (eq? (cog-type hypergraph) 'ListLink)
                             (cog-outgoing-set hypergraph)
                             (list hypergraph)))
         (recovered-primitive (hypergraph-to-scheme-primitive hypergraph-atoms)))
    (values hypergraph recovered-primitive)))

(define (validate-round-trip-accuracy original recovered)
  "Validate that round-trip translation preserves all cognitive primitive properties"
  (let ((name-match? (string=? (cognitive-primitive-name original)
                              (cognitive-primitive-name recovered)))
        (modality-match? (eq? (cognitive-primitive-modality original)
                             (cognitive-primitive-modality recovered)))
        (depth-match? (eq? (cognitive-primitive-depth original)
                          (cognitive-primitive-depth recovered)))
        (context-match? (eq? (cognitive-primitive-context original)
                            (cognitive-primitive-context recovered)))
        (salience-match? (< (abs (- (cognitive-primitive-salience original)
                                   (cognitive-primitive-salience recovered))) 0.01))
        (autonomy-match? (< (abs (- (cognitive-primitive-autonomy original)
                                   (cognitive-primitive-autonomy recovered))) 0.01)))
    
    (and name-match? modality-match? depth-match? 
         context-match? salience-match? autonomy-match?)))

;; ====================================================================
;; TENSOR SHAPE VALIDATION FUNCTIONS
;; ====================================================================

(define (validate-cognitive-primitive-tensor-shape primitive)
  "Validate that cognitive primitive conforms to the 5D tensor signature"
  (let ((modality-valid? (member (cognitive-primitive-modality primitive) modality-types))
        (depth-valid? (member (cognitive-primitive-depth primitive) depth-types))
        (context-valid? (member (cognitive-primitive-context primitive) context-types))
        (salience-valid? (and (>= (cognitive-primitive-salience primitive) 0.0)
                             (<= (cognitive-primitive-salience primitive) 1.0)))
        (autonomy-valid? (and (>= (cognitive-primitive-autonomy primitive) 0.0)
                             (<= (cognitive-primitive-autonomy primitive) 1.0))))
    
    (and modality-valid? depth-valid? context-valid? salience-valid? autonomy-valid?)))

(define (calculate-tensor-degrees-of-freedom)
  "Calculate degrees of freedom for cognitive primitive tensor"
  ;; DOF = (modality_categories - 1) + (depth_categories - 1) + (context_categories - 1) + salience_continuous + autonomy_continuous
  (+ (- (length modality-types) 1)
     (- (length depth-types) 1)
     (- (length context-types) 1)
     1  ; salience continuous
     1  ; autonomy continuous
     ))

;; ====================================================================
;; PRIME FACTORIZATION MAPPING
;; ====================================================================

(define (modality-to-prime modality)
  "Map modality to prime number for tensor signature"
  (case modality
    ((visual) 2)
    ((auditory) 3)
    ((textual) 5)
    ((symbolic) 7)
    (else 1)))

(define (depth-to-prime depth)
  "Map depth to prime number for tensor signature"
  (case depth
    ((surface) 11)
    ((semantic) 13)
    ((pragmatic) 17)
    (else 1)))

(define (context-to-prime context)
  "Map context to prime number for tensor signature"
  (case context
    ((local) 19)
    ((global) 23)
    ((temporal) 29)
    (else 1)))

(define (cognitive-primitive-to-prime-signature primitive)
  "Generate complete prime factorization signature for cognitive primitive"
  (list
    (modality-to-prime (cognitive-primitive-modality primitive))
    (depth-to-prime (cognitive-primitive-depth primitive))
    (context-to-prime (cognitive-primitive-context primitive))
    (+ 31 (* 10 (cognitive-primitive-salience primitive)))
    (+ 41 (* 10 (cognitive-primitive-autonomy primitive)))))

;; ====================================================================
;; AGENTIC GRAMMAR INTEGRATION FUNCTIONS
;; ====================================================================

(define (create-agentic-grammar-context primitives)
  "Create an agentic grammar context from a collection of cognitive primitives"
  (let ((context-atoms (map scheme-primitive-to-hypergraph primitives)))
    (List
      (Concept "agentic-grammar-context")
      (List context-atoms))))

(define (compose-cognitive-grammars . primitive-collections)
  "Compose multiple cognitive primitive collections into unified grammar"
  (let ((all-primitives (apply append primitive-collections)))
    (create-agentic-grammar-context all-primitives)))

(define (execute-cognitive-grammar-flow flow-name primitives)
  "Execute a cognitive flow using grammar-enabled primitives"
  (case flow-name
    ((perception-to-action)
     (perception-action-flow primitives))
    ((reasoning-chain)
     (reasoning-chain-flow primitives))
    ((attention-allocation)
     (attention-allocation-flow primitives))
    (else
     (error "Unknown cognitive flow:" flow-name))))

;; ====================================================================
;; COGNITIVE FLOW IMPLEMENTATIONS
;; ====================================================================

(define (perception-action-flow primitives)
  "Implement perception-to-action cognitive flow"
  (let* ((visual-primitives (filter (lambda (p) 
                                     (eq? (cognitive-primitive-modality p) 'visual)) 
                                   primitives))
         (high-salience-primitives (filter (lambda (p) 
                                            (> (cognitive-primitive-salience p) 0.7)) 
                                          primitives)))
    (if (and (not (null? visual-primitives)) (not (null? high-salience-primitives)))
        (List
          (Concept "perception-action-result")
          (map scheme-primitive-to-hypergraph high-salience-primitives))
        (List (Concept "no-action-generated")))))

(define (reasoning-chain-flow primitives)
  "Implement reasoning chain cognitive flow"
  (let* ((semantic-primitives (filter (lambda (p) 
                                       (eq? (cognitive-primitive-depth p) 'semantic)) 
                                     primitives))
         (global-context-primitives (filter (lambda (p) 
                                             (eq? (cognitive-primitive-context p) 'global)) 
                                           primitives)))
    (List
      (Concept "reasoning-chain-result")
      (map scheme-primitive-to-hypergraph semantic-primitives))))

(define (attention-allocation-flow primitives)
  "Implement attention allocation cognitive flow"
  (let* ((sorted-primitives (sort primitives 
                                 (lambda (a b) 
                                   (> (cognitive-primitive-salience a) 
                                      (cognitive-primitive-salience b)))))
         (top-attention-primitives (take sorted-primitives 
                                        (min 3 (length sorted-primitives)))))
    (List
      (Concept "attention-allocation-result")
      (map scheme-primitive-to-hypergraph top-attention-primitives))))

;; ====================================================================
;; TESTING AND VALIDATION FRAMEWORK
;; ====================================================================

(define (run-cognitive-primitive-tests)
  "Run comprehensive tests for cognitive primitive transformations"
  (let ((test-primitives (list
                          (make-cognitive-primitive "test-visual" 'visual 'surface 'local 0.8 0.6)
                          (make-cognitive-primitive "test-auditory" 'auditory 'semantic 'global 0.5 0.7)
                          (make-cognitive-primitive "test-symbolic" 'symbolic 'pragmatic 'temporal 0.9 0.4))))
    
    (format #t "~%=== COGNITIVE PRIMITIVE TESTS ===~%")
    
    ;; Test round-trip translation accuracy
    (format #t "Testing round-trip translation accuracy...~%")
    (let ((all-accurate? #t))
      (for-each
        (lambda (primitive)
          (let-values (((hypergraph recovered) (cognitive-primitive-round-trip primitive)))
            (let ((accurate? (validate-round-trip-accuracy primitive recovered)))
              (format #t "  ~a: ~a~%" 
                     (cognitive-primitive-name primitive) 
                     (if accurate? "PASS" "FAIL"))
              (unless accurate?
                (set! all-accurate? #f)))))
        test-primitives)
      (format #t "Round-trip accuracy: ~a~%" (if all-accurate? "ALL PASS" "SOME FAIL")))
    
    ;; Test tensor shape validation
    (format #t "~%Testing tensor shape validation...~%")
    (for-each
      (lambda (primitive)
        (let ((valid? (validate-cognitive-primitive-tensor-shape primitive)))
          (format #t "  ~a: ~a~%" 
                 (cognitive-primitive-name primitive) 
                 (if valid? "VALID SHAPE" "INVALID SHAPE"))))
      test-primitives)
    
    ;; Test cognitive flows
    (format #t "~%Testing cognitive flows...~%")
    (let ((perception-result (execute-cognitive-grammar-flow 'perception-to-action test-primitives))
          (reasoning-result (execute-cognitive-grammar-flow 'reasoning-chain test-primitives))
          (attention-result (execute-cognitive-grammar-flow 'attention-allocation test-primitives)))
      (format #t "  Perception-to-action flow: ~a~%" 
             (if (pair? perception-result) "SUCCESS" "FAIL"))
      (format #t "  Reasoning chain flow: ~a~%" 
             (if (pair? reasoning-result) "SUCCESS" "FAIL"))
      (format #t "  Attention allocation flow: ~a~%" 
             (if (pair? attention-result) "SUCCESS" "FAIL")))
    
    ;; Calculate and display DOF
    (format #t "~%Tensor degrees of freedom: ~a~%" (calculate-tensor-degrees-of-freedom))
    
    ;; Test prime factorization
    (format #t "~%Testing prime factorization mapping...~%")
    (for-each
      (lambda (primitive)
        (let ((primes (cognitive-primitive-to-prime-signature primitive)))
          (format #t "  ~a: ~a~%" 
                 (cognitive-primitive-name primitive) 
                 primes)))
      test-primitives)
    
    (format #t "~%=== TESTS COMPLETE ===~%")))

;; ====================================================================
;; VISUALIZATION AND DOCUMENTATION HELPERS
;; ====================================================================

(define (generate-hypergraph-fragment-flowchart primitives)
  "Generate flowchart representation of hypergraph fragments"
  (let ((flowchart-nodes (map
                           (lambda (primitive)
                             (format #f "~a[~a|~a|~a|~a|~a]"
                                    (cognitive-primitive-name primitive)
                                    (cognitive-primitive-modality primitive)
                                    (cognitive-primitive-depth primitive)
                                    (cognitive-primitive-context primitive)
                                    (cognitive-primitive-salience primitive)
                                    (cognitive-primitive-autonomy primitive)))
                           primitives)))
    (string-join flowchart-nodes " -> ")))

(define (document-cognitive-grammar-tensor-signatures primitives)
  "Generate documentation for cognitive grammar tensor signatures"
  (format #f "Cognitive Grammar Tensor Signatures:~%~%")
  (string-append
    (format #f "Standard 5D Tensor Shape: ~a~%" cognitive-primitive-shape)
    (format #f "Modality Types: ~a~%" modality-types)
    (format #f "Depth Types: ~a~%" depth-types)
    (format #f "Context Types: ~a~%" context-types)
    (format #f "Degrees of Freedom: ~a~%~%" (calculate-tensor-degrees-of-freedom))
    (apply string-append
           (map (lambda (primitive)
                  (format #f "~a:~%  Prime Signature: ~a~%  Tensor Shape Valid: ~a~%~%"
                         (cognitive-primitive-name primitive)
                         (cognitive-primitive-to-prime-signature primitive)
                         (validate-cognitive-primitive-tensor-shape primitive)))
                primitives))))

;; ====================================================================
;; EXPORT FUNCTIONS
;; ====================================================================

(export make-cognitive-primitive
        cognitive-primitive?
        cognitive-primitive-name
        cognitive-primitive-modality  
        cognitive-primitive-depth
        cognitive-primitive-context
        cognitive-primitive-salience
        cognitive-primitive-autonomy
        scheme-primitive-to-hypergraph
        hypergraph-to-scheme-primitive
        cognitive-primitive-round-trip
        validate-round-trip-accuracy
        validate-cognitive-primitive-tensor-shape
        calculate-tensor-degrees-of-freedom
        cognitive-primitive-to-prime-signature
        create-agentic-grammar-context
        compose-cognitive-grammars
        execute-cognitive-grammar-flow
        run-cognitive-primitive-tests
        generate-hypergraph-fragment-flowchart
        document-cognitive-grammar-tensor-signatures)