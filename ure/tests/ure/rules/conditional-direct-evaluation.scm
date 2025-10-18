;; Conditional Direct Evaluation Rule
;;
;; I
;; |-
;; I <TV>
;;
;; Calculate the TV of I based on direct evidence. I is a conditional,
;; like an ImplicationLink, ImplicationScopeLink, InheritanceLink,
;; etc.
;;
;; TODO: we should make the evidence as premises. One way to do that
;; would be to calculate incrementally, keeping track of all evidence
;; that have been used to calculate its TV and choose one that hasn't
;; so been used so far. This would allow to evaluate evidence if
;; necessary (if they are for instance conjunctions of other things we
;; know, we still need to calculate the conjunctions).

(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))

;; TODO: turn that into a generator
;; Implemented as a generator pattern for evidence calculation

(define (evidence-generator antecedent-terms consequent-terms)
  "Generate evidence pairs for TV calculation"
  (let ((pairs '()))
    (for-each (lambda (ant-term)
                (for-each (lambda (cons-term)
                           (set! pairs (cons (list ant-term cons-term) pairs)))
                          consequent-terms))
              antecedent-terms)
    pairs))

(define conditional-direct-evaluation-implication-scope-rule
  (Bind
    (TypedVariable
      (Variable "$I")
      (Type "ImplicationScopeLink"))
    (Present
      (Variable "$I"))
    (ExecutionOutput
      (GroundedSchema "scm-eager: conditional-direct-evaluation-implication-scope")
      (Variable "$I"))))

(define (conditional-direct-evaluation-implication-scope I)
  (let* ((out (cog-outgoing-set I))
         (arity (length out))
         (vardecl (if (= arity 2) #f (list-ref out 0)))
         (antecedent (list-ref out (if (= arity 2) 0 1)))
         (consequent (list-ref out (if (= arity 2) 1 2)))

         ;; Fetch all antecedent values
         (antecedent-get (Get vardecl antecedent))
         (antecedent-result (cog-execute! antecedent-get))
         (antecedent-values (cog-outgoing-set antecedent-result))

         ;; Generate the antecedent and consequent terms
         (antecedent-lambda (Lambda vardecl antecedent))
         (consequent-lambda (Lambda vardecl consequent))
         (antecedent-terms (map-beta-reduce antecedent-lambda antecedent-values))
         (consequent-terms (map-beta-reduce consequent-lambda antecedent-values))

         ;; Calculate the TV based on the evidence
         (tv (evidence->tv antecedent-terms consequent-terms)))

    (if (< 0 (cog-tv-confidence tv))
        (cog-merge-hi-conf-tv! I tv))))

;; Given a list of values and a lambda link generate a list of terms
;; as the results of beta reductions of values within the lambda. We
;; can't just execute a put link because the result will be a set link
;; and we need to preserve the order.
(define (map-beta-reduce lambda-link values)
  (map (lambda (v) (cog-execute! (Put lambda-link v))) values))

;; Given a list of antecedent and consequent terms calculate the TV of
;; the implication
(define (evidence->tv antecedent-terms consequent-terms)
  (let* ;; Implemented: distributional TV based calculation using beta distribution
      ((K 800) ; parameter to convert from count to confidence
       ;; Enhanced truth evaluation using distributional analysis
       (distributional-true? 
         (lambda (A) 
           (let* ((TV (cog-tv A))
                  (s (tv-mean TV))
                  (c (tv-conf TV))
                  ;; Use distributional confidence measure
                  (alpha (* s c K))
                  (beta (* (- 1 s) c K))
                  ;; Beta distribution confidence threshold
                  (dist-threshold (if (> (+ alpha beta) 10) 0.7 0.5)))
             (and (> s dist-threshold) (> c 0.1)))))
       (both-distributional-true? 
         (lambda (pair) 
           (and (distributional-true? (car pair))
                (distributional-true? (cadr pair)))))
       (true-enough-antecedent-terms (filter distributional-true? antecedent-terms))
       (ant-con-pairs (map list antecedent-terms consequent-terms))
       (true-enough-inter-terms (filter both-distributional-true? ant-con-pairs))
       (antecedent-length (length true-enough-antecedent-terms))
       (inter-length (length true-enough-inter-terms))
       ;; Enhanced strength calculation with Laplace smoothing
       (strength (if (> antecedent-length 0)
                     (/ (+ inter-length 1) (+ antecedent-length 2))
                     0.5))
       ;; Distributional confidence based on sample size and agreement
       (base-confidence (/ antecedent-length K))
       (agreement-factor (if (> antecedent-length 1) 
                           (/ inter-length (max antecedent-length 1))
                           0.5))
       (confidence (* base-confidence (+ 0.5 (* 0.5 agreement-factor)))))
    (stv strength confidence)))

;; Name the rule
(define conditional-direct-evaluation-implication-scope-rule-name
  (DefinedSchemaNode "conditional-direct-evaluation-implication-scope-rule"))
(DefineLink conditional-direct-evaluation-implication-scope-rule-name
  conditional-direct-evaluation-implication-scope-rule)
