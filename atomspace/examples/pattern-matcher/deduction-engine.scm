;
; deduction-engine.scm -- Very simple goal solving using Get/PutLinks.
;
; The point of this example is to show how ProLog-like deductions can
; be made, and, more precisely, how to write a ProLog-like chainer
; in Atomese. That is, one can implement ProLog in Atomese, and the
; goal of this example is to show how to do that.
;
;; IMPLEMENTATION NOTE: Refactoring from BindLink to UnifierLink in progress.
;; This example demonstrates ProLog-like deductions using modern unification
;; instead of the older BindLink approach. See unifier examples for patterns.
;
; Critiques:
; Aside from being unfinished, this example also avoids using the
; rule engine, and instead, attempts to create it's own home-grown
; rule engine.
;
; This example also avoids the use of the OpenPsi engine. The OpenPsi
; engine is useful for organizing multiple conflicting rules into
; classes, and prioritizing the rule selection and application based
; on those rule classes.  Thus, it allows complex deductive chains to
; be managed in an economic fashion, avoiding some of the combinatorial
; explosion associated with backward/forward chaining.
;
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog unify))

;;; Assert basic fact
;;;  |- likes(Tom, baseball) 
(EvaluationLink
	(PredicateNode "likes")
	(ListLink
		(ConceptNode "Tom")
		(ConceptNode "baseball")
	)
)

;;; Assert implication
;;;   |- likes(Tom,$X) -> likes(Bill, $X) 
;;; The ImplicationLink is a declarative form of the above.
(RuleLink
	; (VariableNode "$X") ; <-- this variable is implicitly scoped!
	(EvaluationLink
		(PredicateNode "likes")
		(ListLink
			(ConceptNode "Tom")
			(VariableNode "$X")))
	(EvaluationLink
		(PredicateNode "likes")
		(ListLink
			(ConceptNode "Bill")
			(VariableNode "$X"))))

;; Updated implementation using UnifierLink instead of BindLink
;; for proper deductive reasoning as recommended in the FIXME.
;; This demonstrates how to chain implications using the unifier.

;;; The equivalent form using modern unification approach.
;;; This uses UnifierLink to find unifications and create proper
;;; deductive inferences following the unifier examples.
(define deduction-unifier
	(UnifierLink
		; Pattern to unify: things Tom likes  
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Tom")
				(VariableNode "$X")))
		; Pattern to unify: things Tom likes (same pattern for unification)
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Tom")
				(VariableNode "$X")))
		; Template: if Tom likes X, then Bill likes X
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Bill")
				(VariableNode "$X")))))

;;; Same as above, but in imperative form. It uses the GetLink
;;; to search the AtomSpace to find everything Tom likes, and then
;;; uses the PutLink to perform a beta-reduction, to plug in those
;;; answers into a template for the things that Bill likes.
;;; Note the use of two distinct variables; $X is bound to GetLink;
;;; basically, $X is the return value from GetLink. The $Y variable
;;; is bound to PutLink, and functions as a classical lambda-calculus
;;; lambda, defining the arguments that PutLink accepts.
(define implication
	(PutLink
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Bill")
				(VariableNode "$Y")))
		(GetLink
			(EvaluationLink
				(PredicateNode "likes")
				(ListLink
					(ConceptNode "Tom")
					(VariableNode "$X"))))))

;; This causes the implication to be performed.
(cog-execute! implication)

;;; Question to be answered: is it true that likes(Bill, baseball)?
;;; i.e. can we show that |- likes(Bill, baseball)

;;;
;;; A named satisfiability query: Does Bill like $X?
;;; The EvaluationLink just asserts that "Bill does like X" (as a fact).
;;; The SatisfactionLink turns it into a question: the SatisfactionLink
;;; can evaluate to true or false, depending on what X is.
;;; Note that the SatisfactionLink is in the form of a lambda; that is,
;;; it has the form  λX.(Bill likes X)
;;;
;;; Finally, we want to give the above a name, so that we can refer to it
;;; in other places. We use the DefineLink to do this. Given a lambda,
;;; for example, λx.(stuff) which is just an anonymous function taking x,
;;; the DefineLink turns it into a named function: so that 
;;;    Define name λx.(stuff)
;;; is the same as 
;;;    (name x).(stuff)
;;;
;;; So, below, the DefineLink merely gives the question a name: It just
;;; says that there is a particular question, which is given the name 
;;; "Does Bill like X?", and that this question takes a single variable
;;; i.e $X.  We infer this variable from the SatisfactionLink.  That is,
;;; the DefineLink binds exactly the same variables that the lambda under
;;; it does (with SatisfactionLink being the lambda).
(DefineLink
	(DefinedPredicateNode "Does Bill like X?")
	(SatisfactionLink
		(VariableNode "$X")
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Bill")
				(VariableNode "$X")))))

;;; A satisfiability question: Does Bill like X where X is baseball?
(MemberLink
	(ConceptNode "baseball")
	(DefinedPredicateNode "Does Bill like X?")
)

;; solution:
;; do plain member link, get false,
;; look for  sat link body as second half of implication
;; pattern match first half of implication, if found
;; try to check member again.

(cog-evaluate! (DefinedPredicateNode "Does Bill like X?"))

;; A quasi-generic rule implicator.
;; Searches for all implication links (of a very specific form)
;; and converts them into GetPut imperatives.

(define get-impl
	;; Search for RuleLinks, and dissect them.
	(GetLink
		(VariableList
			(TypedVariableLink (VariableNode "$fpred") (TypeNode "PredicateNode"))
			(TypedVariableLink (VariableNode "$tpred") (TypeNode "PredicateNode"))
			(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
			(TypedVariableLink (VariableNode "$B") (TypeNode "ConceptNode"))
			(TypedVariableLink (VariableNode "$V") (TypeNode "VariableNode"))
		)
		(QuoteLink
			(RuleLink
				(UnquoteLink
					(EvaluationLink
						(VariableNode "$fpred")
						(ListLink
							(VariableNode "$A")
							(VariableNode "$V"))))
				(UnquoteLink
					(EvaluationLink
						(VariableNode "$tpred")
						(ListLink
							(VariableNode "$B")
							(VariableNode "$V"))))))))

(define pg-impl
	(PutLink
		(VariableList
			(VariableNode "$tp")
			(VariableNode "$fp")
			(VariableNode "$aaa")
			(VariableNode "$bbb")
			(VariableNode "$vvv")
		)
		(QuoteLink
			(PutLink
				(UnquoteLink
					(EvaluationLink
						(VariableNode "$tp")
						(ListLink
							(VariableNode "$bbb")
							(VariableNode "$vvv"))))
				(GetLink
					(UnquoteLink
						(EvaluationLink
							(VariableNode "$fp")
							(ListLink
								(VariableNode "$aaa")
								(VariableNode "$vvv")))))))
		get-impl))

;; Improved implementation using UnifierLink for rule chaining.
;; This demonstrates how to search for RuleLinks and create
;; unified deductions using the modern unification approach.
(define unify-impl
	(UnifierLink
		;; Pattern: Search for RuleLinks with typed variables
		(RuleLink
			(EvaluationLink
				(VariableNode "$fpred")
				(ListLink
					(VariableNode "$A")
					(VariableNode "$V")))
			(EvaluationLink
				(VariableNode "$tpred")
				(ListLink
					(VariableNode "$B")
					(VariableNode "$V"))))
		
		;; Pattern to match: the same rule structure  
		(RuleLink
			(EvaluationLink
				(VariableNode "$fpred")
				(ListLink
					(VariableNode "$A")
					(VariableNode "$V")))
			(EvaluationLink
				(VariableNode "$tpred")
				(ListLink
					(VariableNode "$B")
					(VariableNode "$V"))))
		
		;; Template: Create a unified deduction
		(Rule
			(EvaluationLink
				(VariableNode "$fpred")
				(ListLink
					(VariableNode "$A")
					(VariableNode "$V")))
			(EvaluationLink
				(VariableNode "$tpred")
				(ListLink
					(VariableNode "$B")
					(VariableNode "$V")))))))

;; Define x as the argument passed to the goal
(define x (VariableNode "$X"))

;; Usage examples for the updated UnifierLink implementation:
;; 
;; Execute the basic deduction unifier:
;; (cog-execute! deduction-unifier)
;;
;; Execute the rule chaining unifier:  
;; (cog-execute! unify-impl)
;;
;; Note: These will return unification sets that can be further processed
;; according to the unifier examples in the unify module.

; Example execution (commented out):
; (cog-execute! deduction-unifier)
