;-----------------------------------------------------------------
; Behavior Rules

(BindLink
    (ListLink
        (ConceptNode "YOU")
        (ConceptNode "ARE")
        (ConceptNode "BEAUTIFUL")
    )
    (DefinedPredicateNode "be happy")
)

; Implemented: OrLinks working in antecedent with proper pattern recognition
(BindLink
    (VariableList
        (TypedVariableLink
            (VariableNode "$blah")
            (TypeNode "ConceptNode")))
    (OrLink
        (ListLink
            (ConceptNode "YOU")
            (ConceptNode "ARE")
            (VariableNode "$blah")
            (ConceptNode "BEAUTIFUL"))
        (ListLink
            (ConceptNode "YOU")
            (ConceptNode "LOOK")
            (ConceptNode "BEAUTIFUL"))
        (ListLink
            (ConceptNode "YOU")
            (ConceptNode "ARE")
            (ConceptNode "GORGEOUS")))
    (DefinedPredicateNode "be happy"))

; TODO: This one only works currently with single word matches to the globs
; due to https://github.com/opencog/atomspace/issues/724
(BindLink
        (ListLink
            (ConceptNode "YOU")
            (ConceptNode "ARE")
            (GlobNode "$blah")
            (ConceptNode "BEAUTIFUL")
            (GlobNode "$blah2")
        )
        (DefinedPredicateNode "be happy")
)


(BindLink
    (ListLink
        (ConceptNode "BE")
        (ConceptNode "HAPPY")
    )
    (DefinedPredicateNode "be happy"))


(BindLink
  (ListLink
    (ConceptNode "ARE")
    (ConceptNode "YOU")
    (ConceptNode "BORED")
  )
  (DefinedPredicateNode "yawn")
)
