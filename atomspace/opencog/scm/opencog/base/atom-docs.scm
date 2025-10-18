;
; atom-docs.scm
;
; Provide documentation for all the functions implemented in C++ code.
; These can be viewed the guile interpreter prompt by saying
;
;    guile> ,describe  FUNCTION-NAME
;
; A list of all of these is printed by saying
;
;    guile> ,apropos Link
;    guile> ,apropos Node
;
(set-procedure-property! QueryLink 'documentation
"
  QueryLink -- Define a query, rewriting all matches. Results placed in 
    a QueueValue.

  General form:

      QueryLink
          VariableList decls
          match-body
          rewrite

  See https://wiki.opencog.org/w/QueryLink for more info.

  See also:
     BindLink -- same as above, results placed in SetLink
     GetLink -- no rewrite, results placed in SetLink
     MeetLink -- no rewrite, results place in QueueValue
     SatisfactionLink -- return true, if there are any results at all.
     DualLink -- pattern recognition instead of pattern matching
     JoinLink -- union, instead of intersection
")

(set-procedure-property! Query 'documentation
	(procedure-property QueryLink 'documentation))

; ---------------------------------------------------------------
(set-procedure-property! BindLink 'documentation
"
  BindLink -- Define a query, rewriting all matches. Results placed in 
    a SetLink.

  General form:

      BindLink
          VariableList decls
          match-body
          rewrite

  Same as QueryLink, but results are returned in a SetLink instead of
  a QueueValue. This is useful when you need all results collected in
  a single Atom structure.

  See https://wiki.opencog.org/w/QueryLink for more info.

  See also:
     QueryLink -- same as above, results placed in QueueValue
     GetLink -- no rewrite, results placed in SetLink
     MeetLink -- no rewrite, results place in QueueValue
     SatisfactionLink -- return true, if there are any results at all.
     DualLink -- pattern recognition instead of pattern matching
     JoinLink -- union, instead of intersection
")

(set-procedure-property! BindLink 'documentation
	(procedure-property QueryLink 'documentation))

(set-procedure-property! GetLink 'documentation
	(procedure-property QueryLink 'documentation))

(set-procedure-property! Get 'documentation
	(procedure-property QueryLink 'documentation))

(set-procedure-property! MeetLink 'documentation
	(procedure-property QueryLink 'documentation))

(set-procedure-property! Meet 'documentation
	(procedure-property QueryLink 'documentation))

(set-procedure-property! SatisfactionLink 'documentation
	(procedure-property QueryLink 'documentation))

(set-procedure-property! Satisfaction 'documentation
	(procedure-property QueryLink 'documentation))
