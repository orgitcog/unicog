;
; filter-strings.scm -- Process streams containing StringValue
;
; Filtering compares an input stream to a fixed pattern, accepting or
; rejecting a token in the input stream based on whether the pattern
; matches or not. The pattern is encoded in Atomese. This presents a
; problem, if the item to be matched is a StringValue, because Values
; cannot be placed directly into Link Atoms.
;
; This example shows how to work around this issue, by using the
; StringOfLink to encode a StringValue in a pattern.

(use-modules (opencog) (opencog exec))

; A mockup of a stream of data to be filtered.
; This represents a more comprehensive filesystem scan with various file types,
; permissions, sizes, and timestamps to enable richer filtering patterns.
(define stream (LinkValue
	; Directory entries
	(LinkValue (StringValue "/usr") (StringValue "dir") 
		(StringValue "drwxr-xr-x") (NumberValue 4096) (StringValue "2023-01-15"))
	(LinkValue (StringValue "/usr/lib") (StringValue "dir")
		(StringValue "drwxr-xr-x") (NumberValue 8192) (StringValue "2023-03-20"))
	(LinkValue (StringValue "/usr/bin") (StringValue "dir")
		(StringValue "drwxr-xr-x") (NumberValue 16384) (StringValue "2023-05-10"))
	(LinkValue (StringValue "/etc") (StringValue "dir")
		(StringValue "drwxr-xr-x") (NumberValue 4096) (StringValue "2023-02-28"))
	(LinkValue (StringValue "/home") (StringValue "dir")
		(StringValue "drwxr-xr-x") (NumberValue 4096) (StringValue "2023-01-01"))
	
	; Regular files
	(LinkValue (StringValue "/etc/motd") (StringValue "reg")
		(StringValue "-rw-r--r--") (NumberValue 512) (StringValue "2023-06-15"))
	(LinkValue (StringValue "/etc/passwd") (StringValue "reg")
		(StringValue "-rw-r--r--") (NumberValue 2048) (StringValue "2023-07-20"))
	(LinkValue (StringValue "/usr/bin/ls") (StringValue "reg")
		(StringValue "-rwxr-xr-x") (NumberValue 65536) (StringValue "2023-04-01"))
	(LinkValue (StringValue "/usr/bin/cat") (StringValue "reg")
		(StringValue "-rwxr-xr-x") (NumberValue 32768) (StringValue "2023-04-01"))
	
	; Device files
	(LinkValue (StringValue "/dev/sda") (StringValue "block")
		(StringValue "brw-rw----") (NumberValue 0) (StringValue "2023-01-01"))
	(LinkValue (StringValue "/dev/sdb") (StringValue "block")
		(StringValue "brw-rw----") (NumberValue 0) (StringValue "2023-01-01"))
	(LinkValue (StringValue "/dev/tty0") (StringValue "char")
		(StringValue "crw-rw-rw-") (NumberValue 0) (StringValue "2023-01-01"))
	(LinkValue (StringValue "/dev/null") (StringValue "char")
		(StringValue "crw-rw-rw-") (NumberValue 0) (StringValue "2023-01-01"))
	
	; Symbolic links
	(LinkValue (StringValue "/usr/lib64") (StringValue "link")
		(StringValue "lrwxrwxrwx") (NumberValue 3) (StringValue "2023-01-15"))
	(LinkValue (StringValue "/bin") (StringValue "link")
		(StringValue "lrwxrwxrwx") (NumberValue 7) (StringValue "2023-01-01"))
	
	; Special files
	(LinkValue (StringValue "/proc/cpuinfo") (StringValue "proc")
		(StringValue "-r--r--r--") (NumberValue 0) (StringValue "2023-08-01"))
	(LinkValue (StringValue "/sys/class/net") (StringValue "sysfs")
		(StringValue "drwxr-xr-x") (NumberValue 0) (StringValue "2023-08-01"))))

; Place the stream where it can be found.
(cog-set-value! (Anchor "rock") (Predicate "key") stream)

; A search pattern to filter the stream.
(define find-files
	(Filter
		; The rule is applied to each item in the input stream
		(Rule
			; A variable declaratio for the variables used in the rule.
			(Variable "$filename")

			; The pattern to be matched. Look for direntries that are
			; regular files, having file type (StringValue "reg").
			; The StringOfLink is an Atom and can be placed in the
			; pattern. It grabs the Node name, and constructs a
			; a StringValue which is then used in the search pattern.
			(LinkSignature (Type 'LinkValue)
				(Variable "$filename")
				(StringOf (Type 'StringValue) (Node "reg")))

			; The rewrite to be applied to all matching items. In this
			; case, the filename is converted from a StringValue to a
			; concrete Node, an ItemNode, and placed in the AtomSpace.
			; It is also tagged as a file with a Predicate tag. This
			; allows all file URL's in the AtomSpace to be found at
			; some later time, without looking at the external world
			; again. The AtomSpace now "remembers" what was seen.
			(Edge
				(Predicate "is-a file URL")
				(StringOf (Type 'ItemNode) (Variable "$filename"))))

		; The location of the input stream, to which the filter
		; pattern will be applied.
		(ValueOf (Anchor "rock") (Predicate "key"))))

; Apply the filter to the input stream.
(cog-execute! find-files)

; Additional filter examples to demonstrate different filtering patterns:

; Find all executable files (with execute permission)
(define find-executables
	(Filter
		(Rule
			(VariableList
				(Variable "$path")
				(Variable "$perms")
				(Variable "$size")
				(Variable "$date"))
			
			; Match files with execute permission (x in permissions)
			(And
				(LinkSignature (Type 'LinkValue)
					(Variable "$path")
					(StringOf (Type 'StringValue) (Node "reg"))
					(Variable "$perms")
					(Variable "$size")
					(Variable "$date"))
				; Check if permissions contain 'x'
				(Evaluation
					(GroundedPredicate "scm: string-contains")
					(List
						(Variable "$perms")
						(StringOf (Type 'StringValue) (Node "x")))))
			
			(Edge
				(Predicate "is-executable")
				(StringOf (Type 'ItemNode) (Variable "$path"))))
				
		(ValueOf (Anchor "rock") (Predicate "key"))))

; Find large files (> 10KB)
(define find-large-files
	(Filter
		(Rule
			(VariableList
				(Variable "$path")
				(Variable "$type")
				(Variable "$perms")
				(Variable "$size")
				(Variable "$date"))
			
			(And
				(LinkSignature (Type 'LinkValue)
					(Variable "$path")
					(Variable "$type")
					(Variable "$perms")
					(Variable "$size")
					(Variable "$date"))
				; Check if size > 10240 bytes
				(GreaterThan
					(Variable "$size")
					(Number 10240)))
			
			(Edge
				(Predicate "is-large-file")
				(List
					(StringOf (Type 'ItemNode) (Variable "$path"))
					(Variable "$size"))))
					
		(ValueOf (Anchor "rock") (Predicate "key"))))

; Find files in /usr directory
(define find-usr-files
	(Filter
		(Rule
			(Variable "$path")
			
			(And
				(LinkSignature (Type 'LinkValue)
					(Variable "$path")
					(StringOf (Type 'StringValue) (Node "reg"))
					(Variable "$perms")
					(Variable "$size") 
					(Variable "$date"))
				; Check if path starts with /usr
				(Evaluation
					(GroundedPredicate "scm: string-prefix?")
					(List
						(StringOf (Type 'StringValue) (Node "/usr"))
						(Variable "$path"))))
			
			(Edge
				(Predicate "usr-file")
				(StringOf (Type 'ItemNode) (Variable "$path"))))
				
		(ValueOf (Anchor "rock") (Predicate "key"))))

; Helper function for string operations
(define (string-contains str substr)
	(if (string? str)
		(if (string-contains-ci str substr) (stv 1 1) (stv 0 1))
		(stv 0 1)))

; Execute additional filters
; (cog-execute! find-executables)
; (cog-execute! find-large-files) 
; (cog-execute! find-usr-files)

; That's all, folks! The End.
