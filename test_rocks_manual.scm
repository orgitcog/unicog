#!/usr/bin/guile -s
!#

(use-modules (opencog))
(use-modules (opencog persist))  
(use-modules (opencog persist-rocks))

; Create a test atom
(define test-atom (Concept "TestConcept" (stv 0.8 0.9)))

; Create RocksDB storage with temporary path
(define storage (RocksStorageNode "rocks:///tmp/test_rocks_db/"))

; Open storage and store atom
(display "Opening RocksDB storage...\n")
(cog-open storage)

(display "Storing test atom...\n")
(store-atom test-atom)

(display "Closing storage...\n")
(cog-close storage)

; Clear the atomspace
(cog-atomspace-clear)

; Re-open storage and load atoms
(display "Reopening storage and loading atoms...\n")
(cog-open storage)
(load-atomspace)

; Check if atom exists
(define loaded-atom (cog-node 'Concept "TestConcept"))
(if loaded-atom
    (begin
        (display "SUCCESS: AtomSpace-Rocks integration test passed!\n")
        (display "Loaded atom: ")
        (display loaded-atom)
        (newline))
    (display "FAILED: Could not load atom from RocksDB\n"))

(cog-close storage)
(display "Test completed.\n")