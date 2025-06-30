;
; distributed-cognition.scm
;
; Phase II.3: Distributed Cognition Engine
; Multi-agent Scheme modules for parallel cognitive cycles
;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (ice-9 threads))

; Define cognitive schema for distributed cognition
(define distributed-cognition-schema
  '(schema
     (module distributed-cognition (role "multi-agent-parallel-processor"))
     (features "parallel-cognitive-cycles" "agent-synchronization" "recursive-inter-agent-communication")
     (recursive-pathway (agent-output adjacent-agent-input cognitive-feedback))))

; Cognitive agent state structure
(define-record-type cognitive-agent
  (make-cognitive-agent id state adjacent-agents active? thread-id)
  cognitive-agent?
  (id agent-id)
  (state agent-state set-agent-state!)
  (adjacent-agents agent-adjacent-agents set-agent-adjacent-agents!)
  (active? agent-active? set-agent-active!)
  (thread-id agent-thread-id set-agent-thread-id!))

; Global registry of cognitive agents
(define *cognitive-agents* (make-hash-table))
(define *shared-hypergraph-context* (make-hash-table))
(define *agent-synchronization-mutex* (make-mutex))

; Create a new cognitive agent
(define (create-cognitive-agent agent-id initial-state)
  "Create a new cognitive agent with specified ID and initial state"
  (let ((new-agent (make-cognitive-agent
                     agent-id
                     initial-state
                     '()  ; No adjacent agents initially
                     #t   ; Active by default
                     #f))) ; No thread initially
    
    ; Register agent in global registry
    (with-mutex *agent-synchronization-mutex*
      (hash-set! *cognitive-agents* agent-id new-agent))
    
    ; Initialize agent state in shared context
    (update-shared-hypergraph-context agent-id "agent-state" initial-state)
    
    (format #t "Created cognitive agent: ~a~%" agent-id)
    new-agent))

; Start cognitive processing cycle for an agent
(define (start-cognitive-agent agent-id cycle-frequency)
  "Start the cognitive processing cycle for a specific agent"
  (let ((agent (hash-ref *cognitive-agents* agent-id)))
    (when agent
      (let ((thread-id
             (call-with-new-thread
               (lambda ()
                 (cognitive-agent-loop agent cycle-frequency)))))
        (set-agent-thread-id! agent thread-id)
        (format #t "Started cognitive cycle for agent: ~a at ~a Hz~%" 
                agent-id cycle-frequency)))))

; Main cognitive processing loop for an agent
(define (cognitive-agent-loop agent cycle-frequency)
  "Main processing loop for a cognitive agent"
  (let ((sleep-duration (/ 1.0 cycle-frequency)))
    (while (agent-active? agent)
      ; Perform one cognitive iteration
      (let ((iteration-result (cognitive-agent-iteration agent)))
        ; Distribute output to adjacent agents
        (distribute-agent-output agent iteration-result)
        
        ; Update shared context
        (update-shared-hypergraph-context 
          (agent-id agent) "agent-state" (agent-state agent))
        
        ; Sleep until next cycle
        (usleep (* sleep-duration 1000000))))))

; Single cognitive iteration for an agent
(define (cognitive-agent-iteration agent)
  "Perform one cognitive processing iteration"
  (let* ((agent-id (agent-id agent))
         (current-state (agent-state agent))
         (shared-context (get-aggregated-shared-context))
         (adjacent-inputs (collect-adjacent-agent-inputs agent))
         (combined-inputs (append current-state shared-context adjacent-inputs)))
    
    ; Process cognitive inputs
    (let ((processed-state (process-cognitive-inputs agent combined-inputs)))
      ; Update agent state
      (set-agent-state! agent processed-state)
      
      ; Apply learning and adaptation
      (apply-cognitive-learning agent processed-state)
      
      ; Return output for distribution
      processed-state)))

; Process cognitive inputs for an agent
(define (process-cognitive-inputs agent inputs)
  "Process combined inputs through cognitive transformation"
  (let* ((input-size (length inputs))
         (state-size (length (agent-state agent)))
         (processed-inputs '()))
    
    ; Apply attention mechanism to inputs
    (set! processed-inputs
          (map (lambda (input idx)
                 (let ((attention-weight (get-attention-weight agent idx)))
                   (* input attention-weight)))
               inputs (iota input-size)))
    
    ; Apply cognitive transformation (simplified neural-symbolic processing)
    (let ((transformed-state
           (map (lambda (old-state new-input)
                  (+ (* 0.7 old-state) (* 0.3 new-input)))
                (agent-state agent)
                (take processed-inputs (min state-size input-size)))))
      
      ; Normalize and return transformed state
      (normalize-vector transformed-state))))

; Distribute agent output to adjacent agents
(define (distribute-agent-output agent output)
  "Distribute agent output to adjacent agents (recursive implementation)"
  (for-each
    (lambda (adjacent-id)
      (let ((adjacent-agent (hash-ref *cognitive-agents* adjacent-id)))
        (when adjacent-agent
          ; Recursive pathway: agent output becomes input for adjacent agents
          (receive-agent-input adjacent-agent (agent-id agent) output))))
    (agent-adjacent-agents agent)))

; Receive input from another agent
(define (receive-agent-input receiving-agent source-agent-id input-data)
  "Receive and integrate input from another agent"
  (let ((context-key (string-append "input-from-" source-agent-id)))
    ; Store input in shared context for next cognitive iteration
    (update-shared-hypergraph-context 
      (agent-id receiving-agent) context-key input-data)))

; Add bidirectional connection between agents
(define (connect-cognitive-agents agent1-id agent2-id)
  "Create bidirectional connection between two cognitive agents"
  (let ((agent1 (hash-ref *cognitive-agents* agent1-id))
        (agent2 (hash-ref *cognitive-agents* agent2-id)))
    (when (and agent1 agent2)
      ; Add agent2 to agent1's adjacent list
      (set-agent-adjacent-agents! 
        agent1 
        (cons agent2-id (agent-adjacent-agents agent1)))
      
      ; Add agent1 to agent2's adjacent list
      (set-agent-adjacent-agents! 
        agent2 
        (cons agent1-id (agent-adjacent-agents agent2)))
      
      (format #t "Connected agents: ~a <-> ~a~%" agent1-id agent2-id))))

; Shared hypergraph context management
(define (update-shared-hypergraph-context agent-id context-key context-data)
  "Update shared hypergraph context (thread-safe)"
  (with-mutex *agent-synchronization-mutex*
    (let ((agent-context (hash-ref *shared-hypergraph-context* agent-id '())))
      (hash-set! *shared-hypergraph-context* agent-id
                 (assoc-set! agent-context context-key context-data)))))

(define (get-shared-hypergraph-context agent-id context-key)
  "Get data from shared hypergraph context"
  (with-mutex *agent-synchronization-mutex*
    (let ((agent-context (hash-ref *shared-hypergraph-context* agent-id '())))
      (assoc-ref agent-context context-key))))

(define (get-aggregated-shared-context)
  "Get aggregated context from all agents for cognitive processing"
  (with-mutex *agent-synchronization-mutex*
    (let ((all-contexts '()))
      ; Collect all agent states
      (hash-for-each
        (lambda (agent-id agent-context)
          (let ((state (assoc-ref agent-context "agent-state")))
            (when state
              (set! all-contexts (append all-contexts state)))))
        *shared-hypergraph-context*)
      
      ; Return normalized aggregated context
      (if (null? all-contexts)
          '(0.0)
          (normalize-vector all-contexts)))))

; Collect inputs from adjacent agents
(define (collect-adjacent-agent-inputs agent)
  "Collect recent inputs from adjacent agents"
  (let ((collected-inputs '()))
    (for-each
      (lambda (adjacent-id)
        (let ((input-key (string-append "input-from-" adjacent-id))
              (input-data (get-shared-hypergraph-context (agent-id agent) input-key)))
          (when input-data
            (set! collected-inputs (append collected-inputs input-data)))))
      (agent-adjacent-agents agent))
    
    (if (null? collected-inputs)
        '(0.0)
        collected-inputs)))

; Cognitive learning and adaptation
(define (apply-cognitive-learning agent processed-state)
  "Apply learning based on cognitive processing results"
  (let* ((learning-rate 0.1)
         (current-state (agent-state agent))
         (state-difference (map - processed-state current-state))
         (adapted-state
          (map (lambda (current delta)
                 (+ current (* learning-rate delta)))
               current-state state-difference)))
    
    ; Update agent state with learned adaptations
    (set-agent-state! agent adapted-state)))

; Utility functions
(define (normalize-vector vec)
  "Normalize a vector to unit length"
  (let ((magnitude (sqrt (apply + (map (lambda (x) (* x x)) vec)))))
    (if (> magnitude 0)
        (map (lambda (x) (/ x magnitude)) vec)
        vec)))

(define (get-attention-weight agent index)
  "Get attention weight for a specific input index"
  ; Simple attention mechanism based on index
  (let ((attention-weights (get-shared-hypergraph-context 
                            (agent-id agent) "attention-weights")))
    (if (and attention-weights (< index (length attention-weights)))
        (list-ref attention-weights index)
        1.0)))

; Stop a cognitive agent
(define (stop-cognitive-agent agent-id)
  "Stop the cognitive processing for a specific agent"
  (let ((agent (hash-ref *cognitive-agents* agent-id)))
    (when agent
      (set-agent-active! agent #f)
      (format #t "Stopped cognitive agent: ~a~%" agent-id))))

; Demonstration function
(define (demonstrate-distributed-cognition)
  "Demonstrate the distributed cognition system"
  (format #t "~%=== Distributed Cognition Engine Demonstration ===~%")
  
  ; Create multiple cognitive agents
  (let ((agent1 (create-cognitive-agent "agent-alpha" '(0.5 0.3 0.8)))
        (agent2 (create-cognitive-agent "agent-beta" '(0.7 0.1 0.6)))
        (agent3 (create-cognitive-agent "agent-gamma" '(0.2 0.9 0.4))))
    
    ; Connect agents in a network
    (connect-cognitive-agents "agent-alpha" "agent-beta")
    (connect-cognitive-agents "agent-beta" "agent-gamma")
    (connect-cognitive-agents "agent-gamma" "agent-alpha")
    
    ; Start cognitive cycles
    (start-cognitive-agent "agent-alpha" 5.0)  ; 5 Hz
    (start-cognitive-agent "agent-beta" 7.0)   ; 7 Hz  
    (start-cognitive-agent "agent-gamma" 3.0)  ; 3 Hz
    
    (format #t "Started distributed cognitive network with 3 agents~%")
    (format #t "Agents running at different frequencies for emergent dynamics~%")
    
    ; Let it run for a brief demonstration
    (sleep 2)
    
    ; Show final states
    (format #t "Final agent states:~%")
    (format #t "  Agent Alpha: ~a~%" (agent-state agent1))
    (format #t "  Agent Beta: ~a~%" (agent-state agent2))
    (format #t "  Agent Gamma: ~a~%" (agent-state agent3))
    
    ; Stop all agents
    (stop-cognitive-agent "agent-alpha")
    (stop-cognitive-agent "agent-beta")
    (stop-cognitive-agent "agent-gamma")
    
    (format #t "Distributed cognition demonstration completed~%")))

; Export key functions
(export create-cognitive-agent
        start-cognitive-agent
        stop-cognitive-agent
        connect-cognitive-agents
        demonstrate-distributed-cognition)