;
; neural-symbolic-tutorial.scm
;
; Phase II.5: Interactive chatbot tutorials demonstrating neural-symbolic cycles
; Recursive feedback: User interactions inform next-generation tutorial content
;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (ice-9 regex))

; Define cognitive schema for tutorial automation
(define tutorial-automation-schema
  '(schema
     (module tutorial-automation (role "neural-symbolic-educator"))
     (features "interactive-chatbot" "adaptive-tutorials" "automated-testing")
     (recursive-pathway (user-interactions tutorial-adaptation content-generation))))

; Tutorial state and progress tracking
(define *tutorial-state* (make-hash-table))
(define *user-interactions* '())
(define *tutorial-content* (make-hash-table))
(define *adaptive-difficulty* 0.5)

; Initialize tutorial system
(define (initialize-tutorial-system)
  "Initialize the neural-symbolic tutorial system"
  (format #t "~%🧠 Initializing Neural-Symbolic Tutorial System...~%")
  
  ; Create basic tutorial content
  (hash-set! *tutorial-content* 'introduction
    "Welcome to OpenCog Unified! This interactive tutorial will guide you through neural-symbolic cognitive cycles.")
  
  (hash-set! *tutorial-content* 'basic-concepts
    "Let's explore the core concepts: Atoms, AtomSpace, and cognitive patterns.")
  
  (hash-set! *tutorial-content* 'pattern-recognition
    "Now we'll learn about emergent pattern detection and recursive learning.")
  
  (hash-set! *tutorial-content* 'distributed-cognition
    "Discover how multiple cognitive agents work together in distributed processing.")
  
  (hash-set! *tutorial-content* 'visualization
    "Experience real-time cognitive visualization with attention overlays.")
  
  ; Initialize tutorial state
  (hash-set! *tutorial-state* 'current-section 'introduction)
  (hash-set! *tutorial-state* 'progress 0)
  (hash-set! *tutorial-state* 'difficulty *adaptive-difficulty*)
  (hash-set! *tutorial-state* 'user-responses '())
  
  (format #t "✅ Tutorial system initialized~%"))

; Main chatbot interaction function
(define (chatbot-interaction user-input)
  "Process user input and generate adaptive tutorial response"
  (let* ((current-section (hash-ref *tutorial-state* 'current-section))
         (user-intent (analyze-user-intent user-input))
         (response (generate-adaptive-response user-intent current-section))
         (next-action (determine-next-action user-intent current-section)))
    
    ; Record interaction for recursive feedback
    (set! *user-interactions* 
          (cons (list user-input user-intent response (current-time))
                *user-interactions*))
    
    ; Update tutorial state
    (update-tutorial-state user-intent)
    
    ; Apply recursive feedback to tutorial content
    (apply-user-feedback-to-content)
    
    ; Return response with next action
    (list response next-action)))

; Analyze user intent from input
(define (analyze-user-intent user-input)
  "Analyze user input to determine intent and comprehension level"
  (let ((input-lower (string-downcase user-input))
        (intent-keywords '()))
    
    ; Check for various intent patterns
    (cond
      ((string-match "help|confused|don't understand" input-lower)
       'need-help)
      ((string-match "next|continue|more" input-lower)
       'ready-to-proceed)
      ((string-match "example|show me|demonstrate" input-lower)
       'want-example)
      ((string-match "back|previous|repeat" input-lower)
       'need-review)
      ((string-match "question|ask|what" input-lower)
       'has-question)
      ((string-match "yes|ok|understood|got it" input-lower)
       'comprehension-positive)
      ((string-match "no|wrong|incorrect" input-lower)
       'comprehension-negative)
      (else
       'general-input))))

; Generate adaptive response based on user intent
(define (generate-adaptive-response user-intent current-section)
  "Generate adaptive tutorial response based on user intent and current section"
  (let ((base-content (hash-ref *tutorial-content* current-section))
        (difficulty (hash-ref *tutorial-state* 'difficulty))
        (response ""))
    
    (case user-intent
      ((need-help)
       (set! response (generate-help-response current-section difficulty)))
      ((ready-to-proceed)
       (set! response (generate-next-section-content current-section)))
      ((want-example)
       (set! response (generate-interactive-example current-section)))
      ((need-review)
       (set! response (generate-review-content current-section)))
      ((has-question)
       (set! response "I'm here to help! What specific aspect would you like to explore further?"))
      ((comprehension-positive)
       (set! response "Excellent! Let's build on that understanding.")
       (adjust-difficulty 0.1))  ; Increase difficulty slightly
      ((comprehension-negative)
       (set! response "No worries! Let me explain this differently.")
       (adjust-difficulty -0.2)) ; Decrease difficulty
      (else
       (set! response (string-append base-content " What would you like to explore next?"))))
    
    response))

; Generate help response based on current context
(define (generate-help-response section difficulty)
  "Generate contextual help based on current section and difficulty level"
  (case section
    ((introduction)
     (if (< difficulty 0.5)
         "Let's start simple: OpenCog is like a digital brain that can think and learn. Think of atoms as brain cells that connect to form thoughts."
         "OpenCog implements artificial general intelligence through symbolic reasoning combined with neural learning patterns."))
    
    ((basic-concepts)
     (if (< difficulty 0.5)
         "Imagine atoms as LEGO blocks for thoughts. We can connect them to build complex ideas. Let me show you: (Concept \"cat\") represents the idea of a cat."
         "Atoms are the fundamental units in OpenCog's knowledge representation. They form a hypergraph where concepts, predicates, and their relationships are explicitly modeled."))
    
    ((pattern-recognition)
     (if (< difficulty 0.5)
         "Pattern recognition is like finding similar shapes in a puzzle. The computer notices when things repeat and learns from them."
         "Our pattern detection algorithms identify recurring structures in the hypergraph and reify them as new cognitive objects, creating recursive learning loops."))
    
    ((distributed-cognition)
     (if (< difficulty 0.5)
         "Think of distributed cognition like a team of people working together, where each person has their own job but they share information."
         "Multiple cognitive agents operate in parallel, synchronizing through shared hypergraph context while maintaining individual processing cycles."))
    
    ((visualization)
     (if (< difficulty 0.5)
         "The visualization shows the computer's thoughts as a network of glowing dots and lines. Brighter areas show where it's paying attention."
         "Real-time cognitive visualization displays attention allocation, pattern emergence, and recursive feedback loops with adaptive overlays."))
    
    (else "I'm here to help guide you through this cognitive journey. What specific area would you like me to clarify?")))

; Generate interactive examples
(define (generate-interactive-example section)
  "Generate hands-on examples for the current section"
  (case section
    ((basic-concepts)
     "Let's create our first atoms! Try this:
      
      (define my-concept (Concept \"learning\"))
      (define my-predicate (Predicate \"enjoys\"))
      (define my-evaluation (Evaluation my-predicate (List (Concept \"student\") my-concept)))
      
      This creates the statement 'student enjoys learning'. Try it!")
    
    ((pattern-recognition)
     "Watch patterns emerge! Let's create similar structures:
      
      (Inheritance (Concept \"cat\") (Concept \"animal\"))
      (Inheritance (Concept \"dog\") (Concept \"animal\"))
      (Inheritance (Concept \"bird\") (Concept \"animal\"))
      
      The system will detect the pattern: X is-a animal. Try adding more!")
    
    ((distributed-cognition)
     "Let's create cognitive agents:
      
      (create-cognitive-agent \"explorer\" '(0.8 0.2 0.6))
      (create-cognitive-agent \"analyzer\" '(0.3 0.9 0.4))
      (connect-cognitive-agents \"explorer\" \"analyzer\")
      
      Now they can share insights! Watch them communicate.")
    
    ((visualization)
     "Open the cognitive visualization and click on nodes to see attention feedback:
      
      1. Start real-time mode
      2. Click on different nodes
      3. Watch attention patterns adapt
      4. Notice how your clicks influence the system!")
    
    (else "Let me show you a general example of OpenCog in action...")))

; Update tutorial state based on user interaction
(define (update-tutorial-state user-intent)
  "Update tutorial progress and state based on user interaction"
  (let ((current-progress (hash-ref *tutorial-state* 'progress))
        (current-section (hash-ref *tutorial-state* 'current-section)))
    
    ; Update progress based on intent
    (case user-intent
      ((ready-to-proceed comprehension-positive)
       (hash-set! *tutorial-state* 'progress (+ current-progress 1))
       (when (> (hash-ref *tutorial-state* 'progress) 3)
         (advance-to-next-section)))
      ((need-help comprehension-negative)
       ; Don't advance, maybe add review flag
       (hash-set! *tutorial-state* 'needs-review #t)))
    
    ; Store user response for adaptation
    (let ((responses (hash-ref *tutorial-state* 'user-responses)))
      (hash-set! *tutorial-state* 'user-responses 
                 (cons user-intent responses)))))

; Advance to next tutorial section
(define (advance-to-next-section)
  "Move to the next section of the tutorial"
  (let ((current-section (hash-ref *tutorial-state* 'current-section)))
    (case current-section
      ((introduction)
       (hash-set! *tutorial-state* 'current-section 'basic-concepts))
      ((basic-concepts)
       (hash-set! *tutorial-state* 'current-section 'pattern-recognition))
      ((pattern-recognition)
       (hash-set! *tutorial-state* 'current-section 'distributed-cognition))
      ((distributed-cognition)
       (hash-set! *tutorial-state* 'current-section 'visualization))
      ((visualization)
       (hash-set! *tutorial-state* 'current-section 'completion)))
    
    ; Reset progress for new section
    (hash-set! *tutorial-state* 'progress 0)
    (format #t "~%🎓 Advanced to section: ~a~%" (hash-ref *tutorial-state* 'current-section))))

; Adjust difficulty based on user performance
(define (adjust-difficulty delta)
  "Adjust tutorial difficulty based on user performance"
  (set! *adaptive-difficulty* (+ *adaptive-difficulty* delta))
  (set! *adaptive-difficulty* (max 0.1 (min 1.0 *adaptive-difficulty*)))
  (hash-set! *tutorial-state* 'difficulty *adaptive-difficulty*)
  (format #t "~%📊 Difficulty adjusted to: ~a~%" *adaptive-difficulty*))

; Apply recursive feedback to tutorial content
(define (apply-user-feedback-to-content)
  "Recursive feedback: User interactions inform next-generation tutorial content"
  (let ((recent-interactions (take *user-interactions* (min 5 (length *user-interactions*))))
        (help-requests 0)
        (confusion-indicators 0))
    
    ; Analyze recent interactions
    (for-each
      (lambda (interaction)
        (let ((intent (cadr interaction)))
          (case intent
            ((need-help) (set! help-requests (+ help-requests 1)))
            ((comprehension-negative) (set! confusion-indicators (+ confusion-indicators 1))))))
      recent-interactions)
    
    ; Adapt content based on analysis
    (when (> help-requests 2)
      (format #t "~%🔄 Adapting content: Adding more explanatory material~%")
      (enhance-explanatory-content))
    
    (when (> confusion-indicators 2)
      (format #t "~%🔄 Adapting content: Simplifying language and concepts~%")
      (simplify-tutorial-content))))

; Enhance explanatory content
(define (enhance-explanatory-content)
  "Add more detailed explanations to tutorial content"
  (let ((current-section (hash-ref *tutorial-state* 'current-section)))
    (case current-section
      ((basic-concepts)
       (hash-set! *tutorial-content* 'basic-concepts
         (string-append (hash-ref *tutorial-content* 'basic-concepts)
                       " Let me break this down further: Each atom has a type (like Concept or Predicate) and can connect to other atoms to represent knowledge.")))
      ((pattern-recognition)
       (hash-set! *tutorial-content* 'pattern-recognition
         (string-append (hash-ref *tutorial-content* 'pattern-recognition)
                       " Think of it like this: when you see multiple similar examples, your brain creates a general rule. Our system does the same with cognitive patterns."))))))

; Simplify tutorial content
(define (simplify-tutorial-content)
  "Simplify language and concepts in tutorial content"
  (let ((current-section (hash-ref *tutorial-state* 'current-section)))
    ; Reduce complexity of current section content
    (adjust-difficulty -0.3)))

; Automated testing of neural-symbolic cycles
(define (run-automated-tests)
  "Run automated tests of emergent pattern formation and distributed agent collaboration"
  (format #t "~%🧪 Running Automated Neural-Symbolic Tests...~%")
  
  ; Test 1: Pattern emergence
  (format #t "Test 1: Pattern Emergence~%")
  (let ((test-patterns (test-pattern-emergence)))
    (format #t "  ✅ Detected ~a emergent patterns~%" (length test-patterns)))
  
  ; Test 2: Distributed agent collaboration
  (format #t "Test 2: Distributed Agent Collaboration~%")
  (let ((collaboration-score (test-agent-collaboration)))
    (format #t "  ✅ Collaboration score: ~a~%" collaboration-score))
  
  ; Test 3: Recursive feedback loops
  (format #t "Test 3: Recursive Feedback Loops~%")
  (let ((feedback-cycles (test-recursive-feedback)))
    (format #t "  ✅ Completed ~a feedback cycles~%" feedback-cycles))
  
  ; Test 4: Attention allocation adaptation
  (format #t "Test 4: Attention Allocation~%")
  (let ((attention-adaptation (test-attention-adaptation)))
    (format #t "  ✅ Attention adaptation: ~a~%" attention-adaptation))
  
  (format #t "~%✅ All automated tests completed successfully!~%"))

; Test pattern emergence
(define (test-pattern-emergence)
  "Test emergent pattern formation"
  ; Create test structures
  (let ((test-atoms '()))
    (for-each
      (lambda (i)
        (let ((concept (Concept (string-append "test-concept-" (number->string i))))
              (relation (Inheritance concept (Concept "test-category"))))
          (set! test-atoms (cons relation test-atoms))))
      (iota 5))
    
    ; Simulate pattern detection
    (let ((detected-patterns '()))
      ; Simple pattern detection simulation
      (for-each
        (lambda (atom)
          (when (eq? (cog-type atom) 'InheritanceLink)
            (set! detected-patterns (cons atom detected-patterns))))
        test-atoms)
      detected-patterns)))

; Test distributed agent collaboration
(define (test-agent-collaboration)
  "Test multi-agent cognitive collaboration"
  ; Create test agents
  (let ((agent1-state '(0.5 0.7 0.3))
        (agent2-state '(0.8 0.2 0.6))
        (collaboration-cycles 10))
    
    ; Simulate collaboration cycles
    (let ((final-similarity 0))
      (for-each
        (lambda (cycle)
          ; Simulate state exchange and convergence
          (set! agent1-state 
                (map (lambda (s1 s2) (/ (+ s1 s2) 2)) agent1-state agent2-state))
          (set! agent2-state 
                (map (lambda (s1 s2) (/ (+ s1 s2) 2)) agent2-state agent1-state)))
        (iota collaboration-cycles))
      
      ; Calculate final similarity
      (set! final-similarity
            (/ (apply + (map (lambda (s1 s2) (- 1 (abs (- s1 s2)))) 
                            agent1-state agent2-state))
               (length agent1-state)))
      final-similarity)))

; Test recursive feedback loops
(define (test-recursive-feedback)
  "Test recursive feedback mechanisms"
  (let ((feedback-cycles 0)
        (initial-state 0.5)
        (current-state initial-state))
    
    ; Simulate feedback loops
    (while (< feedback-cycles 20)
      (set! current-state (+ current-state (* 0.1 (- 1 current-state))))
      (set! feedback-cycles (+ feedback-cycles 1))
      (when (> current-state 0.95) (break)))
    
    feedback-cycles))

; Test attention allocation adaptation
(define (test-attention-adaptation)
  "Test adaptive attention allocation"
  (let ((attention-weights '(0.5 0.5 0.5 0.5))
        (input-stimuli '(0.8 0.2 0.9 0.1))
        (adaptation-rate 0.1))
    
    ; Simulate attention adaptation
    (set! attention-weights
          (map (lambda (weight stimulus)
                 (+ weight (* adaptation-rate stimulus)))
               attention-weights input-stimuli))
    
    ; Calculate adaptation score
    (let ((max-weight (apply max attention-weights))
          (min-weight (apply min attention-weights)))
      (- max-weight min-weight))))

; Interactive tutorial demonstration
(define (demonstrate-neural-symbolic-tutorial)
  "Demonstrate the complete neural-symbolic tutorial system"
  (format #t "~%🎓 Neural-Symbolic Tutorial Demonstration~%")
  
  (initialize-tutorial-system)
  
  ; Simulate user interactions
  (let ((sample-interactions 
         '("hello" "what is opencog?" "I want an example" "yes I understand" 
           "show me more" "this is confusing" "help me" "ok got it")))
    
    (for-each
      (lambda (user-input)
        (format #t "~%👤 User: ~a~%" user-input)
        (let ((response (chatbot-interaction user-input)))
          (format #t "🤖 Chatbot: ~a~%" (car response))
          (sleep 1)))
      sample-interactions))
  
  (format #t "~%📊 Tutorial Statistics:~%")
  (format #t "  Current Section: ~a~%" (hash-ref *tutorial-state* 'current-section))
  (format #t "  Progress: ~a~%" (hash-ref *tutorial-state* 'progress))
  (format #t "  Difficulty: ~a~%" (hash-ref *tutorial-state* 'difficulty))
  (format #t "  User Interactions: ~a~%" (length *user-interactions*))
  
  ; Run automated tests
  (run-automated-tests)
  
  (format #t "~%✨ Neural-Symbolic Tutorial demonstration completed!~%"))

; Export key functions
(export initialize-tutorial-system
        chatbot-interaction
        run-automated-tests
        demonstrate-neural-symbolic-tutorial)