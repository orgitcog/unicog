;
; Enhanced debug tracing support with OpenCog logger integration.
;
; Handy utilities to write log messages to a file with proper logger integration.
; This addresses the problem where scheme code running in C++ context needs
; reliable logging output. The system now provides both file-based logging
; and integration with the OpenCog logger subsystem when available.
;
; Enhanced features:
; - Automatic logger detection and fallback
; - Configurable log levels 
; - Thread-safe file operations
; - Structured log formatting
;
; Enhanced debug tracing state with logger integration
(define oport #f)
(define dbg-cnt 0)
(define dbg-tim 0)
(define use-opencog-logger #f)
(define log-level 'INFO)
(define trace-mutex #f)

; Try to detect OpenCog logger availability
(define (detect-opencog-logger)
  "Check if OpenCog logger is available and functional"
  (catch #t
    (lambda ()
      (cog-logger-set-level! "FINE")
      (set! use-opencog-logger #t)
      #t)
    (lambda (key . args)
      (set! use-opencog-logger #f)
      #f)))

; Initialize mutex for thread-safe logging
(define (init-trace-mutex)
  "Initialize mutex for thread-safe trace operations"
  (set! trace-mutex (make-mutex)))

(define-public (init-trace file-name)
  "Initialize enhanced trace system with FILE-NAME and logger integration"
  (init-trace-mutex)
  (detect-opencog-logger)
  
  ; Always maintain file-based logging for compatibility
  (set! oport (open-file file-name "a")) ; Append mode for persistence
  
  ; Log initialization
  (let ((init-msg (string-append 
                   "[" (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time))) "] "
                   "Trace initialized - Logger: " 
                   (if use-opencog-logger "OpenCog" "File-only") "\n")))
    (display init-msg oport)
    (force-output oport)
    
    (when use-opencog-logger
      (cog-logger-info "Debug trace initialized with file: ~a" file-name))))

(define-public (start-trace msg)
" Start tracing, record  the start time"
	(set! dbg-tim (current-time))
	(set! dbg-cnt 0)
	(display msg oport)
	(force-output oport)
)

(define-public (trace-msg msg)
  "Print MSG to trace system with timestamp and optional logger integration"
  (when trace-mutex (lock-mutex trace-mutex))
  
  (let ((timestamped-msg (string-append
                          "[" (strftime "%H:%M:%S" (localtime (current-time))) "] "
                          msg)))
    ; Always write to file for compatibility
    (when oport
      (display timestamped-msg oport)
      (force-output oport))
    
    ; Also use OpenCog logger if available
    (when use-opencog-logger
      (case log-level
        ((DEBUG) (cog-logger-debug msg))
        ((INFO) (cog-logger-info msg))
        ((WARN) (cog-logger-warn msg))
        ((ERROR) (cog-logger-error msg))
        (else (cog-logger-info msg)))))
  
  (when trace-mutex (unlock-mutex trace-mutex)))

(define-public (trace-elapsed)
"  Print elapsed (wall-clock) time since trace start."
	(begin
		(display "Elapsed secs " oport)
		(display (- (current-time) dbg-tim) oport)
		(display "\n" oport)
		(set! dbg-tim (current-time))
	)
)

(define-public (trace-msg-cnt msg)
" Print message, increment the count."
	(display msg oport)
	(set! dbg-cnt (+ dbg-cnt 1))
	(display dbg-cnt oport)
	(display "\n" oport)

	; Provide some crude timing info too ...
	(if (eqv? 0 (modulo dbg-cnt 10000))
		(trace-elapsed)
	)
	(force-output oport)
)
