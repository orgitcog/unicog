;
; tensor-kernel.scm
;
; Scheme bindings and utilities for the unified GGML tensor kernel
;

(define-module (opencog tensor-kernel))

(use-modules (opencog))

; Load the C++ module
(load-extension "libggml-tensor-scm" "opencog_tensor_kernel_init")

; Tensor shape definitions for cognitive modules
(define cogutil-shape '(64 32 16))
(define atomspace-shape '(1024 512 256))
(define cogserver-shape '(128 64 32))
(define relex-shape '(256 128 64))
(define pln-shape '(512 256 128))
(define ecan-shape '(256 128 64))
(define moses-shape '(512 256 128))
(define ghost-shape '(256 128 64))
(define loving-ai-shape '(128 64 32))
(define game-ai-shape '(256 128 64))

; Initialize the tensor kernel with standard modules
(define (initialize-tensor-kernel)
  "Initialize the unified tensor kernel with standard OpenCog modules"
  (let ((kernel (cog-create-tensor-kernel (cog-atomspace))))
    (cog-register-cognitive-module "cogutil" cogutil-shape)
    (cog-register-cognitive-module "atomspace" atomspace-shape)
    (cog-register-cognitive-module "cogserver" cogserver-shape)
    (cog-register-cognitive-module "relex" relex-shape)
    (cog-register-cognitive-module "pln" pln-shape)
    (cog-register-cognitive-module "ecan" ecan-shape)
    (cog-register-cognitive-module "moses" moses-shape)
    (cog-register-cognitive-module "ghost" ghost-shape)
    (cog-register-cognitive-module "loving-ai" loving-ai-shape)
    (cog-register-cognitive-module "game-ai" game-ai-shape)
    kernel))

; Tensor-based AtomSpace operations
(define (tensor-map-atoms atoms)
  "Map a list of atoms to tensor representation"
  (cog-map-atoms-to-tensor atoms))

(define (tensor-allocate-attention atoms context)
  "Allocate attention to atoms using tensor operations"
  (cog-allocate-attention atoms context))

; Grammar registry functions
(define (tensor-execute-grammar function-name . args)
  "Execute a registered grammar function with tensor backend"
  (cog-execute-grammar-function function-name args))

; Neural-symbolic integration
(define (symbolic-to-neural atom)
  "Convert symbolic atom to neural tensor representation"
  (tensor-execute-grammar "symbolic-to-neural" atom))

(define (neural-to-symbolic tensor-handle)
  "Convert neural tensor to symbolic atom representation"
  (tensor-execute-grammar "neural-to-symbolic" tensor-handle))

; Attention allocation helpers
(define (ecan-allocate-attention atoms)
  "Allocate attention using ECAN-style economic flow"
  (tensor-allocate-attention atoms "ecan"))

(define (focus-attention atoms focus-type)
  "Focus attention on specific atoms with given type"
  (tensor-allocate-attention atoms focus-type))

; Pattern matching with neural enhancement
(define (neural-pattern-match pattern target)
  "Pattern match with neural guidance"
  (tensor-execute-grammar "neural-pattern-match" pattern target))

; Distributed execution
(define (distributed-tensor-operation operation-name inputs)
  "Execute tensor operation in distributed mode"
  (cog-execute-tensor-operation operation-name inputs))

; Cognitive primitives
(define (cognitive-primitive primitive-name atom)
  "Encode atom using cognitive primitive"
  (tensor-execute-grammar primitive-name atom))

; Emergent pattern detection
(define (detect-emergent-patterns atoms)
  "Detect emergent patterns in atom set using tensor operations"
  (tensor-execute-grammar "detect-emergent-patterns" atoms))

; Hypergraph tensor encoding
(define (encode-hypergraph atoms)
  "Encode hypergraph structure as tensor"
  (tensor-execute-grammar "encode-hypergraph" atoms))

(define (decode-hypergraph tensor-handle)
  "Decode tensor back to hypergraph structure"
  (tensor-execute-grammar "decode-hypergraph" tensor-handle))

; Information and debugging
(define (tensor-kernel-status)
  "Get tensor kernel status information"
  (cog-tensor-kernel-info))

(define (print-tensor-kernel-info)
  "Print tensor kernel information"
  (display (tensor-kernel-status))
  (newline))

; Cognitive flow orchestration
(define (cognitive-flow-execute flow-name atoms)
  "Execute a cognitive flow with tensor operations"
  (case flow-name
    ('perception-to-action
     (let* ((tensor-repr (tensor-map-atoms atoms))
            (attention-mask (ecan-allocate-attention atoms))
            (patterns (detect-emergent-patterns atoms)))
       patterns))
    ('reasoning-chain
     (let* ((symbolic-knowledge atoms)
            (neural-context (symbolic-to-neural (car atoms)))
            (inference-result (tensor-execute-grammar "pln-infer" atoms)))
       inference-result))
    ('attention-flow
     (let* ((attention-weights (ecan-allocate-attention atoms))
            (focused-atoms (focus-attention atoms "high-priority")))
       focused-atoms))
    (else
     (error "Unknown cognitive flow:" flow-name))))

; Unified cognitive grammar composition
(define (compose-cognitive-grammars . grammar-names)
  "Compose multiple cognitive grammars into unified operation"
  (lambda (atoms)
    (fold (lambda (grammar-name acc-atoms)
            (tensor-execute-grammar grammar-name acc-atoms))
          atoms
          grammar-names)))

; Export main functions
(export initialize-tensor-kernel
        tensor-map-atoms
        tensor-allocate-attention
        tensor-execute-grammar
        symbolic-to-neural
        neural-to-symbolic
        ecan-allocate-attention
        focus-attention
        neural-pattern-match
        distributed-tensor-operation
        cognitive-primitive
        detect-emergent-patterns
        encode-hypergraph
        decode-hypergraph
        tensor-kernel-status
        print-tensor-kernel-info
        cognitive-flow-execute
        compose-cognitive-grammars)