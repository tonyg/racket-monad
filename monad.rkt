#lang racket/base
;; Monads in Racket, including polymorphic bind, return and fail.
;; Haskell-like do-notation.

(provide define-monad-class
         (struct-out monad-class)
         monad?
         gen:monad
         monad->monad-class
         determine-monad

         bind
         return
         fail

         <-
         do
         lift1

         Identity
         List
         Stream

         State
         run-st
         eval-st
         exec-st
         sget
         sput

         IO
         (struct-out io-return)
         (struct-out io-begin)
         run-io
         mprintf
         mdisplay
         mnewline
         mread)

(require (for-syntax racket/base))
(require racket/generic)
(require racket/match)
(require (only-in racket/list append-map))

(define-generics monad
  (monad->monad-class monad)
  #:defaults ([null? (define (monad->monad-class m) List)]
              [pair? (define (monad->monad-class m) List)]
              [stream? (define (monad->monad-class m) Stream)]
              [exn? (define (monad->monad-class m) Exception)]))

;; A MonadClass represents the behaviour associated with a given monad
;; type. MonadClass instances are callable: if M is a MonadClass, then
;; (M ma) will attempt to coerce ma into the monad M.
(struct monad-class
  (;; Symbol
   ;; The name of the monad class; used in error messages.
   name
   ;; (M a) (b -> (M b)) -> (M b)
   ;; Monadic bind operator for this monad.
   binder
   ;; a -> (M a)
   ;; Monadic return operator for this monad.
   returner
   ;; exn -> (M a)
   ;; Fail operator for this monad. Usually `raise`.
   failer
   ;; (M a) N -> (N a)
   ;; Convert or coerce a monad instance into a *possibly-different
   ;; monad* -- so long as it is compatible! This is used to get
   ;; return polymorphism: the Identity monad is considered
   ;; convertible into *any other monad*. It is also used to build up
   ;; chains of binds until we learn which monad we should be working
   ;; in: the Indeterminate monad-class below waits to learn the
   ;; intended monad, and re-applies the bind operation with stored
   ;; arguments.
   determiner
   )
  #:transparent
  #:property prop:procedure
  (lambda (N ma) ((monad-class-determiner (monad->monad-class ma)) N ma)))

;; Intermediate monad-class constructor with defaults for the failer
;; and determiner functions.
(define (monad-class* name binder returner
                      #:fail [failer raise]
                      #:determine [determiner determine-monad])
  (monad-class name binder returner failer determiner))

;; Syntax for defining MonadClasses.
(define-syntax-rule (define-monad-class Name rest ...)
  (define Name (monad-class* 'Name rest ...)))

;; Constructor for polymorphic return. Uses the Identity MonadClass to
;; *later* convert itself into the intended monad, once the type is
;; known.
(struct return (value) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) Identity)])

;; Approximately the Identity monad (wrapped in instances of struct
;; `return`). Can be coerced into any other monad type by calling that
;; monad's returner procedure with the value carried in the `return`
;; struct.
(define-monad-class Identity
  (lambda (r f) (f (return-value r)))
  return
  #:determine (lambda (N r) ((monad-class-returner N) (return-value r))))

;; Represents a (chain of) delayed bind operation(s). When converted
;; into a specific monad, performs the delayed binds in the target
;; monad.
(struct indeterminate-bind (ma a->mb) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) Indeterminate)])

;; Determines an instance by rebinding its pieces using the target
;; monad's bind operation.
(define-monad-class Indeterminate
  indeterminate-bind
  return
  #:determine (lambda (N m) (bind (N (indeterminate-bind-ma m)) (indeterminate-bind-a->mb m))))

;; Exceptions represent pending failures, and like Identity and
;; Indeterminate are not in any particular monad until determined.
(define-monad-class Exception
  indeterminate-bind
  return
  #:determine (lambda (N e) ((monad-class-failer N) e)))

;; User-level API to monadic bind.
(define (bind ma a->mb) ((monad-class-binder (monad->monad-class ma)) ma a->mb))

;; User-level API to monadic failure.
;; Simply constructs and returns an exception, *without* raising it.
(define (fail fmt . args) (exn:fail (apply format fmt args) (current-continuation-marks)))

;; Default determiner for MonadClasses. Most monads will use this
;; determiner. Checks the monad instance: if it is already of the
;; desired type, return it; otherwise, raise an exception.
(define (determine-monad N ma)
  (if (eq? (monad->monad-class ma) N)
      ma
      (error 'determine-monad
             "Monad-class ~a is incompatible with required monad-class ~a"
             (monad-class-name (monad->monad-class ma))
             (monad-class-name N))))

;;---------------------------------------------------------------------------
;; Haskell-like do-notation

(define-syntax <-
  (lambda (stx)
    (raise-syntax-error #f "Illegal use of <- outside monadic (do)" stx)))

(define-syntax do
  (syntax-rules (<-)
    [(_ mexp) mexp]
    [(_ #:let [pat exp] rest ...)
     (match-let ((pat exp)) (do rest ...))]
    [(_ pat <- mexp rest ...)
     (bind mexp (match-lambda
                 [pat (do rest ...)]
                 [_ (fail "monadic (do) pattern failure: ~v" #'pat)]))]
    [(_ #:guard exp rest ...)
     (if exp (do rest ...) (fail "monadic (do) guard failed: ~v" #'exp))]
    [(_ mexp rest ...)
     (bind mexp (lambda (ignored) (do rest ...)))]))

;; (a -> b) -> (M a) -> (M b)
(define ((lift1 f) m)
  (do i <- m
      (return (f i))))

;;---------------------------------------------------------------------------
;; List monad

(define-monad-class List
  (lambda (xs f) (append-map (lambda (x) (List (f x))) xs))
  (lambda (x) (list x))
  #:fail (lambda (e) '())
  #:determine (lambda (N xs) (if (eq? N Stream) xs (determine-monad N xs))))

;;---------------------------------------------------------------------------
;; Stream monad

(require racket/stream)

(define-monad-class Stream
  (lambda (s f)
    (if (stream-empty? s)
        empty-stream
        (let walk ((items (Stream (f (stream-first s)))))
          (if (stream-empty? items)
              (bind (stream-rest s) f)
              (stream-cons (stream-first items) (walk (stream-rest items)))))))
  (lambda (x) (stream-cons x empty-stream))
  #:fail (lambda (e) empty-stream)
  #:determine (lambda (N s) (if (eq? N List) (stream->list s) (determine-monad N s))))

;;---------------------------------------------------------------------------
;; State monad

(struct state (transformer) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) State)])

(define-monad-class State
  (lambda (st f) (state (lambda (s0)
                          (define-values (v s1) (run-st st s0))
                          (run-st (f v) s1))))
  (lambda (v) (state (lambda (s0) (values v s0)))))

;; (State s a) s -> (Values a s)
(define (run-st m initial)
  ((state-transformer (State m)) initial))

;; (State s a) s -> a
(define (eval-st m initial)
  (define-values (v final) (run-st m initial))
  v)

;; (State s a) s -> s
(define (exec-st m initial)
  (define-values (v final) (run-st m initial))
  final)

;; (State s a)
(define sget (state (lambda (s0) (values s0 s0))))

;; s -> (State s Void)
(define (sput a) (state (lambda (s0) (values (void) a))))

;;---------------------------------------------------------------------------

;; Represents a single atomic delayed IO action. Calling the nullary
;; io-return-thunk performs the action.
(struct io-return (thunk) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) IO)])

;; Represents a chain of IO actions. The continuation, io-begin-k,
;; takes the result of the previous action, io-begin-io, and yields
;; another IO action to be performed.
(struct io-begin (io k) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) IO)])

(define-monad-class IO
  io-begin
  (lambda (v) (io-return (lambda () v))))

;; Performs a sequence of IO actions.
(define (run-io io)
  (match (IO io)
    [(io-return thunk) (thunk)]
    [(io-begin io k) (run-io (call-with-values (lambda () (run-io io)) k))]))

;; Constructors for primitive IO actions.
(define (mprintf fmt . args) (io-return (lambda () (apply printf fmt args))))
(define (mdisplay x) (io-return (lambda () (display x))))
(define mnewline (io-return newline))
(define mread (io-return (lambda () (read))))

;;---------------------------------------------------------------------------
;; Examples

(module+ examples
  (provide (all-defined-out))

  ;; (IO Symbol)
  (define io-demo
    (do (mdisplay "Enter a number: ")
        n <- mread
        (if (< n 10)
            (do (mdisplay "It's less than ten. Try again.")
                mnewline
                io-demo)
            (do (mdisplay "It's greater than or equal to ten.\n")
                (return 'done)))))

  ;; (State Int Int)
  (define tick
    (do n <- sget
        (sput (+ n 1))
        (return n)))

  ;; (IO (List Int))
  (define oleg-example-mixed-monad
    (do (mdisplay "Enter a number: ")
        n <- mread
        all-n <- (return (for/list ((i n)) i))
        evens <- (return (List (do i <- all-n
                                   #:guard (even? i)
                                   (return i))))
        #:let [count (length evens)]
        (mprintf "Computed ~a evens." count)
        mnewline
        (return evens))))

(module+ main
  (require (submod ".." examples))

  (run-io (do (mprintf "After three ticks: ~a\n" (eval-st (do tick
                                                              tick
                                                              tick
                                                              sget)
                                                          0))
              io-demo
              mnewline
              (mdisplay "Next example!")
              mnewline
              mnewline
              result <- oleg-example-mixed-monad
              (mdisplay "Evens: ")
              (mdisplay result)
              mnewline)))
