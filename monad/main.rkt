#lang racket/base
;; Monads in Racket, including polymorphic bind, return and fail.
;; Haskell-like do-notation.

(provide define-monad-class
         (struct-out monad-class)
         monad?
         gen:monad
         monad->monad-class
         determine-monad
         co-determine-monad

         bind
         return
         fail

         <-
         do
         lift1

         Identity
         List
         Stream)

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
   ;; N (M a) -> (N a)
   ;; Convert or coerce a monad instance into a *possibly-different
   ;; monad* -- so long as it is compatible! This is used to get
   ;; return polymorphism: the Identity monad is considered
   ;; convertible into *any other monad*. It is also used to build up
   ;; chains of binds until we learn which monad we should be working
   ;; in: the Indeterminate monad-class below waits to learn the
   ;; intended monad, and re-applies the bind operation with stored
   ;; arguments. // An instance of the double-dispatch pattern;
   ;; "determiner" of M is called first, and delegates to
   ;; "co-determiner" of its N argument if it is failing.
   determiner
   co-determiner
   )
  #:transparent
  #:property prop:procedure
  (lambda (N ma) ((monad-class-determiner (monad->monad-class ma)) N ma)))

;; Intermediate monad-class constructor with defaults for the failer
;; and determiner functions.
(define (monad-class* name binder returner
                      #:fail [failer raise]
                      #:determine [determiner determine-monad]
                      #:co-determine [co-determiner co-determine-monad])
  (monad-class name binder returner failer determiner co-determiner))

;; Default determiner for MonadClasses. Most monads will use this
;; determiner. Checks the monad instance: if it is already of the
;; desired type, return it; otherwise, delegate to N's co-determiner.
(define (determine-monad N ma)
  (if (eq? (monad->monad-class ma) N)
      ma
      ((monad-class-co-determiner N) N ma)))

;; Default co-determiner for MonadClasses.
(define (co-determine-monad N ma)
  (error 'co-determine-monad
         "Monad-class ~a is incompatible with required monad-class ~a"
         (monad-class-name (monad->monad-class ma))
         (monad-class-name N)))

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
  #:fail (lambda (e) '()))

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
  #:determine (lambda (N s) (if (eq? N List) (stream->list s) (determine-monad N s)))
  #:co-determine (lambda (N ma) (if (list? ma) ma (co-determine-monad N ma))))