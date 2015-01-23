#lang racket/base

(provide (struct-out monad-class)
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
              [exn? (define (monad->monad-class m) Exception)]))

(struct monad-class (name ;; Symbol
                     binder ;; (M a) (b -> (M b)) -> (M b)
                     returner ;; a -> (M a)
                     failer ;; exn -> (M a)
                     determiner ;; (M a) N -> (N a)
                     )
        #:transparent
        #:property prop:procedure (lambda (N ma)
                                    ((monad-class-determiner (monad->monad-class ma)) N ma)))

(struct return (value) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) Identity)])

(define Identity (monad-class 'Identity
                              (lambda (r f) (f (return-value r)))
                              return
                              raise
                              (lambda (N r) ((monad-class-returner N) (return-value r)))))

(struct indeterminate-bind (ma a->mb) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) Indeterminate)])

(define Indeterminate (monad-class 'Indeterminate
                                   indeterminate-bind
                                   return
                                   raise
                                   (lambda (N m) (bind (N (indeterminate-bind-ma m))
                                                       (indeterminate-bind-a->mb m)))))

(define Exception (monad-class 'Exception
                               indeterminate-bind
                               return
                               raise
                               (lambda (N e) ((monad-class-failer N) e))))

(define (bind ma a->mb) ((monad-class-binder (monad->monad-class ma)) ma a->mb))
(define (fail fmt . args) (exn:fail (apply format fmt args) (current-continuation-marks)))

(define (determine-monad N ma)
  (if (eq? (monad->monad-class ma) N)
      ma
      (error 'determine-monad
             "Monad-class ~a is incompatible with required monad-class ~a"
             (monad-class-name (monad->monad-class ma))
             (monad-class-name N))))

;;---------------------------------------------------------------------------

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

(define ((lift1 f) m)
  (do i <- m
      (return (f i))))

;;---------------------------------------------------------------------------

(define List (monad-class 'List
                          (lambda (xs f) (append-map (lambda (x) (List (f x))) xs))
                          (lambda (x) (list x))
                          (lambda (e) '())
                          determine-monad))

;;---------------------------------------------------------------------------

(define State (monad-class 'State
                           (lambda (st f) (state (lambda (s0)
                                                   (define-values (v s1) (run-st st s0))
                                                   (run-st (f v) s1))))
                           (lambda (v) (state (lambda (s0) (values v s0))))
                           raise
                           determine-monad))

(struct state (transformer) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) State)])

(define (run-st m initial)
  ((state-transformer (State m)) initial))

(define (eval-st m initial)
  (define-values (v final) (run-st m initial))
  v)

(define (exec-st m initial)
  (define-values (v final) (run-st m initial))
  final)

(define sget (state (lambda (s0) (values s0 s0))))
(define (sput a) (state (lambda (s0) (values (void) a))))

;;---------------------------------------------------------------------------

(define IO (monad-class 'IO
                        (lambda (io f) (io-begin io f))
                        (lambda (v) (io-return (lambda () v)))
                        raise
                        determine-monad))

(struct io-return (thunk) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) IO)])

(struct io-begin (io k) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) IO)])

(define (run-io io)
  (match (IO io)
    [(io-return thunk) (thunk)]
    [(io-begin io k) (run-io (call-with-values (lambda () (run-io io)) k))]))

(define (mprintf fmt . args) (io-return (lambda () (apply printf fmt args))))
(define (mdisplay x) (io-return (lambda () (display x))))
(define mnewline (io-return newline))
(define mread (io-return (lambda () (read))))

;;---------------------------------------------------------------------------

(module+ examples
  (provide (all-defined-out))

  (define io-demo
    (do (mdisplay "Enter a number: ")
        n <- mread
        (if (< n 10)
            (do (mdisplay "It's less than ten. Try again.")
                mnewline
                io-demo)
            (do (mdisplay "It's greater than or equal to ten.\n")
                (return 'done)))))

  (define tick
    (do n <- sget
        (sput (+ n 1))
        (return n)))

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
  (printf "After three ticks: ~a\n"
          (eval-st (do tick
                       tick
                       tick
                       sget)
                   0))
  (for-each run-io (list io-demo
                         mnewline
                         (mdisplay "Next example!")
                         mnewline
                         mnewline
                         (do result <- oleg-example-mixed-monad
                             (mdisplay "Evens: ")
                             (mdisplay result)
                             mnewline))))
