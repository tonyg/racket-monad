#lang racket/base

(provide (struct-out monad-class)
         monad?
         gen:monad
         monad->monad-class
         determine

         bind
         return
         fail

         <-
         do
         lift1

         List
         run-list

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

(struct monad-class (name ;; Symbol
                     binder ;; (M a) (b -> (M b)) -> (M b)
                     returner ;; a -> (M a)
                     failer ;; exn -> (M a)
                     ) #:transparent)

(struct return (value) #:transparent)
(struct indeterminate-bind (ma a->mb) #:transparent)

(define-generics monad
  (monad->monad-class monad)
  #:fast-defaults ([null? (define (monad->monad-class m) List)]
                   [pair? (define (monad->monad-class m) List)]))

(define (determine a ma)
  (match ma
    [(? monad?)
     (if (eq? (monad->monad-class ma) a)
         ma
         (error 'determine
                "Monad-class ~a is incompatible with required monad-class ~a"
                (monad-class-name (monad->monad-class ma))
                (monad-class-name a)))]
    [(return v) ((monad-class-returner a) v)]
    [(? exn?) ((monad-class-failer a) ma)]
    [(indeterminate-bind mb b->mc) (bind (determine a mb) b->mc)]
    [_ (error 'determine "Could not coerce ~v into monad class ~a" ma (monad-class-name a))]))

(define (bind ma a->mb)
  (cond
   [(monad? ma)
    (define a (monad->monad-class ma))
    (determine a ((monad-class-binder a) ma a->mb))]
   [(return? ma) (a->mb (return-value ma))]
   [(or (exn? ma) (indeterminate-bind? ma)) (indeterminate-bind ma a->mb)]
   [else (error 'bind "Could not interpret ~v as a monad" ma)]))

(define (fail fmt . args) (exn:fail (apply format fmt args) (current-continuation-marks)))

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
    [(_ mexp rest ...)
     (bind mexp (lambda (ignored) (do rest ...)))]))

(define ((lift1 f) m)
  (do i <- m
      (return (f i))))

;;---------------------------------------------------------------------------

(define List (monad-class 'List
                          (lambda (xs f) (append-map (lambda (x) (determine List (f x))) xs))
                          (lambda (x) (list x))
                          (lambda (e) '())))

(define (run-list xs) (determine List xs))

;;---------------------------------------------------------------------------

(define State (monad-class 'State
                           (lambda (st f) (state (lambda (s0)
                                                   (define-values (v s1) (run-st st s0))
                                                   (run-st (f v) s1))))
                           (lambda (v) (state (lambda (s0) (values v s0))))
                           raise))

(struct state (transformer) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) State)])

(define (run-st m initial)
  ((state-transformer (determine State m)) initial))

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
                        raise))

(struct io-return (thunk) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) IO)])

(struct io-begin (io k) #:transparent
        #:methods gen:monad [(define (monad->monad-class m) IO)])

(define (run-io io)
  (match (determine IO io)
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
        evens <- (return (run-list (do i <- all-n
                                       (if (even? i)
                                           (return i)
                                           (fail "odd")))))
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
