#lang racket/base

(provide (struct-out monad-class)
         monad?
         gen:monad
         monad->monad-class
         determine

         bind
         return
         fail

         List
         run-list

         IO
         (struct-out io-return)
         (struct-out io-begin)
         run-io
         mdisplay
         mnewline
         mread)

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

(define (determine a)
  (define (walk ma)
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
      [(indeterminate-bind mb b->mc) (bind (walk mb) b->mc)]
      [_ (error 'determine "Could not coerce ~v into monad class ~a" ma (monad-class-name a))]))
  walk)

(define (bind ma a->mb)
  (cond
   [(monad? ma)
    (define a (monad->monad-class ma))
    ((determine a) ((monad-class-binder a) ma a->mb))]
   [(return? ma) (a->mb (return-value ma))]
   [(or (exn? ma) (indeterminate-bind? ma)) (indeterminate-bind ma a->mb)]
   [else (error 'bind "Could not interpret ~v as a monad" ma)]))

(define (fail fmt . args) (exn:fail (apply format fmt args) (current-continuation-marks)))

;;---------------------------------------------------------------------------

(define List (monad-class 'List
                          (lambda (xs f) (append-map (compose (determine List) f) xs))
                          (lambda (x) (list x))
                          (lambda (e) '())))

(define (run-list xs) ((determine List) xs))

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
  (match ((determine IO) io)
    [(io-return thunk) (thunk)]
    [(io-begin io k) (run-io (call-with-values (lambda () (run-io io)) k))]))

(define (mdisplay x) (io-return (lambda () (display x))))
(define mnewline (io-return newline))
(define mread (io-return (lambda () (read))))

;;---------------------------------------------------------------------------

(module+ examples
  (provide (all-defined-out))

  (define-syntax mlet*
    (syntax-rules ()
      ((_ () mexpN) mexpN)
      ((_ ((pat mexp) rest ...) mexpN)
       (bind mexp (match-lambda
                   [pat (mlet* (rest ...) mexpN)]
                   [_ (fail "mlet* pattern failure: ~v" #'pat)])))))

  (define io-demo
    (mlet* ((_ (mdisplay "Enter a number: "))
            (n mread))
           (if (< n 10)
               (mlet* ((_ (mdisplay "It's less than ten. Try again."))
                       (_ mnewline))
                      io-demo)
               (mlet* ((_ (mdisplay "It's greater than or equal to ten.\n")))
                      (return 'done)))))

  (define oleg-example-mixed-monad
    (mlet* ((_ (mdisplay "Enter a number: "))
            (n mread)
            (all-n (return (for/list ((i n)) i)))
            (evens (return (run-list (mlet* ((i all-n))
                                            (if (even? i)
                                                (return i)
                                                (fail "odd"))))))
            (_ (mdisplay "Computed "))
            (_ (mdisplay (length evens)))
            (_ (mdisplay " evens."))
            (_ mnewline))
           (return evens))))

(module+ main
  (require (submod ".." examples))
  (run-io io-demo)
  (run-io mnewline)
  (run-io (mdisplay "Next example!"))
  (run-io mnewline)
  (run-io mnewline)
  (run-io oleg-example-mixed-monad))