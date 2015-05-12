#lang racket/base
;; IO monad

(provide IO
         (struct-out io-return)
         (struct-out io-begin)
         run-io
         mprintf
         mdisplay
         mnewline
         mread)

(require racket/match)
(require "main.rkt")

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
