#lang racket/base
;; State monad

(provide State
         run-st
         eval-st
         exec-st
         sget
         sput)

(require "main.rkt")

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
