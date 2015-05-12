#lang racket

(provide (all-defined-out))

(require "../main.rkt")
(require "../state.rkt")
(require "../io.rkt")

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
      (return evens)))

(module+ main
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
