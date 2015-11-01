#lang racket

(provide (contract-out
          [number->list (-> natural-number/c list?)]
          [join-numbers (->* () () #:rest (or/c (listof number?)
                                                list?) number?)]))

; Split natural number into a list of numbers. Ex: 4321 -> '(4 3 2 1)
(define (number->list n)
  (filter identity
          (map
           (compose string->number string)
           (string->list (number->string n)))))

; Join numbers together, like a string. Ex: '(-9 1.1) -> -91.1
(define (join-numbers . l)
    (string->number (string-join (map number->string (flatten l)) "")))

(module+ test
  (require rackunit)

  (test-equal? "number->list makes a list of numbers"
               (number->list 4321) '(4 3 2 1))

  (test-equal? "join-numbers combines a number as if it was a string"
               (join-numbers '(-9 1.1)) -91.1)

  (test-equal? "join-numbers combines a number as if it was a string"
               (join-numbers -9 1.1) -91.1))