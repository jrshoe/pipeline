#lang racket
(require racket/stxparam)

(provide pipeline
         steps
         stage
         env
         echo
         setup
         sh
         current-stage
         )

(define-syntax pipeline
  (syntax-rules ()
    [(pipeline stage ...)
     (begin
       stage
       ...)]))

(define-syntax steps
  (syntax-rules ()
    [(steps step ...) (begin
                       ; (printf "Running steps at ~a\n" current-stage)
                       ; (printf "[~a] running steps\n" current-stage)
                        step
                        ...)]))

(define-syntax-parameter current-stage
  (lambda (stx)
    (raise-syntax-error 'current-stage
                        "can't use current-stage outside of stage")))

(define-syntax (stage stx)
  (syntax-case stx ()
   [(_ label step ...)
    #'(begin
        ; (printf "Running stage ~a\n" label)
        ; (printf "[~a] beginning stage\n" label)
        (syntax-parameterize 
            ([current-stage (syntax-id-rules () [_ label])])
           step ...))]))

(define-syntax echo
  (syntax-rules ()
    [(echo msg)
     (begin
       (sleep 2))]))

(define-syntax sh
  (syntax-rules ()
    [(sh cmd)
     (begin
       (sleep 5))]))

(define-syntax setup
  (syntax-rules ()
    [(setup lang)
     (begin
       (sleep 10))]))

(define-syntax env
  (syntax-rules ()
    [(env var val)
     (begin
       (sleep 5))]))