#lang racket
(require gregor)
(require gregor/time)
(require "pipeline.rkt")

(define (logging msg)
  (let ([current-date (date->iso8601 (today))])
    (display-lines-to-file `(,msg) current-date #:exists 'append)))

(define-syntax log-pipeline
  (syntax-rules ()
    [(log-pipeline stage ...)
     (begin
       (logging "[pipeline] start pipeline building")
       (pipeline stage ...))]))

(define-syntax log-steps
  (syntax-rules ()
    [(log-steps step ...)
     (begin
       (logging (format "[~a] running steps" current-stage))
       (steps step ...))]))

(define-syntax log-stage
  (syntax-rules ()
    [(log-stage label step ...)
     (begin
       (logging (format "[~a] beginning stage" label))
       (stage label step ...))]))

(define-syntax log-echo
  (syntax-rules ()
    [(log-echo msg)
     (begin
       (logging (format "[~a] ~a" current-stage msg))
       (echo msg))]))

(define-syntax log-sh
  (syntax-rules ()
    [(log-sh cmd)
     (begin
       (logging (format "[~a] excuting ~a" current-stage cmd))
       (sh cmd))]))

(define-syntax log-env
  (syntax-rules ()
    [(log-env var val)
     (begin
       (logging (format "[~a] set enviorment var ~a to ~a" current-stage var val))
       (env var val))]))

(define-syntax log-setup
  (syntax-rules ()
    [(log-setup lang)
     (begin
       (logging (format "[~a] setup ~a environment" current-stage lang)))]))
       

(provide (rename-out [log-steps steps]
                     [log-pipeline pipeline]
                     [log-stage stage]
                     [log-setup setup]
                     [log-env env]
                     [log-sh sh]
                     [log-echo echo])
         current-stage)
