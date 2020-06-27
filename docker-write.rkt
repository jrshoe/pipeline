#lang racket

(provide docker-write)

(define (docker-write #:base image-tag #:env envs #:runs runs #:cmd cmd #:out out)
  (docker-write/from image-tag out)
  (map (lambda (x) (docker-write/env x) out) envs)
  (map (λ (x) (docker-write/run x out)) runs)
  (docker-write/cmd cmd out))

;; string? output-port?
(define (docker-write/from image-tag out)
  (write-string (string-append "FROM " image-tag "\n") out))

;; string?
(define (docker-write/run cmd out)
  (write-string (string-append "RUN " cmd "\n") out))

;; string?
(define (docker-write/cmd cmd out)
  (write-string (string-append "CMD [\"" cmd "\"]\n") out))

;; string?
(define (docker-write/env cmd out)
  (write-string (string-append "ENV " cmd "\n") out))
