#lang racket
(require "parse-travisci.rkt")
(require "../pipeline.rkt")
(require yaml)
(require racket/future)

(define travis (parse-travis (file->yaml "../misc/travis.yml")))

(define (travis->pipeline travis)
  (define lang (hash-ref travis 'lang))
  ;; list?
  (define before-install
    (let ([temp-before (hash-ref travis 'before-install)])
      (map (λ (x) `(sh ,x)) temp-before)))
  ;; list?
  (define install
    (let ([temp-install (hash-ref travis 'install)])
      (map (λ (x) `(sh ,x)) temp-install)))
  ;; list?
  (define after-install
    (let ([temp-after (hash-ref travis 'after-install)])
      (map (λ (x) `(sh ,x)) temp-after)))
  ;; list?
  (define script (hash-ref travis 'cmd))
  ;; listof list?
  (define envs
    (let ([temp-env (hash-ref travis 'env)])
      (map (λ (x) (map (λ (y)
                         `(env ,@(string-split y " ")))
                       x))
           temp-env)))
  (for/list ([env envs])
    #`(pipeline (stage "setup"
                       (steps
                        (setup #,lang))
                       #,@(for/list ([env-item env])
                            `(steps ,env-item)))
                (stage "before_install"
                       (steps #,@before-install))
                (stage "install"
                       (steps #,@install))
                (stage "after_install"
                       (steps #,@after-install))
                (stage "excute_script"
                       (steps (sh #,script)))
                )))
; (travis->pipeline travis)

;(require future-visualizer)
#;(visualize-futures
 (let ([f (future (λ () (eval-syntax (car (travis->pipeline travis)))))])
   (eval-syntax (cadr (travis->pipeline travis)))
   (touch f)))

;; (map eval-syntax (travis->pipeline travis))
(define one-syntax (car (travis->pipeline travis)))

(define (length-of-stage stage)
  (if (list? stage)
      (let ([len (length (cddr stage))])
        `(,(cadr stage) . ,len))
      `(,(symbol->string stage) . 0)))

; (make-hash (map length-of-stage (syntax->datum one-syntax)))
; (eval-syntax (car (travis->pipeline travis)))
(car (travis->pipeline travis))