#lang racket
(require simple-http)
(require json)

(provide
 list-containers
 list-images
 pull-image
 create-container
 start-container
 build-image
 )

(define (get-body response)
  (json-response-body response))

(define docker
  (update-port
   (update-host json-requester "127.0.0.1")
   2376))

;; jsexpr?
(define (list-containers #:all [all #f])
  (get-body (get docker "/containers/json"
                 #:params (if all
                              '((all . "1"))
                              '()))))
;; jsexpr?
(define (list-images)
  (get-body (get docker "/images/json")))

;; jsexpr?
(define (pull-image #:image image #:tag [tag "latest"])
  (get-body (post docker "/images/create" #:params
                  `((fromImage . ,image)
                    (tag . ,tag)))))

;; jsexpr?
(define (create-container #:name name #:image image #:tag [tag "latest"])
  (get-body (post (update-headers docker
                                  '("Content-Type: application/json"))
                  "/containers/create"
                  #:data (jsexpr->string (hasheq 'Image (string-append image ":" tag)))
                  #:params `((name . ,name)))))


(define (start-container #:name name)
  ;; handle bug from simple-http:
  ;; hash-ref: no value found for key 'Content-Type
  (with-handlers ([exn:fail? (λ (exn) 'no-response)])
    (post (update-headers docker
                          '("Content-Type: application/json"))
          (string-append "/containers/" name "/start"))))


;; ELEMENTALY
;; currently it should be tar file
;; (listof jsexpr?)
(define (build-image #:dockerfile path #:name name)
  (strings->jsexprs
   ((sh (string-append "curl --data-binary '@"
                       path
                       "' 127.0.0.1:2376/build?t="
                       name))
    'result-info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string? -> (or/c 'exit-code 'result-info)
(define (sh cmd)
  (display (string-append "Executing: "
                          cmd
                          "\n"))
  ;; input-port? -> string?
  (define (get-lines an-input-port)
    (define (recr/get-lines another-input-port str)
      (let
          ((line (read-line another-input-port)))
        (if (not (eof-object? line))
            (recr/get-lines another-input-port
                            (string-append str line "\n"))
            str)))
    (recr/get-lines an-input-port ""))
  
  (define command-execution (process cmd))
  ((last command-execution) 'wait)
  (define exit-code ((last command-execution) 'exit-code))
  (define result-info (get-lines (first command-execution)))
  (λ (x)
    (match x
      ['exit-code exit-code]
      ['result-info result-info]
  )))

;; (listof string?) -> (listof jsexpr?)
(define (strings->jsexprs strings)
  (let ([lst (string-split strings
                           "\r\n")])
    (map string->jsexpr lst)
    ))
