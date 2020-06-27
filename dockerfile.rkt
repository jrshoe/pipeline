#lang racket

(provide dockerfile)

;; input-port? -> (listof pair?)
(define (dockerfile an-input-port)
  ;; input-port? -> (listof string?)
  (define (get-lines an-input-port)
    (define (recr/get-lines another-input-port lst)
      (let ([line (read-line another-input-port)])
        (if (not (eof-object? line))
            (recr/get-lines another-input-port
                            (cons line lst))
            (reverse lst))))
    (recr/get-lines an-input-port '()))
  ;; read function lists used for mapping
  (define read-lst (list read-comment
                         read-blank
                         read-from
                         read-copy
                         read-run
                         read-expose
                         read-cmd))
  ;; recursively use map read to contents
  (define (recr/read func-lst contents)
    (if (equal? func-lst '())
        contents
        (recr/read (cdr func-lst)
                   (map (λ (x)
                          (if (string? x)
                              (let ([res ((car func-lst) x)])
                                (if res res x))
                              x))
                        contents))))
  (recr/read read-lst (get-lines an-input-port)))


;; string? regex? -> boolean?
(define (start-with str regex)
  (let ([matched (regexp-match-positions regex str)])
    (if matched
        (= (caar matched) 0)
        #f)))

;; string? -> pair?
(define (read-comment line)
  (if (start-with line #rx"# ")
      `(COMMENT . ,(substring line 2))
      #f))

(define (read-blank line)
  (if (equal? line "")
      `(BLANK . "")
      #f))

;; string? -> pair?
(define (read-from line)
  (if (start-with line #rx"FROM ")
      `(FROM . ,(substring line 5))
      #f))

;; string? -> pair?
(define (read-copy line)
  (if (start-with line #rx"COPY ")
      `(COPY . ,(substring line 5))
      #f))

;; string? -> pair?
(define (read-run line)
  (if (start-with line #rx"RUN ")
      `(RUN . ,(substring line 4))
      #f))

;; string? -> pair?
(define (read-expose line)
  (if (start-with line #rx"EXPOSE ")
      `(EXPOSE . ,(substring line 7))
      #f))

;; string? -> pair?
(define (read-cmd line)
  (if (start-with line #rx"CMD ")
      `(CMD . ,(substring line 6 (- (string-length line) 2)))
      #f))