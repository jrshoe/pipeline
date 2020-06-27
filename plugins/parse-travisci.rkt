#lang racket
(require racket/set)

(provide parse-travis)

(define (parse-travis travis)
  ;; currently we support single language
  (define lang
    (car (hash-ref travis "language")))

  ;; various versions
  ;; if not specified, use "latest"
  (define lang-vers
    (if (hash-has-key? travis lang)
        (map number->string (hash-ref travis lang))
        '("latest")))

  ;; else '()
  (define before-install
    (if (hash-has-key? travis "before_install")
        (hash-ref travis "before_install")
        '()))

  (define install
    (if (hash-has-key? travis "install")
        (hash-ref travis "install")
        '()))

  (define after-install
    (if (hash-has-key? travis "after_install")
        (hash-ref travis "after_install")
        '()))

  ;; support single script
  (define script
    (car (hash-ref travis "script")))


  ;; environment vars
  (define env
    (when (hash-has-key? travis "env")
      (hash-ref travis "env")))

  (define env-hash
    (let ([lst (map (λ (x) (string-split x "=")) env)])
      (let ([keys-dups (map car lst)])
        (let ([keys (remove-duplicates keys-dups)])
          (let ([lang-vers (for/list ([key keys])
                             `(,key . ()))])
            (let ([lang-vers-ht (make-hash lang-vers)])
              (for ([pair lst])
                (let ([vers (hash-ref lang-vers-ht (car pair))])
                  (hash-set! lang-vers-ht
                             (car pair)
                             (cons (car (cdr pair)) vers))))
              lang-vers-ht))))))

  (define env-lsts
    (let ([envs (hash-keys env-hash)])
      (for/list ([env envs])
        (let ([vars (hash-ref env-hash env)])
          (map (λ (x) (string-append env " " x)) vars)))))

  (define lang-lst (map (λ (x)
                          (string-append lang " " x)) lang-vers))

  (define (cartesian-product lists)
    (foldr (λ (xs ys)
             (append-map (λ (x) (map (λ (y) (cons x y)) ys)) xs)) '(())
           lists))

  (define dispatch-envs (cartesian-product (cons lang-lst env-lsts)))

  ;; deal with matrix filter
  (define matrix
    (when (hash-has-key? travis "matrix")
      (hash-ref travis "matrix")))

  (define exclude
    (when (and (hash? matrix)
               (hash-has-key? matrix "exclude"))
      (hash-ref matrix "exclude")))


  (define dispatch-excludes
    (let ([exclude-lsts (map hash->list exclude)])
      (map (λ (metax)
             (flatten
              (map (λ (x) (if (string=? (car x) "env")
                              (let ([strs (string-split (cdr x) " ")])
                                (let ([lsts  (map (λ (y) (string-split y "="))
                                                  strs)])
                                  (map (λ (z) (string-append (car z)
                                                             " "
                                                             (cadr z)))
                                       lsts)))
                              (string-append (car x) " "
                                             (number->string (cdr x)))))
                   metax)))
           exclude-lsts)))

  ;; use set to filter
  (define dispatch-envs-filtered
    (map set->list
         (filter (λ (x)
                   (ormap (λ (y) (not (subset? y x)))
                          (map list->set dispatch-excludes)))
                 (map list->set dispatch-envs))))

  `#hash((lang . ,lang)
         #;(run . ,(append before-install
                         install
                         after-install))
         (before-install . ,before-install)
         (install . ,install)
         (after-install . ,after-install)
         (cmd . ,script)
         (env . ,dispatch-envs-filtered)))
