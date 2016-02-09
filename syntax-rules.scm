;;; 'expand-syntax adapted from "Scheme 9 From Empty Space" by Nils M. Holm
;;; TODO This could use some more testing

;; BUGS
;;  - Recursive macros do not work properly: ((f) (newline)) ((f n) (f))

;----------------------------------------------------------------------------

;; Scan a `form` and determine which elements correspond to the elements
;; of `pattern`.  Returns an association list (environment) of the elements
;; of `pattern` mapping to the corresponding elements in `form`.
(define (syntax-match form pattern keywords)
  (define (match form pattern env)
    (cond ((pair? pattern)
            (cond ((and (pair? (cdr pattern))
                        (eq? '... (cadr pattern)))
                    (let ((e* (map (lambda (x)
                                     (match x (car pattern) '()))
                                   form)))
                      (if (memq #f e*)
                        #f
                        (let ((m (cons (cons '... e*) env)))
                          m))))
                  ((pair? form)
                    (let* ((e (match (car form) (car pattern) env))
                           (e! (and e (match (cdr form) (cdr pattern) e))))
                      e!))
                  (else #f)))
          ((memq pattern keywords)
            (if (eq? pattern form) env #f))
          ((symbol? pattern)
            (cons (cons pattern form) env))
          (else
            (if (equal? pattern form) env #f))))
  (let ((e (match form pattern '())))
    (if e (reverse e) e)))

;; Alias all the symbols in `form`.
;; TODO Maybe don't rename a symbol if it's quoted?
(define (alpha-conv form bound rename compare)
  (define (map-improper f a r)
    (cond ((null? a)
            (reverse r))
          ((not (pair? a))
            (append (reverse r) (f a)))
          (else
            (map-improper f (cdr a) (cons (f (car a)) r)))))

  (define (vector-map f a)
    (let ((new (make-vector (vector-length a))))
      (do ((i 0 (+ 1 i))) ((>= i (vector-length a)) new)
        (vector-set! new i (f (vector-ref a i))))))

  (let ((mapfn (lambda (x)
                 (alpha-conv x bound rename compare))))
    (cond ((symbol? form) (rename form))
          ((pair? form) (map-improper mapfn form '()))
          ((vector? form) (vector-map mapfn form))
          (else form))))

;; Substitute variables in the template `tmpl` by the corresponding values
;; in the environment `env`.
(define (syntax-expand bound tmpl env rename compare)
  (define (expand tmpl env)
    (cond ((and (pair? tmpl)
                (pair? (cdr tmpl))
                (eq? (cadr tmpl) '...))
            (let ((eenv (assq '... env)))
              (if (not eenv)
                (throw "syntax-rules: no matching ... in pattern" tmpl)
                (begin
                  (set-car! eenv '(#f))
                  (append (map (lambda (x)
                                 (expand (car tmpl) x))
                               (cdr eenv))
                          (expand (cddr tmpl) eenv))))))
          ((not (pair? tmpl))
            (cond ((assq tmpl env) => cdr)
                  (else tmpl)))
          (else
            (cons (expand (car tmpl) env)
                  (expand (cdr tmpl) env)))))
  (expand (alpha-conv tmpl bound rename compare) env))

;; Produce a list of (test . expand) pairs, where `test` returns an
;; expansion environment if the pair represents a rule suitable for the
;; given form (`app`) (else #f), and `expand` takes that environment
;; and produces the expansion of the appropriate rule in that environment.
(define (rewrite-rules app keywords rules-in rules-out rename compare)
  (define pattern caar)
  (define template cadar)
    (if (null? rules-in)
      (reverse rules-out)
      (rewrite-rules app
                     keywords
                     (cdr rules-in)
                     (cons (cons (lambda ()
                                   (syntax-match app
                                                 (pattern rules-in)
                                                 keywords))
                                 (lambda (env)
                                   (syntax-expand (pattern rules-in)
                                                  (template rules-in)
                                                  env
                                                  rename
                                                  compare)))
                           rules-out)
                     rename
                     compare)))

;; As in R5RS
(define-syntax syntax-rules
  (transformer
    (lambda (form rename compare)
      (define (list-of? p? a)
        (or (null? a)
            (and (p? (car a))
                 (list-of? p? (cdr a)))))

      (define (keywords-ok? x)
        (list-of? symbol? x))

      (define (rules-ok? x)
        (list-of? (lambda (x)
                    (and (pair? x)
                         (pair? (car x))
                         (pair? (cdr x))
                         (null? (cddr x))))
                  x))

      (let ((keywords (cadr form))
            (rules (cddr form)))
        ;; do some syntax checking
        (cond ((null? rules)
                (throw "syntax-rules: too few arguments" rules))
              ((not (keywords-ok? keywords))
                (throw "syntax-rules: malformed keyword list" keywords))
              ((not (rules-ok? rules))
                (throw "syntax-rules: invalid clause in rules" rules))
              (else
                (lambda (form)
                  (let loop ((rewrite (rewrite-rules form
                                                     keywords
                                                     rules
                                                     '()
                                                     rename
                                                     compare)))
                    (if (null? rewrite)
                      (throw "syntax error")
                      (let ((match ((caar rewrite))))
                        (if match
                          ((cdar rewrite) match)
                          (loop (cdr rewrite)))))))))))))

