;;; 'expand-syntax adapted from "Scheme 9 From Empty Space" by Nils M. Holm
;;; TODO Adaptation for Tiny7 is a work in progress

(define *debug* #f)

(define (print . args)
  (for-each (lambda (arg)
              (write arg)
              (display " "))
            args)
  (newline))

(define (info . args)
  (when *debug*
    (apply print args)))

;----------------------------------------------------------------------------

;; `expand-syntax` uses this to check for a 'syntax' type tag
(define (syntax->list syn)
  (info 'syntax->list)
  (cond ((not (pair? syn)) #f)
        ((not (eq? '!<syntax-rules> (car syn))) #f)
        (else (cdr syn))))

;; Output a description of the syntax rules in a format for `expand-syntax`
(define-syntax syntax-rules
  (lambda (form)
    (info 'syntax-rules)
    `(quote (!<syntax-rules> ,@(cdr form)))))

;; Given a syntax transformer, expand an application of that transformer
;; to a form that is free of macros.
(define (expand-syntax form)
  (info 'expand-syntax)

  ;; Extend the environment env
  (define (ext-env name value env)
    (info 'ext-env)
    (cons (cons name value) env))

  ;; Match the ellipsis in a pattern.  Employs the longest match.
  (define (match-ellipsis form pattern literals env)
    (info 'match-ellipsis)
    (define (try-match head tail)
      (info 'try-match)
      (let ((v (match tail pattern literals env)))
        (cond ((v (ext-env '... (reverse head) v)))
              ((null? head) #f)
              (else (try-match (cdr head) (cons (car head) tail))))))
    (try-match (reverse form) '()))

  ;; Match a pattern against a form.  Return an alist of bindings.
  (define (match form pattern literals env)
    (info 'match)
    (define (_match form pattern env)
      (info '_match)
      (cond ((memq pattern literals)
              (if (eq? form pattern) env #f))
            ((and (pair? pattern) (eq? (car pattern) '...))
              (match-ellipsis form (cdr pattern) literals env))
            ((symbol? pattern)
              (ext-env pattern form env))
            ((and (pair? pattern) (pair? form))
              (let ((e (_match (car form) (car pattern) env)))
                (and e (_match (cdr form) (cdr pattern) e))))
            (else (if (equal? form pattern) env #f))))
    (_match form pattern env))

    ;; Find a rule whose pattern matches a given form.
    ;; Returns (pattern template environment)
    (define (find-rule form rules name literals)
      (info 'find-rule)
      (cond ((null? rules)
              (throw "syntax-rules: bad syntax" name rules))
            (else (let ((e (match form (caar rules) literals '())))
                     (if e
                       (list (caar rules) (cadar rules) e)
                       (find-rule form (cdr rules) name literals))))))

    ;; Like 'map, but also works for improper lists.
    (define (map-improper f a)
      (info 'map-improper)
      (letrec ((map-i (lambda (a r)
                        (cond ((null? a) (reverse r))
                              ((not (pair? a)) (append (reverse r) (f a)))
                              (else (map-i (cdr a) (cons (f (car a)) r)))))))
        (map-i a '())))

    ;; Get a list of forms created by substituting `var` in `tmpl` for
    ;; possible values listed in `val*`.  Other values from `env` are
    ;; also substituted.
    (define (subst-ellipsis var tmpl val* env)
      (info 'subst-ellipsis)
      (map (lambda (v)
             (tmpl->form #f tmpl (cons (cons var v) env)))
           val*))

    ;; Substitute names from `env` with their associated values in `form`.
    ;; If `pattern` is `#f`, no ellipsis substitution is performed.
    (define (tmpl->form pattern form env)
      (info 'tmpl->form)
      (cond ((not (pair? form)) (let ((v (assv form env)))
                                  (if v (cdr v) form)))
            ((and (pair? form)
                  (pair? (cdr form))
                  (eq? (cadr form) '...))
              (let ((var (if (pair? pattern)
                           (car pattern)
                           pattern)))
                (let ((v-ell (assq '... env))
                      (v-var (assq var env)))
                  (if v-ell
                    (if v-var
                      (append (subst-ellipsis var
                                              (car form)
                                              (if v-var
                                                (cons (cdr v-var) (cdr v-ell))
                                                (cdr v-ell))
                                              env)
                              (cddr form))
                      (append (list (tmpl->form #f (car form) env))
                              (cdr v-ell)
                              (cddr form)))
                  (throw "syntax-rules: unmatched ellipsis")))))
              ((pair? form) (cons (tmpl->form (if (pair? pattern)
                                                (car pattern)
                                                #f)
                                              (car form)
                                              env)
                                  (tmpl->form (if (pair? pattern)
                                                (cdr pattern)
                                                #f)
                                              (cdr form)
                                              env)))
              (else form)))

    ;; Perfom syntax transformation on `form`.
    (define (transform form)
      (info 'transform)
      (let ((syn (syntax->list (car form))))
        (if (not syn)
          (throw "expand-syntax: not a syntax transformer" (car form))
          (let* ((name (car form))
                (literals (car syn))
                (rules (cdr syn))
                (to-expand (cadr form))
                (pat/tmpl/env (find-rule to-expand rules name literals)))
            (info 'name: name)
            (info 'literals: literals)
            (info 'rules: rules)
            (info 'to-expand: to-expand)
            (info 'pat/tmpl/env: pat/tmpl/env)
            (expand-all (apply tmpl->form pat/tmpl/env))))))

    ;; Expand all applications of syntax transformers in the given `form`.
    (define (expand-all form)
      (info 'expand-all)
      (cond ((not (pair? form)) form)
            ((eq? (car form) 'quote) form)
            ((syntax->list (car form)) (transform form))
            (else (map-improper expand-all form))))

    (expand-all form))

;----------------------------------------------------------------------------

(let ((test (syntax-rules (print)
              ((print a) (begin
                           (display a)
                           (newline))))))
  (print 'test-macro: test)
  (print 'expansion: (expand-syntax '(test (print 'hi)))))

