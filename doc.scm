;; A documentation generator!
;; TODO Work in progress

(define (about sym doc)
  (property-put sym 'doc-string doc))

(about 'generate-doc
  "Format documentation for the procedure associated with symbol `sym`")
(define (generate-doc sym)
  (let ((func (catch #f (eval sym (current-environment)))))
    (if (not (closure? func))
      (throw "function not defined:" sym)
      (let ((args (cadr (get-closure-code func)))
            (doc  (property-get sym 'doc-string)))
        (display "<procedure> ")
        (display sym)
        (display " ")
        (write args)
        (newline)
        (display #\tab)
        (display doc)
        (newline)))))

;----------------------------------------------------------------------------

(about 'document-everything
  "Find all the procedures in the current environment that have doc-strings
   and format documentation for them.")
(define (document-everything)
  (let ((all-funcs (filter (lambda (x)
                             (and (not (null? (property-get x 'doc-string)))
                                  (closure? (catch #f
                                              (eval x (current-environment))))))
                           (map car (oblist)))))
    (for-each generate-doc all-funcs)))

