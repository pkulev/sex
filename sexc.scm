(declare (unit sexc)
         (uses templates))

(import brev-separate
        (chicken file)
        (chicken pathname)
        (chicken plist)
        (chicken pretty-print)
        (chicken process)
        (chicken process-context)
        (chicken string)
        fmt
        fmt-c
        getopt-long
        regex
        srfi-1                          ; list routines
        tree)

(define (unkebabify sym)
  (case sym
    ((-) sym)
    ((--) sym)
    ((->) sym)
    ((-=) sym)
    (else
     (string->symbol
      (string-substitute "-(?!>)" "_"
                         (symbol->string sym) #t)))))

(define (atom-to-fmt-c atom)
  (case atom
    ((fn) '%fun)
    ((prototype) '%prototype)
    ((var) '%var)
    ((begin) '%begin)
    ((define) '%define)
    ((pointer) '%pointer)
    ((array) '%array)
    ((@) 'vector-ref)
    ((include) '%include)
    ((cast) '%cast)
    (else
     (if (symbol? atom)
         (unkebabify atom)
         atom))))

(define (tree-finder symbol)
  (lambda (node)
    (or (and (tree? node)
             (eq? (car node) symbol))
        #f)))

(define (make-field-access form)
  (assert (= 2 (length form)) "Wrong field access format")
  (unkebabify
   (string->symbol
    (fmt #f (cadr form) (car form)))))

(define (walk-generic form acc)
  (cond
   ((null? form) (cons '() acc))

   ;; vector, e.g. {}-initializer
   ((vector? form)
    (cons
     (list->vector
      (car (walk-sex-tree (vector->list form) (list))))
     acc))

   ;; atom (hopefully)
   ((not (list? form)) (cons (atom-to-fmt-c form) acc))

   ;; special case - replace unquote with its expansion
   ((eq? (car form) 'unquote)
    (fold
     cons
     acc
     (car                               ; bc walk-sex-tree always
                                        ; wraps its result
      (walk-sex-tree (eval (cadr form)) (list)))))

   ;; another special case - field access
   ((and (symbol? (car form))
         (char=? #\. (string-ref (symbol->string (car form)) 0)))
    (cons (make-field-access form) acc))

   ;; another special case - template
   ((template? form)
    (append (fold
             walk-generic
             (list)
             (eval form))
            acc))

   ;; toplevel, or a start of a regular list form
   (else
    (let ((new-acc (list)))
      (cons (reverse
             (fold
              walk-generic
              new-acc
              form))
            acc)))))

(define (normalize-fn-form form)
  ;; (fn ret-type name arglist body) -> normal function
  ;; (fn ret-type name arglist) -> prototype
  (if (>= (length form) 5)
      form
      (cons 'prototype (cdr form))))

(define (walk-function form static acc)
  (if static
      (append (walk-generic (list 'static (normalize-fn-form form))
                            (list))
              acc)
      (append (walk-generic (normalize-fn-form (cdr form))
                            (list))
              acc)))

(define (walk-struct form acc)
  (let ((name (unkebabify (cadr form))))
    (append (walk-generic form (list))
          (cons `(typedef struct ,name ,name) acc))))

(define (walk-extern form acc)
  (case (cadr form)
    ((fn)
     (append
      (list (cons 'extern (walk-function form #f (list))))
      acc))
    ((var)
     (append
      (list (cons 'extern (walk-generic (cdr form) (list))))
      acc))
    (else (error "Extern what?"))))

(define (walk-public form acc)
  (case (cadr form)
    ((fn)
     (walk-function form #f acc))
    ((var)
     (append (walk-generic (list 'static (cdr form)) (list)) acc))
    (else (error "Pub what?"))))

(define (template? form)
  (and (list? form)
       (symbol? (car form))
       (get (car form) 'sex-template)))

(define (walk-sex-tree form acc)
  (if (list? form)
      (if (template? form)
          (fold (fn (walk-sex-tree x y))
                acc
                (eval form))
          (case (car form)
            ((fn) (walk-function form #t acc))
            ((extern) (walk-extern form acc))
            ((pub) (walk-public form acc))
            ((struct union) (walk-struct form acc))
            ((unquote) (fold (fn (walk-sex-tree x y))
                             acc
                             (eval (cadr form))))
            (else (append (walk-generic form (list)) acc))))
      ;; only for unquote support
      (list (list (atom-to-fmt-c form)))))

(define (process-form form acc)
  (case (car form)
    ((chicken-define) (eval (cons 'define (cdr form))) acc)
    ((template) (eval form) acc)
    ((chicken-load)
     (load (cadr form)) acc)
    ((chicken-import)
     (eval (cons 'import (cdr form))) acc)
    (else
     (walk-sex-tree form acc))))

(define (process-raw-forms raw-forms acc)
  (if (null? raw-forms)
      (reverse acc)
      (process-raw-forms (cdr raw-forms)
                         (process-form (car raw-forms) acc))))

(define (read-forms acc)
  (let ((r (read)))
    (if (eof-object? r) (reverse acc)
        (read-forms (cons r acc)))))

(define (emit-c forms)
  (for-each (lambda (form)
              (fmt #t (c-expr form))
              (fmt #t "\n"))
            forms))

;;; Main function facilities

(define opts-grammar
  `((c-compiler "Select C compiler. Defaults to value of SEX_CC environment variable, or if it is empty, to cc"
                (required #f)
                (value #t))
    (compile-object "Compile object file instead of executable program"
                    (required #f)
                    (value #f)
                    (single-char #\c))
    (preprocess "Emit C code"
                (required #f)
                (value #f)
                (single-char #\E))
    (help "Show this help"
          (required #f)
          (value #f)
          (single-char #\h))
    (macro-expand "Emit macro-expanded Sex code"
                  (required #f)
                  (value #f)
                  (single-char #\m))
    (output "Write output to file. Default file name is a.out. If -E or -m options are provided, defaults to stdout"
            (required #f)
            (value #t)
            (single-char #\o))))

(define (print-help)
  (fmt #t "Usage: sexc [options] filename [-- options-for-c-compiler]\n")
  (fmt #t "Options:\n")
  (fmt #t (usage opts-grammar))
  (fmt #t ""))

(define (help-arg? args)
  (assoc 'help args))

(define (get-arg args arg-name default)
  (let ((arg (assoc arg-name args)))
    (if arg (cdr arg)
        default)))

(define (get-rest-args args)
  (cdr (assoc '@ args)))

(define (get-c-compiler-args args)
  (do ((c-args (get-rest-args args) (cdr c-args)))
      ((or (null? c-args)
           (char=? #\- (string-ref (car c-args) 0)))
       c-args)))

(define (get-input-file args)
  (let ((rest-args (get-rest-args args)))
    (if (null? rest-args)
        'stdin
        (car rest-args))))

(define (read-from-file file)
  (with-input-from-file (pathname-strip-directory file)
    (fn (read-forms (list)))))

(define (set-working-directory file)
  (change-directory
   (normalize-pathname
    (make-absolute-pathname
     (current-directory)
     (pathname-directory file)))))

(define (preprocess-or-macroexpand sex-forms output args)
  (if (not (eq? output 'default))
      (with-output-to-file output
        (lambda ()
          (if (get-arg args 'macro-expand #f)
              (map pp sex-forms)
              (emit-c sex-forms))))
      (if (get-arg args 'macro-expand #f)
          (map pp sex-forms)
          (emit-c sex-forms))))

(define (get-env-var name)
  (get-environment-variable name))

(define (compile-to-file sex-forms output args)
  (let ((compiler (or (get-arg args 'c-compiler #f)
                      (get-env-var "SEX_CC")
                      "cc"))
        (out-file (if (eq? output 'default)
                      "a.out"
                      output))
        (temp-c-out (create-temporary-file ".sex.c")))
    (with-output-to-file temp-c-out
      (lambda ()
        (emit-c sex-forms)))
    (call-with-values
        (lambda () (process compiler (append (list temp-c-out "-o" out-file)
                                             (if (get-arg args 'compile-object #f)
                                                 (list "-c")
                                                 (list))
                                             (get-c-compiler-args args))))
      (lambda (out-port in-port pid)
        (process-wait pid)))))

(define (main)
  (let* ((raw-args (command-line-arguments))
         (current-dir (current-directory))
         (args (getopt-long raw-args
                            opts-grammar))
         (output (get-arg args 'output 'default))
         (help (help-arg? args))

         (input (get-input-file args)))
    (if help (print-help)
        (let* ((raw-forms
                (if (eq? input 'stdin)
                    (read-forms (list))
                    (begin
                      (set-working-directory input)
                      (read-from-file input))))
               (sex-forms (process-raw-forms raw-forms (list))))
          (change-directory current-dir)
          (if (or (get-arg args 'macro-expand #f)
                  (get-arg args 'preprocess #f))
              ;; Preprocess or macroexpand
              (preprocess-or-macroexpand sex-forms output args)
              ;; Compile file!
              (compile-to-file sex-forms output args))))))
