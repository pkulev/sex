(declare (unit utils))

(include "utils.macros.scm")

(import
  (chicken pathname)
  (chicken process-context))

(define (get-env-var name)
  (get-environment-variable name))

(define (set-working-directory file)
  (change-directory
   (normalize-pathname
    (if (absolute-pathname? file)
        (pathname-directory file)
        (make-absolute-pathname
         (current-directory)
         (pathname-directory file))))))
