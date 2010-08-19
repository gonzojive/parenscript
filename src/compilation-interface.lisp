(in-package #:parenscript)

(defparameter *js-target-version* 1.3)

(defvar *parenscript-stream* nil)

(defmacro ps (&body body)
  "Given Parenscript forms (an implicit progn), compiles those forms
to a JavaScript string at macro-expansion time."
  (let ((printed-forms (parenscript-print
                        (compile-statement `(progn ,@body))
                        nil)))
    (if (and (not (cdr printed-forms))
             (stringp (car printed-forms)))
        (car printed-forms)
        (let ((s (gensym)))
          `(with-output-to-string (,s)
             ,@(mapcar (lambda (x) `(write-string ,x ,s)) printed-forms))))))

(defmacro ps-to-stream (stream &body body)
  (let ((printed-forms (parenscript-print
                        (compile-statement `(progn ,@body))
                        nil)))
    `(let ((*parenscript-stream* ,stream))
       ,@(mapcar (lambda (x) `(write-string ,x *parenscript-stream*)) printed-forms))))

(defun ps* (&rest body)
  "Compiles BODY to a JavaScript string.
Body is evaluated."
  (let ((*psw-stream* (or *parenscript-stream*
                          (make-string-output-stream))))
    (parenscript-print (compile-statement `(progn ,@body)) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

(defun ps** (form &key expressionp)
  "Stupidly named Compiles BODY to a JavaScript string.
Body is evaluated."
  (let ((*psw-stream* (or *parenscript-stream*
                          (make-string-output-stream))))
    (parenscript-print (funcall (if expressionp 'compile-expression 'compile-statement)
                                form) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

(defmacro ps-doc (&body body)
  "Expands Parenscript forms in a clean environment."
  (let ((*ps-gensym-counter* 0)
        (*ps-special-variables* nil))
     (macroexpand-1 `(ps ,@body))))

(defun ps-doc* (&rest body)
  (let ((*ps-gensym-counter* 0)
        (*ps-special-variables* nil))
    (apply #'ps* body)))

(defvar *js-inline-string-delimiter* #\"
  "Controls the string delimiter char used when compiling Parenscript in ps-inline.")

(defun ps-inline* (form &optional (*js-string-delimiter* *js-inline-string-delimiter*))
  (concatenate 'string "javascript:" (ps* form)))

(defmacro+ps ps-inline (form &optional (string-delimiter *js-inline-string-delimiter*))
  `(concatenate 'string "javascript:"
                ,@(let ((*js-string-delimiter* string-delimiter))
                    (parenscript-print (compile-statement form) nil))))

(defvar *ps-read-function* #'read
  "This should be a function that takes the same inputs and returns the same
outputs as the common lisp read function.  We declare it as a variable to allow
a user-supplied reader instead of the default lisp reader.")


(defun ps-compile-stream (stream)
  "Compiles a character stream of Parenscript code to a Javascript string."
  (let ((*ps-compilation-level* :toplevel)
        (*package* *package*)
        (end-read-form '#:unique))
    (flet ((read-form () (funcall *ps-read-function* stream nil end-read-form)))
      (let* ((js-string
              ;; cons up the forms, compiling as we go, and print the result
              (do ((form (read-form) (read-form))
                   (compiled-forms nil))
                  ((eql form end-read-form)
                     (with-output-to-string (*psw-stream*)
                       (loop :for compiled-form :in (nreverse compiled-forms)
                             :do (parenscript-print compiled-form t)
                             :do (format *psw-stream* ";~%"))))
                (push (compile-statement form) compiled-forms))))
        js-string))))

(defun ps-compile-file (source-file)
  "Compiles the given Parenscript source file and returns a Javascript string."
  (let ((*ps-source-file* source-file)
        (*ps-source-position* 1))
    (with-open-file (stream source-file :direction :input)
      (ps-compile-stream stream))))
