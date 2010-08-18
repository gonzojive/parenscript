(in-package #:parenscript)

(defvar *ps-reserved-symbol-names*
  (list "break" "case" "catch" "continue" "default" "delete" "do" "else"
        "finally" "for" "function" "if" "in" "instanceof" "new" "return"
        "switch" "this" "throw" "try" "typeof" "var" "void" "while" "with"
        "abstract" "boolean" "byte" "char" "class" "const" "debugger" "double"
        "enum" "export" "extends" "final" "float" "goto" "implements" "import"
        "int" "interface" "long" "native" "package" "private" "protected"
        "public" "short" "static" "super" "synchronized" "throws" "transient"
        "volatile" "{}" "true" "false" "null" "undefined"))

(defun ps-reserved-symbol? (symbol)
  (find (string-downcase (symbol-name symbol)) *ps-reserved-symbol-names* :test #'string=))

;;; special forms

(defvar *ps-special-forms* (make-hash-table :test 'eq))

(defun get-ps-special-form (name)
  (gethash name *ps-special-forms*))

(defmacro define-ps-special-form (name lambda-list &rest body)
  `(setf (gethash ',name *ps-special-forms*)
         (lambda (&rest whole)
           (destructuring-bind ,lambda-list
               whole
             ,@body))))

(defun ps-special-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (gethash (car form) *ps-special-forms*)))

;;; scoping

(defvar *enclosing-lexical-block-declarations* ()
  "This special variable is expected to be bound to a fresh list by
special forms that introduce a new JavaScript lexical block (currently
function definitions and lambdas). Enclosed special forms are expected
to push variable declarations onto the list when the variables
declaration cannot be made by the enclosed form \(for example, a
\(x,y,z\) expression progn\). It is then the responsibility of the
enclosing special form to introduce the variable bindings in its
lexical block.")

(defvar *ps-special-variables* ())

(defun ps-special-variable-p (sym)
  (member sym *ps-special-variables*))

;;; macros

(defparameter *ps-source-file* nil
  "Source file being compiled or loaded.")
(defparameter *ps-source-position* nil
  "Position inside of *ps-source-file* of current form, an integer.")
(defparameter *ps-source-buffer* nil)
(defparameter *ps-source-definer-name* nil)

;;; macro expansion
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-macro-dictionary ()
    (make-hash-table :test 'eq))

  (defvar *ps-function-toplevel-cache* (make-macro-dictionary)
    "Toplevel environment dictionary from function name to lambda
    list, for use in SLIME.")

  (defvar *ps-function-location-toplevel-cache* (make-macro-dictionary)
  "Toplevel environment dictionary from function name to source location,
  for use in SLIME.")
  
  (defvar *ps-macro-toplevel* (make-macro-dictionary)
    "Toplevel macro environment dictionary.")

  (defvar *ps-macro-toplevel-lambda-list* (make-macro-dictionary)
    "Toplevel macro environment dictionary (but maps macro name to
    lambda list).  For SLIME.")

  (defvar *ps-macro-env* (list *ps-macro-toplevel*)
    "Current macro environment."))

(defvar *ps-symbol-macro-toplevel* (make-macro-dictionary))

(defvar *ps-symbol-macro-env* (list *ps-symbol-macro-toplevel*))

(defvar *ps-local-function-names* ()) ;; contains a subset of
(defvar *ps-enclosing-lexicals* ())

(defvar *ps-setf-expanders* (make-macro-dictionary)
  "Setf expander dictionary. Key is the symbol of the access
function of the place, value is an expansion function that takes the
arguments of the access functions as a first value and the form to be
stored as the second value.")

(defparameter *ps-compilation-level* :toplevel
  "This value takes on the following values:
:toplevel indicates that we are traversing toplevel forms.
:inside-toplevel-form indicates that we are inside a call to ps-compile-*
nil indicates we are no longer toplevel-related.")

(defun lookup-macro-def (name env)
  (loop for e in env thereis (gethash name e)))

(defun make-ps-macro-function (args body)
  "Given the arguments and body to a parenscript macro, returns a
function that may be called on the entire parenscript form and outputs
some parenscript code.  Returns a second value that is the effective
lambda list from a Parenscript perspective."
  (let* ((whole-var (when (eql '&whole (first args)) (second args)))
         (effective-lambda-list (if whole-var (cddr args) args))
         (whole-arg (or whole-var (gensym "ps-macro-form-arg-"))))
    (values 
      `(lambda (,whole-arg)
         (destructuring-bind ,effective-lambda-list
             (cdr ,whole-arg)
           ,@body))
      effective-lambda-list)))

;; fixme document how source location annotation works and make it
;; support defvar, defparameter, and more.
(defun make-source-location (definer name lambda-list body &optional sl)
  (flet ((msl (file buffer position)
           (let ((snippet (format nil "(~s ~s ~s ~{~s~}" definer name lambda-list body)))
             `(,(format nil "(~s ~s)" definer name)
                (:location
                 ,@(if file
                       `((:file ,(namestring file)))
                       `((:buffer ,buffer)))
                 (:position ,position)
                 (:snippet ,(subseq snippet 0 (min 256 (length snippet)))))))
))
 (cond
     ((and *ps-source-position* (or *ps-source-file* *ps-source-buffer*))
      (msl *ps-source-file* *ps-source-buffer* *ps-source-position*))
     #+sbcl
     ((and sl (let* ((file (or (getf (sb-c:definition-source-location-plist sl) :emacs-filename)
                               (sb-c:definition-source-location-namestring sl)))
                     (buffer (getf (sb-c:definition-source-location-plist sl) :emacs-buffer))
                     (position (or (getf (sb-c:definition-source-location-plist sl) :emacs-position)
                                   1)))
                (when (or buffer file)
                  (msl file buffer position))))))))

(defmacro defpsmacro (name args &body body)
  (multiple-value-bind (macro-fn-form effective-lambda-list)
      (make-ps-macro-function args body)
    `(progn (setf (gethash ',name *ps-macro-toplevel*) ,macro-fn-form)
            (setf (gethash ',name *ps-macro-toplevel-lambda-list*) ',effective-lambda-list)
            (setf (gethash ',name *ps-function-location-toplevel-cache* nil)
                  (list (make-source-location ',(or *ps-source-definer-name* 'defpsmacro)
                                              ',name ',args ',body
                                              #+sbcl (sb-c:source-location))))
            ',name)))

(defmacro define-ps-symbol-macro (symbol expansion)
  `(setf (gethash ',symbol *ps-symbol-macro-toplevel*)
         (lambda (form)
           (declare (ignore form))
           ',expansion)))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the ParenScript macro
environment. When the imported macro is macroexpanded by ParenScript,
it is first fully macroexpanded in the Lisp macro environment, and
then that expansion is further expanded by ParenScript."
  (dolist (name names)
    (eval `(defpsmacro ,name (&rest args)
             (macroexpand `(,',name ,@args))))))

(defmacro defmacro+ps (name args &body body)
  "Define a Lisp macro and a ParenScript macro with the same macro
function (ie - the same result from macroexpand-1), for cases when the
two have different full macroexpansions (for example if the CL macro
contains implementation-specific code when macroexpanded fully in the
CL environment)."
  `(progn (defmacro ,name ,args ,@body)
          (defpsmacro ,name ,args ,@body)))

(defun ps-macroexpand (form)
  (aif (or (and (symbolp form) (lookup-macro-def form *ps-symbol-macro-env*))
           (and (consp form) (lookup-macro-def (car form) *ps-macro-env*)))
       (values (ps-macroexpand (funcall it form)) t)
       form))

(defun ps-macroexpand-1 (form)
  "Analog of CL:MACROEXPAND-1 for parenscript form."
  (aif (or (lookup-macro-def form *ps-symbol-macro-env*)
           (and (consp form) (lookup-macro-def (car form) *ps-macro-env*)))
       (values (funcall it form) t)
       form))

;;;; compiler interface
(defun adjust-ps-compilation-level (form level)
  "Given the current *ps-compilation-level*, LEVEL, and the fully macroexpanded
form, FORM, returns the new value for *ps-compilation-level*."
  (cond ((or (and (consp form)
                  (member (car form)
                          '(progn locally macrolet symbol-macrolet)))
             (and (symbolp form)
                  (eq :toplevel level)))
         level)
        ((eq :toplevel level)
         :inside-toplevel-form)))

(defun ps-compile (obj)
  (etypecase obj
    ((or number string character)
     obj)
    (vector
     (ps-compile `(quote ,(coerce obj 'list))))
    (symbol
     (multiple-value-bind (expansion expanded?)
         (ps-macroexpand obj)
       (if expanded?
           (ps-compile expansion)
           obj)))
    (cons
     (multiple-value-bind (form expanded?)
         (ps-macroexpand obj)
       (let ((*ps-compilation-level*
              (if expanded?
                  *ps-compilation-level*
                  (adjust-ps-compilation-level form *ps-compilation-level*))))
         (cond (expanded?
                (ps-compile form))
               ((ps-special-form-p form)
                (apply (get-ps-special-form (car form)) (cdr form)))
               (t
                (compile-funcall-form form))))))))

(defun compile-funcall-form (form)
  `(js:funcall
    ,(if (symbolp (car form))
         (maybe-rename-local-function (car form))
         (compile-expression (car form)))
    ,@(mapcar #'compile-expression (cdr form))))

(defvar compile-expression?)

(defun compile-statement (form)
  (let ((compile-expression? nil))
    (ps-compile form)))

(defun compile-expression (form)
  (let ((compile-expression? t))
    (ps-compile form)))

(defvar *ps-gensym-counter* 0)

(defun ps-gensym (&optional (prefix "_js"))
  (let ((prefix (if (stringp prefix) prefix (symbol-to-js-string prefix nil))))
    (make-symbol (format nil "~A~:[~;_~]~A" prefix
                         (digit-char-p (char prefix (1- (length prefix))))
                         (incf *ps-gensym-counter*)))))

(defmacro with-ps-gensyms (symbols &body body)
  "Each element of SYMBOLS is either a symbol or a list of (symbol
gensym-prefix-string)."
  `(let* ,(mapcar (lambda (symbol)
                    (destructuring-bind (symbol &optional prefix)
                        (if (consp symbol)
                            symbol
                            (list symbol))
                      (if prefix
                          `(,symbol (ps-gensym ,prefix))
                          `(,symbol (ps-gensym ,(symbol-to-js-string symbol))))))
                  symbols)
     ,@body))

(defun %check-once-only-vars (vars)
  (let ((bad-var (find-if (lambda (x) (or (not (symbolp x)) (keywordp x))) vars)))
    (when bad-var
      (error "PS-ONLY-ONCE expected a non-keyword symbol but got ~s" bad-var))))

(defun %check-once-only-vars-superior (vars)
  (let ((bad-var (find-if (lambda (x)
                            (typecase x
                              (symbol (keywordp x))
                              (cons (not (and (symbolp (first x)) (not (keywordp (first x)))
                                              (< 0 (length x) 3))))
                              (t t)))
                          vars)))
    (when bad-var
      (error "PS-ONLY-ONCE expected a non-keyword symbol or (<non-keyword symbol> <anything>) but got ~s" bad-var))))

(defmacro ps-once-only ((&rest vars) &body body)
  (let ((gensyms (mapcar (lambda (x) (ps-gensym (string x))) vars)))
    `(let ,(mapcar (lambda (g v) `(,g (ps-gensym ,(string v)))) gensyms vars)
       `(let* (,,@(mapcar (lambda (g v) ``(,,g ,,v)) gensyms vars))
          ,(let ,(mapcar (lambda (g v) `(,v ,g)) gensyms vars)
             ,@body)))))

(defmacro ps-once-only-superior ((&rest vars) &body body)
  (%check-once-only-vars-superior vars)
  (let ((vars (mapcar #'(lambda (x) (typecase x
                                      (symbol x)
                                      (cons (first x))))
                      vars))
        (evaled-forms (mapcar #'(lambda (x) (typecase x
                                      (symbol x)
                                      (cons (second x))))
                      vars)))
    (let ((gensyms (mapcar (lambda (x) (ps-gensym (string x))) vars)))
      `(let ,(mapcar (lambda (g v) `(,g (ps-gensym ,(string v)))) gensyms vars)
         `(let* (,,@(mapcar (lambda (g e) ``(,,g ,,e)) gensyms evaled-forms))
            ,(let ,(mapcar (lambda (g v) `(,v ,g)) gensyms vars)
               ,@body))))))
