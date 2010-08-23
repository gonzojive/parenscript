(in-package #:parenscript)

(defvar *ps-print-pretty* t)
(defvar *indent-num-spaces* 4)
(defvar *js-string-delimiter* #\'
  "Specifies which character should be used for delimiting strings.

This variable is used when you want to embed the resulting JavaScript
in an html attribute delimited by #\\\" as opposed to #\\', or
vice-versa.")

(defvar *indent-level*)

(defvar *psw-stream*)

(defun parenscript-print (form immediate?)
  "Prints intermediate Parenscript representation FORM.  If IMMEDIATE?
is non-null, writes output to *PSW-STREAM*.  Otherwise, returns a
list of strings."
  (declare (special immediate?))
  (let ((*indent-level* 0)
        (*psw-stream* (if immediate?
                          *psw-stream*
                          (make-string-output-stream)))
        (%psw-accumulator ()))
    (declare (special %psw-accumulator))
    (if (and (listp form) (eq 'ps-js:block (car form))) ; ignore top-level block
        (loop for (statement . remaining) on (cdr form) do
             (ps-print statement) (psw #\;) (when remaining (psw #\Newline)))
        (ps-print form))
    (unless immediate?
      (reverse (cons (get-output-stream-string *psw-stream*) %psw-accumulator)))))

(defun psw (&rest objs)
  (dolist (obj objs)
    (declare (special %psw-accumulator immediate?))
    (typecase obj
      (string (write-string obj *psw-stream*))
      (character (write-char obj *psw-stream*))
      (otherwise
       (if immediate?
           (write-string (eval obj) *psw-stream*)
           (setf %psw-accumulator
                 (cons obj
                       (cons (get-output-stream-string *psw-stream*)
                             %psw-accumulator))))))))

(defgeneric ps-print% (js-primitive args))

(defmacro defprinter (js-primitive args &body body)
  (if (listp js-primitive)
      (cons 'progn (mapcar (lambda (p)
                             `(defprinter ,p ,args ,@body))
                           js-primitive))
      (let ((pargs (gensym)))
        `(defmethod ps-print% ((op (eql ',js-primitive)) ,pargs)
           (declare (ignorable op))
           (destructuring-bind ,args
               ,pargs
             ,@(loop for x in body collect
                    (if (or (characterp x)
                            (stringp x))
                        (list 'psw x)
                        x)))))))

(defmethod ps-print ((x null))
  (psw "null"))

(defmethod ps-print ((x (eql t)))
  (psw "true"))

(defmethod ps-print ((x (eql 'ps-js:f)))
  (psw "false"))

(defmethod ps-print ((s symbol))
  (if (keywordp s)
      (ps-print (string-downcase s))
      (psw (symbol-to-js-string s))))

(defmethod ps-print ((compiled-form cons))
  (ps-print% (car compiled-form) (cdr compiled-form)))

(defun newline-and-indent ()
  (if *ps-print-pretty*
      (progn (psw #\Newline)
             (loop repeat (* *indent-level* *indent-num-spaces*) do (psw #\Space)))
      (psw #\Space)))

(defparameter *js-lisp-escaped-chars*
  '((#\' . #\')
    (#\\ . #\\)
    (#\b . #\Backspace)
    (#\f . #.(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defmethod ps-print ((char character))
  (ps-print (string char)))

(defmethod ps-print ((string string))
  (flet ((lisp-special-char-to-js (lisp-char)
           (car (rassoc lisp-char *js-lisp-escaped-chars*))))
    (psw *js-string-delimiter*)
    (loop for char across string
          for code = (char-code char)
          for special = (lisp-special-char-to-js char)
          do (cond (special (psw #\\) (psw special))
                   ((or (<= code #x1f) (>= code #x80)) (format *psw-stream* "\\u~4,'0x" code))
                   (t (psw char))))
    (psw *js-string-delimiter*)))

(defmethod ps-print ((number number))
  (format *psw-stream* (if (integerp number) "~S" "~F") number))

(let ((precedence-table (make-hash-table :test 'eq)))
  (loop for level in '((ps-js:getprop ps-js:aref ps-js:new ps-js:funcall)
                       (ps-js:lambda) ;; you won't find this in JS books
                       (ps-js:++ ps-js:-- ps-js:post++ ps-js:post--)
                       (ps-js:! ps-js:~ ps-js:negate ps-js:typeof ps-js:delete)
                       (ps-js:* ps-js:/ ps-js:%)
                       (ps-js:-)
                       (ps-js:+)
                       (ps-js:<< ps-js:>> ps-js:>>>)
                       (ps-js:< ps-js:> ps-js:<= ps-js:>= ps-js:instanceof ps-js:in)
                       (ps-js:== ps-js:!= ps-js:=== ps-js:!==)
                       (ps-js:&)
                       (ps-js:^)
                       (ps-js:\|)
                       (ps-js:&&)
                       (ps-js:\|\|)
                       (ps-js:?)
                       (ps-js:= ps-js:*= ps-js:/= ps-js:%= ps-js:+= ps-js:-= ps-js:<<= ps-js:>>= ps-js:>>>= ps-js:&= ps-js:^= ps-js:\|=)
                       (ps-js:return ps-js:throw)
                       (ps-js:|,|))
     for i from 0
     do (mapc (lambda (symbol)
                (setf (gethash symbol precedence-table) i))
              level))

  (defun op-precedence (op)
    (gethash op precedence-table -1)))

(defun associative? (op)
  (member op '(ps-js:+ ps-js:* ps-js:& ps-js:&& ps-js:\| ps-js:\|\|
               ps-js:funcall ps-js:aref ps-js:getprop))) ;; these aren't really associative, but RPN

(defun parenthesize-print (ps-form)
  (psw #\() (ps-print ps-form) (psw #\)))

(defun print-op-argument (op argument)
  (let ((arg-op (when (listp argument) (car argument))))
    (if (or (< (op-precedence op) (op-precedence arg-op))
            (and (eq op arg-op) (not (associative? op))))
        (parenthesize-print argument)
        (ps-print argument))))

(defun print-op (op)
  (psw (string-downcase op)))

(defprinter (ps-js:! ps-js:~ ps-js:++ ps-js:--) (x)
  (print-op op) (print-op-argument op x))

(defprinter ps-js:negate (x)
  "-"(print-op-argument op x))

(defprinter (ps-js:delete ps-js:typeof ps-js:new ps-js:throw ps-js:return) (x)
  (print-op op)" "(print-op-argument op x))

(defprinter ps-js:post++ (x)
  (ps-print x)"++")

(defprinter ps-js:post-- (x)
  (ps-print x)"--")

(defprinter (ps-js:+ ps-js:- ps-js:* ps-js:/ ps-js:% ps-js:&& ps-js:\|\| ps-js:& ps-js:\| ps-js:-= ps-js:+= ps-js:*= ps-js:/= ps-js:%= ps-js:^ ps-js:&= ps-js:^= ps-js:\|= ps-js:= ps-js:== ps-js:=== ps-js:!== ps-js:in ps-js:!= ps-js:> ps-js:>= ps-js:< ps-js:<=)
    (&rest args)
  (loop for (arg . remaining) on args do
       (print-op-argument op arg)
       (when remaining (format *psw-stream* " ~(~A~) " op))))

(defprinter ps-js:aref (array &rest indices)
  (print-op-argument 'ps-js:aref array)
  (dolist (idx indices)
    (psw #\[) (ps-print idx) (psw #\])))

(defun print-comma-delimited-list (ps-forms)
  (loop for (form . remaining) on ps-forms do
        (print-op-argument 'ps-js:|,| form)
        (when remaining (psw ", "))))

(defprinter ps-js:array (&rest initial-contents)
  "["(print-comma-delimited-list initial-contents)"]")

(defprinter (ps-js:|,|) (&rest expressions)
  (print-comma-delimited-list expressions))

(defprinter ps-js:funcall (fun-designator &rest args)
  (print-op-argument op fun-designator)"("(print-comma-delimited-list args)")")

(defprinter ps-js:block (&rest statements)
  "{" (incf *indent-level*)
  (dolist (statement statements)
    (newline-and-indent) (ps-print statement) (psw #\;))
  (decf *indent-level*) (newline-and-indent)
  "}")

(defprinter ps-js:lambda (args body)
  (print-fun-def nil args body))

(defprinter ps-js:defun (name args body)
  (print-fun-def name args body))

(defun print-fun-def (name args body-block)
  (format *psw-stream* "function ~:[~;~A~](" name (symbol-to-js-string name))
  (loop for (arg . remaining) on args do
        (psw (symbol-to-js-string arg)) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

(defprinter ps-js:object (&rest slot-defs)
  "{ "(loop for ((slot-name . slot-value) . remaining) on slot-defs do
           (ps-print slot-name) (psw " : ") (ps-print slot-value)
           (when remaining (psw ", ")))" }")

(defprinter ps-js:getprop (obj slot)
  (print-op-argument op obj)"."(psw (symbol-to-js-string slot)))

(defprinter ps-js:if (test consequent &rest clauses)
  "if ("(ps-print test)") "
  (ps-print consequent)
  (loop while clauses do
       (ecase (car clauses)
         (:else-if (psw " else if (") (ps-print (cadr clauses)) (psw ") ")
                   (ps-print (caddr clauses))
                   (setf clauses (cdddr clauses)))
         (:else (psw " else ")
                (ps-print (cadr clauses))
                (return)))))

(defprinter ps-js:? (test then else)
  (print-op-argument op test) " ? "
  (print-op-argument op then) " : "
  (print-op-argument op else))

(defprinter ps-js:var (var-name &rest var-value)
  "var "(psw (symbol-to-js-string var-name))
  (when var-value
    (psw " = ") (print-op-argument 'ps-js:= (car var-value))))

(defprinter ps-js:label (label statement)
  (psw (symbol-to-js-string label))": "(ps-print statement))

(defprinter (ps-js:continue ps-js:break) (&optional label)
  (print-op op) (when label
                  (psw " " (symbol-to-js-string label))))

;;; iteration
(defprinter ps-js:for (vars tests steps body-block)
  (psw "for (")
  (loop for ((var-name . var-init) . remaining) on vars
        for decl = "var " then "" do
        (psw decl (symbol-to-js-string var-name) " = ") (ps-print var-init) (when remaining (psw ", ")))
  "; "
  (loop for (test . remaining) on tests do
       (ps-print test) (when remaining (psw ", ")))
  "; "
  (loop for (step . remaining) on steps do
       (ps-print step) (when remaining (psw ", ")))
  ") "
  (ps-print body-block))

(defprinter ps-js:for-in (var object body-block)
  "for (var "(ps-print var)" in "(ps-print object)") "
  (ps-print body-block))

(defprinter (ps-js:with ps-js:while) (expression body-block)
  (print-op op)" ("(ps-print expression)") "
  (ps-print body-block))

(defprinter ps-js:switch (test &rest clauses)
  "switch ("(ps-print test)") {"
  (flet ((print-body-statements (body-statements)
           (incf *indent-level*)
           (loop for statement in body-statements do
                (progn (newline-and-indent)
                       (ps-print statement)
                       (psw #\;)))
           (decf *indent-level*)))
    (loop for (val . statements) in clauses
       do (progn (newline-and-indent)
                 (if (eq val 'ps-js:default)
                     (progn (psw "default:")
                            (print-body-statements statements))
                     (progn (psw "case ") (ps-print val) (psw #\:)
                            (print-body-statements statements))))))
  (newline-and-indent)
  "}")

(defprinter ps-js:try (body-block &key catch finally)
  "try "(ps-print body-block)
  (when catch
    (psw " catch ("(symbol-to-js-string (first catch))") ")
    (ps-print (second catch)))
  (when finally
    (psw " finally ") (ps-print finally)))

(defprinter ps-js:regex (regex)
  (let ((slash (unless (and (> (length regex) 0) (char= (char regex 0) #\/)) "/")))
    (psw (concatenate 'string slash regex slash))))

(defprinter ps-js:instanceof (value type)
  "("(print-op-argument op value)" instanceof "(print-op-argument op type)")")

(defprinter ps-js:escape (literal-js)
  ;; literal-js should be a form that evaluates to a string containing
  ;; valid JavaScript
  (psw literal-js))
