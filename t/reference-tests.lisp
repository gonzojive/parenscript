(in-package #:parenscript-test)

(cl:declaim (cl:optimize (cl:debug 3)))

;;; These tests were originally generated from the reference manual

(in-suite ref-tests)

(test-ps-js statements-and-expressions-1
  (+ i (if 1 2 3))
  "i + (1 ? 2 : 3);")

(test-ps-js statements-and-expressions-2
  (if 1 2 3)
  "if (1) {
    2;
} else {
    3;
};")

(test-ps-js symbol-conversion-1
  !?#@%
  "bangwhathashatpercent;")

(test-ps-js symbol-conversion-2
  bla-foo-bar
  "blaFooBar;")

(test-ps-js symbol-conversion-3
  *array
  "Array;")

(test-ps-js symbol-conversion-4
  *global-array*
  "GLOBALARRAY;")

(test-ps-js number-literals-1
  1
  "1;")

(test-ps-js number-literals-2
  123.123
  "123.123;")

(test-ps-js number-literals-3
  #x10
  "16;")

(test-ps-js string-literals-1
  "foobar"
  "'foobar';")

(test-ps-js string-literals-2
  "bratzel bub"
  "'bratzel bub';")

(test-ps-js string-literals-3
  "	"
  "'\\t';")

(test-ps-js array-literals-1
  (array)
  "[  ];")

(test-ps-js array-literals-2
  (array 1 2 3)
  "[ 1, 2, 3 ];")

(test-ps-js array-literals-3
  (array (array 2 3)
       (array "foobar" "bratzel bub"))
  "[ [ 2, 3 ], [ 'foobar', 'bratzel bub' ] ];")

(test-ps-js array-literals-4
  (make-array)
  "new Array();")

(test-ps-js array-literals-5
  (make-array 1 2 3)
  "new Array(1, 2, 3);")

(test-ps-js array-literals-6
  (make-array
 (make-array 2 3)
 (make-array "foobar" "bratzel bub"))
  "new Array(new Array(2, 3), new Array('foobar', 'bratzel bub'));")

(test-ps-js object-literals-1
  (create foo "bar" :blorg 1)
  "{ foo : 'bar', 'blorg' : 1 };")

(test-ps-js object-literals-2
  (create foo "hihi"
        blorg (array 1 2 3)
        another-object (create :schtrunz 1))
  "{ foo : 'hihi',
  blorg : [ 1, 2, 3 ],
  anotherObject : { 'schtrunz' : 1 } };")

(test-ps-js object-literals-3
  (getprop an-object 'foo)
  "anObject.foo;")

(test-ps-js object-literals-4
  (@ an-object foo bar)
  "anObject.foo.bar;")

(test-ps-js object-literals-5
  (with-slots (a b c) this
  (+ a b c))
  "this.a + this.b + this.c;")

(test-ps-js regular-expression-literals-1
  (regex "foobar")
  "/foobar/;")

(test-ps-js regular-expression-literals-2
  (regex "/foobar/i")
  "/foobar/i;")

(test-ps-js literal-symbols-1
  T
  "true;")

(test-ps-js literal-symbols-2
  FALSE
  "false;")

(test-ps-js literal-symbols-3
  F
  "false;")

(test-ps-js literal-symbols-4
  (lambda () NIL)
  "function () {
    return null;
};")

(test-ps-js literal-symbols-5
  UNDEFINED
  "undefined;")

(test-ps-js literal-symbols-6
  THIS
  "this;")

(test-ps-js variables-1
  variable
  "variable;")

(test-ps-js variables-2
  a-variable
  "aVariable;")

(test-ps-js variables-3
  *math
  "Math;")

(test-ps-js function-calls-and-method-calls-1
  (blorg 1 2)
  "blorg(1, 2);")

(test-ps-js function-calls-and-method-calls-2
  (foobar (blorg 1 2) (blabla 3 4) (array 2 3 4))
  "foobar(blorg(1, 2), blabla(3, 4), [ 2, 3, 4 ]);")

(test-ps-js function-calls-and-method-calls-3
  ((getprop this 'blorg) 1 2)
  "this.blorg(1, 2);")

(test-ps-js function-calls-and-method-calls-4
  ((aref foo i) 1 2)
  "foo[i](1, 2);")

(test-ps-js function-calls-and-method-calls-5
  ((getprop (aref foobar 1) 'blorg) NIL T)
  "foobar[1].blorg(null, true);")

(test-ps-js operator-expressions-1
  (* 1 2)
  "1 * 2;")

(test-ps-js operator-expressions-2
  (= 1 2)
  "1 === 2;")

(test-ps-js operator-expressions-3
  (* 1 (+ 2 3 4) 4 (/ 6 7))
  "1 * (2 + 3 + 4) * 4 * 6 / 7;")

(test-ps-js operator-expressions-4
  (incf i)
  "++i;")

(test-ps-js operator-expressions-5
  (decf i)
  "--i;")

(test-ps-js operator-expressions-6
  (1- i)
  "i - 1;")

(test-ps-js operator-expressions-7
  (1+ i)
  "i + 1;")

(test-ps-js operator-expressions-8
  (not (< i 2))
  "i >= 2;")

(test-ps-js body-forms-1
  (progn (blorg i) (blafoo i))
  "blorg(i);
blafoo(i);")

(test-ps-js body-forms-2
  (+ i (progn (blorg i) (blafoo i)))
  "i + (blorg(i), blafoo(i));")

(test-ps-js function-definition-1
  (defun a-function (a b)
  (return (+ a b)))
  "function aFunction(a, b) {
    return a + b;
};")

(test-ps-js function-definition-2
  (lambda (a b) (return (+ a b)))
  "function (a, b) {
    return a + b;
};")

(test-ps-js assignment-1
  (setf a 1)
  "a = 1;")

(test-ps-js assignment-2
  (setf a 2 b 3 c 4 x (+ a b c))
  "a = 2;
b = 3;
c = 4;
x = a + b + c;")

(test-ps-js assignment-3
  (setf a (+ a 2 3 4 a))
  "a = a + 2 + 3 + 4 + a;")

(test-ps-js assignment-4
  (setf a (- 1 a))
  "a = 1 - a;")

(test-ps-js assignment-5
  (let ((a 1) (b 2))
  (psetf a b b a))
  "var a = 1;
var b = 2;
var _js1 = b;
var _js2 = a;
a = _js1;
b = _js2;")

(test-ps-js assignment-6
  (setq a 1)
  "a = 1;")

(test-ps-js assignment-8
  (progn
    (defun (setf color) (new-color el)
      (setf (getprop (getprop el 'style) 'color) new-color))
    (setf (color some-div) (+ 23 "em")))
  "function __setf_color(newColor, el) {
    return el.style.color = newColor;
};
__setf_color(23 + 'em', someDiv);")

(test-ps-js assignment-10
  (progn
    (defsetf left (el) (offset)
      `(setf (getprop (getprop ,el 'style) 'left) ,offset))
    (setf (left some-div) (+ 123 "px")))
  "var _js2 = someDiv;
var _js1 = 123 + 'px';
_js2.style.left = _js1;")

(test-ps-js assignment-12
  (macrolet ((left (el)
             `(getprop ,el 'offset-left)))
    (left some-div))
  "someDiv.offsetLeft;")

(test-ps-js single-argument-statements-1
  (return 1)
  "return 1;")

(test-ps-js single-argument-statements-2
  (throw "foobar")
  "throw 'foobar';")

(test-ps-js single-argument-expression-1
  (delete (new (*foobar 2 3 4)))
  "delete new Foobar(2, 3, 4);")

(test-ps-js single-argument-expression-2
  (if (= (typeof blorg) *string)
    (alert (+ "blorg is a string: " blorg))
    (alert "blorg is not a string"))
  "if (typeof blorg === String) {
    alert('blorg is a string: ' + blorg);
} else {
    alert('blorg is not a string');
};")

(test-ps-js conditional-statements-1
  (if ((@ blorg is-correct))
    (progn (carry-on) (return i))
    (alert "blorg is not correct!"))
  "if (blorg.isCorrect()) {
    carryOn();
    return i;
} else {
    alert('blorg is not correct!');
};")

(test-ps-js conditional-statements-2
  (+ i (if ((@ blorg add-one)) 1 2))
  "i + (blorg.addOne() ? 1 : 2);")

(test-ps-js conditional-statements-3
  (when ((@ blorg is-correct))
  (carry-on)
  (return i))
  "if (blorg.isCorrect()) {
    carryOn();
    return i;
};")

(test-ps-js conditional-statements-4
  (unless ((@ blorg is-correct))
  (alert "blorg is not correct!"))
  "if (!blorg.isCorrect()) {
    alert('blorg is not correct!');
};")

(test-ps-js variable-declaration-1
  (defvar *a* (array 1 2 3))
  "var A = [ 1, 2, 3 ];")

(test-ps-js variable-declaration-2
  (progn 
  (defvar *a* 4)
  (let ((x 1)
        (*a* 2))
    (let* ((y (+ x 1))
           (x (+ x y)))
      (+ *a* x y))))
  "var A = 4;
var x = 1;
var A_TMPSTACK1;
try {
    A_TMPSTACK1 = A;
    A = 2;
    var y = x + 1;
    var x2 = x + y;
    A + x2 + y;
} finally {
    A = A_TMPSTACK1;
};")

(test-ps-js iteration-constructs-1
  (do* ((a) b (c (array "a" "b" "c" "d" "e"))
      (d 0 (1+ d))
      (e (aref c d) (aref c d)))
     ((or (= d (@ c length)) (string= e "x")))
  (setf a d b e)
  (funcall (@ document write) (+ "a: " a " b: " b "<br/>")))
  "for (var a = null, b = null, c = ['a', 'b', 'c', 'd', 'e'], d = 0, e = c[d]; !(d === c.length || e === 'x'); d += 1, e = c[d]) {
    a = d;
    b = e;
    document.write('a: ' + a + ' b: ' + b + '<br/>');
};")

(test-ps-js iteration-constructs-2
  (do ((i 0 (1+ i))
     (s 0 (+ s i (1+ i))))
    ((> i 10))
  (funcall (@ document write) (+ "i: " i " s: " s "<br/>")))
  "var i = 0;
var s = 0;
for (; i <= 10; ) {
    document.write('i: ' + i + ' s: ' + s + '<br/>');
    var _js1 = i + 1;
    var _js2 = s + i + i + 1;
    i = _js1;
    s = _js2;
};")

(test-ps-js iteration-constructs-3
  (do* ((i 0 (1+ i))
      (s 0 (+ s i (1- i))))
     ((> i 10))
  ((@ document write) (+ "i: " i " s: " s "<br/>")))
  "for (var i = 0, s = 0; i <= 10; i += 1, s = s + i + i - 1) {
    document.write('i: ' + i + ' s: ' + s + '<br/>');
};")

(test-ps-js iteration-constructs-4
  (let ((arr (array "a" "b" "c" "d" "e")))
  (dotimes (i (@ arr length))
    ((@ document write) (+ "i: " i " arr[i]: " (aref arr i) "<br/>"))))
  "var arr = ['a', 'b', 'c', 'd', 'e'];
for (var i = 0; i < arr.length; i += 1) {
    document.write('i: ' + i + ' arr[i]: ' + arr[i] + '<br/>');
};")

(test-ps-js iteration-constructs-5
  (let ((res 0))
  (alert (+ "Summation to 10 is "
            (dotimes (i 10 res)
              (incf res (1+ i))))))
  "var res = 0;
alert('Summation to 10 is ' + (function () {
    for (var i = 0; i < 10; i += 1) {
        res += i + 1;
    };
    return res;
})());")

(test-ps-js iteration-constructs-6
  (let ((l (list 1 2 4 8 16 32)))
  (dolist (c l)
    ((@ document write) (+ "c: " c "<br/>"))))
  "var l = [1, 2, 4, 8, 16, 32];
for (var c = null, _js_idx1 = 0; _js_idx1 < l.length; _js_idx1 += 1) {
    c = l[_js_idx1];
    document.write('c: ' + c + '<br/>');
};")

(test-ps-js iteration-constructs-7
  (let ((l '(1 2 4 8 16 32))
        (s 0))
    (alert (+ "Sum of " l " is: "
              (dolist (c l s)
                (incf s c)))))
  "var l = [1, 2, 4, 8, 16, 32];
var s = 0;
alert('Sum of ' + l + ' is: ' + (function () {
    for (var c = null, _js_idx1 = 0; _js_idx1 < l.length; _js_idx1 += 1) {
        c = l[_js_idx1];
        s += c;
    };
    return s;
})());")

(test-ps-js iteration-constructs-8
  (let ((obj (create a 1 b 2 c 3)))
  (for-in (i obj)
    ((@ document write) (+ i ": " (aref obj i) "<br/>"))))
  "var obj = { a : 1, b : 2, c : 3 };
for (var i in obj) {
    document.write(i + ': ' + obj[i] + '<br/>');
};")

(test-ps-js iteration-constructs-9
  (while ((@ film is-not-finished))
  ((@ this eat) (new *popcorn)))
  "while (film.isNotFinished()) {
    this.eat(new Popcorn);
};")

(test-ps-js the-case-statement-1
  (case (aref blorg i)
  ((1 "one") (alert "one"))
  (2 (alert "two"))
  (t (alert "default clause")))
  "switch (blorg[i]) {
    case 1:
    case 'one':
        alert('one');
        break;
    case 2:
        alert('two');
        break;
    default: 
        alert('default clause');
    };")

(test-ps-js the-case-statement-2
  (switch (aref blorg i)
  (1 (alert "If I get here"))
  (2 (alert "I also get here"))
  (default (alert "I always get here")))
  "switch (blorg[i]) {
    case 1: alert('If I get here');
    case 2: alert('I also get here');
    default: alert('I always get here');
};")

(test-ps-js the-with-statement-1
  (with (create foo "foo" i "i")
  (alert (+ "i is now intermediary scoped: " i)))
  "with ({ foo : 'foo', i : 'i' }) {
    alert('i is now intermediary scoped: ' + i);
};")

(test-ps-js the-try-statement-1
  (try (throw "i")
 (:catch (error)
   (alert (+ "an error happened: " error)))
 (:finally
   (alert "Leaving the try form")))
  "try {
    throw 'i';
} catch (error) {
    alert('an error happened: ' + error);
} finally {
    alert('Leaving the try form');
};")

(test-ps-js the-html-generator-1
  (ps-html ((:a :href "foobar") "blorg"))
  "'<A HREF=\"foobar\">blorg</A>';")

(test-ps-js the-html-generator-2
  (ps-html ((:a :href (generate-a-link)) "blorg"))
  "['<A HREF=\"', generateALink(), '\">blorg</A>']['join']('');")

(test-ps-js the-html-generator-3
  (funcall (getprop document 'write)
           (ps-html ((:a :href "#"
                         :onclick (ps-inline (transport))) "link")))
  "document.write(['<A HREF=\"#\" ONCLICK=\"', 'javascript:' + 'transport()', '\">link</A>']['join'](''));")

(test-ps-js the-html-generator-4
  (let ((disabled nil)
        (authorized t))
    (setf (getprop element 'inner-h-t-m-l)
          (ps-html ((:textarea (or disabled (not authorized)) :disabled "disabled")
                    "Edit me"))))
  "var disabled = null;
var authorized = true;
element.innerHTML = ['<TEXTAREA', disabled || !authorized ? [' DISABLED=\"', 'disabled', '\"']['join']('') : '', '>Edit me</TEXTAREA>']['join']('');")
