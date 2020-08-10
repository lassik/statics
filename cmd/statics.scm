(import (scheme base) (scheme read) (scheme write) (scheme char))
(import (gambit match))

(define (displayln x) (display x) (newline))
(define sa string-append)

(define (delimit-map delimiter mapfun xs)
  (if (null? xs) ""
      (let loop ((acc (mapfun (car xs))) (xs (cdr xs)))
        (if (null? xs) acc
            (loop (sa acc delimiter (mapfun (car xs)))
                  (cdr xs))))))

(define (par-delimit-map delimiter mapfun xs)
  (sa "(" (delimit-map delimiter mapfun xs) ")"))

(define (translate-integer-literal x)
  (unless (exact-integer? x) (error "Not an integer:" x))
  (if (< x 0) (sa "~" (number->string (abs x)))
      (number->string x)))

(define (translate-string-literal x)
  (unless (string? x) (error "Not a string:" x))
  (with-output-to-string (lambda () (write x))))

(define (translate-name name)
  (unless (symbol? name) (error "Bad name:" name))
  (let ((name (symbol->string name)))
    (when (or (= 0 (string-length name))
              (not (char-alphabetic? (string-ref name 0))))
      (error "Bad name:" name))
    (let loop ((cs '()) (i 0))
      (if (= i (string-length name)) (list->string (reverse cs))
          (let ((c (string-ref name i)))
            (cond ((char=? #\- c)
                   (loop (cons #\_ cs) (+ i 1)))
                  ((or (char-alphabetic? c) (char-numeric? c))
                   (loop (cons c cs) (+ i 1)))
                  (else
                   (error "Bad name:" name))))))))

(define (names-tuple vars)
  (cond ((null? vars) "()")
        ((null? (cdr vars)) (translate-name (car vars)))
        (else (sa "("
                  (let loop ((acc (translate-name (car vars)))
                             (vars (cdr vars)))
                    (if (null? vars) acc
                        (loop (sa acc ", " (translate-name (car vars)))
                              (cdr vars))))
                  ")"))))

(define (translate-binding prefix binding)
  (match binding
    ((,name ,value)
     (sa prefix "val "
         (translate-name name) " = " (translate-expr value) "\n"))))

(define (translate-tuple tail)
  (par-delimit-map ", " translate-expr tail))

(define (translate-body body)
  (cond ((null? body) "()")
        ((null? (cdr body)) (translate-expr (car body)))
        (else (sa "("
                  (let loop ((acc (translate-expr (car body)))
                             (body (cdr body)))
                    (if (null? body) acc
                        (loop (sa acc "; " (translate-expr (car body)))
                              (cdr body))))
                  ")"))))

(define (translate-named-function name args body)
  (sa "fun " (translate-name name) " " (names-tuple args)
      " = " (translate-body body)))

(define (translate-fun-call head tail)
  (sa (translate-expr head) " "
      (cond ((null? tail) "()")
            ((null? (cdr tail)) (translate-expr (car tail)))
            (else (translate-tuple tail)))))

(define (translate-expr expr)
  (match expr

    (,const
     when (integer? const)
     (translate-integer-literal const))

    (,const
     when (string? const)
     (translate-string-literal const))

    (,var
     when (symbol? var)
     (translate-name var))

    ((if ,test-expr ,then-expr ,else-expr)
     (sa "if " (translate test-expr) " then "
         (translate then-expr)
         (translate else-expr)))

    ((let (,binding0 . ,bindings) ,body0 . ,body)
     (sa (translate-binding "let " binding0)
         (apply sa (map (lambda (binding)
                          (translate-binding "    " binding))
                        bindings))
         "in " (translate-expr `(begin ,body0 ,@body)) " "
         "end"))

    ((let ,name (,binding0 . ,bindings) ,body0 . ,body)
     (let ((bindings (cons binding0 bindings))
           (body (cons body0 body)))
       (sa "let " (translate-named-function name (map car bindings) body)
           " in " (translate-fun-call name (map cadr bindings))
           " end")))

    ((begin . ,body)
     (translate-body body))

    ((lambda (,var0 . ,vars) ,body0 . ,body)
     (sa "(fn " (names-tuple (cons var0 vars)) " => "
         (translate-body (cons body0 body))
         ")"))

    ((*) (translate-integer-literal 1))
    ((* ,operand) (translate-expr `(* 1 ,operand)))
    ((* . ,operands) (par-delimit-map " * " translate-expr operands))

    ((+) (translate-integer-literal 0))
    ((+ ,operand) (translate-expr `(+ 0 ,operand)))
    ((+ . ,operands) (par-delimit-map " + " translate-expr operands))

    ((-) (error "Minus needs one or more arguments"))
    ((- ,operand) (translate-expr `(- 0 ,operand)))
    ((- . ,operands) (par-delimit-map " - " translate-expr operands))

    ((/) (error "Divide needs one or more arguments"))
    ((/ ,operand) (translate-expr `(/ 1 ,operand)))
    ((/ . ,operands) (par-delimit-map " div " translate-expr operands))

    ((,head . ,tail)
     (translate-fun-call head tail))

    (,_ (error "Huh?"))))

(define (translate-top-level expr)
  (match expr

    ((define (,name . ,args) . ,body)
     (sa (translate-named-function name args body) ";"))

    ((define ,name ,value)
     (translate-integer-literal const))

    (,_ (error "Huh?"))))

(displayln (translate-top-level
            `(define (foo)
               (let loop ((x 5))
                 ((lambda (a b) (+ 2 4 x (/ -5)))
                  "a" "bb")))))
