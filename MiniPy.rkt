#lang eopl

;; ======================================================================================
;;
;;  /$$$$$$$  /$$$$$$$   /$$$$$$  /$$     /$$ /$$$$$$$$  /$$$$$$  /$$$$$$$$ /$$$$$$  
;;  | $$__  $$| $$__  $$ /$$__  $$|  $$   /$$/| $$_____/ /$$__  $$|__  $$__//$$__  $$ 
;;  | $$  \ $$| $$  \ $$| $$  \ $$ \  $$ /$$/ | $$      | $$  \__/   | $$  | $$  \ $$ 
;;  | $$$$$$$/| $$$$$$$/| $$  | $$  \  $$$$/  | $$$$$   | $$         | $$  | $$  | $$ 
;;  | $$____/ | $$__  $$| $$  | $$   \  $$/   | $$__/   | $$         | $$  | $$  | $$ 
;;  | $$      | $$  \ $$| $$  | $$    | $$    | $$      | $$    $$   | $$  | $$  | $$ 
;;  | $$      | $$  | $$|  $$$$$$/    | $$    | $$$$$$$$|  $$$$$$/   | $$  |  $$$$$$/ 
;;  |__/      |__/  |__/ \______/     |__/    |________/ \______/    |__/   \______/ 
                                                                                 
;;  ███╗   ███╗██╗███╗   ██╗██╗██████╗ ██╗   ██╗
;;  ████╗ ████║██║████╗  ██║██║██╔══██╗╚██╗ ██╔╝
;;  ██╔████╔██║██║██╔██╗ ██║██║██████╔╝ ╚████╔╝ 
;;  ██║╚██╔╝██║██║██║╚██╗██║██║██╔═══╝   ╚██╔╝  
;;  ██║ ╚═╝ ██║██║██║ ╚████║██║██║        ██║   
;;  ╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝╚═╝╚═╝        ╚═╝
;;
;; ========================================================================================

;; Integrantes del grupo #15:
;; - Jonathan Aristizabal Vargas - 2322626
;; - Andrey Quiceno Cabrera      - 2326081
;; - Francesco García Vargas     - 

;; Fecha de entrega: 03-05-2025
;; ========================================================================================



;; ======================================================================================
;;;;;;;;;;;;;;;;; Grammar of the language ;;;;;;;;;;;;;;;;;

;;<program>         ::= <expression>

;; <expression>      ::= <number>
;;                    | <float>
;;                    | <hex>
;;                    | <string>
;;                    | <bool>
;;                    | <identifier>
;;                    | <primitive>(<expression> { , <expression> }*)
;;                    | if <expr-bool> then <expression> [else <expression>] end
;;                    | let <identifier> = <expression> { , <identifier> = <expression> } in <expression>
;;                    | var <identifier> = <expression> { , <identifier> = <expression> } in <expression>
;;                    | const <identifier> = <expression> { , <identifier> = <expression> } in <expression>
;;                    | rec <identifier>(<identifier> { , <identifier> }*) = <expression> { ... } in <expression>
;;                    | proc(<identifier> { , <identifier> }*) <expression>
;;                    | (<expression> <expression>*)
;;                    | begin <expression> { ; <expression> }+ end
;;                    | while <expr-bool> do <expression> done
;;                    | for <identifier> in <expression> do <expression> done
;;                    | tuple [ <expression> { ; <expression> } ]
;;                    | { <identifier> = <expression> { ; <identifier> = <expression> } }
;;                    | [ <expression> { ; <expression> } ]
;;                    | <expr-bool>
;;                    | ref <expression>
;;                    | deref <expression>
;;                    | set-ref <expression> <expression>
;;                    | new <identifier>(<expression> { , <expression> })
;;                    | send <expression> <identifier>(<expression> { , <expression> })
;;                    | super <identifier>(<expression> { , <expression> })

;; <primitive>       ::= + | - | * | add1 | sub1 | zero? | list | cons | nil | car | cdr | null?
;;                   ::= string-length | string-append
;;                   ::= create-list | append | ref-list | set-list 
;;                   ::= crear-tupla | tupla? | ref-tuple
;;                   ::= crear-registro | registros? | ref-registro | set-registro
 
;; <expr-bool>       ::= <pred-prim>(<expression>, <expression>)
;;                    | <oper-bin-bool>(<expr-bool>, <expr-bool>)
;;                    | <oper-un-bool>(<expr-bool>)

;; <pred-prim>       ::= < | > | <= | >= | == | <>

;; <oper-bin-bool>   ::= and | or
;; <oper-un-bool>    ::= not

;; <list>            ::= [ <expression> { ; <expression> } ]
;; <tuple>           ::= tuple [ <expression> { ; <expression> } ]
;; <record>          ::= { <identifier> = <expression> { ; <identifier> = <expression> } }

;; <bool>            ::= true | false
;; <string>          ::= "..." 
;; <hex>             ::= 0x[0-9A-Fa-f]
;; <float>           ::= [0-9].[0-9]

;; <class-decl>      ::= class <identifier> extends <identifier>
;;                         { field <identifier> }*
;;                         { method <identifier>(<identifier> { , <identifier> }) <expression> }*
;;
;; ========================================================================================



;; ========================================================================================
;;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

;; Lexical spec: skip whitespace/comments; identifier → symbol; number → numeric literal
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)
    ; Hexadecimal
    (hexadecimal ("0x" (arbno (or digit (range "A" "F") (range "a" "f")))) hex-token)
    ;; Float: número con punto decimal
    (float ((arbno digit) "." (arbno digit)) float-token)
    ))

;; Complete grammar
(define the-grammar
  '((program ((arbno class-decl) expression) a-program)
    (expression ("mostrar") mostrar-exp)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (number) number-exp)
    
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
   (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("set" identifier "=" expression) varassign-exp)

    ;;;;;; Building the circuits ;;;;;;
    (circuit ("(" "circuit" "(" "gate-list" gate-list ")" ")") a-circuit)
    (gate-list () empty-gate-list)
    (gate-list (gate gate-list) cons-gate-list)
    (gate ("(" "gate" identifier "(" "type" type ")" "(" "input-list" input-list ")" ")")
          a-gate)
    (type ("xor") xor-type)
    (input-list () empty-input-list)
    (input-list (input input-list) cons-input-list)
    (input (identifier) ref-input)
    (input (expression) bool-input)
    ;; Primitivas de circuitos
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)

    ;;;;;; Project implementations ;;;;;;
    (expression (string) string-exp)
    (expression ("True") true-lit)
    (expression ("False") false-lit)
    ; Definitions
    (expression
      ("var" identifier "=" expression
          (arbno "," identifier "=" expression)
          "in" expression)
     var-decls-exp)
    (expression
      ("const" identifier "=" expression
          (arbno "," identifier "=" expression)
          "in" expression)
     const-decls-exp)
     (expression
       ("rec" identifier "=" expression
          (arbno "," identifier "=" expression)
          "in" expression)
     rec-decls-exp)
    ; Data builders
    (expression ("[" expression (arbno (";" expression)) "]") list-exp)
    (expression ("tupla" "[" expression (arbno (";" expression)) "]") tuple-exp)
    (expression ("{" identifier "=" expression (arbno (";" identifier "=" expression)) "}") record-exp)
    ; Structures of control
    (expression
      ("if" expression "then" expression "else" expression "end")
      if-exp)
    (expression
      ("while" expression "do" expression "done")
      while-exp)
    (expression
      ("for" identifier "in" expression "do" expression "done")
       for-exp)
    ; Bool expressions
    (expression
      (primitive "(" expression "," expression ")")
       pred-bool-exp)
    (expression
      (type "(" expression "," expression ")")
       bin-bool-exp)
    (expression
      (type "(" expression ")")
       una-bool-exp)
    ; Primitives of lists
    (primitive ("crear-lista(" expression (arbno "," expression) ")")
           create-list-prim)
    (primitive ("lista?(" expression ")")
               list?-prim)
    (primitive ("vacio?(" expression ")")
               empty-list?-prim)
    (primitive ("vacio()")
               empty-list-prim)
    (primitive ("cabeza(" expression ")")
               head-list-prim)
    (primitive ("cola(" expression ")")
               tail-list-prim)
    (primitive ("append(" expression "," expression ")")
               append-list-prim)
    (primitive ("ref-list(" expression "," expression ")")
               ref-list-prim)
    (primitive ("set-list(" expression "," expression "," expression ")")
               set-list-prim)
    ; Primitivas de tuplas
    (primitive ("crear-tupla(" expression (arbno "," expression) ")")
           create-tuple-prim)
    (primitive ("tupla?(" expression ")")
               tuple?-prim)
    (primitive ("vacio?(" expression ")")
               empty-tuple?-prim)
    (primitive ("vacio()")
               empty-tuple-prim)
    (primitive ("cabeza(" expression ")")
               head-tuple-prim)
    (primitive ("cola(" expression ")")
               tail-tuple-prim)
    (primitive ("ref-tuple(" expression "," expression ")")
               ref-tuple-prim)
    ; Primitives for strings
    (primitive ("longitud" "(" expression ")") string-length-prim)
    (primitive ("concatenar" "(" expression (arbno "," expression) ")") string-concat-prim)
    ; Predicate primitives
    (primitive ("<") less-than-prim)
    (primitive (">") greater-than-prim)
    (primitive ("<=") less-equal-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("==") equal-prim)
    (primitive ("<>") not-equal-prim)
    ; Binary and unary operators
    (type ("and") and-type)
    (type ("or") or-type)
    (type ("not") not-type)
    ; Arithmetic primitives
    (primitive ("%")     modulo-prim)
              
    ;;;;;; Basic primitives ;;;;;;
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    (primitive ("list") list-prim)
    (primitive ("cons") cons-prim)
    (primitive ("nil")  nil-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)
    (primitive ("null?") null?-prim)
    
    ;;;;;; Oriented objects Programming grammar ;;;;;;
    (class-decl                         
      ("class" identifier 
        "extends" identifier                   
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("method" identifier 
        "("  (separated-list identifier ",") ")" ; method ids
        expression 
        )
      a-method-decl)

    (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)

    (expression                                
      ("super" identifier    "("  (separated-list expression ",") ")")
      super-call-exp)
    ))

;;;;;;;;;;;;;;;;; AST Generation and Scanner/Parser Functions ;;;;;;;;;;;;;;;;;

;; Generates the AST data-type definitions based on the lexical specification and grammar
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

;; Defines a function to list all generated AST data-type definitions
(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

;; Takes a code string, tokenizes it, and parses it into an AST
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Takes a code string and returns only the list of tokens
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


;; ========================================================================================



;; ========================================================================================
;;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (empty-env)))

; Evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (mostrar-exp () the-class-env)
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body)
        (closure ids body env))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands      rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression 
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      (varassign-exp (id rhs-exp)
        (setref!
          (apply-env-ref env id)
          (eval-expression rhs-exp env))
        1)

      ;;;;;; Oriented objet ;;;;;;
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))

      ;;;;;; Building the circuits ;;;;;;
      (bool-exp (b)
                (cases bool b
                  (true-lit () #t)
                  (false-lit () #f)))
      (type-exp (ty)
                (cases type ty
                  (and-type () 'and)
                  (or-type () 'or)
                  (not-type () 'not)
                  (xor-type () 'xor)))
      (circuit-exp (circ)
                   circ)
      
      ;;;;;; Project implementations
      ; Eval definitions
      (const-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env-const ids args env))))
      (var-exp* (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env-var ids args env))))
      ; Eval data Builders
      (list-exp (expresiones)
        (map (lambda (e) (evaluar-expresión e entorno)) expresiones))
      (tuple-exp (expresiones)
        (list->vector (map (lambda (e) (evaluar-expresión e entorno)) expresiones)))
      (record-exp (campos)
        (hacer-registro
         (map (lambda (campo)
                (cons (car campo) (evaluar-expresión (cdr campo) entorno)))
              campos)))
      ; Eval structures of control
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (while-exp (cond-exp body-exp)
        (let loop ()
          (if (true-value? (eval-expression cond-exp env))
              (begin
                (eval-expression body-exp env)
                (loop))
              1)))
      (for-exp (id from-exp to-exp body-exp)
        (let ((from (eval-expression from-exp env))
              (to (eval-expression to-exp env)))
          (let loop ((i from))
            (if (> i to)
                1
                (begin
                  (eval-expression body-exp (extend-env (list id) (list i) env))
                  (loop (+ i 1)))))))
      ; Eval bool expressions
      (pred-bool-exp (exp1 exp2)
        (let ((valor1 (eval-expression exp1 env))
              (valor2 (eval-expression exp2 env)))
          (and valor1 valor2)))
      (bin-bool-exp (exp1 exp2)
        (let ((valor1 (eval-expression exp1 env))
              (valor2 (eval-expression exp2 env)))
          (if (equal? (type exp) 'and)
              (and valor1 valor2)
              (if (equal? (type exp) 'or)
                  (or valor1 valor2)))))
      (bin-bool-exp (exp1)
        (let ((valor1 (eval-expression exp1 env)))
          (if (equal? (type exp) 'not)
              (not valor1))))

      )))

;; ========================================================================================



;; ========================================================================================
; Lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

; apply-primitive
(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim
      ;; Primitivas básicas
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      (list-prim () args)               ;already a list
      (nil-prim () '())
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (cons-prim () (cons (car args) (cadr args)))
      (null?-prim () (if (null? (car args)) 1 0))

      ;; Primitiva: eval-circuit(circuito, entrada)
      (eval-circuit-prim ()
                         (let ((circ (car args)))
                           (eval-circuit circ env)))
                         

      ;; Primitiva: connect-circuits(c1, c2, input)
      (connect-circuits-prim ()
                             (connect-circuits (car args) (cadr args) (caddr args)))

      ;; Primitiva: merge-circuits(c1, c2, tipo, nombre)
      (merge-circuits-prim ()
                           (let ((circ1 (car args))
                                 (circ2 (cadr args))
                                 (type-symbol (caddr args)) ; Símbolo como 'and
                                 (new-name (cadddr args)))  ; Nombre de la nueva compuerta (debe ser un símbolo)
                             (let ((gate-type 
                                    (case type-symbol
                                      ('and (and-type))
                                      ('or (or-type))
                                      ('xor (xor-type))
                                      )))
                               (merge-circuits circ1 circ2 gate-type new-name))))
      
      ;;;;;; Project implementations ;;;;;;
      ;; Apply primitives for lists
      (create-list-prim () args)
      (list?-prim () (list? (car args)))
      (empty-list?-prim () (null? (car args)))
      (empty-list-prim () '())
      (head-list-prim () (car (car args)))
      (tail-list-prim () (cdr (car args)))
      (append-list-prim () (append (car args) (cadr args)))
      (ref-list-prim () (list-ref (car args) (cadr args)))
      (set-list-prim ()
        (let* ((lst (car args))
               (idx (cadr args))
               (val (caddr args))
               (new-list (list-copy lst)))
          (list-set! new-list idx val)
          new-list))
      ; Apply primitives for tuples
      (create-tuple-prim () (list->vector args))
      (tuple?-prim () (vector? (car args)))
      (empty-tuple?-prim () (= (vector-length (car args)) 0))
      (empty-tuple-prim () (vector))
      (head-tuple-prim () (vector-ref (car args) 0))
      (tail-tuple-prim ()
        (let* ((vec (car args))
               (len (vector-length vec)))
          (list->vector (vector->list vec 1 len))))
      (ref-tuple-prim () (vector-ref (car args) (cadr args)))
      ; Apply primitives for strings
      (string-length-prim ()
        (string-length (car args)))
      (string-concat-prim ()
        (let loop ((rest args) (acc ""))
          (if (null? rest)
              acc
              (loop (cdr rest) (string-append acc (car rest))))))
      ;; Aply primitives for predicates
      (less-than-prim ()
        (< (car args) (cadr args)))
      (greater-than-prim ()
        (> (car args) (cadr args)))
      (less-equal-prim ()
        (<= (car args) (cadr args)))
      (greater-equal-prim ()
        (>= (car args) (cadr args)))
      (equal-prim ()
        (= (car args) (cadr args)))
      (not-equal-prim ()
        (not (= (car args) (cadr args))))
      ;; Aply arithmetic primitives 
      (modulo-prim () (modulo (car args) (cadr args)))

))) 

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;Determina si es un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))
;; ========================================================================================



;; ========================================================================================
;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
;; ========================================================================================



;; ========================================================================================
;^;;;;;;;;;;;;;;; manage environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;(define rib-find-position 
;  (lambda (sym los)
;    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))
;; ========================================================================================



;; ========================================================================================
;;;;;;;;;;;;;;;; evaluate circuits ;;;;;;;;;;;;;;;

; eval-circuit: evalúa un circuito completo compuesto por una lista de compuertas.
(define eval-circuit
  (lambda (circ env)
    (cases circuit circ
      (a-circuit (gate-list)
        (let ((env-updated (eval-gate-list gate-list env)))
          (apply-env env-updated (get-last-gate-id gate-list)))))))

; eval-gate-list: evalúa secuencialmente una lista de compuertas.
(define eval-gate-list
  (lambda (gl env)
    (cases gate-list gl
      (empty-gate-list () env)
      (cons-gate-list (g r)
        (let ((env-updated (eval-gate g env)))
          (eval-gate-list r env-updated))))))

; eval-gate: evalúa una compuerta lógica usando un ambiente dado.
(define eval-gate
  (lambda (g env)
    (cases gate g
      (a-gate (id typ input-list)
        (let ((val (apply-gate typ (eval-input-list input-list env))))
          (extend-env (list id) (list val) env))))))

; eval-input-list: evalúa una lista de entradas de una compuerta lógica.
(define eval-input-list
  (lambda (il env)
    (cases input-list il
      (empty-input-list () '())
      (cons-input-list (input rest)
        (cons (eval-input input env) (eval-input-list rest env))))))

; eval-input: evalúa una entrada individual (referencia o literal booleano).
(define eval-input
  (lambda (inp env)
    (cases input inp
      (ref-input (id) (apply-env env id))
      (bool-input (b)
        (cases bool b
          (true-lit () #t)
          (false-lit () #f))))))

; apply-gate: aplica el comportamiento lógico de una compuerta a una lista de valores booleanos
(define apply-gate
  (lambda (t inputs)
    (cases type t
      (and-type ()
        (let inps ((lst inputs))
          (cond ((null? lst) #t)
                ((eq? (car lst) #f) #f)
                (else (inps (cdr lst))))))
      (or-type ()
        (let inps ((lst inputs))
          (cond ((null? lst) #f)
                ((eq? (car lst) #t) #t)
                (else (inps (cdr lst))))))
      (not-type ()
        (if (eq? (car inputs) #t) #f #t))
      (xor-type ()
        (let inps ((lst inputs) (count 0))
          (if (null? lst)
              (if (= count 1) #t #f)
              (inps (cdr lst) (+ count (if (car lst) 1 0)))))))))


;;;;;;;;;;;;;;;; primitives of circuits ;;;;;;;;;;;;;;;

; combine-gates: une dos listas de compuertas en una sola
(define combine-gates
  (lambda (g1 g2)
    (cases gate-list g1
      (empty-gate-list () g2)
      (cons-gate-list (g rest)
        (cons-gate-list g (combine-gates rest g2))))))

; connect-circuits: combina dos circuitos, reemplazando una entrada del segundo circuito
(define connect-circuits
  (lambda (c1 c2 input-to-replace)
    (cases circuit c1
      (a-circuit (gates1)
        (cases circuit c2
          (a-circuit (gates2)
            (make-a-circuit
             (combine-gates
              gates1
              (replace-gates gates2  input-to-replace (get-last-gate-id gates1))))))))))

; merge-circuits: une dos circuitos agregando una nueva compuerta lógica al final   
(define merge-circuits
  (lambda (c1 c2 gate-type new-name)
    (cases circuit c1
      (a-circuit (gates1)
        (cases circuit c2
          (a-circuit (gates2)
            (let* (
              (last-id1 (get-last-gate-id gates1)) ; ID última compuerta de C1
              (last-id2 (get-last-gate-id gates2))  ; ID última compuerta de C2
              (new-gate (a-gate new-name 
                              gate-type 
                              (cons-input-list 
                                (ref-input last-id1)
                                (cons-input-list 
                                  (ref-input last-id2)
                                  (empty-input-list)))))
              (combined-gates (append-gate-lists 
                                gates1 
                                (append-gate-lists 
                                  gates2 
                                  (cons-gate-list new-gate (empty-gate-list))))))
            (a-circuit combined-gates))))))))


;;;;;;;;;;;;;;;; Auxiliary functions ;;;;;;;;;;;;;;;; 

; get-last-gate-id: obtiene el identificador (id) de la última compuerta en una lista de compuertas (gate-list)
(define get-last-gate-id
  (lambda (gl)
    (cases gate-list gl
      (empty-gate-list () '()) 
      (cons-gate-list (g rest)
        (cases gate-list rest
          (empty-gate-list ()
            (cases gate g
              (a-gate (id type inputs) id)))
          (cons-gate-list (g2 rest2)
            (get-last-gate-id rest)))))))

; replace-inputs: recorre una lista de entradas de una compuerta
(define replace-inputs
  (lambda (ilist input-to-replace new-id)
    (cases input-list ilist
      (empty-input-list ()
        (empty-input-list))
      (cons-input-list (i rest)
        (cons-input-list
         (cases input i
           (ref-input (id)
             (if (equal? id input-to-replace)
                 (ref-input new-id)
                 i))
           (bool-input (b) i))
         (replace-inputs rest input-to-replace new-id))))))

; replace-gates: recorre una lista de compuertas (gate-list)
(define replace-gates
  (lambda (gl input-to-replace new-id)
    (cases gate-list gl
      (empty-gate-list ()
        (empty-gate-list))
      (cons-gate-list (g rest)
        (cases gate g
          (a-gate (id type inputs)
            (cons-gate-list
             (a-gate id type (replace-inputs inputs input-to-replace new-id))
             (replace-gates rest input-to-replace new-id))))))))

; combine-gates: une dos listas de compuertas en una sola
(define combine-gates
  (lambda (g1 g2)
    (cases gate-list g1
      (empty-gate-list () g2)
      (cons-gate-list (g rest)
        (cons-gate-list g (combine-gates rest g2))))))

; append-gate-lists: concatena dos listas de compuertas (gate-list) en una sola
(define append-gate-lists
  (lambda (gl1 gl2)
    (cases gate-list gl1
      (empty-gate-list () gl2)
      (cons-gate-list (g rest)
        (cons-gate-list g (append-gate-lists rest gl2))))))

;; ========================================================================================



;; ========================================================================================
;;;;;;;;;;;;;;;;; procedures of plane objects interpreter ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env))))))
               
;^;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val)))
    1))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))

;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

;;;; constructing classes

(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl super-name field-ids)))))))

(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))


;^;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

;^; an object is now just a single part, with a vector representing the
;^; managed storage for the all the fields. 

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name))))) ;\new1

;^;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (super-name symbol?)
    (field-ids (list-of symbol?))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name 'object)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)
          (let ((method (lookup-method m-name ;^ m-decl -> method
                          (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (class-name->super-name host-name))))))))

(define apply-method
  (lambda (method host-name self args)                ;\new5
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (extend-env-refs field-ids fields (empty-env)))))))

(define rib-find-position
  (lambda (name symbols)
    (list-find-last-position name symbols)))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

(define method-environment? (list-of method?)) 

(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of classes (not class decls)

(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        super-name))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))


(define read-eval-print 
  (sllgen:make-rep-loop  "-->" eval-program
                         (sllgen:make-stream-parser
                                  the-lexical-spec 
                                  the-grammar)))

;; ========================================================================================



;; ========================================================================================
;;;;;;;;;;;;;;;; pruebes of the language ;;;;;;;;;;;;;;;;