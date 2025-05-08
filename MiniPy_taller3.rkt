#lang eopl
(require racket/string)



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
;; <hex>             ::= 16x([0-15]*)
;; <float>           ::= [0-9].[0-9]

;; <class-decl>      ::= class <identifier> extends <identifier>
;;                         { field <identifier> }*
;;                         { method <identifier>(<identifier> { , <identifier> }) <expression> }*
;;
;; ========================================================================================
;; INDICE:
;; 1. Especificación_Léxica
;; 2. Especificación_Sintáctica
;; 3. Parser_Scanner_Interfaz
;; 4. Evaluación_De_Expresiones
;; 5. Evaluación_De_Circuitos
;; 6. Conexión_De_Circuitos_En_Serie
;; 7. Conexión_De_Circuitos_En_Paralelo
;; 8. Funciones_Auxiliares
;; 9. Procedimientos_
;; 10. Ambientes_
;; 11. Funciones_Para_Asignación_De_Variables
;; 11. Funciones_Auxiliares_Para_Encontrar_La_Posición_De_Un_Símbolo
;; 12. Pruebas de funciones principales
;; ========================================================================================
;;*******************************************************************************************


;; 1. Especificación_Léxica
;;*******************************************************************************************
(define the-lexical-spec
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (string
   ("\"" (arbno (not #\")) "\"")
   string)
  (quote-token ("'") symbol) ; reconoce el carácter de comilla simple (') y lo trata como un símbolo
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit) "." digit (arbno digit))
   number)
  (number
   (digit (arbno digit))
   number)
  (number
   ("-" digit (arbno digit) "," digit (arbno digit))
   number)
  (number
   ("-" digit (arbno digit))
   number)))
;;*******************************************************************************************


;; 2. Especificación_Sintáctica (gramática)
;;*******************************************************************************************
(define the-grammar
  '((program (expression) a-program)

    ;; Expresiones del lenguaje
    (expression 
      ("x16" "(" expression (arbno expression) ")")
      hex-exp)
    (expression (number) lit-exp)
    (expression (string) string-exp)
    (expression (identifier) var-exp)
    (expression ("'" identifier) quoted-exp) ;; permite usar la sintaxis `'id` como una expresión citada
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     app-exp)
    (expression
     ("rec"
      (arbno identifier "(" (separated-list identifier ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)
    ; Asignación de variables
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("var" (arbno identifier "=" expression) "in" expression)
                var-assign-exp)
    (expression ("const" (arbno identifier "=" expression) "in" expression)
                const-assign-exp)
    (expression ("set" identifier "=" expression)
                set-exp)
    (expression (bool) bool-exp)
    (expression (type) type-exp)
    (expression (circuit) circuit-exp)

    ;; Primitivas aritmeticas
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("mod") mod-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)

    ;; Primitivas hexadecimales
    (primitive ("hex") dec-hex-prim)

    ;; Primitivas de cadenas
    (primitive ("s-len") string-length-prim)
    (primitive ("s-append") string-append-prim)
    ;; Primitivas de circuitos
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)

    ;; Construcción del circuito
    (circuit ("C(" "circuit" "(" "gate-list" gate-list ")" ")") a-circuit)
    (gate-list () empty-gate-list)
    (gate-list (gate gate-list) cons-gate-list)
    (gate ("(" "gate" identifier "(" "type" type ")" "(" "input-list" input-list ")" ")")
          a-gate)
    (type ("and") and-type)
    (type ("or") or-type)
    (type ("not") not-type)
    (type ("xor") xor-type)
    (input-list () empty-input-list)
    (input-list (input input-list) cons-input-list)
    (input (identifier) ref-input)
    (input (bool) bool-input)
    (bool ("True") true-lit)
    (bool ("False") false-lit)))

;Construidos automáticamente:
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

;*******************************************************************************************
;; 3. Parser_Scanner_Interfaz
;*******************************************************************************************


;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      the-lexical-spec
      the-grammar)))

;*******************************************************************************************

;; 4. Evaluación_De_Expresiones
;*******************************************************************************************
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
      (hex-exp (d1 ds)
               (let loop ((acc (eval-expression d1 env))
                          (rest ds))
                 (if (null? rest)
                     acc
                     (loop (+ (* acc 16)
                              (eval-expression (car rest) env))
                           (cdr rest)))))
      (lit-exp (datum) datum)
      (string-exp (datum) (substring datum 1 (- (string-length datum) 1))) ; elimina las comillas
      (var-exp (id) (apply-env env id))
      (quoted-exp (id) id) ; evalúa una expresión citada devolviendo directamente el símbolo sin buscarlo en el ambiente
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args env)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
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
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps)
                       acc
                       (loop (eval-expression (car exps)
                                              env)
                             (cdr exps)))))
      (var-assign-exp (ids rands body)
                      (let ((vals (eval-rands rands env)))
                        (let ((env2 (extend-env ids vals env)))
                          (eval-expression body env2))))
      (const-assign-exp (ids rands body)
                        (let ((vals (eval-rands rands env)))
                          (eval-expression
                           body
                           (extend-const-env ids vals env))))
      (set-exp (id rhs-exp)
               (begin
                 (let* ((ref   (apply-env-ref env id))
                        (muts  (cases environment env
                                 (empty-env-record ()
                                                   (eopl:error 'set-exp "No binding for ~s" id))
                                 (extended-env-record (syms _ muts parent)
                                                      (let ((i (list-index (lambda (s) (eq? s id)) syms)))
                                                        (if (number? i)
                                                            (vector-ref muts i)
                                                            (apply-env-ref parent id)))))))
                   (unless muts
                     (eopl:error 'set-exp "No se puede reasignar: ~s es const" id))
                   (setref! ref (eval-expression rhs-exp env)))
                 1))
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
                   circ))))

;*******************************************************************************************

;*******************************************************************************************
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
      ;; Primitivas aritméticas
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (if (= (cadr args) 0)
                      (eopl:error 'apply-primitive "Division by zero")
                      (/ (car args) (cadr args))))
      (mod-prim () (if (= (cadr args) 0)
                       (eopl:error 'apply-primitive "Division by zero")
                       (remainder (car args) (cadr args))))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      ;; Primitivas hexadecimales
      (dec-hex-prim ()
                     (let* ([n (car args)]
                            ;; recorre n dividiendo entre 16 hasta obtener todos los dígitos
                            [digits (let loop ([num n] [acc '()])
                                      (if (< num 16)
                                          (cons num acc)
                                          (loop (quotient num 16)
                                                (cons (remainder num 16) acc))))]
                            ;; formatea "x16 (" + "d1 d2 …" + ")"
                            [out    (string-append "x16 ("
                                                   (string-join (map number->string digits)" ")
                                                   ")")])
                       out))

      ;; Primitivas de cadenas:
      (string-length-prim ()
                          (string-length (car args)))
      (string-append-prim ()
                          (apply string-append args))
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
                               (merge-circuits circ1 circ2 gate-type new-name)))) ))) 

;Determina si es un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;; 5. Evaluación_De_Circuitos
;*******************************************************************************************

; eval-circuit: evalúa un circuito completo compuesto por una lista de compuertas.
;   circ: un circuito (a-circuit) con una lista de compuertas
;   env: ambiente inicial (valores booleanos asociados a entradas)
(define eval-circuit
  (lambda (circ env)
    (cases circuit circ
      (a-circuit (gate-list)
        (let ((env-updated (eval-gate-list gate-list env)))
          (apply-env env-updated (get-last-gate-id gate-list)))))))

; eval-gate-list: evalúa secuencialmente una lista de compuertas.
;   gl: gate-list, una lista de compuertas
;   env: ambiente de evaluación actual
(define eval-gate-list
  (lambda (gl env)
    (cases gate-list gl
      (empty-gate-list () env)
      (cons-gate-list (g r)
        (let ((env-updated (eval-gate g env)))
          (eval-gate-list r env-updated))))))

; eval-gate: evalúa una compuerta lógica usando un ambiente dado.
;   g: una compuerta (a-gate)
;   env: ambiente de evaluación (asociación de identificadores a valores booleanos)
(define eval-gate
  (lambda (g env)
    (cases gate g
      (a-gate (id typ input-list)
        (let ((val (apply-gate typ (eval-input-list input-list env))))
          (extend-env (list id) (list val) env))))))

; eval-input-list: evalúa una lista de entradas de una compuerta lógica.
;   il: input-list (lista de entradas de una compuerta)
;   env: ambiente de evaluación (valores actuales de las referencias)
(define eval-input-list
  (lambda (il env)
    (cases input-list il
      (empty-input-list () '())
      (cons-input-list (input rest)
        (cons (eval-input input env) (eval-input-list rest env))))))

; eval-input: evalúa una entrada individual (referencia o literal booleano).
;   inp: input (ref-input o bool-input)
;   env: ambiente actual
(define eval-input
  (lambda (inp env)
    (cases input inp
      (ref-input (id) (apply-env env id))
      (bool-input (b)
        (cases bool b
          (true-lit () #t)
          (false-lit () #f))))))

; apply-gate: aplica el comportamiento lógico de una compuerta a una lista de valores booleanos
;   t: tipo de compuerta (and-type, or-type, not-type, xor-type)
;   inputs: lista de valores booleanos (solo #t o #f) representando las entradas de la compuerta
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

;*******************************************************************************************
;; 6. Conexión_De_Circuitos_En_Serie: se llama a connect-circuits
;*******************************************************************************************

; connect-circuits: combina dos circuitos, reemplazando una entrada del segundo circuito
; por el id de la última compuerta del primero, y luego une ambas listas de compuertas.
;   c1: circuito base (se mantiene igual)
;   c2: circuito que se conectará al primero
;   input-to-replace: identificador de entrada en c2 que será reemplazado
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



;*******************************************************************************************
;; 7. Conexión_De_Circuitos_En_Paralelo: se llama a merge-circuits
;*******************************************************************************************

; merge-circuits: une dos circuitos agregando una nueva compuerta lógica al final  
;   c1: primer circuito   
;   c2: segundo circuito  
;   gate-type: tipo de compuerta lógica a usar (and-type, or-type, xor-type)  
;   new-name: identificador (símbolo) que tendrá la nueva compuerta creada    
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Funciones_Auxiliares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1) get-last-gate-id: obtiene el identificador (id) de la última compuerta en una lista de compuertas (gate-list)
;   gl: lista de compuertas a analizar
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

;2) replace-inputs: recorre una lista de entradas de una compuerta
; y reemplaza cualquier referencia a input-to-replace por new-id
;   ilist: lista de entradas
;   input-to-replace: identificador que se desea reemplazar
;   new-id: identificador que se usará en su lugar
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

;3) replace-gates: recorre una lista de compuertas (gate-list)
; y aplica replace-inputs a cada una para cambiar referencias en las entradas
;   gl: lista de compuertas
;   input-to-replace: identificador a reemplazar
;   new-id: identificador que lo sustituye
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

;4) combine-gates: une dos listas de compuertas en una sola
;   g1 y g2: listas de compuertas que se concatenarán
(define combine-gates
  (lambda (g1 g2)
    (cases gate-list g1
      (empty-gate-list () g2)
      (cons-gate-list (g rest)
        (cons-gate-list g (combine-gates rest g2))))))

;5) append-gate-lists: concatena dos listas de compuertas (gate-list) en una sola
; gl1: primera lista de compuertas
; gl2: segunda lista de compuertas a anexar
; Devuelve una lista que contiene las compuertas de gl1 seguidas de las de gl2.
; Si la primera lista gl1 está vacía, retorna directamente gl2.
(define append-gate-lists
  (lambda (gl1 gl2)
    (cases gate-list gl1
      (empty-gate-list () gl2)
      (cons-gate-list (g rest)
        (cons-gate-list g (append-gate-lists rest gl2))))))



;*******************************************************************************************
;; 9. Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))
;*******************************************************************************************
;;;;;;;;;;;;;;;;; procedures of plane objects interpreter ;;;;;;;;;;;;;;;;

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;; 10. Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (muts vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record
     syms
     (list->vector vals)
     ;; Vector paralelo de mut flags, todos #t
     (make-vector (length syms) #t)
     env)))

(define extend-const-env
  (lambda (syms vals env)
    (extended-env-record
     syms
     (list->vector vals)
     ;; Vector paralelo de mut flags, todos #f para const
     (make-vector (length syms) #f)
     env)))




;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((muts(make-vector len #t)))
          (let ((env (extended-env-record proc-names vec old-env)))
            (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
            env))))))


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals muts env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;****************************************************************************************
;; 11. Funciones_Para_Asignación_De_Variables
;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;; 12. Funciones_Auxiliares_Para_Encontrar_La_Posición_De_Un_Símbolo
; en la lista de símbolos de un ambiente
(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Pruebas de funciones principales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (scan&parse "(circuit(gate-list 
;   (gate G1(type or)(input-list A B))
;   (gate G2(type and)(input-list A B))
;   (gate G3(type not)(input-list G2))
;   (gate G4(type and)(input-list G1 G3))))")

; ====================
; Pruebas eval-circuit
; ====================


; Primera Prueba:
; Crea una variable A con valor True.
; Define un circuito C1 con una única compuerta G1 de tipo NOT que toma A como entrada.
; eval-circuit evalúa el circuito, aplicando NOT a True, por lo que el resultado esperado es False.
; (scan&parse "
; let  
;   A = True
;   C1 = C(circuit (gate-list 
;            (gate G1 (type not) (input-list A))))
; in 
;   eval-circuit(C1)
; ")

(interpretador)