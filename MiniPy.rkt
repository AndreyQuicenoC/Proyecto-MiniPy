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
;; - Jonathan Aristizabal Vargas   - 2322626
;; - Andrey Quiceno Cabrera        - 2326081
;; - Juan Francesco García Vargas  - 2310174

;; Fecha de entrega: 10-06-2025
;; ========================================================================================



;; ======================================================================================
;;;;;;;;;;;;;;;;; Grammar of the language ;;;;;;;;;;;;;;;;;

;;<program>         ::= <expression>

;; <expression>      ::= <number>
;;                       <lit-exp (datum)>
;;                   ::= <identifier>
;;                       <var-exp (id)>
;;                   ::= <string>
;;                       <string-exp (datum)>
;;                   ::= <bool>
;;                       <bool-exp (b)>
;;                   ::= <x16 (<number> { , <number> }*)>
;;                       <hex-exp (d1 ds)>
;;                   ::= <' identifier>
;;                       <quoted-exp (id)>
;;                   ::= <circuit>
;;                       <circuit-exp (circ)>
;;                       <circuit>         ::= <gate-list>
;;                       <gate-list>       ::= empty
;;                                             <gate> <gate-list>
;;                       <gate>            ::= <identifier> <type> <input-list>
;;                       <type>            ::= and | or | not | xor
;;                       <input-list>      ::= empty
;;                                           | <bool> <input-list>
;;                                           | <identifier> <input-list>
;;                   ::= <primitive>(<expression> { , <expression> }*)
;;                       <primapp-exp (prim rands)>
;;                   ::= if <expression> then <expression> else <expression>
;;                       <if-exp (test-exp true-exp false-exp)>
;;                   ::= var { <identifier> = <expression> }* in <expression>
;;                       <var-assign-exp (ids rands body)>
;;                   ::= const { <identifier> = <expression> }* in <expression>
;;                       <const-assign-exp (ids rands body)>
;;                   ::= rec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                       <letrec-exp (proc-names idss bodies letrec-body)>
;;                   ::= proc({<identifier>}*(,)) <expression>
;;                       <proc-exp (ids body)>
;;                   ::= (<expression> <expression>*)
;;                       <app-exp (rator rands)>
;;                   ::= begin <expression> { ; <expression> }* end
;;                       <begin-exp (exp exps)>
;;                   ::= while <expression> do <expression> done
;;                       <while-exp (test body)>
;;                   ::= for <identifier> in <expression> do <expression> done
;;                       <for-exp (iter struct body)>
;;                   ::= [ <expression> { ; <expression> }* ]
;;                       <list-exp (elements)>
;;                   ::= tuple [ <expression> {  <expression> }* ]
;;                       <tuple-exp (elements)>
;;                   ::= { <identifier> = <expression> { ; <identifier> = <expression> }* }
;;                       <record-exp (key value keys values)>
;;                   
;;                   POO inspirada en una mezcla de Python e interpretador del curso:
;;                   ::= class <identifier> extends <identifier> {field <identifier>}*{method-decl}*
;;                       <class-decl (class-name super-name field-ids m-decls)>
;;                   ::= def <identifier>(<identifier> { , <identifier> }*) <expression>
;;                       <method-decl (method-name ids body)>
;;                   ::= new <identifier>(<expression> { , <expression> }*)
;;                       <new-object-exp (class-name rands)>
;;                   ::= send <expression> <identifier>(<expression> { , <expression> }*)
;;                       <method-app-exp (obj-exp method-name rands)>
;;                   ::= super <identifier>(<expression> { , <expression> }*)
;;                       <super-call-exp (method-name rands)>

;; <primitive>       ::= + | - | * | / | mod | add1 | sub1 
;;                   ::= s-len | s-append
;;                   ::= crear-lista | append | ref-list | set-list | vacio | vacio? | lista?|
;;                       cabeza | cola
;;                   ::= crear-tupla | tupla? | ref-tuple | cabeza | cola | vacio | vacio?
;;                   ::= crear-registro | registro? | ref-registro | set-registro

;; <expr-bool>       ::= <pred-prim>(<expression>, <expression>)
;;                    | <oper-bin-bool>(<expr-bool>, <expr-bool>)
;;                    | <oper-un-bool>(<expr-bool>)

;; <pred-prim>       ::= < | > | <= | >= | == | != 

;; <oper-bin-bool>   ::= && | ||   (Operadores inspirados en Python)
;; <oper-un-bool>    ::= !

;; <list>            ::= [ <expression> { ; <expression> }* ]
;; <tuple>           ::= tuple [ <expression> { ; <expression> }* ]
;; <record>          ::= { <identifier> = <expression> { ; <identifier> = <expression> }* }

;; <bool>            ::= True | False
;; <string>          ::= "..."
;; <hex>             ::= 16x(<0-15>*)
;; <int>             ::= <0-9>+
;; <float>           ::= <0-9>+.<0-9>+
;;
;; ========================================================================================
;; INDICE:
;; 1.Especificación_Léxica
;; 2.Especificación_Sintáctica
;; 3.Parser_Scanner_Interfaz
;; 4.Evaluación_De_Expresiones
;; 5.Funciones_Evaluación
;; 6.Primitivas
;; 7.Evaluación_De_Circuitos
;; 8.Datatypes_de_List_Tuple_y_Registro
;; 9.Funciones_Ambientes
;; 10.Funciones_Auxiliares_Para_Listas_Tuplas_y_Registros
;; 11.Funciones_Auxiliares_Para_Hexadecimales
;; 12.Funciones_Auxiliares_Para_Asignación_Variables_Valor_y_Referencia
;; 13.Funciones_Auxiliares_Para_OOP
;; 14.Funciones_Auxiliares_Para_Encontrar_La_Posición_De_Un_Símbolo
;; 15.Funciones_Auxiliares_Circuitos
;; 16.Otras_Funciones_Auxiliares

;; ========================================================================================
;;*******************************************************************************************
;; 1.Especificación_Léxica
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
      (letter (arbno (or letter digit "_" "-" "?")))
        symbol)
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
        number)
    (identifier
      ("__" (arbno (or letter digit "_" "-" "?")) "__")
        symbol)))
    

 
;;*******************************************************************************************
;; 2.Especificación_Sintáctica (gramática)
;;*******************************************************************************************
(define the-grammar
  '(;;(program (expression) a-program)
    (program ((arbno class-decl) expression) a-program)

    ;; Expresiones del lenguaje
    (expression
     ("x16" "(" number (arbno number) ")")
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
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    (expression ("var" (arbno identifier "=" expression) "in" expression) var-assign-exp)
    (expression ("const" (arbno identifier "=" expression) "in" expression) const-assign-exp)
    (expression ("set" identifier "=" expression) set-exp)

    ; Expresiones de circuitos y booleanas
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

    ;; Primitivas booleanas
    (primitive ("<") less-prim)
    (primitive (">") greater-prim)
    (primitive ("<=") less-equal-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("==") equal-prim)
    (primitive ("!=") not-equal-prim)
    (primitive ("&&") and-prim)
    (primitive ("||")  or-prim)
    (primitive ("!") not-prim)

    ;; Primitivas hexadecimales
    (primitive ("hex+") add-hex-prim)
    (primitive ("hex-") sub-hex-prim)
    (primitive ("hex*") mult-hex-prim)
    (primitive ("hex/") div-hex-prim)  
    (primitive ("hexmod") mod-hex-prim)  
    (primitive ("hexadd1") incr-hex-prim)
    (primitive ("hexsub1") decr-hex-prim)

    ;; Primitivas de cadenas
    (primitive ("s-len") string-length-prim)
    (primitive ("s-append") string-append-prim)
    (primitive ("num->str")  num->str-prim)

    ;; Primitivas de circuitos
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)

    ;; Primitivas listas
    (primitive ( "vacio?" ) empty-list-prim)
    (primitive ( "vacio" ) create-empty-list-prim)
    (primitive ( "crear-lista" ) create-param-list-prim)
    (primitive ( "lista?" ) is-list-prim)
    (primitive ("cabeza") head-list-prim)
    (primitive ("cola") last-list-prim)
    (primitive ("append") append-list-prim)
    (primitive ("ref-list") index-list-prim)
    (primitive ("set-list") set-list-prim)

    ;; Primitivas tuplas
    (primitive ("crear-tupla") create-param-tuple-prim)
    (primitive ("tupla?") is-tuple-prim )
    (primitive ("ref-tuple") index-tuple-prim )

    ;; Primitivas registros
    (primitive ("crear-registro") create-param-record-prim)

    ; (primitive ("crear-registro-veloz") create-param-record-fast-prim)
    (primitive ("registro?") is-record-prim )
    (primitive ("ref-registro") index-record-prim )
    (primitive ("set-registro") set-record-prim )

    ;; Primitiva para imprimir
    (primitive ("print") print-prim)

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
    (bool ("False") false-lit)

    ;; control structs
    (expression ("for" identifier "in" expression "do" expression "done") for-exp) ;; #1 -> iterative var, #2 -> data struct #3 -> body
    (expression ("while" expression "do" expression "done") while-exp) ;; #1 -> condition, #2 -> body
    
    ;; data structs
    (expression ("[" (arbno expression) "]") list-exp)
    (expression ("tuple" "[" (arbno expression) "]") tuple-exp)
    (expression ("{" identifier "=" expression (arbno ";" identifier "=" expression) "}") record-exp)
    
    ;;*******************************************************************************************
    ;; 2.1. Nuevas producciones para OOP
    ;;*******************************************************************************************
    (class-decl                         
      ("class" identifier 
        "extends" identifier                   
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("def" identifier 
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
      super-call-exp)))

;Construidos automáticamente:
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
;;*******************************************************************************************



;; 3.Parser_Scanner_Interfaz
;;*******************************************************************************************

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
;;*******************************************************************************************


;; 4.Evaluación_De_Expresiones
;;*******************************************************************************************

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls) ;\new1
        (eval-expression exp (empty-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (empty-env)))

; Evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (hex-exp (d1 ds)
               (hex-val (cons d1 ds)))
      (lit-exp (datum) datum)
      (string-exp (datum) (substring datum 1 (- (string-length datum) 1))) ; elimina las comillas
      (var-exp (id) (apply-env env id))
      (quoted-exp (id) id) ; evalúa una expresión citada devolviendo directamente el símbolo sin buscarlo en el ambiente
      (primapp-exp (prim rands)
                   (cases primitive prim
                     ;; 1) Si es set-list, pasamos rands sin evaluar
                     (set-list-prim                ()
                                                   (apply-primitive prim rands env))
                     (append-list-prim             ()
                                                   (apply-primitive prim rands env))
                     ;; 1.b) Igual para las primitivas de registro
                     (set-record-prim              ()
                                                   (apply-primitive prim rands env))
                     (create-param-record-prim     ()
                                                   (apply-primitive prim rands env))
                    ;  (create-param-record-fast-prim()
                    ;                                (apply-primitive prim rands env))
                     ;; 2) En los demás casos, evaluamos los rands antes
                     (else
                      (let ((args (eval-primapp-exp-rands rands env)))
                        (apply-primitive prim args env)))))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (proc-exp (ids body)
              (closure ids body env))
      (app-exp (rator rands)
               (let* ((proc     (eval-expression rator env))
                      ;; args iniciales envueltos en targets
                      (raw-args (eval-rands       rands env))
                      ;; flags = #t para cada arg que sea lista o registro
                      (flags
                       (map (lambda (t)
                              (let ((v (deref-target t)))
                                (or (lista? v)
                                    (registro? v))))
                            raw-args)))
                 ;; si no hay flags (lista vacía), comportamiento original
                 (if (null? flags)
                     (if (procval? proc)
                         (apply-procval proc raw-args)
                         (eopl:error 'eval-expression
                                     "Attempt to apply non-procedure ~s"
                                     proc))
                     ;; si hay flags, iteramos y devolvemos el valor de la última llamada
                     (let loop ((fs flags))
                       (let* ((flag (car fs))
                              (args1 (if flag
                                         (eval-rands-ref rands env)
                                         raw-args))
                              (val   (if (procval? proc)
                                         (apply-procval proc args1)
                                         (eopl:error 'eval-expression
                                                     "Attempt to apply non-procedure ~s"
                                                     proc))))
                         (if (null? (cdr fs))
                             ;; último flag: devolvemos su resultado
                             val
                             ;; aún quedan flags: seguimos iterando
                             (loop (cdr fs))))))))
      (letrec-exp (proc-names idss bodies letrec-body)
              (eval-expression letrec-body
                  (extend-env-recursively proc-names idss bodies env)))
      (begin-exp (exp exps)
              (let loop 
                  ((acc (eval-expression exp env)) (exps exps))
                  (if (null? exps)
                      acc
                      (loop (eval-expression (car exps) env)
                          (cdr exps)))))
      (var-assign-exp (ids rands body)
                      (let ((vals (eval-let-exp-rands rands env)))
                        (let ((env2 (extend-env ids vals env)))
                          (eval-expression body env2))))
      ;; Constant definition, uses env extend to process
      (const-assign-exp (ids rands body)
              (let ((vals (eval-rands rands env)))
                  (eval-expression
                      body
                      (extend-const-env ids vals env))))
      ; Reasignación de variables
      (set-exp (id rhs-exp)
              (let ((ref  (apply-env-ref env id))
                  (muts 
                      (cases environment env
                          (empty-env-record ()
                              (eopl:error 'set-exp "No binding for ~s" id))
                          (extended-env-record (syms _ muts parent)
                              (let ((i (list-index (lambda (s) (eq? s id)) syms)))
                                  (if (number? i)
                                      (vector-ref muts i)
                                      (apply-env-ref parent id)))))))
                 (unless muts (eopl:error 'set-exp "No se puede reasignar: ~s es const" id))
                 (setref! ref (eval-expression rhs-exp env)) 1))
      
      ;; Expresiones para circuitos
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

      ; Expresiones de control
      (for-exp (iter struct body)
              (let ((estructura (eval-expression struct env)))
                (let ((elements (get-li estructura)))
                    ;; Caso 2: Estructura es una colección iterable (comportamiento original)               
                      (let loop ((items elements)
                                (last-val 1))
                        (if (null? items)
                            last-val
                            (let ((item (car items))
                                  (rest (cdr items)))
                              (let ((nuevo-env (extend-env
                                                (list iter)
                                                (list (if (pair? item) (cdr item) item))
                                                env)))
                                (loop rest (eval-expression body nuevo-env)))))))))
      (while-exp (test body)
                 (let loop ((last-val 1))
                   (if (true-value? (eval-expression test env))
                       (loop (eval-expression body env))
                       last-val)))

      ; Expresiones de datos estructurados
      (list-exp (elements)
                (let ((vals (eval-rands elements env)))
                  (listica vals))
                )
      (tuple-exp (elements)
                 (let ((vals (eval-rands elements env)))
                   (tuplita vals))
                 )
      (record-exp (key value keys values)
                  (let* ([ks    (cons key  keys)]          ; tus identificadores de campo
                         [targs (eval-rands (cons value values) env)]) ; ya vienen como targets
                    (registrico
                     (map (lambda (k tgt)
                            (list (symbol->string k) tgt))
                          ks targs))))

      ; Expresiones de OOP
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            '__init__ class-name obj args)
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
            method-name (apply-env env '%super) obj args))))))


;;;;;;;;;;;;; Declaraciones ;;;;;;;;;;;;;

; Elaboración de declaraciones de clase
(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

; Obtener el nombre de la superclase
(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

; Obtener los identificadores de campo de la clase
(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

; Obtener las declaraciones de método de la clase
(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

; Elaboración de declaraciones de clase
(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

; Obtener los identificadores de método
(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

; Obtener el cuerpo de la declaración del método
(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

; Obtener los nombres de los métodos de una lista de declaraciones de método
(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
;;*******************************************************************************************



;; 5.Funciones_Evaluación
;;*******************************************************************************************
; Evalúa una lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

; Evalúa un único operando (expresión) y devuelve un target
(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (direct-target (apply-env env id)))
      (else
       (direct-target (eval-expression rand env))))))

; Evalúa una lista de operandos (expresiones) y devuelve una lista de targets
(define eval-rands-ref
  (lambda (rands env)
    (map (lambda (x) (eval-rand-ref x env)) rands)))

; Evalúa un único operando (expresión) y devuelve un target indirecto
(define eval-rand-ref
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

; Evalúa los operandos de una aplicación de primitiva
(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (expr)
           (unwrap-target (eval-expression expr env)))
         rands)))

; Evalúa los operandos de una expresión let
(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

; Evalúa un único operando (expresión) en el contexto de una expresión let
(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))
;;*******************************************************************************************



;; 6. Primitivas
;;*******************************************************************************************
(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim

      ;; Primitivas aritméticas
      (add-prim ()
              (if (null? args) (eopl:error 'add-prim "No arguments provided")
                  (let loop ((acc (car args)) (rest (cdr args)))
                      (if (null? rest) acc
                          (loop (+ acc (car rest)) (cdr rest))))))
      (substract-prim ()
              (if (null? args) (eopl:error 'substract-prim "No arguments provided")
                  (let loop ((acc (car args)) (rest (cdr args)))
                      (if (null? rest) acc
                          (loop (- acc (car rest)) (cdr rest))))))
      (mult-prim ()
              (if (null? args) (eopl:error 'mult-prim "No arguments provided")
                  (let loop ((acc (car args)) (rest (cdr args)))
                      (if (null? rest) acc
                          (loop (* acc (car rest)) (cdr rest))))))
      (div-prim ()
              (if (null? args) (eopl:error 'div-prim "No arguments provided")
                  (let loop ((acc (car args)) (rest (cdr args)))    
                      (if (null? rest) acc
                          (let ((next (car rest)))
                              (if (= next 0) (eopl:error 'div-prim "Division by zero")
                                  (loop (/ acc next) (cdr rest))))))))
      (incr-prim ()
              (if (null? args) (eopl:error 'incr-prim "No argument provided")
                  (+ (car args) 1)))
      (decr-prim ()
              (if (null? args) (eopl:error 'decr-prim "No argument provided")
                  (- (car args) 1)))
      ;; Primitiva de módulo.
      (mod-prim ()
              (let* ([a (car args)]
                  [b (cadr args)])
                  (when (= b 0)
                      (eopl:error 'mod-prim "Division by zero"))
                  ;; módulo uniforme para enteros y reales:
                  (let ([q (floor (/ a b))])
                      (- a (* b q)))))

      ;; Primitivas booleanas
      (less-prim ()
              (if (null? args)
                  (eopl:error 'less-prim "No arguments provided")
                  (let loop ([prev (num-of (car args))] [rs (cdr args)])
                      (if (null? rs)
                          #t
                          (let ([cur (num-of (car rs))])
                              (if (< prev cur)
                                  (loop cur (cdr rs))
                                  #f))))))
      (greater-prim ()
              (if (null? args)
                  (eopl:error 'greater-prim "No arguments provided")
                  (let loop ([prev (num-of (car args))] [rs (cdr args)])
                      (if (null? rs)
                          #t
                          (let ([cur (num-of (car rs))])
                              (if (> prev cur)
                                  (loop cur (cdr rs))
                                  #f))))))
      (less-equal-prim ()
              (if (null? args)
                  (eopl:error 'less-equal-prim "No arguments provided")
                  (let loop ([prev (num-of (car args))] [rs (cdr args)])
                      (if (null? rs)
                          #t
                          (let ([cur (num-of (car rs))])
                              (if (<= prev cur)
                                  (loop cur (cdr rs))
                                  #f))))))
      (greater-equal-prim ()
              (if (null? args)
                  (eopl:error 'greater-equal-prim "No arguments provided")
                  (let loop ([prev (num-of (car args))] [rs (cdr args)])
                      (if (null? rs)
                          #t
                          (let ([cur (num-of (car rs))])
                              (if (>= prev cur)
                                  (loop cur (cdr rs))
                                  #f))))))
      (equal-prim ()
              (if (null? args)
                  (eopl:error 'equal-prim "No arguments provided")
                  (let ([x0 (car args)])
                      (cond
                          ;; caso string
                          [(string? x0)
                              (let loop ([rs (cdr args)])
                                  (if (null? rs)
                                      #t
                                      (let ([y (car rs)])
                                          (if (and (string? y)
                                                  (string=? x0 y))
                                              (loop (cdr rs))
                                              #f))))]
                          ;; caso numérico/boolean/hex
                          [else
                              (let ([base (num-of x0)])
                                  (let loop ([rs (cdr args)])
                                      (if (null? rs)
                                          #t
                                          (if (= base (num-of (car rs)))
                                              (loop (cdr rs))
                                              #f))))]))))
      (not-equal-prim ()
                      (if (null? args)
                          (eopl:error 'not-equal-prim "No arguments provided")
                          (let ([base (num-of (car args))])
                            (let loop ([rs (cdr args)])
                              (cond
                                [(null? rs)     #f]
                                [(not (= base (num-of (car rs)))) #t]
                                [else           (loop (cdr rs))])))))
      (and-prim ()
                (if (< (length args) 2) (eopl:error 'and-prim "Need at least two args")
                    (let loop ((v (deref-target (car args))) (rest (cdr args)))
                      (cond [(eq? v #f) #f]
                            [(null? rest)   #t]
                            [else (loop (deref-target (car rest)) (cdr rest))]))))
      (or-prim ()
               (if (< (length args) 2) (eopl:error 'or-prim "Need at least two args")
                   (let loop ((v (deref-target (car args))) (rest (cdr args)))
                     (cond [(eq? v #t) #t]
                           [(null? rest)   #f]
                           [else (loop (deref-target (car rest)) (cdr rest))]))))
      (not-prim ()
                (if (not (= (length args) 1)) (eopl:error 'not-prim "Need exactly one arg")
                    (let ((v (deref-target (car args))))
                      (if (eq? v #t) #f #t))))

      ;; Primitivas de listas
      (empty-list-prim ()                    
              (let ((val (get-li (car args))))
                  (cond
                      [(lista? (car args)) (null? val)]
                      [(tupla? (car args)) (null? val)]
                      [(eopl:error 'empty-prim "Not a list or tuple: ~s" val)])))
      (create-empty-list-prim ()
              (cond
                  [(null? args) (listica '())]  ; Sin argumentos, crea lista vacía por defecto
                  [(lista? (car args)) (listica '())]
                  [(tupla? (car args)) (tuplita '())]
                  [(eopl:error 'empty-prim "Not a list or tuple:")]))
      (create-param-list-prim ()
              (repetir (car args) (cadr args) 'lista))
      (is-list-prim ()
              (let ((val (car args))) 
                  (if (lista? val)
                      #t
                      #f)))
      (head-list-prim ()
              (car (get-li (car args))))
      (last-list-prim ()
                      (last (get-li (car args))))
      (append-list-prim ()
                        (let* ((eval-args (eval-primapp-exp-rands args env))
                               (id-eval   (obtener-id (car args)))
                               (elem-exp  (cadr args))
                               (is-mutable
                                (let find-muts ((e env))
                                  (cases environment e
                                    (empty-env-record ()
                                                      (eopl:error 'set-list-prim
                                                                  "No existe binding para ~s" id-eval))
                                    (extended-env-record (syms _ muts parent)
                                                         (let ((i (list-index (lambda (s) (eq? s id-eval)) syms)))
                                                           (if (number? i)
                                                               (vector-ref muts i)
                                                               (find-muts parent))))))))
                          ;; chequeo de mutabilidad
                          (unless is-mutable
                            (eopl:error 'append-list-prim
                                        "No se puede reasignar: ~s es const" id-eval))
                          (setref!
                           (apply-env-ref env id-eval)
                           (putf
                            (car eval-args)
                            (if (var-exp? elem-exp)
                                ;; paso por referencia (solo listas o registros)
                                (let* ((id    (obtener-id elem-exp))
                                       (ref   (apply-env-ref env id))
                                       (value (deref ref)))
                                  (if (or (lista? value) (registro? value))
                                      ;; devolvemos la referencia (indirect-target interna)
                                      ref
                                      ;; si no es lista/registro, lo pasamos como valor
                                      (direct-target value)))
                                ;; paso por valor puro
                                (let ((lit (cadr eval-args)))
                                  (if (target? lit)
                                      lit
                                      (direct-target lit))))))
                          1))
      (index-list-prim ()
                       (if (lista? (car args)) (list-pos (get-li (car args)) (cadr args))
                           (eopl:error 'empty-prim "Not a list")))
      (set-list-prim ()
                     (let* ((eval-args (eval-primapp-exp-rands args env))
                            (id-eval   (obtener-id (car args)))
                            (elem-exp  (caddr args))
                            (is-mutable
                             (let find-muts ((e env))
                               (cases environment e
                                 (empty-env-record ()
                                                   (eopl:error 'set-list-prim
                                                               "No existe binding para ~s" id-eval))
                                 (extended-env-record (syms _ muts parent)
                                                      (let ((i (list-index (lambda (s) (eq? s id-eval)) syms)))
                                                        (if (number? i)
                                                            (vector-ref muts i)
                                                            (find-muts parent))))))))
                       ;; chequeo de mutabilidad
                       (unless is-mutable
                         (eopl:error 'set-list-prim
                                     "No se puede reasignar: ~s es const" id-eval))
                       ;; elegimos referencia o valor literal según AST
                       (setref!
                        (apply-env-ref env id-eval)
                        (insertar-en-posicion
                         (car eval-args)
                         (cadr eval-args)
                         (if (var-exp? elem-exp)
                             ;; paso por referencia (solo listas o registros)
                             (let* ((id    (obtener-id elem-exp))
                                    (ref   (apply-env-ref env id))
                                    (value (deref ref)))
                               (if (or (lista? value) (registro? value))
                                   ;; devolvemos la referencia (indirect-target interna)
                                   ref
                                   ;; si no es lista/registro, lo pasamos como valor
                                   (direct-target value)))
                             ;; paso por valor puro
                             (let ((lit (caddr eval-args)))
                               (if (target? lit)
                                   lit
                                   (direct-target lit))))))
                       1))

      ;; Primitivas de Tuplas
      (create-param-tuple-prim ()
              (repetir (car args) (cadr args) 'tupla))
      (is-tuple-prim ()
              (let ((val (car args))) 
                  (if (tupla? val)
                      #t
                      #f)))   
      (index-tuple-prim ()
              (if (tupla? (car args)) (list-pos (get-li (car args)) (cadr args))
                  (eopl:error 'empty-prim "Not a tuple")))

      ;; Primivitas de Registros
      (is-record-prim ()
              (let ((val (car args))) 
                  (if (registro? val)
                      #t
                      #f)))
      (create-param-record-prim ()
                                (let* ([keys-val (eval-expression (car args) env)]
                                       [vals-val (eval-expression (cadr args) env)]
                                       [keys     (get-li keys-val)]
                                       [vals     (get-li vals-val)])
                                  (unless (= (length keys) (length vals))
                                    (eopl:error 'create-param-record-prim
                                                "Number of keys and values must match"))
                                  (registrico
                                   (map (lambda (k-target v-target)
                                          (let ([rk (deref-target k-target)])
                                            (cond
                                              [(symbol? rk)
                                               (list (symbol->string rk) v-target)]
                                              [(string? rk)
                                               (list rk                   v-target)]
                                              [(number? rk)
                                               (list (number->string rk)  v-target)]
                                              [else
                                               (eopl:error 'create-param-record-prim
                                                           "Record key must be symbol, string or number, got: ~s"
                                                           rk)])))
                                        keys vals))))
      (index-record-prim ()
              (if (and (registro? (car args)) (symbol? (cadr args))) 
                  (record-pos (get-li (car args)) (cadr args))
                  (eopl:error 'empty-or-wrong-prim "Not a record or not a key")))
      (set-record-prim ()
                       (let* ((raw-rec (car args))
                              (raw-key (cadr args))
                              (raw-val (caddr args))
                              (rec-ref (apply-env-ref env (obtener-id raw-rec)))
                              (is-mutable
                               (let find-muts ([e env])
                                 (cases environment e
                                   (empty-env-record ()
                                                     (eopl:error 'set-record-prim
                                                                 "No binding for ~s"
                                                                 raw-rec))
                                   (extended-env-record (syms _ muts parent)
                                                        (let ((i (list-index
                                                                  (lambda (s) (eq? s (obtener-id raw-rec)))
                                                                  syms)))
                                                          (if (number? i)
                                                              (vector-ref muts i)
                                                              (find-muts parent))))))))
                         (unless is-mutable
                           (eopl:error 'set-record-prim
                                       "Cannot reassign: ~s is const"
                                       raw-rec))
                         (let* ((rec       (deref-target rec-ref))
                                (pairs     (get-li rec))
                                (key-lit   (eval-expression raw-key env))
                                (key-sym   (cond
                                             [(symbol? key-lit) key-lit]
                                             [(string? key-lit) (string->symbol key-lit)]
                                             [else
                                              (eopl:error 'set-record-prim
                                                          "Record key must be symbol or string, got: ~s"
                                                          key-lit)]))
                                (value-target
                                 (if (var-exp? raw-val)
                                     (let* ((ref (apply-env-ref env (obtener-id raw-val)))
                                            (v   (deref-target (primitive-deref ref))))
                                       (if (or (lista? v) (registro? v))
                                           ref
                                           (direct-target v)))
                                     (direct-target (eval-expression raw-val env))))
                                (new-pairs (insertar-en-clave-aux pairs key-sym value-target))
                                (new-rec   (registrico new-pairs)))
                           (setref! rec-ref new-rec)
                           1)))
      
      ;; Primitivas hexadecimales
      (add-hex-prim ()
                    (let* ([ds1 (get-hex-digits (car  args))]
                           [ds2 (get-hex-digits (cadr args))]
                           [sum ( + (hex-list->decimal ds1)
                                    (hex-list->decimal ds2)) ])
                      (hex-val (decimal->hex-list sum))))

      (sub-hex-prim ()
                    (let* ([ds1 (get-hex-digits (car  args))]
                           [ds2 (get-hex-digits (cadr args))]
                           [diff (- (hex-list->decimal ds1)
                                    (hex-list->decimal ds2))])
                      (if (< diff 0)
                          (eopl:error 'sub-hex-prim "Resultado negativo")
                          (hex-val (decimal->hex-list diff)))))
      (mult-hex-prim ()
                     (let* ([ds1  (get-hex-digits (car  args))]
                            [ds2  (get-hex-digits (cadr args))]
                            [prod (* (hex-list->decimal ds1)
                                     (hex-list->decimal ds2))])
                       (hex-val (decimal->hex-list prod))))
      (incr-hex-prim ()
                     (let* ([ds  (get-hex-digits (car args))]
                            [inc (+ 1 (hex-list->decimal ds))])
                       (hex-val (decimal->hex-list inc))))
      (decr-hex-prim ()
                     (let* ([ds  (get-hex-digits (car args))]
                            [dec (- (hex-list->decimal ds) 1)])
                       (if (< dec 0)
                           (eopl:error 'decr-hex-prim "Resultado negativo")
                           (hex-val (decimal->hex-list dec)))))
      (div-hex-prim ()
        (let* ([ds1 (get-hex-digits (car args))]
               [ds2 (get-hex-digits (cadr args))]
               [n1  (hex-list->decimal ds1)]
               [n2  (hex-list->decimal ds2)])
          (if (= n2 0)
              (eopl:error 'div-hex-prim "Division by zero")
              (hex-val (decimal->hex-list (quotient n1 n2))))))
      (mod-hex-prim ()
        (let* ([ds1 (get-hex-digits (car args))]
               [ds2 (get-hex-digits (cadr args))]
               [n1  (hex-list->decimal ds1)]
               [n2  (hex-list->decimal ds2)])
          (if (= n2 0)
              (eopl:error 'mod-hex-prim "Division by zero")
              (hex-val (decimal->hex-list (remainder n1 n2))))))
      
      ;; Primitivas de cadenas
      (string-length-prim ()
                          (if (null? args)
                              (eopl:error 'string-length-prim "No argument provided")
                              (string-length (car args))))
      (string-append-prim ()
                          (apply string-append args))
      (num->str-prim ()
                     (cond
                       [(null? args)
                        (eopl:error 'num->str-prim "Se necesita al menos un argumento")]
                       [(= (length args) 1)
                        (number->string (car args))]              
                       [else
                        ;; varios números, concatenamos sus strings
                        (apply string-append (map number->string args))]))


      ;; Primitiva de impresión
      (print-prim ()
                  (let* ((raw-tgt (car args))
                         (v       (deref-target raw-tgt)))
                    (print-value v)
                    (newline)
                    1))

      ;; Primitivas de circuitos
      (eval-circuit-prim () ;; Primitiva: eval-circuit(circuito, entrada)
                         (let ((circ (car args)))
                           (eval-circuit circ env)))
      (connect-circuits-prim () ;; Primitiva: connect-circuits(c1, c2, input)
              (connect-circuits (car args) (cadr args) (caddr args)))
      (merge-circuits-prim () ;; Primitiva: merge-circuits(c1, c2, tipo, nombre)
              (let ((circ1 (car args))
                  (circ2 (cadr args))
                  (type-symbol (caddr args)) ; Símbolo como 'and
                  (new-name (cadddr args)))  ; Nombre de la nueva compuerta (debe ser un símbolo)
                  (let ((gate-type 
                      (case type-symbol
                          ('and (and-type))
                          ('or (or-type))
                          ('xor (xor-type)))))
                      (merge-circuits circ1 circ2 gate-type new-name))))))) 
;;*******************************************************************************************



;; 7.Evaluación_De_Circuitos
;;*******************************************************************************************
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
              (let* ((targs (eval-input-list input-list env))
                     (vals  (map deref-target targs))         ; puros #t/#f
                     (out   (apply-gate typ vals)))           ; lógica booleana
                ;; guardamos de nuevo como target
                (extend-env (list id)
                            (list (direct-target out))
                            env))))))

; eval-input-list: evalúa una lista de entradas de una compuerta lógica.
;   il: input-list (lista de entradas de una compuerta)
;   env: ambiente de evaluación (valores actuales de las referencias)
(define eval-input-list
  (lambda (il env)
    (cases input-list il
      (empty-input-list () '())
      (cons-input-list (inp rest)
                       (cons (eval-input inp env)
                             (eval-input-list rest env))))))

; eval-input: evalúa una entrada individual (referencia o literal booleano).
;   inp: input (ref-input o bool-input)
;   env: ambiente actual
(define eval-input
  (lambda (inp env)
    (cases input inp
      (ref-input (id)
                 ;; una referencia viva al binding
                 (indirect-target (apply-env-ref env id)))
      (bool-input (b)
                  (cases bool b
                    (true-lit  () (direct-target #t))
                    (false-lit () (direct-target #f)))))))

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

;; Conexión_De_Circuitos_En_Serie: se llama a connect-circuits
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

;; Conexión_De_Circuitos_En_Paralelo: se llama a merge-circuits
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
;;*******************************************************************************************



;; 8.Datatypes_de_List_Tuple_y_Registro, además de objetos, clases, métodos y procedimientos
;;*******************************************************************************************
(define-datatype lista lista?
  (listica (l (list-of scheme-value?))))

(define-datatype tupla tupla?
  (tuplita (l (list-of scheme-value?))))

(define-datatype registro registro?
  (registrico (pairs (list-of pair?))))

;;;;;;;;;;;;;;;;;;; Para OOP ;;;;;;;;;;;;;;;;;;;

; Definición de tipos de datos para clases, objetos y métodos
(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

; Definición de tipos de datos para objetos y métodos
(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

; Definición de tipos de datos para métodos
(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (super-name symbol?)
    (field-ids (list-of symbol?))))

;; Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))


;;;;;;;;;;;;;;;;; procedures of plane objects interpreter ;;;;;;;;;;;;;;;;

; apply-procval: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))
;;*******************************************************************************************



;; 9.Funciones_Ambientes
;;*******************************************************************************************
; enviroment: definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (muts vector?)
   (env environment?)))

(define scheme-value? 
  (lambda (v) 
    #t))

; empty-env: -> enviroment
; función que crea un ambiente vacío
(define empty-env
  (lambda ()
    ; llamado al constructor de ambiente vacío
    (empty-env-record)))       

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

; extend-const-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
; - correspondiente a extend-env pero para variables constantes
(define extend-const-env
  (lambda (syms vals env)
    (extended-env-record
     syms
     (list->vector vals)
     ;; Vector paralelo de mut flags, todos #f para const
     (make-vector (length syms) #f)
     env)))

; extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
; - función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names))
          (idxs (iota (length proc-names))))
      (let ((vec  (make-vector len))
            (muts (make-vector len #t)))
        (let ((env (extended-env-record proc-names vec muts old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec
                          pos
                          (direct-target (closure ids body env))))
           idxs idss bodies)
          env)))))

; aply-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
; - función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

; apply-env-ref: <list-of symbols> <list-of numbers> enviroment -> enviroment
; - función que busca un símbolo en un ambiente
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

; Crea un ambiente extendido con referencias a símbolos
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec (make-vector (length syms) #t) env)))
;;*******************************************************************************************



;; 10.Funciones_Auxiliares_Para_Listas_Tuplas_y_Registros
;;*******************************************************************************************

; Repetir: función auxiliar para crear listas, tuplas o registros con valores repetidos
(define repetir
  (lambda (b a type)
    (let
      ((li 
        (cond
          [(eq? type 'registro) (aux-repetir-registro b 0 a)]
          [else (aux-repetir b a)] )))
      (cond
        [(eq? type 'lista) (listica li)]
        [(eq? type 'tupla) (tuplita li)]
        [(eq? type 'registro) (registrico li)]
        [else (eopl:error 'append "not data struct type provided")])
      )
    )
  )

; Auxiliar de repetir: crea una lista o tupla con el valor b repetido a veces
(define aux-repetir
  (lambda (b a)
    (if (zero? a)
        '()
        (cons b (aux-repetir b (- a 1))))))

; Auxiliar de repetir: crea un registro con la clave-valor b repetido a veces 
;    - diferenciando claves con indices
(define aux-repetir-registro
  (lambda (b i a)
    (if (equal? i a)
        '()
        (cons (cons (string-append (car b) (number->string i))
                    (cdr b))
              (aux-repetir-registro b (+ i 1) a))
        )))

; Función auxiliar para obtener el valor de una lista, tupla o registro
(define get-li
  (lambda (li)
    (let ((v (cond
               ((target? li)
                (deref-target li))
               ((reference? li)
                (deref-target (primitive-deref li)))
               (else
                li))))
      (cond
        ((lista? v)
         (cases lista v (listica (l) l)))
        ((tupla? v)
         (cases tupla v (tuplita (l) l)))
        ((registro? v)
         (cases registro v (registrico (pairs) pairs)))
        (else
         (eopl:error 'get-li
                     "Not a data struct, got: ~s"
                     v))))))

; Función auxiliar para obtener el último elemento de una lista, tupla o registro
(define last
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (listica (cdr lst)))))

; Usa putf-aux para agregar un elemento a una lista, tupla o registro
(define putf 
  (lambda (lst elem)
    (cond
      [(lista? lst)
       (let
           ((val (putf-aux (get-li lst) elem)))
         (listica val)
         )]
      [(registro? lst)
       (let
           ((val (putf-aux (get-li lst) elem)))
         (registrico val)
         )]
      [(tupla? lst) (eopl:error 'append "Inmutable data struct, tuple")]
      [else (eopl:error 'append "Not a list")]
      )))

; Consiste en agregar un elemento a una lista, tupla o registro
(define putf-aux
  (lambda (lst val)
    (let ((new-cell
           (cond
             [(target? val)
              ;; ya es direct-target o indirect-target
              val]
             [(reference? val)
              ;; lo convertimos en indirect-target
              (indirect-target val)]
             [else
              ;; valor normal → direct-target
              (direct-target val)])))     ; si no, lo envolvemos
      (if (null? lst)
          ;; caso base: lista vacía → devolvemos solo el new-cell
          (list new-cell)
          ;; caso recursivo: conservamos la cabeza y seguimos bajando
          (cons (car lst)
                (putf-aux (cdr lst) val))))))

; Función auxiliar para obtener el elemento en la posición x de una lista
(define list-pos
  (lambda (lst x)
    (cond
      [(registro? lst)
       (list-pos (get-li lst) x)]
      [else
       (if (zero? x)
           (car lst)
           (list-pos (cdr lst) (- x 1)))])))

; Función auxiliar para obtener el elemento en la clave x de un registro
(define record-pos
  (lambda (lst x)
    (if (null? lst)
      (eopl:error 'regis-key "Not a key on the record")
      (let ((pair (car lst)))
        (if (eq? x (string->symbol (car pair)))
          (cadr pair)
          (record-pos (cdr lst) x))))))

; Función auxiliar para insertar un elemento en una lista, tupla o registro
(define insertar-en-posicion
  (lambda (lst pos val)
    (cond
      [(lista? lst)
       (let
           ((val (insertar-en-posicion-aux (get-li lst) pos val)))
         (listica val)
         )]
      [(registro? lst)
       (let
           ((val (insertar-en-posicion-aux (get-li lst) pos val)))
         (registrico val)
         )]
      ;; [(tupla? lst) (eopl:error 'append "Inmutable data struct, tuple")]
      [else (eopl:error 'append "Not a list")]
      )))

; Función auxiliar para insertar un elemento en una lista
(define insertar-en-posicion-aux
  (lambda (lst pos val)
    (let ((new-cell
           (cond
             [(target? val)
              ;; ya es direct-target o indirect-target
              val]
             [(reference? val)
              ;; lo convertimos en indirect-target
              (indirect-target val)]
             [else
              ;; valor normal → direct-target
              (indirect-target val)])))
      (cond
        [(not (list? lst))
         (eopl:error 'insertar-en-posicion-aux "Not a list")]
        [(negative? pos)
         (eopl:error 'insertar-en-posicion-aux "Negative index")]
        [(null? lst)
         (eopl:error 'insertar-en-posicion-aux "Index out of range")]
        [(zero? pos)
         (cons new-cell (cdr lst))]
        [else
         (cons (car lst)
               (insertar-en-posicion-aux
                (cdr lst)
                (- pos 1)
                val))]))))



; Función auxiliar para insertar un elemento en un registro
(define insertar-en-clave-aux
  (lambda (lst key val)
    (cond
      [(null? lst)
       (eopl:error 'insertar-en-clave-aux "empty-record")]
      [(eq? key (string->symbol (car (car lst))))
       (cons (list (car (car lst)) val)
             (cdr lst))]
      [else
       (cons (car lst)
             (insertar-en-clave-aux (cdr lst) key val))])))

; Busca un símbolo en una lista y devuelve su posición
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))
;;****************************************************************************************



;; 11.Funciones_Auxiliares_Para_Hexadecimales
;;****************************************************************************************

; Convierte un número hexadecimal (hex-val) a una lista de dígitos hexadecimales
(define get-hex-digits
  (lambda (hex)
    (cases hexadecimal hex
      (hex-val (ds) ds))))

;; Convierte una lista de dígitos hexadecimales (enteros 0–15) a un número decimal
(define (hex-list->decimal digits)
  (let loop ([acc 0] [lst digits])
    (if (null? lst)
        acc
        (loop (+ (* acc 16) (car lst))
              (cdr lst)))))

;; Convierte un número decimal ≥0 en la lista de sus dígitos hexadecimales
(define (decimal->hex-list n)
  (let loop ([num n] [acc '()])
    (if (< num 16)
        (cons num acc)
        (loop (quotient num 16)
              (cons (remainder num 16) acc)))))
;;****************************************************************************************



;; 12.Funciones_Auxiliares_Para_Asignación_Variables_Valor_y_Referencia
;;****************************************************************************************

; Función auxiliar que obtiene el id de una var-exp
(define (obtener-id exp)
  (cases expression exp
    (var-exp (id) id)
    (else (eopl:error "No es una var-exp"))))

; iota: number -> list
; Función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))

; reference: <list-of symbols> <list-of numbers> enviroment -> enviroment
; - función que busca un símbolo en un ambiente y devuelve una referencia
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

; deref: reference -> value
; - desreferencia una referencia y obtiene su valor
(define deref
  (lambda (ref)
    (deref-target (primitive-deref ref))))

; primitive-deref: reference -> value
; - primitiva que obtiene el valor de una referencia
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

; setref!: reference value -> void
; - asigna un nuevo valor a una referencia
(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

; primitive-setref!: reference value -> void
; - primitiva que asigna un nuevo valor a una referencia
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

; Definición tipos de datos referencia y blanco
(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

; Definición de tipos de datos para expresiones
(define expval?
  (lambda (x)
    (or (number? x)
        (boolean? x)
        (symbol? x)
        (circuit? x)
        (tupla? x)
        (object? x)
        (hexadecimal? x)
        (string? x)
        (registro? x)
        (lista? x)
        (procval? x))))

(define var-exp?
  (lambda (exp)
    (and (expression? exp)
         (cases expression exp
           (var-exp (id)          #t)
           (_                    #f)))))

(define-datatype hexadecimal hexadecimal?
  (hex-val (digits (list-of integer?))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

;; Predicado para un único target
(define es-lista-element?
  (lambda (t)
    (cases target t
      (direct-target (val)
                     (lista? val))
      (indirect-target (ref)
                       (es-lista-element? (primitive-deref ref))))))

;; Función principal: lista de targets -> lista de booleans
(define es-lista?
  (lambda (lts)
    (unless (list? lts)
      (eopl:error 'es-lista? "Se esperaba una lista, pero recibió: ~s" lts))
    (map es-lista-element? lts)))

;; Helper para deshacer cualquier cosa a su “valor crudo”
(define deref-target
  (lambda (x)
    (if (target? x)
        (cases target x
          (direct-target (v) v)
          (indirect-target (ref)
                           (deref-target (primitive-deref ref))))
        ;; si no es target, lo devuelvo tal cual:
        x)))
;;****************************************************************************************



;; 13.Funciones_Auxiliares_Para_OOP
;;****************************************************************************************

;;;;;;;;;;;;;;;; Construcción de clases ;;;;;;;;;;;;;;;;

; Elabora una lista de declaraciones de clasesccreando las clases correspondientes en el entorno de clases.
(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

; Elabora una declaración de clase, añadiéndola al entorno de clases.
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

; Convierte una declaración de clase en una lista de métodos, incluyendo los heredados.
(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))

;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;
(define new-object
  (lambda (class-name)
    (let* ([len    (class-name->field-length class-name)]
           [fields (make-vector len (direct-target 0))])
      (an-object class-name fields))))

;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

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
  (lambda (method host-name self args)                
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
;;****************************************************************************************



;; 14.Funciones_Auxiliares_Para_Encontrar_La_Posición_De_Un_Símbolo
;;****************************************************************************************
(define rib-find-position
  (lambda (name symbols)
    (list-find-last-position name symbols)))

(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))
;;*******************************************************************************************



;; 15.Funciones_Auxiliares_Circuitos
;;*******************************************************************************************

;a) get-last-gate-id: obtiene el identificador (id) de la última compuerta en una lista de compuertas (gate-list)
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

;b) replace-inputs: recorre una lista de entradas de una compuerta
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

;c) replace-gates: recorre una lista de compuertas (gate-list)
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

;d) combine-gates: une dos listas de compuertas en una sola
;   g1 y g2: listas de compuertas que se concatenarán
(define combine-gates
  (lambda (g1 g2)
    (cases gate-list g1
      (empty-gate-list () g2)
      (cons-gate-list (g rest)
                      (cons-gate-list g (combine-gates rest g2))))))

;e) append-gate-lists: concatena dos listas de compuertas (gate-list) en una sola
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
;;****************************************************************************************



;; 16.Otras_Funciones_Auxiliares
;;****************************************************************************************
;Determina si es un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (cond
      ;; Booleanos de MiniPy (pueden venir como símbolos o strings)
      [(eq? x #t) #t]
      [(eq? x #f) #f]
      ;; Booleanos de MiniPy (pueden venir como símbolos o strings)
      [(eq? x 'True) #t]
      [(eq? x "True") #t]
      [(eq? x 'False) #f]
      [(eq? x "False") #f]
      ;; Valores numéricos (0 es falso, el resto verdadero)
      [(number? x) (not (zero? x))]
      ;; Por defecto, cualquier otro valor se considera verdadero
      [else #t])))

;; Función que si recibe un target, lo dereferencia; si no, lo deja igual, útil para función "cabeza"
(define unwrap-target
  (lambda (x)
    (if (target? x) (deref-target x) x)))

;; Función que convierte un valor a su representación numérica, útil para las exp booleanas
(define (num-of v)
  (cond
    [(boolean? v)        (if v 1 0)]
    [(hexadecimal? v)    (hex-list->decimal (get-hex-digits v))]
    [(number? v)         v]
    [else                (eopl:error 'num-of
                                     "Expected boolean, number or hex, got: ~s" v)]))

;; Función recursiva que imprime un valor “v”:
(define print-value
  (lambda (v)
    (cond
      ;; Caso para targets normales
      [(target? v)
       (print-value (deref-target v))]
      
      [(lista? v)
       (let ((elems (get-li v)))
         (display "[")
         (let loop ((cells elems))
           (when (pair? cells)
             (print-value (deref-target (car cells)))
             (when (cdr cells)
               (display " ")
               (loop (cdr cells)))))
         (display "]"))]

      [(tupla? v)
       (let ((elems (get-li v)))
         (display "(")
         (let loop ((cells elems))
           (when (pair? cells)
             (print-value (deref-target (car cells)))
             (when (cdr cells)
               (display " ")
               (loop (cdr cells)))))
         (display ")"))]

      [(hexadecimal? v)
       (display "x16(")
       (let loop ((ds (get-hex-digits v)))
         (when (pair? ds)
           (display (car ds))
           (when (cdr ds) (display " "))
           (loop (cdr ds))))
       (display ")")]

      [(registro? v)
       (let ((pairs (get-li v)))
         (display "{")
         (let loop ((ps pairs) (first? #t))
           (when (pair? ps)
             (unless first?
               (display "; "))
             (let* ((pair (car ps))
                    (key  (car pair))
                    (tgt  (cadr pair))
                    (val  (deref-target tgt)))
               (display key)
               (display " = ")
               (cond
                 [(string? val)
                  (display "\"")
                  (display val)
                  (display "\"")]
                 [else
                  (print-value val)]))
             (loop (cdr ps) #f)))
         (display "}"))]
      [(boolean? v)
       (display (if v "True" "False"))]
      [else
       (display v)])))
;;****************************************************************************************



;; 17. Pruebas
;;****************************************************************************************
; Pruebas de asignación de variables:
(scan&parse "
var x = 10 in 
print(x)
")
(scan&parse "
const y = 3.14 in 
print(y)
")
(scan&parse "
rec Fact (a)= if ||(==(a,0),==(a,1)) then 1 else *(a,(Fact -(a,1))) in 
(Fact 5)
")

; Pruebas de expresiones:
(scan&parse "
var x=[4 3.141519 C(circuit(gate-list 
   (gate G1(type or)(input-list True False)) 
   (gate G2(type and)(input-list True False)) 
   (gate G3(type not)(input-list G2)) 
   (gate G4(type and)(input-list G1 G3))))  
   x16(1 10 4) \"Proyecto\" 
   True 
   proc(a) set a=1 
   [1 2] 
   {nombre=\"Jonathan\";edad=25} 
   tuple[10 20]] 
   in x 
")
(scan&parse "
var y ='x  in y 
")

; Pruebas de estructuras de control:
; if-then-else:
(scan&parse "
  var x = 10 in 
  begin 
    if ==(x, 10) then 
      print(\"x es igual a 10\") 
    else 
      print(\"x no es igual a 10\") 
  end 
")
(scan&parse "
  var y = 5 in 
  begin 
    if <(y, 10) then 
      print(\"y es menor que 10\") 
    else if >(y, 10) then 
      print(\"y es mayor que 10\") 
    else 
      print(\"y es igual a 10\") 
  end 
")

; Pruebas ciclo for
(scan&parse "
  var suma = 0 in 
  begin 
    for i in [1 5 1] do 
      begin 
        set suma = +(suma, i) 
      end 
    done; 
    print(suma) 
  end 
")
(scan&parse "
  var lista = [1 2 3 4 5]
  resultado = [] in 
  begin 
    for i in lista do 
      begin 
        append(resultado, *(i, 2)) 
      end 
    done;  
    print(resultado) 
  end 
")
(scan&parse "
  var lista = [1 2 3 4 5]  
  resultado = [] in 
  begin 
    for i in lista do 
      begin 
        if ==(mod(i, 2), 0) then 
          append(resultado, *(i, 10))  
        else 
          append(resultado, *(i, 5)) 
      end 
    done; 
    print(resultado) 
  end 
")

; Pruebas ciclo while
(scan&parse "
  var contador = 0 
  suma = 0 in 
  begin 
    while <(contador, 5) do 
      begin 
        set suma = +(suma, contador); 
        set contador = +(contador, 1) 
      end 
    done;  
    print(suma) 
  end 
")
(scan&parse "
  var lista = [1 2 3 4 5] 
  resultado = []  
  i = 0 in 
  begin 
    while <(i, 5) do  
      begin 
        append(resultado, *(ref-list(lista, i), 2)); 
        set i = +(i, 1) 
      end 
    done;   
    print(resultado) 
  end 
")
(scan&parse "
  var n = 5  
  factorial = 1 in 
  begin 
    while >(n, 1) do 
      begin 
        set factorial = *(factorial, n); 
        set n = -(n, 1) 
      end 
    done; 
    print(factorial) 
  end 
")

; Pruebas de listas
(scan&parse "
  var lista = [1 2 3 4 5] in 
  begin 
    print(lista); 
    print(ref-list(lista, 0)); 
    print(ref-list(lista, 2)) 
  end 
")
(scan&parse "
  var lista = [10 20 30 40 50] in 
  begin 
    print(lista); 
    set-list(lista, 1, 99); 
    print(lista); 
    set-list(lista, 3, 77); 
    print(lista) 
  end 
")
(scan&parse "
  var lista1 = crear-lista(0, 5)  
  lista2 = vacio() in 
  begin 
    print(lista1);  
    append(lista1, 100); 
    print(lista1); 
    print(vacio?(lista2)); 
    append(lista2, 42); 
    print(vacio?(lista2)); 
    print(lista2)  
  end 
") 

; Pruebas de tuplas
(scan&parse "
  var tupla = tuple[1 2 3 4 5] in 
  begin 
    print(tupla); 
    print(ref-tuple(tupla, 0)); 
    print(ref-tuple(tupla, 2)) 
  end  
")
(scan&parse "
  var t = tuple[10 20 30]  
  l = [10 20 30] in 
  begin
    print(t);
    print(tupla?(t));
    print(tupla?(l));
    print(lista?(t));
    print(lista?(l))
  end
")
(scan&parse "
  var calcular = proc(x, y) tuple[+(x,y) -(x,y) *(x,y)] in 
  var resultado = (calcular 10 5) in 
  begin  
    print(resultado); 
    print(ref-tuple(resultado, 0)); %suma  
    print(ref-tuple(resultado, 1)); %resta  
    print(ref-tuple(resultado, 2))  %multiplicación  
  end 
") 

; Pruebas de registros
;; Quitar los símbolos \ a la hora de correr el código en el interpretador
(scan&parse "
  var persona = {nombre = \"Juan\"; edad = 30; activo = True} in 
  begin 
    print(persona); 
    print(ref-registro(persona, 'nombre)); 
    print(ref-registro(persona, 'edad)); 
    print(ref-registro(persona, 'activo)) 
  end
")
(scan&parse "
  var punto = {x = 10; y = 20; z = 30} in 
  begin 
    print(punto); 
    set-registro(punto, 'x, 100); 
    print(punto); 
    set-registro(punto, 'z, 300); 
    print(punto) 
  end
")
;; Quitar los símbolos \ a la hora de correr el código en el interpretador
(scan&parse "
  var claves = [\"nombre\" \"edad\" \"ciudad\"]  
  valores = [\"María\" 25 \"Bogotá\"] in 
  var persona = crear-registro(claves, valores) in 
  begin 
    print(persona); 
    print(ref-registro(persona, 'nombre)); 
    set-registro(persona, 'ciudad, \"Medellín\"); 
    print(persona)  
  end 
")

; Pruebas operadores lógicos y expresiones booleanas
(scan&parse "
  var a = True  
  b = False in 
  begin 
    print(&&(a, b)); 
    print(||(a, b)); 
    print(!(a)); 
    print(!(b)) 
  end 
")
(scan&parse "
  var x = 5 
  y = 10 in 
  begin 
    print(<(x, y)); 
    print(>(x, y)); 
    print(<=(x, y)); 
    print(>=(x, y)); 
    print(==(x, y)); 
    print(!=(x, y)) 
  end
")
(scan&parse "
  var a = 10  
  b = 20 in 
  begin 
    print(&&(<(a, b),  >(b,15))); 
    print(||(>(a, b),  <(b,25))); 
    print(!(==(a, b))); 
    print(==(+(a,b), 30)) 
  end 
")

; Pruebas de POO
(scan&parse "
class Animal extends object 
  field tipo 
  field edad 

  def __init__(t, e) 
    begin 
      set tipo = t; 
      set edad = e 
    end 
 
  def getTipo() 
    tipo 

  def setTipo(nuevoTipo) 
    set tipo = nuevoTipo 

  def getEdad()  
    edad 

  def setEdad(nuevaEdad) 
    set edad = nuevaEdad 

class Perro extends Animal 
  field raza 

  def __init__(t, e, r) 
    begin 
      super __init__(t, e); 
      set raza = r 
    end 
 
  def getRaza() 
    raza 

  def setRaza(nuevaRaza) 
    set raza = nuevaRaza 

  def ladrar() 
    print(s-append(\"Guau! Soy un \" , raza , \" y tengo \" , num->str(edad) , \" años.\")) 

var miPerro = new Perro(\"Mamífero\", 5, \"Labrador\") in 
begin 
  print(\"Perro 1\"); 
  print(send miPerro ladrar()) 
end
")

(scan&parse "
class Vehiculo extends object 
  field marca 
  field modelo 
 
  def __init__(m, d) 
    begin 
      set marca = m; 
      set modelo = d 
    end 

  def getMarca()  
    marca 
 
  def setMarca(nuevaMarca) 
    set marca = nuevaMarca 

  def getModelo() 
    modelo 

  def setModelo(nuevoModelo) 
    set modelo = nuevoModelo 


class Moto extends Vehiculo  
  field cilindrada 

  def __init__(m, d, c) 
    begin 
      super __init__(m, d); 
      set cilindrada = c 
    end 

  def getCilindrada() 
    cilindrada 

  def setCilindrada(nuevaCilindrada) 
    set cilindrada = nuevaCilindrada 


var moto1 = new Moto(\"Honda\", \"CBR\", 600) 
moto2 = new Moto(\"Yamaha\", \"R1\", 1000) 
moto3 = new Moto(\"Ducati\", \"Monster\", 821) in 


begin 
  print(\"Moto 1\"); 
  print(send moto1 getMarca()); 
  print(send moto1 getModelo()); 
  print(send moto1 getCilindrada()); 
  print(\"-------\"); 


  send moto2 setModelo(\"MT-09\"); 
  send moto3 setCilindrada(900); 


  print(\"Moto 2\"); 
  print(send moto2 getMarca()); 
  print(send moto2 getModelo()); 
  print(send moto2 getCilindrada()); 
  print(\"-------\"); 


  print(\"Moto 3\"); 
  print(send moto3 getMarca()); 
  print(send moto3 getModelo()); 
  print(send moto3 getCilindrada()) 
end 
")

(interpretador)
