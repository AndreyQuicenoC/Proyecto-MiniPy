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



;; ========================================================================================
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
    (number (digit (arbno digit)) number)))

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
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    ;; Primitivas de circuitos
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)

    ;; Construcción del circuito
    (circuit ("(" "circuit" "(" "gate-list" gate-list ")" ")") a-circuit)
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
    (input (expression) bool-input)

    ;; Implementaciones del proyecto
    (expression (string) string-exp)
    (expression ("True") true-lit)
    (expression ("False") false-lit)
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
    (expression ("[" expression (arbno (";" expression)) "]") list-exp)
    (expression ("tupla" "[" expression (arbno (";" expression)) "]") tuple-exp)
    (expression ("{" identifier "=" expression (arbno (";" identifier "=" expression)) "}") record-exp)
    (expression
      ("if" bool-expression "then" expression "else" expression "end")
      if-exp)
    (expression
      ("while" expression "do" expression "done")
      while-exp)
    (expression
      ("for" identifier "in" expression "do" expression "done")
       for-exp)
    (expression
      (primitive "(" expression "," expression ")")
       pred-bool-exp)
    (expression
      (primitive "(" expression "," expression ")")
       bin-bool-exp)
    (primitive ("<") less-than-prim)
    (primitive (">") greater-than-prim)
    (primitive ("<=") less-equal-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("==") equal-prim)
    (primitive ("<>") not-equal-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
              
    ;; Primitivas básicas
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
    
    ;;; oop grammar
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