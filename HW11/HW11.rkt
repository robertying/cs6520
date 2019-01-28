#lang plait

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (listV [elems : (Listof Value)]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [n : Symbol]
        [arg-type : Type]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (emptyE)
  (consE [l : Exp]
         [r : Exp])
  (firstE [a : Exp])
  (restE [a : Exp])
  (if0E [cond : Exp]
        [thn : Exp]
        [els : Exp]))

(define-type Type
  (numT)
  (boolT)
  (arrowT [arg : Type]
          [result : Type])
  (varT [is : (Boxof (Optionof Type))])
  (listofT [elem : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `empty s) (emptyE)]
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse-type (third bs))
                   (parse (third (s-exp->list s))))
             (parse (fourth bs))))]
    [(s-exp-match? `{lambda {[SYMBOL : ANY]} ANY} s)
     (let ([arg (s-exp->list
                 (first (s-exp->list 
                         (second (s-exp->list s)))))])
       (lamE (s-exp->symbol (first arg))
             (parse-type (third arg))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{cons ANY ANY} s)
     (consE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{first ANY} s)
     (firstE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{rest ANY} s)
     (restE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
    [(s-exp-match? `num s) 
     (numT)]
    [(s-exp-match? `bool s)
     (boolT)]
    [(s-exp-match? `(ANY -> ANY) s)
     (arrowT (parse-type (first (s-exp->list s)))
             (parse-type (third (s-exp->list s))))]
    [(s-exp-match? `(Listof ANY) s)
     (listofT (parse-type (second (s-exp->list s))))]
    [(s-exp-match? `? s) 
     (varT (box (none)))]
    [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{if0 0 1 8})
        (if0E (numE 0)
              (numE 1)
              (numE 8)))
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE 'x (numT) (idE 'y))
              (plusE (numE 1) (numE 2))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE 'x (numT) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `empty)
        (emptyE))
  (test (parse `{cons 1 2})
        (consE (numE 1) (numE 2)))
  (test (parse `{first 1})
        (firstE (numE 1)))
  (test (parse `{rest 1})
        (restE (numE 1)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (numT) (boolT)))
  (test (parse-type `?)
        (varT (box (none))))
  (test (parse-type `(Listof num))
        (listofT (numT)))
  (test/exn (parse-type `1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(lamE n t body)
     (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(emptyE) (listV empty)]
    [(consE l r) (let ([v-l (interp l env)]
                       [v-r (interp r env)])
                   (type-case Value v-r
                     [(listV elems) (listV (cons v-l elems))]
                     [else (error 'interp "not a list")]))]
    [(firstE a) (type-case Value (interp a env)
                  [(listV elems) (if (empty? elems)
                                     (error 'interp "list is empty")
                                     (first elems))]
                  [else (error 'interp "not a list")])]
    [(restE a) (type-case Value (interp a env)
                 [(listV elems) (if (empty? elems)
                                    (error 'interp "list is empty")
                                    (listV (rest elems)))]
                 [else (error 'interp "not a list")])]
    [(if0E cond thn els) (type-case Value (interp cond env)
                           [(numV n) (if (= 0 n)
                                         (interp thn env)
                                         (interp els env))]
                           [else (error 'interp "not a number")])]))

(define (run-prog [e : S-Exp]) :  S-Exp
  (let ([exp (parse e)])
    (begin
      (typecheck exp mt-env)
      (type-case Value (interp exp mt-env)
        [(numV n) (number->s-exp n)]
        [(closV a b c) `function]
        [(listV a) `list]))))

(module+ test
  (test (run-prog `empty)
        `list)
  
  (test (run-prog `{cons 1 empty})
        `list)
  (test (run-prog `{cons empty empty})
        `list)
  (test/exn (run-prog `{cons 1 {cons empty empty}})
            "no type")
  
  (test/exn (run-prog `{first 1})
            "no type")
  (test/exn (run-prog `{rest 1})
            "no type")
  
  (test/exn (run-prog `{first empty})
            "list is empty")
  (test/exn (run-prog `{rest empty})
            "list is empty")
  
  (test (run-prog `{let {[f : ?
                            {lambda {[x : ?]} {first x}}]}
                     {+ {f {cons 1 empty}} 3}})
        `4)
  (test (run-prog `{let {[f : ?
                            {lambda {[x : ?]} {rest x}}]}
                     {+ {first {f {cons 1 {cons 2 empty}}}} 3}})
        `5)
  (test (run-prog `{let {[f : ?
                            {lambda {[x : ?]}
                              {lambda {[y : ?]}
                                {cons x y}}}]}
                     {first {rest {{f 1} {cons 2 empty}}}}})
        `2)
  
  (test/exn (run-prog `{lambda {[x : ?]}
                         {cons x x}})
            "no type")
  
  (test/exn (run-prog `{let {[f : ? {lambda {[x : ?]} x}]}
                         {cons {f 1} {f empty}}})
            "no type")
  (test (run-prog `1)
        `1)
  
  (test (run-prog `{if0 0 1 2})
        `1)
  (test (run-prog `{if0 2 1 0})
        `0)
  (test/exn (run-prog `{if0 2 1 {lambda {[x : num]} x}})
            "no type")
  (test (run-prog `{if0 2 {lambda {[x : ?]} x} {lambda {[x : ?]} {+ 1 x}}})
        `function)
  (test/exn (run-prog `{if0 {lambda {[x : ?]} x} 1 2})
            "no type")
  (test/exn (run-prog `{if0 0 {lambda {[x : ?]} x} 2})
            "no type")
  (test (run-prog `{let {[f : ?
                            {lambda {[x : ?]}
                              {lambda {[y : ?]}
                                {lambda {[z : ?]}
                                  {if0 x y z}}}}]}
                     {{{f 1} 2} 3}})
        `3)
  (test (run-prog `{let {[f : ?
                            {lambda {[x : ?]}
                              {lambda {[y : ?]}
                                {lambda {[z : ?]}
                                  {if0 x y {lambda ([x : ?]) z}}}}}]}
                     {{{{f 1} {lambda {[x : num]} 2}} 3} 4}})
        `3)
  (test/exn (run-prog `{let {[f : ?
                                {lambda {[x : ?]}
                                  {if0 x x {x 1}}}]}
                         {f 1}})
            "no type")
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `{if0 {lambda {[x : num]} x} 1 2}) mt-env)
            "not a number")
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))
  (test (interp (parse `empty)
                mt-env)
        (listV empty))
  (test (interp (parse `{cons 1 empty})
                mt-env)
        (listV (list (numV 1))))
  (test (interp (parse `{first {cons 1 empty}})
                mt-env)
        (numV 1))
  (test (interp (parse `{rest {cons 1 empty}})
                mt-env)
        (listV empty))
  (test/exn (interp (parse `{cons 1 2})
                    mt-env)
            "not a list")
  (test/exn (interp (parse `{first 1})
                    mt-env)
            "not a list")
  (test/exn (interp (parse `{rest 1})
                    mt-env)
            "not a list")
  (test/exn (interp (parse `{first empty})
                    mt-env)
            "list is empty")
  (test/exn (interp (parse `{rest empty})
                    mt-env)
            "list is empty")

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (cond
      [(empty? vals)
       (error 'find "free variable")]
      [else (if (equal? name (name-of (first vals)))
                (val-of (first vals))
                ((make-lookup name-of val-of) name (rest vals)))])))

(define lookup
  (make-lookup bind-name bind-val))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; typecheck ----------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env])
  (type-case Exp a
    [(numE n) (numT)]
    [(plusE l r) (typecheck-nums l r tenv)]
    [(multE l r) (typecheck-nums l r tenv)]
    [(idE n) (type-lookup n tenv)]
    [(lamE n arg-type body)
     (arrowT arg-type
             (typecheck body 
                        (extend-env (tbind n arg-type)
                                    tenv)))]
    [(appE fun arg)
     (local [(define result-type (varT (box (none))))]
       (begin
         (unify! (arrowT (typecheck arg tenv)
                         result-type)
                 (typecheck fun tenv)
                 fun)
         result-type))]
    [(if0E cond thn els)
     (begin
       (unify! (typecheck cond tenv)
               (numT)
               cond)
       (unify! (typecheck thn tenv)
               (typecheck els tenv)
               thn)
       (typecheck thn tenv))]
    [(emptyE) (listofT (varT (box (none))))]
    [(consE l r)
     (local [(define result-type (typecheck r tenv))]
       (begin
         (unify! result-type
                 (listofT (typecheck l tenv))
                 l)
         result-type))]
    [(firstE a)
     (local [(define result-type (varT (box (none))))]
       (begin
         (unify! (listofT result-type)
                 (typecheck a tenv)
                 a)
         result-type))]
    [(restE a)
     (local [(define result-type (listofT (varT (box (none)))))]
       (begin
         (unify! result-type
                 (typecheck a tenv)
                 a)
         result-type))]))

(define (typecheck-nums l r tenv)
  (begin
    (unify! (typecheck l tenv)
            (numT)
            l)
    (unify! (typecheck r tenv)
            (numT)
            r)
    (numT)))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} 12}) mt-env)
        (arrowT (numT) (numT)))
  (test (typecheck (parse `{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (numT) (arrowT (boolT)  (numT))))

  (test (resolve (typecheck (parse `{{lambda {[x : num]} 12}
                                     {+ 1 17}})
                            mt-env))
        (numT))

  (test (resolve (typecheck (parse `{let {[x : num 4]}
                                      {let {[f : (num -> num)
                                               {lambda {[y : num]} {+ x y}}]}
                                        {f x}}})
                            mt-env))
        (numT))

  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))

;; unify! ----------------------------------------
(define (unify! [t1 : Type] [t2 : Type] [expr : Exp])
  (type-case Type t1
    [(varT is1)
     (type-case (Optionof Type) (unbox is1)
       [(some t3) (unify! t3 t2 expr)]
       [(none)
        (local [(define t3 (resolve t2))]
          (if (eq? t1 t3)
              (values)
              (if (occurs? t1 t3)
                  (type-error expr t1 t3)
                  (begin
                    (set-box! is1 (some t3))
                    (values)))))])]
    [else
     (type-case Type t2
       [(varT is2) (unify! t2 t1 expr)]
       [(numT) (type-case Type t1
                 [(numT) (values)]
                 [else (type-error expr t1 t2)])]
       [(boolT) (type-case Type t1
                  [(boolT) (values)]
                  [else (type-error expr t1 t2)])]
       [(arrowT a2 b2) (type-case Type t1
                         [(arrowT a1 b1)
                          (begin
                            (unify! a1 a2 expr)
                            (unify! b1 b2 expr))]
                         [else (type-error expr t1 t2)])]
       [(listofT e2) (type-case Type t1
                       [(listofT e1) (unify! e1 e2 expr)]
                       [else (type-error expr t1 t2)]) ])]))

(define (resolve [t : Type]) : Type
  (type-case Type t
    [(varT is)
     (type-case (Optionof Type) (unbox is)
       [(none) t]
       [(some t2) (resolve t2)])]
    [else t]))

(define (occurs? [r : Type] [t : Type]) : Boolean
  (type-case Type t
    [(numT) #f]
    [(boolT) #f]
    [(arrowT a b)
     (or (occurs? r a)
         (occurs? r b))]
    [(varT is) (or (eq? r t) ; eq? checks for the same box
                   (type-case (Optionof Type) (unbox is)
                     [(none) #f]
                     [(some t2) (occurs? r t2)]))]
    [(listofT e) (occurs? r e)]))

(define (type-error [a : Exp] [t1 : Type] [t2 : Type])
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append
                       " type "
                       (string-append
                        (to-string t1)
                        (string-append
                         " vs. "
                         (to-string t2))))))))

(module+ test
  (define a-type-var (varT (box (none))))
  (define an-expr (numE 0))
  
  (test (unify! (numT) (numT) an-expr)
        (values))
  (test (unify! (boolT) (boolT) an-expr)
        (values))
  (test (unify! (arrowT (numT) (boolT)) (arrowT (numT) (boolT)) an-expr)
        (values))
  (test (unify! (varT (box (some (boolT)))) (boolT) an-expr)
        (values))
  (test (unify! (boolT) (varT (box (some (boolT)))) an-expr)
        (values))
  (test (unify! a-type-var a-type-var an-expr)
        (values))
  (test (unify! a-type-var (varT (box (some a-type-var))) an-expr)
        (values))
  
  (test (let ([t (varT (box (none)))])
          (begin
            (unify! t (boolT) an-expr)
            (unify! t (boolT) an-expr)))
        (values))
  
  (test/exn (unify! (numT) (boolT) an-expr)
            "no type")
  (test/exn (unify! (listofT (numT)) (boolT) an-expr)
            "no type")
  (test/exn (unify! (boolT) (listofT (numT)) an-expr)
            "no type")
  (test/exn (unify! (numT) (arrowT (numT) (boolT)) an-expr)
            "no type")
  (test/exn (unify! (arrowT (numT) (numT)) (arrowT (numT) (boolT)) an-expr)
            "no type")
  (test/exn (let ([t (varT (box (none)))])
              (begin
                (unify! t (boolT) an-expr)
                (unify! t (numT) an-expr)))
            "no type")
  (test/exn (unify! a-type-var (arrowT a-type-var (boolT)) an-expr)
            "no type")
  (test/exn (unify! a-type-var (arrowT (boolT) a-type-var) an-expr)
            "no type")
  
  (test (resolve a-type-var)
        a-type-var)
  (test (resolve (varT (box (some (numT)))))
        (numT))
  
  (test (occurs? a-type-var a-type-var)
        #t)
  (test (occurs? a-type-var (varT (box (none))))
        #f)
  (test (occurs? a-type-var (varT (box (some a-type-var))))
        #t)
  (test (occurs? a-type-var (numT))
        #f)
  (test (occurs? a-type-var (boolT))
        #f)
  (test (occurs? a-type-var (arrowT a-type-var (numT)))
        #t)
  (test (occurs? a-type-var (arrowT (numT) a-type-var))
        #t))