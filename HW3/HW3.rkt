#lang plait

(define-type Value
  (numV [n : Number])
  (boolV [flag : Boolean])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (thunkV [exp : Exp]
          [env : Env]))

(define-type Exp
  (numE [n : Number])
  (boolE [flag : Boolean])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (eqE [l : Exp]
       [r : Exp])
  (ifE [cond : Exp]
       [t : Exp]
       [f : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (unletE [n : Symbol]
          [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (delayE [e : Exp])
  (forceE [e : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (local [(define sym (s-exp->symbol s))]
       (if (or (eq? sym 'true)
               (eq? sym 'false))
           (boolE (if (eq? sym 'true) #t #f))
           (idE sym)))]
    [(s-exp-match? `{delay ANY} s)
     (delayE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{force ANY} s)
     (forceE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (eqE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{unlet SYMBOL ANY} s)
     (let ([bs (s-exp->list s)])
       (unletE (s-exp->symbol (second bs))
               (parse (third bs))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `true)
        (boolE #t))
  (test (parse `x)
        (idE 'x))
  (test (parse `{delay 1})
        (delayE (numE 1)))
  (test (parse `{force 1})
        (forceE (numE 1)))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{= 1 2})
        (eqE (numE 1) (numE 2)))
  (test (parse `{if true 1 2})
        (ifE (boolE #t) (numE 1) (numE 2)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{unlet x {+ 1 2}})
        (unletE 'x (plusE (numE 1) (numE 2))))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(boolE f) (boolV f)]
    [(delayE d) (thunkV d env)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(eqE l r) (numV=? (interp l env) (interp r env))]
    [(ifE cond t f) (if (is-boolean? (interp cond env))
                        (if (equal? (boolV #t) (interp cond env))
                            (interp t env)
                            (interp f env))
                        (error 'interp "not a boolean"))]
    [(letE n rhs body) (interp body
                               (extend-env
                                (bind n (interp rhs env))
                                env))]
    [(unletE n body) (interp body
                             (remove n env))]
    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(forceE f)(type-case Value (interp f env)
                 [(thunkV exp t-env) (interp exp t-env)]
                 [else (error 'interp "not a thunk")])
               ]))

(define (numV=? [l : Value] [r : Value]) : Value
  (type-case Value l
    [(numV n)
     (type-case Value r
       [(numV m) (boolV (= n m))]
       [(boolV g) (error 'interp "not a number")]
       [(closV a b c) (error 'interp "not a number")]
       [(thunkV exp env) (error 'interp "not a number")])]
    [(boolV f) (error 'interp "not a number")]
    [(closV a b c) (error 'interp "not a number")]
    [(thunkV exp env) (error 'interp "not a number")]))

(define (is-boolean? [v : Value]) : Boolean
  (type-case Value v
    [(numV n) #f]
    [(boolV f) #t]
    [(closV a b c) #f]
    [(thunkV exp env) #f]))

(module+ test
  (test (interp (parse `{delay {+ 1 2}}) mt-env)
        (thunkV (plusE (numE 1) (numE 2)) mt-env))
  (test (interp (parse `{if {= 8 8} {delay 7} {delay 9}}) mt-env)
        (thunkV (numE 7) mt-env))
  (test (interp (parse `{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            d}}) mt-env)
        (thunkV (plusE (idE 'y) (numE 7)) (list (bind 'y (numV 8)))))
  (test/exn (interp (parse `{force 1})
                    mt-env)
            "not a thunk")
  (test (interp (parse `{force {if {= 8 8} {delay 7} {delay 9}}})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            {force d}}})
                mt-env)
        (interp (parse `15)
                mt-env))
  (test (is-boolean? (numV 1))
        #f)
  (test (is-boolean? (boolV #t))
        #t)
  (test (is-boolean? (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
        #f)
  (test (is-boolean? (thunkV (plusE (idE 'y) (numE 7)) (list (bind 'y (numV 8)))))
        #f)
  (test (numV=? (numV 1) (numV 1))
        (boolV #t))
  (test (numV=? (numV 1) (numV 2))
        (boolV #f))
  (test/exn (numV=? (numV 1) (boolV #t))
            "not a number")
  (test/exn (numV=? (numV 1) (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
            "not a number")
  (test/exn (numV=? (numV 1) (thunkV (plusE (idE 'y) (numE 7)) (list (bind 'y (numV 8)))))
            "not a number")
  (test/exn (numV=? (boolV #f) (boolV #f))
            "not a number")
  (test/exn (numV=? (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
                    (boolV #f))
            "not a number")
  (test/exn (numV=? (thunkV (plusE (idE 'y) (numE 7)) (list (bind 'y (numV 8))))
                    (boolV #f))
            "not a number")
  (test (interp (parse `{= 1 2}) mt-env)
        (boolV #f))
  (test (interp (parse `{= 1 1}) mt-env)
        (boolV #t))
  (test/exn (interp (parse `{= true 1}) mt-env)
            "not a number")
  (test (interp (parse `{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse `9)
                mt-env))
  (test (interp (parse `{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse `10)
                mt-env))
  (test/exn (interp (parse `{if 1 2 3})
                    mt-env)
            "not a boolean")
  (test (interp (parse `2) mt-env)
        (numV 2))
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
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test  (interp (parse `{let {[x 1]}
                           {let {[x 2]}
                             {unlet x
                                    x}}})
                 mt-env)
         (interp (parse `1) mt-env))
  (test/exn (interp (parse `{let {[x 1]}
                              {unlet x
                                     x}})
                    mt-env)
            "free variable")
  (test (interp (parse `{let {[x 1]}
                          {+ x {unlet x 1}}})
                mt-env)
        (interp (parse `2) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env)
        (interp (parse `3) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env)
        (interp (parse `6) mt-env))
  (test (interp (parse `{let {[f {lambda {z}
                                   {let {[z 8]}
                                     {unlet z
                                            z}}}]}
                          {f 2}})
                mt-env)
        (interp (parse `2) mt-env))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

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
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-val b)]
                        [else (lookup n rst-env)])]))

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

;; remove ----------------------------------------
(define (remove [n : Symbol] [env : Env]) : (Listof Binding)
  (type-case (Listof Binding) env
    [empty empty]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         rst-env]
                        [else (cons b (remove n rst-env))])]))

(module+ test
  (test (remove 'x (extend-env (bind 'x (numV 8)) mt-env))
        empty)
  (test (remove 'x mt-env)
        empty)
  (test (remove 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (list (bind 'x (numV 8))))
  (test (remove 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (list (bind 'x (numV 9)))))
