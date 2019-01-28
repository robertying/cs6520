#lang plait

(define-type Value
  (numV [n : Number])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (contV [k : Cont]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [body : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)])
  (let/ccE [n : Symbol]
           [body : Exp])
  (negE [e : Exp])
  (avgE [fst : Exp]
        [snd : Exp]
        [thd : Exp])
  (if0E [e : Exp]
        [thn : Exp]
        [els : Exp])
  (tryE [e : Exp]
        [handler : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)
(define extend-env* append)

(define-type Cont
  (doneK)
  (plusSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doPlusK [v : Value]
           [k : Cont])
  (multSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doMultK [v : Value]
           [k : Cont])
  (appArgsK [clos : (Optionof Value)]
            [vs : (Listof Value)]   
            [args : (Listof Exp)]
            [env : Env]
            [k : Cont])
  (doAppK [f : Value]
          [vs : (Listof Value)]
          [k : Cont])
  (doNegK   [k : Cont])
  (avgSecondK [snd : Exp]
              [thd : Exp]
              [e : Env]
              [k : Cont])
  (avgThirdK [fstV : Value]
             [thd : Exp]
             [e : Env]
             [k : Cont])
  (doAvgK [fstV : Value]
          [sndV : Value]
          [k : Cont])
  (if0BranchK [thn : Exp]
              [els : Exp]
              [e : Env]
              [k : Cont])
  (doIf0K [k : Cont])
  (tryK [handler : Exp]
        [env : Env]
        [k : Cont]))

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{neg ANY} s)
     (negE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{avg ANY ANY ANY} s)
     (avgE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{try ANY {lambda {} ANY}} s)
     (tryE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list (third (s-exp->list s))))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (second bs)))))]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (lamE (map s-exp->symbol (s-exp->list 
                               (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let/cc SYMBOL ANY} s)
     (let/ccE (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `{neg 1})
        (negE (numE 1)))
  (test (parse `{avg 1 2 3})
        (avgE (numE 1) (numE 2) (numE 3)))
  (test (parse `{if0 1 2 3})
        (if0E (numE 1) (numE 2) (numE 3)))
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
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {x} 9})
        (lamE (list 'x) (numE 9)))
  (test (parse `{let/cc k 0})
        (let/ccE 'k (numE 0)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test/exn (parse `{})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : Exp] [env : Env] [k : Cont] [hs : (Listof Cont)]) : Value
  (type-case Exp a
    [(numE n) (continue k (numV n) hs)]
    [(idE s) (lookup s env k hs)]
    [(plusE l r) (interp l env
                         (plusSecondK r env k) hs)]
    [(multE l r) (interp l env
                         (multSecondK r env k) hs)]
    [(lamE ns body)
     (continue k (closV ns body env) hs)]
    [(appE fun args) (interp fun env
                             (appArgsK (none) empty args env k) hs)]
    [(let/ccE n body)
     (interp body
             (extend-env (bind n (contV k))
                         env)
             k hs)]
    [(negE e) (interp e env
                      (doNegK k) hs)]
    [(avgE fst snd thd) (interp fst env
                                (avgSecondK snd thd env k) hs)]
    [(if0E e thn els)
     (interp e env
             (if0BranchK thn els env k) hs)]
    [(tryE e handler)
     (interp e env k (cons (tryK handler env k) hs))]))

(define (continue [k : Cont] [v : Value] [hs : (Listof Cont)]) : Value
  (type-case Cont k
    [(doneK) v]
    [(plusSecondK r env next-k)
     (interp r env
             (doPlusK v next-k) hs)]
    [(doPlusK v-l next-k)
     (num+ v-l v next-k hs)]
    [(multSecondK r env next-k)
     (interp r env
             (doMultK v next-k) hs)]
    [(doMultK v-l next-k)
     (num* v-l v next-k hs)]
    [(appArgsK clos vs args env next-k)
     (type-case (Listof Exp) args
       [(cons fst rst)
        (if (none? clos)
            (interp fst env
                    (appArgsK (some v) empty rst env next-k) hs)
            (interp fst env
                    (appArgsK clos (cons v vs) rst env next-k) hs))]
       [empty
        (if (none? clos)
            (continue (doAppK v vs next-k) v hs)
            (continue (doAppK (some-v clos) (cons v vs) next-k) v hs))])]
    [(doAppK v-f vs next-k)
     (type-case Value v-f
       [(closV ns body c-env)
        (if (= 0 (length vs))
            (interp body
                    c-env
                    next-k
                    hs)
            (interp body
                    (extend-env*
                     (map2 bind (reverse ns) vs)
                     c-env)
                    next-k
                    hs))]
       [(contV k-v) (continue k-v v hs)]
       [else
        (if (empty? hs)
            (error 'interp "not a function")
            (continue (first hs) v (rest hs)))])]
    [(doNegK next-k)
     (type-case Value v
       [(numV num)
        (continue next-k (numV (- 0 num)) hs)]
       [else
        (if (empty? hs)
            (error 'interp "not a number")
            (continue (first hs) v (rest hs)))])]
    [(avgSecondK snd thd env next-k)
     (interp snd env
             (avgThirdK v thd env next-k) hs)]
    [(avgThirdK fstV thd env next-k)
     (interp thd env
             (doAvgK fstV v next-k) hs)]
    [(doAvgK fstV sndV next-k)
     (num/ (num+ (num+ fstV sndV next-k hs) v next-k hs) (numV 3) next-k hs)]
    [(if0BranchK thn els env next-k)
     (type-case Value v
       [(numV n)
        (if (= 0 n)
            (interp thn env
                    (doIf0K  next-k) hs)
            (interp els env
                    (doIf0K  next-k) hs))]
       [else
        (if (empty? hs)
            (error 'interp "not a number")
            (continue (first hs) v (rest hs)))])]
    [(doIf0K next-k)
     (continue next-k v hs)]
    [(tryK h env next-k)
     (interp h env
             next-k hs)]))

(define (interp-expr [e : Exp]) : S-Exp
  (type-case Value (interp e mt-env (doneK) empty)
    [(numV n) (number->s-exp n)]
    [(closV arg body env) `function]
    [(contV l) `function]))

(module+ test
  (test (interp (parse `{if0 a 2 3}) mt-env (doneK) (list (tryK (numE 1) mt-env (doneK))))
        (numV 1))
  (test (interp-expr (parse `{let/cc esc esc}))
        `function)
  (test (continue (doNegK (doneK)) (numV 1) empty)
        (numV -1))
  (test (continue (if0BranchK (numE 2) (numE 3) '() (doneK)) (numV 1) '())
        (numV 3))
  (test/exn (continue (if0BranchK (numE 1) (numE 2) mt-env (doneK)) (contV (doneK)) empty)
            "not a number")
  (test (continue (if0BranchK (numE 1) (numE 2) mt-env (doneK)) (contV (doneK)) (list (tryK (numE 1) mt-env (doneK))))
        (numV 1))
  (test (continue (doNegK (doneK)) (contV (doneK)) (list (tryK (numE 1) mt-env (doneK))))
        (numV 1))
  (test/exn (interp (parse `{if0 {0 0} 2 3}) mt-env (doneK) empty) 
            "not a function")
  (test (interp (parse `{try 0 {lambda {} 1}}) mt-env (doneK) empty) 
        (numV 0))
  (test (interp (parse `{try {0 0} {lambda {} 1}}) mt-env (doneK) empty) 
        (numV 1))
  (test/exn (interp (parse `{try {0 0} {lambda {} {1 1}}}) mt-env (doneK) empty) 
            "not a function")
  (test (interp (parse `{+ {try 2 {lambda {} 1}} 3}) mt-env (doneK) empty) 
        (numV 5))
  (test (interp (parse `{+ {try {2 2} {lambda {} 1}} 3}) mt-env (doneK) empty) 
        (numV 4))
  (test (interp (parse `{try {try {0 0}
                                  {lambda {} 1}}
                             {lambda {} 2}}) mt-env (doneK) empty) 
        (numV 1))
  (test (interp (parse `{try {try {0 0}
                                  {lambda {} {1 1}}}
                             {lambda {} 2}}) mt-env (doneK) empty) 
        (numV 2))
  (test (interp (parse `{try 0 {lambda {} 10}}) mt-env (doneK) empty)
        (numV 0))
  (test (interp (parse `{try {try {1 2}
                                  {lambda {} 9}}
                             {lambda {} 10}}) mt-env (doneK) empty)
        (numV 9))
  (test (interp (parse `{try {try {1 2}
                                  {lambda {} {2 2}}}
                             {lambda {} 10}}) mt-env (doneK) empty)
        (numV 10))
  (test (interp-expr (parse `{{lambda {x y} {+ y {neg x}}} 10 12}))
        `2)
  (test (interp-expr (parse `{lambda {} 12}))
        `function)
  (test (interp-expr (parse `{lambda {x} {lambda {} x}}))
        `function)
  (test (interp-expr (parse `{{{lambda {x} {lambda {} x}} 13}}))
        `13)

  (test (interp-expr (parse `{let/cc esc {{lambda {x y} x} 1 {esc 3}}}))
        `3)
  (test (interp-expr (parse `{{let/cc esc {{lambda {x y} {lambda {z} {+ z y}}}
                                           1 
                                           {let/cc k {esc k}}}}
                              10}))
        `20)
  (test (interp-expr (parse `{neg 2}))
        `-2)
  (test (interp-expr (parse `{avg 0 6 6}))
        `4)
  (test (interp-expr (parse `{let/cc k {neg {k 3}}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg 0 {k 3} 0}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg {k 2} {k 3} 0}}))
        `2)
  (test (interp-expr (parse `{if0 1 2 3}))
        `3)
  (test (interp-expr (parse `{if0 0 2 3}))
        `2)
  (test (interp-expr (parse `{let/cc k {if0 {k 9} 2 3}}))
        `9)
  (test (interp (parse `2) mt-env (doneK) empty )
        (numV 2))
  (test/exn (interp (parse `x) mt-env (doneK)  empty)
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK) empty )
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env (doneK)  empty)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env (doneK)  empty)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK)  empty)
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                (doneK) empty)
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK) empty)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK) empty)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK) empty)
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK)  empty)
        (numV 16))

  (test (interp (parse `{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK)  empty)
        (numV 0))
  (test (interp (parse `{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK)  empty)
        (numV 10))

  (test/exn (interp (parse `{1 2}) mt-env (doneK) empty)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env (doneK) empty)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK) empty)
            "free variable")
  ;; Eager:
  (test/exn (interp (parse `{{lambda {x} 0} {1 2}}) mt-env (doneK) empty)
            "not a function")

  (test (continue (doneK) (numV 5) empty)
        (numV 5))
  (test (continue (plusSecondK (numE 6) mt-env (doneK)) (numV 5) empty)
        (numV 11))
  (test/exn (continue (doNegK (doneK)) (contV (doneK)) empty)
            "not a number")
  (test (continue (doPlusK (numV 7) (doneK)) (numV 5) empty)
        (numV 12))
  (test (continue (multSecondK (numE 6) mt-env (doneK)) (numV 5) empty)
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5) empty)
        (numV 35))
  (test (continue (appArgsK (none) empty (list (numE 5)) mt-env (doneK)) (closV (list 'x) (idE 'x) mt-env) empty)
        (numV 5))
  (test (continue (doAppK (closV (list 'x) (idE 'x) mt-env) (list (numV 8)) (doneK)) (numV 8)  empty)
        (numV 8)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value] [k : Cont] [hs : (Listof Cont)]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (continue k (numV (op (numV-n l) (numV-n r))) hs)]
    [else
     (if (empty? hs)
         (error 'interp "not a number")
         (continue (first hs) (numV 0) (rest hs)))]))
(define (num+ [l : Value] [r : Value] [k : Cont] [hs : (Listof Cont)]) : Value
  (num-op + l r k hs))
(define (num/ [l : Value] [r : Value] [k : Cont] [hs : (Listof Cont)]) : Value
  (num-op / l r k hs))
(define (num* [l : Value] [r : Value] [k : Cont] [hs : (Listof Cont)]) : Value
  (num-op * l r k hs))

(module+ test
  (test (num+ (numV 1) (numV 2) (doneK) empty)
        (numV 3))
  (test (num+ (contV (doneK)) (numV 2) (doneK) (list (tryK (numE 4) mt-env (doneK))))
        (numV 4))
  (test (num* (numV 2) (numV 3) (doneK) empty)
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env] [k : Cont] [hs : (Listof Cont)]) : Value
  (type-case (Listof Binding) env
    [empty
     (if (empty? hs)
         (error 'interp "free variable")
         (continue (first hs) (numV 0) (rest hs)))]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (continue k (bind-val b) hs)    ]
                        [else (lookup n rst-env k hs)])]))

(module+ test
  (test/exn (lookup 'x mt-env (doneK) empty)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env) (doneK) empty)
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)) (doneK) empty)
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)) (doneK) (list (tryK (numE 1) mt-env (doneK))))
        (numV 1))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)) (doneK) empty)
        (numV 8)))