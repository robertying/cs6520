#lang plait

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (pairV [arg : Symbol]
         [body : Exp]
         [env : Env]))

(define-type Thunk
  (delay [body : Exp]
         [env : Env]
         [done : (Boxof (Optionof Value))]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Thunk]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

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
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{pair ANY ANY} s)
     (let ([first-s (second (s-exp->list s))]
           [second-s (third (s-exp->list s))])
       (parse `{lambda {s} {if0 s ,first-s ,second-s}}))]
    [(s-exp-match? `{fst ANY} s)
     (let ([inputPair (second (s-exp->list s))])
       (parse `{,inputPair 0}))]
    [(s-exp-match? `{snd ANY} s)
     (let ([inputPair (second (s-exp->list s))])
       (parse `{,inputPair 1}))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

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
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE 'x (idE 'y))
              (plusE (numE 1) (numE 2))))
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
    [(idE s) (force (lookup s env))]
    [(plusE l r) (num+ (interp l env)
                       (interp r env))]
    [(multE l r) (num* (interp l env)
                       (interp r env))]
    [(lamE n body)
     (type-case Exp body
       [(if0E sym fst snd) (pairV n body env)]
       [else (closV n body env)])]
    [(if0E tst thn els)
     (interp (if (num-zero? (interp tst env))
                 thn
                 els)
             env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n (delay arg env (box (none))))
                                c-env))]
                      [(pairV n body c-env)
                       (interp body
                               (extend-env
                                (bind n (delay arg env (box (none))))
                                c-env))]
                      [else (error 'interp "not a function")])]))

(define (num-zero? [v : Value]) : Boolean
  (type-case Value v
    [(numV n) (zero? n)]
    [else (error 'interp "not a number")]))

(define (interp-expr [e : Exp]) : S-Exp
  (type-case Value (interp e mt-env)
    [(numV n) (number->s-exp n)]
    [(closV arg body env) `function]
    [(pairV arg body env) `pair]))

(module+ test
  (test (interp-expr (parse `{lambda {r} 1}))
        `function)
  (test/exn (num-zero? (closV 'x (numE 2) mt-env))
            "not a number")
  (test (interp-expr (parse `10))
        `10)
  (test (interp-expr (parse `{+ 10 17}))
        `27)
  (test (interp-expr (parse `{* 10 7}))
        `70)
  (test (interp-expr (parse `{{lambda {x} {+ x 12}}
                              {+ 1 17}}))
        `30)
  
  (test (interp-expr (parse `{let {[x 0]}
                               {let {[f {lambda {y} {+ x y}}]}
                                 {+ {f 1}
                                    {let {[x 3]}
                                      {f 2}}}}}))
        `3)
  
  (test (interp-expr (parse `{if0 0 1 2}))
        `1)
  (test (interp-expr (parse `{if0 1 1 2}))
        `2)
  
  (test (interp-expr (parse `{pair 1 2}))
        `pair)
  (test (interp-expr (parse `{pair 1 {1 2}}))
        `pair)
  (test (interp-expr (parse `{fst {pair 1 2}}))
        `1)
  (test (interp-expr (parse `{snd {pair 1 2}}))
        `2)
  (test (interp-expr (parse `{let {[p {pair 1 2}]}
                               {+ {fst p} {snd p}}}))
        `3)
  
  ;; Lazy evaluation:
  (test (interp-expr (parse `{{lambda {x} 0}
                              {+ 1 {lambda {y} y}}}))
        `0)
  (test (interp-expr (parse `{let {[x {+ 1 {lambda {y} y}}]}
                               0}))
        `0)
  (test (interp-expr (parse `{fst {pair 3
                                        {+ 1 {lambda {y} y}}}}))
        `3)
  (test (interp-expr (parse `{snd {pair {+ 1 {lambda {y} y}}
                                        4}}))
        `4)
  (test (interp-expr (parse `{fst {pair 5
                                        ;; Infinite loop:
                                        {{lambda {x} {x x}}
                                         {lambda {x} {x x}}}}}))
        `5)
  (test (interp-expr (parse `{fst {pair 1 {1 2}}}))
        `1)
  
  (test (interp-expr 
         (parse 
          `{let {[mkrec
                  ;; This is call-by-name mkrec
                  ;;  (simpler than call-by-value):
                  {lambda {body-proc}
                    {let {[fX {lambda {fX}
                                {body-proc {fX fX}}}]}
                      {fX fX}}}]}
             {let {[fib
                    {mkrec
                     {lambda {fib}
                       ;; Fib:
                       {lambda {n}
                         {if0 n
                              1
                              {if0 {+ n -1}
                                   1
                                   {+ {fib {+ n -1}}
                                      {fib {+ n -2}}}}}}}}]}
               ;; Call fib on 4:
               {fib 4}}}))
        `5)

  (test (interp-expr 
         (parse 
          `{let {[mkrec
                  ;; This is call-by-name mkrec
                  ;;  (simpler than call-by-value):
                  {lambda {body-proc}
                    {let {[fX {lambda {fX}
                                {body-proc {fX fX}}}]}
                      {fX fX}}}]}
             {let {[nats-from
                    {mkrec
                     {lambda {nats-from}
                       ;; nats-from:
                       {lambda {n}
                         {pair n {nats-from {+ n 1}}}}}}]}
               {let {[list-ref
                      {mkrec
                       {lambda {list-ref}
                         ;; list-ref:
                         {lambda {n}
                           {lambda {l}
                             {if0 n
                                  {fst l}
                                  {{list-ref {+ n -1}} {snd l}}}}}}}]}
                 ;; Call list-ref on infinite list:
                 {{list-ref 4} {nats-from 2}}}}}))
        `6)
  
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (delay (numE 9) mt-env (box (none)))) mt-env))
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
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test (interp (parse `{{lambda {x} 5} {1 2}})
                mt-env)
        (numV 5))

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

;; force ----------------------------------------

(define (force [t : Thunk]) : Value
  (type-case Thunk t
    [(delay b e d) (type-case (Optionof Value) (unbox d)
                     [(none )
                      (let ([v (interp b e)])
                        (begin
                          (set-box! d (some v))
                          v))]
                     [(some v) v])]))

(module+ test
  (test (force (delay (numE 8) mt-env (box (none))))
        (numV 8))
  (test (let ([v (delay (numE 8) mt-env (box (none)))])
          (begin
            (force v)
            (force v)))
        (numV 8))
  (test (force (delay (numE 8) mt-env (box (some (numV 9)))))
        (numV 9))
  (test (force (delay (idE 'x)
                      (extend-env (bind 'x (delay (numE 9) mt-env (box (none))))
                                  mt-env)
                      (box (none))))
        (numV 9)))

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
(define (lookup [n : Symbol] [env : Env]) : Thunk
  (type-case (Listof Binding) env
    [empty (error 'lookup "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-val b)]
                        [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (delay (numE 8) mt-env (box (none)))) mt-env))
        (delay (numE 8) mt-env (box (none))))
  (test (lookup 'x (extend-env
                    (bind 'x (delay (numE 9) mt-env (box (none))))
                    (extend-env (bind 'x (delay (numE 8) mt-env (box (none)))) mt-env)))
        (delay (numE 9) mt-env (box (none))))
  (test (lookup 'y (extend-env
                    (bind 'x (delay (numE 9) mt-env (box (none))))
                    (extend-env (bind 'y (delay (numE 8) mt-env (box (none)))) mt-env)))
        (delay (numE 8) mt-env (box (none)))))