#lang plait

(define-type (Value 'a)
  (litV [n : 'a])
  (closV [arg : Symbol]
         [body : (Exp 'a)]
         [env : Env]))

(define-type (Exp 'a)
  (litE [n : 'a])
  (idE [s : Symbol])
  (plusE [l : (Exp 'a)] 
         [r : (Exp 'a)])
  (multE [l : (Exp 'a)]
         [r : (Exp 'a)])
  (lamE [n : Symbol]
        [body : (Exp 'a)])
  (appE [fun : (Exp 'a)]
        [arg : (Exp 'a)]))

(define-type (Binding 'a)
  (bind [name : Symbol]
        [val : (Value 'a)]))

(define-type-alias (Env 'a) (Listof (Binding 'a)))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define parse : (S-Exp S-Exp (S-Exp -> 'a) -> (Exp 'a))
  (lambda (s pattern s-exp->)
    (cond
      [(s-exp-match? pattern s) (litE (s-exp-> s))]
      [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
      [(s-exp-match? `{+ ANY ANY} s)
       (plusE (parse (second (s-exp->list s)) pattern s-exp->)
              (parse (third (s-exp->list s)) pattern s-exp->))]
      [(s-exp-match? `{* ANY ANY} s)
       (multE (parse (second (s-exp->list s)) pattern s-exp->)
              (parse (third (s-exp->list s)) pattern s-exp->))]
      [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
       (let ([bs (s-exp->list (first
                               (s-exp->list (second
                                             (s-exp->list s)))))])
         (appE (lamE (s-exp->symbol (first bs))
                     (parse (third (s-exp->list s)) pattern s-exp->))
               (parse (second bs) pattern s-exp->)))]
      [(s-exp-match? `{lambda {SYMBOL} ANY} s)
       (lamE (s-exp->symbol (first (s-exp->list 
                                    (second (s-exp->list s)))))
             (parse (third (s-exp->list s)) pattern s-exp->))]
      [(s-exp-match? `{ANY ANY} s)
       (appE (parse (first (s-exp->list s)) pattern s-exp->)
             (parse (second (s-exp->list s)) pattern s-exp->))]
      [else (error 'parse "invalid input")])))

(define (parse/num [s : S-Exp]) : (Exp 'a)
  (parse s `NUMBER s-exp->number))

(define (parse/str [s : S-Exp]) : (Exp 'a)
  (parse s `STRING s-exp->string))

(module+ test
  (test (parse/num `2)
        (litE 2))
  (test (parse/num `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse/num `{+ 2 1})
        (plusE (litE 2) (litE 1)))
  (test (parse/num `{* 3 4})
        (multE (litE 3) (litE 4)))
  (test (parse/num `{+ {* 3 4} 8})
        (plusE (multE (litE 3) (litE 4))
               (litE 8)))
  (test (parse/num `{let {[x {+ 1 2}]}
                      y})
        (appE (lamE 'x (idE 'y))
              (plusE (litE 1) (litE 2))))
  (test (parse/num `{lambda {x} 9})
        (lamE 'x (litE 9)))
  (test (parse/num `{double 9})
        (appE (idE 'double) (litE 9)))
  (test/exn (parse/num `{{+ 1 2}})
            "invalid input")
  (test/exn (parse/num `"a")
            "invalid input"))

(module+ test
  (test (parse/str `"a")
        (litE "a"))
  (test (parse/str `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse/str `{+ "b" "a"})
        (plusE (litE "b") (litE "a")))
  (test (parse/str `{* "c" "d"})
        (multE (litE "c") (litE "d")))
  (test (parse/str `{+ {* "c" "d"} "e"})
        (plusE (multE (litE "c") (litE "d"))
               (litE "e")))
  (test (parse/str `{let {[x {+ "a" "b"}]}
                      y})
        (appE (lamE 'x (idE 'y))
              (plusE (litE "a") (litE "b"))))
  (test (parse/str `{lambda {x} "g"})
        (lamE 'x (litE "g")))
  (test (parse/str `{double "g"})
        (appE (idE 'double) (litE "g")))
  (test/exn (parse/str `{{+ "a" "b"}})
            "invalid input")
  (test/exn (parse/str `1)
            "invalid input"))

;; interp ----------------------------------------
(define interp : ((Exp 'a)
                  (Listof ((Value 'a)
                           (Value 'a)
                           -> (Value 'a)))
                  Env
                  -> (Value 'a))
  (lambda (a ops env)
    (let ([op+ (first ops)]
          [op* (second ops)])
      (type-case (Exp 'a) a
        [(litE n) (litV n)]
        [(idE s) (lookup s env)]
        [(plusE l r) (op+ (interp l ops env) (interp r ops env))]
        [(multE l r) (op* (interp l ops env) (interp r ops env))]
        [(lamE n body)
         (closV n body env)]
        [(appE fun arg) (type-case (Value 'a) (interp fun ops env)
                          [(closV n body c-env)
                           (interp body ops
                                   (extend-env
                                    (bind n
                                          (interp arg ops env))
                                    c-env))]
                          [else (error 'interp "not a function")])]))))

(define (interp/num [a : (Exp 'a)] [env : Env]) : (Value 'a)
  (interp a (list num+ num*) env))

(define (interp/str [a : (Exp 'a)] [env : Env]) : (Value 'a)
  (interp a (list str+ str*) env))

(module+ test
  (test (interp/num (parse/num `2) mt-env)
        (litV 2))
  (test/exn (interp/num (parse/num `x) mt-env)
            "free variable")
  (test (interp/num (parse/num `x) 
                    (extend-env (bind 'x (litV 9)) mt-env))
        (litV 9))
  (test (interp/num (parse/num `{+ 2 1}) mt-env)
        (litV 3))
  (test (interp/num (parse/num `{* 2 1}) mt-env)
        (litV 2))
  (test (interp/num (parse/num `{+ {* 2 3} {+ 5 8}})
                    mt-env)
        (litV 19))
  (test (interp/num (parse/num `{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {+ x x}})
                    mt-env)
        (litV 10))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {let {[x {+ 1 x}]}
                                    {+ x x}}})
                    mt-env)
        (litV 12))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {let {[y 6]}
                                    x}})
                    mt-env)
        (litV 5))
  (test (interp/num (parse/num `{{lambda {x} {+ x x}} 8})
                    mt-env)
        (litV 16))

  (test/exn (interp/num (parse/num `{1 2}) mt-env)
            "not a function")
  (test/exn (interp/num (parse/num `{+ 1 {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/num (parse/num `{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y 5]}
                                        {bad 2}}})
                        mt-env)
            "free variable"))

(module+ test
  (test (interp/str (parse/str `"b") mt-env)
        (litV "b"))
  (test/exn (interp/str (parse/str `x) mt-env)
            "free variable")
  (test (interp/str (parse/str `x) 
                    (extend-env (bind 'x (litV "g")) mt-env))
        (litV "g"))
  (test (interp/str (parse/str `{+ "b" "a"}) mt-env)
        (litV "ba"))
  (test (interp/str (parse/str `{* "b" "a"}) mt-env)
        (litV "a"))
  (test (interp/str (parse/str `{+ {* "a" "b"} {+ "c" "d"}})
                    mt-env)
        (litV "bcd"))
  (test (interp/str (parse/str `{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp/str (parse/str `{let {[x "e"]}
                                  {+ x x}})
                    mt-env)
        (litV "ee"))
  (test (interp/str (parse/str `{let {[x "e"]}
                                  {let {[x {+ "a" x}]}
                                    {+ x x}}})
                    mt-env)
        (litV "aeae"))
  (test (interp/str (parse/str `{let {[x "e"]}
                                  {let {[y "f"]}
                                    x}})
                    mt-env)
        (litV "e"))
  (test (interp/str (parse/str `{{lambda {x} {+ x x}} "f"})
                    mt-env)
        (litV "ff"))

  (test/exn (interp/str (parse/str `{"a" "b"}) mt-env)
            "not a function")
  (test/exn (interp/str (parse/str `{+ "a" {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/str (parse/str `{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y "e"]}
                                        {bad "b"}}})
                        mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define num-op : ((Number Number -> Number)
                  (Value 'a)
                  (Value 'a)
                  -> (Value 'a))
  (lambda (op l r)
    (cond
      [(and (litV? l) (litV? r))
       (litV (op (litV-n l) (litV-n r)))]
      [else
       (error 'interp "not a literal")])))

(define (num+ [l : (Value 'a)] [r : (Value 'a)]) : (Value 'a)
  (num-op + l r))
(define (num* [l : (Value 'a)] [r : (Value 'a)]) : (Value 'a)
  (num-op * l r))

(module+ test
  (test (num+ (litV 1) (litV 2))
        (litV 3))
  (test (num* (litV 2) (litV 3))
        (litV 6)))

;; str+ and str* ----------------------------------------
(define str-op : ((String String -> String)
                  (Value 'a)
                  (Value 'a)
                  -> (Value 'a))
  (lambda (op l r)
    (cond
      [(and (litV? l) (litV? r))
       (litV (op (litV-n l) (litV-n r)))]
      [else
       (error 'interp "not a literal")])))

(define (str+ [l : (Value 'a)] [r : (Value 'a)]) : (Value 'a)
  (str-op string-append l r))
(define (str* [l : (Value 'a)] [r : (Value 'a)]) : (Value 'a)
  (str-op string-mult l r))

(define (string-mult [a : String] [b : String])
  (foldl (lambda (c r) (string-append b r))
         ""
         (string->list a)))

(module+ test
  (test (str+ (litV "abc") (litV "de"))
        (litV "abcde"))
  (test (str* (litV "abc") (litV "de"))
        (litV "dedede")))

;; lookup ----------------------------------------
(define lookup : (Symbol Env -> (Value 'a))
  (lambda (n env)
    (cond
      [(empty? env) (error 'lookup "free variable")]
      [else (cond
              [(symbol=? n (bind-name (first env)))
               (bind-val (first env))]
              [else (lookup n (rest env))])])))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (litV 8)) mt-env))
        (litV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'x (litV 8)) mt-env)))
        (litV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'y (litV 8)) mt-env)))
        (litV 8)))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (litV "f")) mt-env))
        (litV "f"))
  (test (lookup 'x (extend-env
                    (bind 'x (litV "g"))
                    (extend-env (bind 'x (litV "f")) mt-env)))
        (litV "g"))
  (test (lookup 'y (extend-env
                    (bind 'x (litV "g"))
                    (extend-env (bind 'y (litV "f")) mt-env)))
        (litV "f")))