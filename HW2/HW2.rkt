#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (maxE [l : Exp]
        [r : Exp])
  (appE [s : Symbol]
        [args : (Listof Exp)]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [args : (Listof Symbol)] 
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{max EXP EXP}
;; - `{SYMBOL EXP ...}

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL ...} EXP}

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
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{max 3 4})
        (maxE (numE 3) (numE 4)))
  (test (parse `{five})
        (appE 'five empty))
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
  (test (parse `{area x 10})
        (appE 'area (list (idE 'x) (numE 10))))
  (test (parse `{area 9 10})
        (appE 'area (list (numE 9) (numE 10))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))
  
(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (local [(define arguments (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s))))))]
       (if (same-arguments? arguments)
           (error 'parse-fundef "bad syntax")
           (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
               arguments
               (parse (third (s-exp->list s))))))]
    [else (error 'parse-fundef "invalid input")]))

(define (same-arguments? [args : (Listof Symbol)]) : Boolean
  (type-case (Listof Symbol) args
    [empty #f]
    [(cons fst rst)(or (member fst rst) (same-arguments? rst))]))

(module+ test
  (test (same-arguments? '(x x))
        #t)
  (test (same-arguments? '(x y))
        #f)
  (test (same-arguments? '(x))
        #f)
  (test (same-arguments? '())
        #f)
  (test (same-arguments? '(x y x))
        #t)
  (test (same-arguments? '(x y z p q y))
        #t)
  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test (parse-fundef `{define {five} 5})
        (fd 'five empty (numE 5)))
  (test (parse-fundef `{define {area w h} {* w h}})
        (fd 'area (list 'w 'h) (multE (idE 'w) (idE 'h))))
  (test (parse-fundef `{define {f x y} x})
        (fd 'f (list 'x 'y) (idE 'x)))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")
  (test/exn (parse-fundef `{define {f x x} x})
            "bad syntax")
  (test/exn (parse-fundef `{define {f x y z x} x})
            "bad syntax"))

(define double-def
  (parse-fundef `{define {double x} {+ x x}}))
(define quadruple-def
  (parse-fundef `{define {quadruple x} {double {double x}}}))
(define area-def
  (parse-fundef `{define {area w h} {* w h}}))
(define five-def
  (parse-fundef `{define {five} 5}))

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test (get-fundef 'five (list five-def area-def))
        five-def)
  (test (get-fundef 'area (list five-def area-def))
        area-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp]) : Exp
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]
    [(appE s args) (appE s (map (lambda (arg) (subst what for arg)) args))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{max 4 x}))
        (parse `{max 4 8}))
  (test (subst (parse `8) 'x (parse `{max y x}))
        (parse `{max y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8}))
  (test (subst (parse `8) 'w (parse `{area w h}))
        (parse `{area 8 h}))
  (test (subst (parse `9) 'h (parse `{area w h}))
        (parse `{area w 9}))
  (test (subst (parse `9) 'h (parse `{area 8 h}))
        (parse `{area 8 9})))

(define (subst-list [what : (Listof Exp)] [for : (Listof Symbol)] [in : Exp]) : Exp
  (type-case (Listof Exp) what
    [empty in]
    [(cons fst-what rst-what)
     (type-case Exp in
       [(numE n) in]
       [(idE s) (if (eq? (first for) s)
                    fst-what
                    (subst-list rst-what (rest for) (subst fst-what (first for) in)))]
       [(plusE l r) (plusE (subst-list rst-what (rest for) (subst fst-what (first for) l))
                           (subst-list rst-what (rest for) (subst fst-what (first for) r)))]
       [(multE l r) (multE (subst-list rst-what (rest for) (subst fst-what (first for) l))
                           (subst-list rst-what (rest for) (subst fst-what (first for) r)))]
       [(maxE l r) (maxE (subst-list rst-what (rest for) (subst fst-what (first for) l))
                         (subst-list rst-what (rest for) (subst fst-what (first for) r)))]
       [(appE s args) (appE s (map (lambda (arg) (subst-list rst-what (rest for) (subst fst-what (first for) arg))) args))])]))

(module+ test
  (test (subst-list (list (parse `9) (parse `10)) (list 'w 'h) (parse `{area w h}))
        (parse `{area 9 10}))
  (test (subst-list (list) (list) (parse `{five}))
        (parse `{five}))
  (test (subst-list (list (parse `9)) (list 'x) (parse `{double x}))
        (parse `{double 9}))
  )

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(maxE l r) (max (interp l defs) (interp r defs))]
    [(appE s args) (local [(define fd (get-fundef s defs))]
                     (if (= (length args) (length (fd-args fd)))
                         (interp (subst-list (map numE (map (lambda (arg) (interp arg defs)) args))
                                             (fd-args fd)
                                             (fd-body fd))
                                 defs)
                         (error 'interp "wrong arity")
                         ))]))

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{max 3 4})
                empty)
        4)
  (test (interp (parse `{max {+ 2 3} 4})
                empty)
        5)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                empty)
        9)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)
  (test (interp (parse `{five})
                (list area-def five-def))
        5)
  (test (interp (parse `{area 8 9})
                (list area-def five-def))
        72)
  (test (interp (parse `{f 1 2})
                (list (parse-fundef `{define {f x y} {+ x y}})))
        3)
  (test (interp (parse `{f 1 2 3})
                (list (parse-fundef `{define {f x y z} {+ {+ x y} z}})))
        6)
  (test (interp (parse `{f 1 2 3})
                (list (parse-fundef `{define {f x y z} {+ {max x y} z}})))
        5)
  (test (interp (parse `{f 1 2 3})
                (list (parse-fundef `{define {f x y z} {* 5 {+ {max x y} z}}})))
        25)
  (test (interp (parse `{f 1 2 3 5})
                (list (parse-fundef `{define {f x y z p} {* p {+ {max x y} z}}})))
        25)
  (test (interp (parse `{+ {f} {f}})
                (list (parse-fundef `{define {f} 5})))
        10)
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f x y} {+ x y}})))
            "wrong arity")
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f} 5})))
            "wrong arity"))
