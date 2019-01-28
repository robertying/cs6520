#lang plait

(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location])
  (recV [ns : (Listof Symbol)]
        [vs : (Listof Value)]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (beginE [exps : (Listof Exp)])
  (recordE [ns : (Listof Symbol)]
           [args : (Listof Exp)])
  (getE [rec : Exp]
        [n : Symbol])
  (setE [rec : Exp]
        [n : Symbol]
        [nv : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  (v*s [v : Value] [s : Store]))

(define-type Results
  (vs*s [vs : (Listof Value)]
        [s : Store]))

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
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY ...} s)
     (beginE (map parse (rest (s-exp->list s))))]
    [(s-exp-match? `{record {SYMBOL ANY} ...} s)
     (recordE (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (getE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? `{set! ANY SYMBOL ANY} s)
     (setE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
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
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  (test (parse `{begin 1})
        (beginE (list (numE 1))))
  (test (parse `{begin 1 2})
        (beginE (list (numE 1) (numE 2))))
  (test (parse `{begin 1 2 3})
        (beginE (list (numE 1) (numE 2) (numE 3))))
  (test (parse `{record {x 2} {y 3}})
        (recordE (list 'x 'y)
                 (list (numE 2) (numE 3))))
  (test (parse `{get {+ 1 2} a})
        (getE (plusE (numE 1) (numE 2)) 'a))
  (test (parse `{let {[r {record {x 1}}]}
                  {set! r x 1}})
        (letE 'r (recordE '(x) (list (numE 1)))
              (setE (idE 'r) 'x (numE 1))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
        body)
  (type-case Result call
    [(v*s v-id sto-id) body]))
                                
;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (with [(v-l sto-l) (interp l env sto)]
           (with [(v-r sto-r) (interp r env sto-l)]
                 (v*s (num+ v-l v-r) sto-r)))]
    [(multE l r)
     (with [(v-l sto-l) (interp l env sto)]
           (with [(v-r sto-r) (interp r env sto-l)]
                 (v*s (num* v-l v-r) sto-r)))]
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
           (interp body
                   (extend-env
                    (bind n v-rhs)
                    env)
                   sto-rhs))]
    [(lamE n body)
     (v*s (closV n body env) sto)]
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
           (with [(v-a sto-a) (interp arg env sto-f)]
                 (type-case Value v-f
                   [(closV n body c-env)
                    (interp body
                            (extend-env
                             (bind n v-a)
                             c-env)
                            sto-a)]
                   [else (error 'interp "not a function")])))]
    [(boxE a)
     (with [(v sto-v) (interp a env sto)]
           (let ([l (new-loc sto-v)])
             (v*s (boxV l) 
                  (override-store (cell l v) 
                                  sto-v))))]
    [(unboxE a)
     (with [(v sto-v) (interp a env sto)]
           (type-case Value v
             [(boxV l) (v*s (fetch l sto-v) 
                            sto-v)]
             [else (error 'interp "not a box")]))]
    [(setboxE bx val)
     (with [(v-b sto-b) (interp bx env sto)]
           (with [(v-v sto-v) (interp val env sto-b)]
                 (type-case Value v-b
                   [(boxV l)
                    (v*s v-v
                         (replace-store sto-v
                                        (cell l v-v)))]
                   [else (error 'interp "not a box")])))]
    [(beginE exps)
     (type-case (Listof Exp) exps
       [empty (error 'interp "this should never happen!")]
       [(cons fst rst)
        (if (empty? rst)
            (interp fst env sto)
            (with [(v-l sto-l) (interp fst env sto)]
                  (interp (beginE rst) env sto-l)))])]
    [(recordE ns as)
     (type-case (Listof Exp) as
       [empty (v*s
               (recV ns empty)
               sto)]
       [(cons fst rst)
        (with [(v-l sto-l) (interp fst env sto)]
              (type-case Results (interp-list rst env sto-l)
                [(vs*s vlist sto-ll)
                 (let ([nl (new-loc sto-ll)])
                   (let ([nl+1 (new-loc (override-store (cell nl v-l) sto-ll))])
                     (v*s
                      (recV ns (cons (boxV nl) vlist))
                      (override-store (cell nl+1 (boxV nl))
                                      (override-store (cell nl v-l) sto-ll)))))]))])]
    [(getE a n)
     (with [(v-l sto-l) (interp a env sto)]
           (type-case Value v-l
             [(recV ns vs)
              (type-case Value (find n ns vs)
                [(boxV loc)
                 (v*s (fetch loc sto-l) sto-l)]
                [else (error 'interp "record field should be a box")])]
             [else
              (error 'interp "not a record")]))]
    [(setE r a n)
     (with [(v-l sto-l) (interp r env sto)]
           (with [(v-n sto-n) (interp n env sto-l)]
                 (type-case Value v-l
                   [(recV ns vs)
                    (type-case Value (find a ns vs)
                      [(boxV loc) (v*s
                                   v-n
                                   (replace-store sto-n (cell loc v-n)))]
                      [else (error 'interp "record field should be a box")])]
                   [else (error 'interp "not a record")])))]))

(define (interp-list [as : (Listof Exp)] [env : Env] [sto : Store]) : Results
  (type-case (Listof Exp) as
    [empty (vs*s empty sto)]
    [(cons fst rst)
     (with [(v-l sto-l) (interp fst env sto)]
           (type-case Results (interp-list rst env sto-l)
             [(vs*s vlist sto-ll)
              (let ([nl (new-loc sto-ll)])
                (let ([nl+1 (new-loc (override-store (cell nl v-l) sto-ll))])
                  (vs*s
                   (cons (boxV nl) vlist)
                   (override-store (cell nl+1 (boxV nl))
                                   (override-store (cell nl v-l) sto-ll)))))]))]))

(define (interp-expr [e : Exp]) : S-Exp
  (with [(v sto) (interp e mt-env mt-store)]
        (type-case Value v
          [(numV n) (number->s-exp n)]
          [(closV arg body env) `function]
          [(boxV l) `box]
          [(recV ns vs) `record])))

(define (replace-store [sto : Store]
                       [c : Storage]) : Store
  (type-case (Listof Storage) sto
    [empty empty]
    [(cons fst rst)
     (type-case Storage c
       [(cell l v)
        (type-case Storage fst
          [(cell loc val)
           (if (= l loc)
               (cons (cell l v) rst)
               (cons fst (replace-store rst c)))])])]))
         
(module+ test
  (test/exn (interp (beginE empty) mt-env mt-store)
            "this should never happen!")
  (test (interp (parse `{record}) mt-env mt-store)
        (v*s
         (recV empty empty)
         mt-store))
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{set-box! 1 1}) mt-env mt-store)
            "not a box")
  (test (interp-expr (parse `{let {[r {record {x {lambda {r} {get r a}}}}]}
                               {get r x}}))
        `function)
  (test (interp-expr (parse `{let {[r {record {x {box 1}}}]}
                               {get r x}}))
        `box)
  (test/exn (interp (parse `{get r x})
                    (list (bind 'r (numV 1)))
                    (list (cell 1 (numV 1))))
            "not a record")
  (test/exn (interp (parse `{set! r x 1})
                    (list (bind 'r (numV 1)))
                    (list (cell 1 (numV 1))))
            "not a record")
  (test/exn (interp (parse `{get r a})
                    (list (bind 'r (recV (list 'a) (list (numV 2))))
                          (bind 'a (numV 2)))
                    (list (cell 2 (numV 3))))
            "record field should be a box")
  (test/exn (interp (parse `{set! r a 1})
                    (list (bind 'r (recV (list 'a) (list (numV 2))))
                          (bind 'a (numV 2)))
                    (list (cell 2 (numV 3))))
            "record field should be a box")
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {get r x}}))
        `1)
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {begin
                                 {set! r x 5}
                                 {get r x}}}))
        `5)
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {let {[get-r {lambda {d} r}]}
                                 {begin
                                   {set! {get-r 0} x 6}
                                   {get {get-r 0} x}}}}))
        `6)
  (test (interp-expr (parse `{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set! r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        `5)
  (test (replace-store (list)
                       (cell 1 (numV 3)))
        empty)
  (test (replace-store (list (cell 1 (numV 2)))
                       (cell 1 (numV 3)))
        (list (cell 1 (numV 3))))
  (test (replace-store (list (cell 2 (numV 3)) (cell 1 (numV 2)))
                       (cell 1 (numV 4)))
        (list (cell 2 (numV 3)) (cell 1 (numV 4))))
  (test (replace-store (list (cell 2 (numV 3)) (cell 1 (numV 2)))
                       (cell 2 (numV 4)))
        (list (cell 2 (numV 4)) (cell 1 (numV 2))))
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {set-box! b 2}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 1)
             (override-store (cell 1 (numV 1))
                             mt-store)))
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {set-box! b {+ 2 {unbox b}}}
                            {set-box! b {+ 3 {unbox b}}}
                            {set-box! b {+ 4 {unbox b}}}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 10))
                             mt-store)))
  (test (interp-expr (parse `{let {[b {box 0}]}
                               {let {[r {record
                                         {x {set-box! b 1}}
                                         {y {unbox b}}}]}
                                 {get r y}}}))
        `1)
  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{let {[b {box 0}]}
                               {let {[r {record {a {unbox b}}}]}
                                 {begin
                                   {set-box! b 1}
                                   {get r a}}}}))
        `0)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}} b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z 0}}}} r} z}))
        `0)
  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  (test/exn (interp (parse `{get 6 x}) mt-env mt-store)
            "not a record")
  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
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
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
    [empty 0]
    [(cons c rst-sto) (max (cell-location c)
                           (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
    [empty (error 'interp "unallocated location")]
    [(cons c rst-sto) (if (equal? l (cell-location c))
                          (cell-val c)
                          (fetch l rst-sto))]))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))

;; find & update ----------------------------------------

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : Symbol] [ns : (Listof Symbol)] [vs : (Listof Value)])
  : Value
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (first vs)
              (find n (rest ns) (rest vs)))]))

;; Takes a name n, value v, and two parallel lists, returning a list
;; like the second of the given lists, but with v in place
;; where n matches the item from the first list.
(define (update [n : Symbol]
                [v : Value]
                [ns : (Listof Symbol)]
                [vs : (Listof Value)]) : (Listof Value)
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (cons v (rest vs))
              (cons (first vs) 
                    (update n v (rest ns) (rest vs))))]))

(module+ test
  (test (find 'a (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 1))
  (test (find 'b (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 2))
  (test/exn (find 'a empty empty)
            "no such field")

  (test (update 'a (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 0) (numV 2)))
  (test (update 'b (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 1) (numV 0)))
  (test/exn (update 'a (numV 0) empty empty)
            "no such field"))
