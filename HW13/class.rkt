#lang plait

(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (argE)
  (thisE)
  (newE [class-name : Symbol]
        [args : (Listof Exp)])
  (getE [obj-expr : Exp]
        [field-name : Symbol])
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  (castE [class-name : Symbol]
         [obj-expr : Exp])
  (if0E [cond : Exp]
        [thn : Exp]
        [els : Exp])
  (nullE)
  (setE [obj-expr : Exp]
        [field-name : Symbol]
        [field-value : Exp]))

(define-type Class
  (classC
   [super-name : Symbol]
   [field-names : (Listof Symbol)]
   [methods : (Listof (Symbol * Exp))]))

(define-type Value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof (Boxof Value))])
  (nullV))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
  (type-case (Listof (Symbol * 'a)) l
    [empty
     (error 'find (string-append "not found: " (symbol->string name)))]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (snd p)
         (find rst-l name))]))

(module+ test
  (test (find (list (values 'a 1)) 'a)
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

(define (set-field [field-vals : (Listof (Boxof Value))] [field-names : (Listof Symbol)] [name : Symbol] [new-val : Value])
  (type-case (Listof Symbol) field-names
    [empty (error 'set-field "field does not exist")]
    [(cons p rst-l)
     (if (symbol=? p name)
         (set-box! (first field-vals) new-val)
         (set-field (rest field-vals) rst-l name new-val))]))

(module+ test
  (test (let ([b (list (box (numV 1)))])
          (begin
            (set-field b (list 'x) 'x (numV 2))
            (unbox (first b))))
        (numV 2))
  
  (test (let ([b (list (box (numV 1)) (box (numV 2)))])
          (begin
            (set-field b (list 'x 'y) 'y (numV 0))
            (unbox (second b))))
        (numV 0))
  
  (test/exn (let ([b (list (box (numV 1)))])
              (set-field b (list 'x) 'y (numV 2)))
            "field does not exist"))


;; ----------------------------------------

(define (subclass? obj-class-name class-name classes)
  (cond
    [(equal? obj-class-name class-name) #t]
    [(equal? obj-class-name 'Object) #f]
    [else
     (type-case Class (find classes obj-class-name)
       [(classC super-name field-names methods)
        (subclass? super-name class-name classes)])]))

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE) this-val]
        [(argE) arg-val]
        [(newE class-name field-exprs)
         (local [(define c (find classes class-name))
                 (define vals (map box (map recur field-exprs)))]
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name vals)
               (error 'interp "wrong field count")))]
        [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super-name field-names methods)
               (find (map2 (lambda (n v) (values n v))
                           field-names
                           (map unbox field-vals))
                     field-name)])]
           [else (error 'interp "not an object")])]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]
        [(castE class-name obj-expr)
         (local [(define val (recur obj-expr))]
           (type-case Value val
             [(objV obj-class-name field-vals)
              (if (subclass? obj-class-name class-name classes)
                  val
                  (error 'interp "cast: not an instance of certain class"))]
             [else (error 'interp "not an object")]))]
        [(if0E cond thn els)
         (local [(define cond-val (recur cond))]
           (type-case Value cond-val
             [(numV n)
              (if (= n 0)
                  (recur thn)
                  (recur els))]
             [else (error 'interp "not a number")]))]
        [(nullE) (nullV)]
        [(setE obj-expr field-name field-value-expr)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super-name field-names methods)
               (begin
                 (set-field field-vals field-names field-name (recur field-value-expr))
                 (objV class-name field-vals))])]
           [else (error 'interp "not an object")])]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC super-name field-names methods)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (values 'Posn
            (classC
             'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2))))
                   (values 'test-imperative
                           (plusE (getE (setE (thisE) 'x (numE 100)) 'x) (getE (setE (thisE) 'y (numE 101)) 'x)))))))
    
  (define posn3D-class
    (values 'Posn3D
            (classC
             'Posn
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  ;;Q2
  (test (interp-posn (castE 'Posn (newE 'Posn (list (numE 2) (numE 7)))))
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (castE 'Object (newE 'Posn (list (numE 2) (numE 7)))))
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (castE 'Posn (newE 'Posn3D (list (numE 2) (numE 7) (numE 7)))))
        (objV 'Posn3D (list (box (numV 2)) (box (numV 7)) (box (numV 7)))))

  (test/exn (interp-posn (castE 'Posn (multE (numE 10) (numE 7))))
            "not an object")

  (test/exn (interp-posn (castE 'Posn3D (newE 'Posn (list (numE 2) (numE 7)))))
            "cast: not an instance")
  ;;Q2

  ;;Q3

  (test (interp (if0E (numE 0) (numE 1) (numE 2)) empty (numV -1) (numV -1))
        (numV 1))

  (test (interp (if0E (numE 1) (numE 1) (numE 2)) empty (numV -1) (numV -1))
        (numV 2))

  (test/exn (interp-posn (if0E (castE 'Object (newE 'Posn (list (numE 2) (numE 7)))) (numE 1) (numE 2)))
            "not a number")

  ;;Q3

  ;;Q4

  (test/exn (interp-posn (sendE posn27 'addDist (nullE)))
            "not an object")
  
  (test (interp (nullE) empty (numV -1) (numV -1))
        (nullV))

  (test/exn (interp-posn (sendE (nullE) 'mdist (numE 0)))
            "not an object")

  (test/exn (interp-posn (getE (nullE) 'x))
            "not an object")

  ;;Q4

  ;;Q5

  (test (interp-posn (setE (newE 'Posn3D (list (numE 2) (numE 7) (numE 7))) 'x (numE 0)))
        (objV 'Posn3D (list (box (numV 0)) (box (numV 7)) (box (numV 7)))))

  (test (interp-posn (setE (newE 'Posn3D (list (numE 2) (numE 7) (numE 7))) 'z (numE 0)))
        (objV 'Posn3D (list (box (numV 2)) (box (numV 7)) (box (numV 0)))))

  (test/exn (interp-posn (setE (numE 1) 'z (numE 0)))
            "not an object")

  ;; cannot pass typecheck but okay with interp
  (test (interp-posn (setE (newE 'Posn3D (list (numE 2) (numE 7) (numE 7))) 'z (newE 'Posn3D (list (numE 2) (numE 7) (numE 7)))))
        (objV 'Posn3D (list (box (numV 2)) (box (numV 7)) (box (objV 'Posn3D (list (box (numV 2)) (box (numV 7)) (box (numV 7))))))))

  ;;fail with functional update but pass with imperative update
  ;;(plusE (getE (setE (thisE) 'x (numE 100)) 'x) (getE (setE (thisE) 'y (numE 101)) 'x))
  (test (interp-posn (sendE posn27 'test-imperative (numE 0)))
        (numV 200))

  ;;Q5

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))

  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count"))
