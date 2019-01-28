#lang plait

(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  (classT [super-name : Symbol]
          [fields : (Listof (Symbol * Type))]
          [methods : (Listof (Symbol * MethodT))]))

(define-type MethodT
  (methodT [arg-type : Type]
           [result-type : Type]
           [body-expr : ExpI]))

(define-type Type
  (numT)
  (objT [class-name : Symbol])
  (nullT))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'Object)
      empty        
      (type-case ClassT (find t-classes class-name)
        [(classT super-name fields methods)
         (append 
          (get-all-field-types super-name t-classes)
          (map snd fields))])))

;; ----------------------------------------

(define (make-find-in-tree class-items)
  (lambda (name class-name t-classes)
    (local [(define t-class (find t-classes class-name))
            (define items (class-items t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'Object)
          (find items name)
          (try (find items name)
               (lambda ()
                 ((make-find-in-tree class-items)
                  name 
                  super-name
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree classT-fields))

(define find-method-in-tree
  (make-find-in-tree classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) #t]
    [(equal? name1 'Object) #f]
    [else
     (type-case ClassT (find t-classes name1)
       [(classT super-name fields methods)
        (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2 
       [(objT name2)
        (is-subclass? name1 name2 t-classes)]
       [else #f])]
    [(nullT)
     (type-case Type t2 
       [(numT) #f]
       [else #t])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (values 'A (classT 'Object empty empty)))
  (define b-t-class (values 'B (classT 'A empty empty)))
  (define c-t-class (values 'C (classT 'A empty empty)))
  (define d-t-class (values 'D (classT 'B empty empty)))
  (define e-t-class (values 'E (classT 'Object empty empty)))
  (define f-t-class (values 'F (classT 'E empty empty)))

  (test (is-subclass? 'Object 'Object empty)
        #t)
  (test (is-subclass? 'A 'B (list a-t-class b-t-class))
        #f)
  (test (is-subclass? 'B 'A (list a-t-class b-t-class))
        #t)

  (test (is-subtype? (numT) (nullT) empty)
        #f)
  (test (is-subtype? (nullT) (numT) empty)
        #f)
  (test (is-subtype? (nullT) (objT 'Object) empty)
        #t)
  (test (is-subtype? (nullT) (nullT) empty)
        #t)
  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (objT 'Object) empty)
        #f)
  (test (is-subtype? (objT 'Object) (numT) empty)
        #f)
  (test (is-subtype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #t))

;; ----------------------------------------

(define (least-upper-bound [e : ExpI] [a-type : Type] [b-type : Type] [t-classes : (Listof (Symbol * ClassT))]) : Type
  (type-case Type a-type
    [(numT)
     (type-case Type b-type
       [(numT) (numT)]
       [else (type-error e "doesn't match")])]
    [(objT a-class-name)
     (type-case Type b-type
       [(objT b-class-name)
        (if (is-subclass? a-class-name b-class-name t-classes)
            b-type
            (least-upper-bound e a-type (objT (classT-super-name (find t-classes b-class-name))) t-classes))]
       [(nullT) (nullT)]
       [else (type-error e "doesn't match")])]
    [(nullT)
     (type-case Type b-type
       [(nullT) (nullT)]
       [(objT class-name) (nullT)]
       [else (type-error e "doesn't match")])]))

(module+ test
  (define test-t-classes (list a-t-class b-t-class c-t-class d-t-class e-t-class f-t-class))

  ;;Q3
  (test (least-upper-bound
         (numI 0)
         (numT)
         (numT)
         test-t-classes)
        (numT))

  (test/exn (least-upper-bound
             (numI 0)
             (numT)
             (objT 'B)
             test-t-classes)
            "no type")

  (test/exn (least-upper-bound
             (numI 0)
             (objT 'B)
             (numT)
             test-t-classes)
            "no type")
  
  (test (least-upper-bound
         (numI 0)
         (objT 'B)
         (objT 'B)
         test-t-classes)
        (objT 'B))

  (test (least-upper-bound
         (numI 0)
         (objT 'A)
         (objT 'B)
         test-t-classes)
        (objT 'A))

  (test (least-upper-bound
         (numI 0)
         (objT 'C)
         (objT 'F)
         test-t-classes)
        (objT 'Object))

  (test (least-upper-bound
         (numI 0)
         (objT 'C)
         (objT 'D)
         test-t-classes)
        (objT 'A))

  ;;Q3

  ;;Q4

  (test (least-upper-bound
         (numI 0)
         (nullT)
         (nullT)
         test-t-classes)
        (nullT))

  (test/exn (least-upper-bound
             (numI 0)
             (nullT)
             (numT)
             test-t-classes)
            "no type")

  (test (least-upper-bound
         (numI 0)
         (objT 'C)
         (nullT)
         test-t-classes)
        (nullT))

  (test (least-upper-bound
         (numI 0)
         (nullT)
         (objT 'C)
         test-t-classes)
        (nullT))
  
  ;;Q4
  )

(define (check-set-field [fields : (Listof (Symbol * Type))] [name : Symbol] [new-expr : ExpI] [new-val-type : Type] [t-classes : (Listof (Symbol * ClassT))])
  (type-case (Listof (Symbol * Type)) fields
    [empty (type-error new-expr "field")]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (if (is-subtype? new-val-type (snd p) t-classes)
             (values)
             (type-error new-expr "type"))
         (check-set-field rst-l name new-expr new-val-type t-classes))]))

(define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type -> Type)
  (lambda (expr t-classes this-type arg-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes this-type arg-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [(numT)
                 (type-case Type (recur r)
                   [(numT) (numT)]
                   [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExpI expr
        [(numI n) (numT)]
        [(plusI l r) (typecheck-nums l r)]
        [(multI l r) (typecheck-nums l r)]
        [(argI) arg-type]
        [(thisI) this-type]
        [(newI class-name exprs)
         (local [(define arg-types (map recur exprs))
                 (define field-types
                   (get-all-field-types class-name t-classes))]
           (if (and (= (length arg-types) (length field-types))
                    (foldl (lambda (b r) (and r b))
                           #t
                           (map2 (lambda (t1 t2) 
                                   (is-subtype? t1 t2 t-classes))
                                 arg-types
                                 field-types)))
               (objT class-name)
               (type-error expr "field type mismatch")))]
        [(getI obj-expr field-name)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            (find-field-in-tree field-name
                                class-name
                                t-classes)]
           [else (type-error obj-expr "object")])]
        [(sendI obj-expr method-name arg-expr)
         (local [(define obj-type (recur obj-expr))
                 (define arg-type (recur arg-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (typecheck-send class-name method-name
                              arg-expr arg-type
                              t-classes)]
             [else
              (type-error obj-expr "object")]))]
        [(superI method-name arg-expr)
         (local [(define arg-type (recur arg-expr))
                 (define this-class
                   (find t-classes (objT-class-name this-type)))]
           (typecheck-send (classT-super-name this-class)
                           method-name
                           arg-expr arg-type
                           t-classes))]
        [(castI class-name obj-expr)
         (local [(define obj-type (recur obj-expr))]
           (type-case Type obj-type
             [(objT obj-class-name)
              (if (or (is-subtype? obj-type (objT class-name) t-classes) (is-subtype? (objT class-name) obj-type t-classes))
                  (objT class-name)
                  (type-error obj-expr "object"))]
             [else
              (type-error obj-expr "object")]))]
        [(if0I cond thn els)
         (local [(define cond-type (recur cond))
                 (define thn-type (recur thn))
                 (define els-type (recur els))]
           (type-case Type cond-type
             [(numT)
              (least-upper-bound thn thn-type els-type t-classes)]
             [else
              (type-error cond "num")]))]
        [(nullI) (nullT)]
        [(setI obj-expr field-name field-value-expr)
         (local [(define obj-type (recur obj-expr))
                 (define field-value-type (recur field-value-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (begin
                (check-set-field
                 (classT-fields (find t-classes class-name))
                 field-name
                 field-value-expr
                 field-value-type
                 t-classes)
                (objT class-name))]
             [else (type-error obj-expr "object")]))]))))

(define (typecheck-send [class-name : Symbol]
                        [method-name : Symbol]
                        [arg-expr : ExpI]
                        [arg-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))])
  (type-case MethodT (find-method-in-tree
                      method-name
                      class-name
                      t-classes)
    [(methodT arg-type-m result-type body-expr)
     (if (is-subtype? arg-type arg-type-m t-classes)
         result-type
         (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))]) : ()
  (type-case MethodT method
    [(methodT arg-type result-type body-expr)
     (if (is-subtype? (typecheck-expr body-expr t-classes
                                      this-type arg-type)
                      result-type
                      t-classes)
         (values)
         (type-error body-expr (to-string result-type)))]))

(define (check-override [method-name : Symbol]
                        [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (Listof (Symbol * ClassT))])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree method-name
                                  super-name
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string method-name))))))

(define (typecheck-class [class-name : Symbol]
                         [t-class : ClassT]
                         [t-classes : (Listof (Symbol * ClassT))])
  (type-case ClassT t-class
    [(classT super-name fields methods)
     (map (lambda (m)
            (begin
              (typecheck-method (snd m) (objT class-name) t-classes)
              (check-override (fst m) (snd m) t-class t-classes)))
          methods)]))

(define (typecheck [a : ExpI]
                   [t-classes : (Listof (Symbol * ClassT))]) : Type
  (begin
    (map (lambda (tc)
           (typecheck-class (fst tc) (snd tc) t-classes))
         t-classes)
    (typecheck-expr a t-classes (objT 'Object) (numT))))

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (values 'Posn
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))
                          (values 'return0
                                  (methodT (objT 'Posn) (numT)
                                           (numI 0)))))))
  (define posn-with-parent-t-class
    (values 'Posn-with-parent
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)) (values 'p (objT 'Posn)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'get-parent
                                  (methodT (numT) (objT 'Posn) 
                                           (getI (thisI) 'p)))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))
                          (values 'return0
                                  (methodT (objT 'Posn) (numT)
                                           (numI 0)))))))

  (define posn3D-t-class 
    (values 'Posn3D
            (classT 'Posn
                    (list (values 'z (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT)
                                           (plusI (getI (thisI) 'z) 
                                                  (superI 'mdist (argI)))))))))

  (define square-t-class 
    (values 'Square
            (classT 'Object
                    (list (values 'topleft (objT 'Posn)))
                    (list))))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class posn-with-parent-t-class)))
  
  (define new-posn27 (newI 'Posn (list (numI 2) (numI 7))))
  (define new-posn531 (newI 'Posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (typecheck-posn (sendI new-posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI new-posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI new-posn531 'addDist new-posn27))
        (numT))  
  (test (typecheck-posn (sendI new-posn27 'addDist new-posn531))
        (numT))

  (test (typecheck-posn (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))))
        (objT 'Square))
  (test (typecheck-posn (newI 'Square (list (newI 'Posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'Square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  ;;Q2
  (test (typecheck-posn (castI 'Posn (newI 'Posn3D (list (numI 2) (numI 7) (numI 7)))))
        (objT 'Posn))

  (test (typecheck-posn (castI 'Posn3D (newI 'Posn (list (numI 2) (numI 7)))))
        (objT 'Posn3D))

  (test/exn (typecheck-posn (castI 'Posn3D (plusI (numI 1) (numI 1))))
            "no type")

  (test/exn (typecheck (castI 'Other (newI 'Posn3D (list (numI 2) (numI 7) (numI 7))))
                       (list posn-t-class posn3D-t-class square-t-class
                             (values 'Other
                                     (classT 'Object
                                             (list (values 'x (numT)))
                                             (list)))))
            "no type")
  ;;Q2

  ;;Q3
  (define (typecheck-abcd a)
    (typecheck a
               test-t-classes))
  
  (test (typecheck-abcd (if0I (numI 0) (numI 1) (numI 2)))
        (numT))
  
  (test/exn (typecheck-abcd (if0I (newI 'Object empty) (numI 1) (numI 2)))
            "no type")

  (test/exn (typecheck-abcd (if0I (numI 0) (numI 1) (newI 'Object empty)))
            "no type")

  (test (typecheck-abcd (if0I (numI 0) (newI 'B empty) (newI 'A empty)))
        (objT 'A))
  
  (test (typecheck-abcd (if0I (numI 0) (newI 'B empty) (newI 'C empty)))
        (objT 'A))

  (test (typecheck-abcd (if0I (numI 0) (newI 'A empty) (newI 'D empty)))
        (objT 'A))

  (test (typecheck-abcd (if0I (numI 0) (newI 'F empty) (newI 'A empty)))
        (objT 'Object))

  (test (typecheck-abcd (if0I (numI 0) (newI 'D empty) (newI 'B empty)))
        (objT 'B))

  ;;Q3

  ;;Q4

  (test/exn (typecheck (multI (nullI) (numI 2))
                       empty)
            "no type")

  (test/exn (typecheck (sendI (nullI) 'm (numI 0))
                       empty)
            "no type")

  (test/exn (typecheck (getI (nullI) 'm)
                       empty)
            "no type")

  ;; pass null as object, but will be used --> only will be detected by `interp` so typecheck passes
  (test (typecheck-posn (sendI new-posn27 'addDist (nullI)))
        (numT))

  ;; pass null as object, but won't be used
  (test (typecheck-posn (sendI new-posn27 'return0 (nullI)))
        (numT))

  (test (typecheck-posn (sendI (newI 'Posn-with-parent (list (numI 1) (numI 2) new-posn27)) 'get-parent (numI 0)))
        (objT 'Posn))

  ;; null is used as non-number field value
  (test (typecheck-posn (sendI (newI 'Posn-with-parent (list (numI 1) (numI 2) (nullI))) 'get-parent (numI 0)))
        (objT 'Posn))
  
  ;;Q4

  ;;Q5

  (test (typecheck-posn (setI new-posn27 'x (numI 0)))
        (objT 'Posn))

  (test/exn (typecheck-posn (setI (numI 1) 'x (numI 0)))
            "no type")

  (test/exn (typecheck-posn (setI (newI 'Posn-with-parent (list (numI 1) (numI 2) (nullI))) 'p (newI 'Posn-with-parent (list (numI 1) (numI 2) (nullI)))))
            "no type")

  (test (typecheck-posn (setI (newI 'Posn-with-parent (list (numI 1) (numI 2) (nullI))) 'p new-posn531))
        (objT 'Posn-with-parent))

  (test/exn (typecheck-posn (setI new-posn27 'p (newI 'Object empty)))
            "no type")

  (test/exn (typecheck-posn (setI new-posn27 'z (numI 0)))
            "no type")

  ;;Q5

  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI new-posn27 'mdist new-posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'Object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class
                             (values 'Other
                                     (classT 'Posn
                                             (list)
                                             (list (values 'mdist
                                                           (methodT (objT 'Object) (numT)
                                                                    (numI 10))))))))
            "bad override")
  (test/exn (typecheck-method (methodT (numT) (objT 'Object) (numI 0)) (objT 'Object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (values 'Cube
                                     (classT 'Square
                                             empty
                                             (list
                                              (values 'm
                                                      (methodT (numT) (numT)
                                                               ;; No such method in superclass:
                                                               (superI 'm (numI 0)))))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (classI
        super-name
        (map fst fields)
        (map (lambda (m)
               (values (fst m)
                       (type-case MethodT (snd m)
                         [(methodT arg-type result-type body-expr)
                          body-expr])))
             methods))])))
  
(define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map (lambda (c)
                     (values (fst c) (strip-types (snd c))))
                   t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI new-posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI new-posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI new-posn531 'addDist new-posn27))
        (numV 18))
  (test (interp-t-posn (sendI new-posn27 'addDist new-posn531))
        (numV 18)))
