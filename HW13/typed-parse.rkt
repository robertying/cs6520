#lang plait
(require "class.rkt"
         "inherit.rkt"
         "typed-class.rkt"
         "inherit-parse.rkt")

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (parse-t-class [s : S-Exp]) : (Symbol * ClassT)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (values
      (s-exp->symbol (second (s-exp->list s)))
      (classT (s-exp->symbol (fourth (s-exp->list s)))
              (map parse-t-field
                   (s-exp->list (fourth (rest (s-exp->list s)))))
              (map parse-t-method 
                   (rest (rest (rest (rest (rest (s-exp->list s)))))))))]
    [else (error 'parse-t-class "invalid input")]))

(define (parse-t-field [s : S-Exp]) : (Symbol * Type)
  (cond
    [(s-exp-match? `[SYMBOL : ANY] s)
     (values (s-exp->symbol (first (s-exp->list s)))
             (parse-type (third (s-exp->list s))))]
    [else (error 'parse-t-field "invalid input")]))

(define (parse-t-method [s : S-Exp]) : (Symbol * MethodT)
  (cond
    [(s-exp-match? `[SYMBOL {[arg : ANY]} : ANY ANY] s)
     (values
      (s-exp->symbol (first (s-exp->list s)))
      (methodT (parse-type (local [(define args (second (s-exp->list s)))
                                   (define arg (first (s-exp->list args)))]
                             (third (s-exp->list arg))))
               (parse-type (fourth (s-exp->list s)))
               (parse (fourth (rest (s-exp->list s))))))]
    [else (error 'parse-t-method "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
    [(s-exp-match? `num s)
     (numT)]
    [(s-exp-match? `SYMBOL s)
     (objT (s-exp->symbol s))]
    [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse-type `num)
        (numT))
  (test (parse-type `Object)
        (objT 'Object))
  (test/exn (parse-type `{})
            "invalid input")
  
  (test (parse-t-field `[x : num])
        (values 'x (numT)))
  (test/exn (parse-t-field `{x 1})
            "invalid input")

  (test (parse-t-method `[m {[arg : num]} : Object this])
        (values 'm (methodT (numT) (objT 'Object) (thisI))))
  (test/exn (parse-t-method `{m 1})
            "invalid input")
  
  (test (parse-t-class `{class Posn3D extends Posn
                          {[x : num] [y : num]}
                          [m1 {[arg : num]} : num arg]
                          [m2 ([arg : num]) : Object this]})
        (values 'Posn3D
                (classT 'Posn
                        (list (values 'x (numT))
                              (values 'y (numT)))
                        (list (values 'm1 (methodT (numT) (numT) (argI)))
                              (values 'm2 (methodT (numT) (objT 'Object) (thisI)))))))
  (test/exn (parse-t-class `{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-t-prog [classes : (Listof S-Exp)] [a : S-Exp]) : S-Exp
  (let ([v (interp-t (parse a)
                     (map parse-t-class classes))])
    (type-case Value v
      [(numV n) (number->s-exp n)]
      [(objV class-name field-vals) `object]
      [(nullV) `null])))

(module+ test
  (test (interp-t-prog
         (list
          `{class Empty extends Object
             {}})
         `null)
        `null)
  
  (test (interp-t-prog
         (list
          `{class Empty extends Object
             {}})
         `{new Empty})
        `object)

  (test (interp-t-prog 
         (list
          `{class Posn extends Object
             {[x : num]
              [y : num]}
             [mdist {[arg : num]} : num
                    {+ {get this x} {get this y}}]
             [addDist {[arg : posn]} : num
                      {+ {send arg mdist 0}
                         {send this mdist 0}}]}
         
          `{class Posn3D extends Posn
             {[z : num]}
             [mdist {[arg : num]} : num
                    {+ {get this z} 
                       {super mdist arg}}]})
        
         `{send {new Posn3D 5 3 1} addDist {new Posn 2 7}})
        `18))
