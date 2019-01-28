#lang plait

; Part 1
(define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))

(define (sum [tree : Tree]) : Number
  (type-case Tree tree
    [(leaf val) val]
    [(node val left right) (+ val (+ (sum left) (sum right)))]
    )
  )

(test (sum (node 5 (leaf 6) (leaf 7)))
      18)

(test (sum (leaf 2))
      2)

(test (sum (node 5 (node 3 (leaf 2) (leaf 1)) (leaf 4)))
      15)


; Part 2
(define (negate [tree : Tree]) : Tree
  (type-case Tree tree
    [(leaf val) (leaf (- 0 val))]
    [(node val left right) (node (- 0 val) (negate left) (negate right))]
    )
  )

(test (negate (node 5 (leaf 6) (leaf 7)))
      (node -5 (leaf -6) (leaf -7)))

(test (negate (node 5 (node 3 (leaf 2) (leaf 1)) (leaf 4)))
      (node -5 (node -3 (leaf -2) (leaf -1)) (leaf -4)))

(test (negate (leaf 2))
      (leaf -2))


; Part 3
(define (contains? [tree : Tree] [num : Number]) : Boolean
    (type-case Tree tree
    [(leaf val) (= val num)]
    [(node val left right) (or
                            (or (contains? (leaf val) num) (contains? left num))
                            (contains? right num))]
    )
  )

(test (contains? (node 5 (leaf 6) (leaf 7)) 5)
      #t)

(test (contains? (node 5 (node 3 (leaf 2) (leaf 1)) (leaf 4)) 0)
      #f)

(test (contains? (leaf 2) 2)
      #t)


; Part 4
(define (bigger-leaves? [tree : Tree] [sum : Number]) : Boolean
  (type-case Tree tree
    [(leaf val) (> val sum)]
    [(node val left right) (and
                            (bigger-leaves? left (+ val sum))
                            (bigger-leaves? right (+ val sum)))]
    )
  )

(define (big-leaves? [tree : Tree]) : Boolean
  (type-case Tree tree
    [(leaf val) (bigger-leaves? (leaf val) 0) ]
    [(node val left right) (and
                            (bigger-leaves? left val)
                            (bigger-leaves? right val))]
    )
  )

(test (big-leaves? (node 5 (leaf 6) (leaf 7)))
      #t)

(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)))
      #f)

(test (big-leaves? (leaf 4))
      #t)


(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 9)) (node 2 (leaf 8) (leaf 6))))
      #f)

(test (big-leaves? (node 5
                         (node 2 (node 2 (leaf 10) (leaf 11)) (leaf 8))
                         (node 2 (leaf 8) (leaf 8))))
      #t)

(test (big-leaves? (node 5
                         (node 2 (node 2 (leaf 10) (leaf 11)) (leaf 7))
                         (node 2 (leaf 8) (leaf 8))))
      #f)

(test (big-leaves? (leaf -1))
      #f)

(test (big-leaves? (node -1 (leaf 1) (leaf 2)))
      #t)

(test (big-leaves? (leaf 0))
      #f)


; Part 5
; #t for left, #f for right
(define (sorted?-raw [tree : Tree] [is-left : Boolean]) : (Boolean * Number)
  (type-case Tree tree
    [(leaf val) (pair #t val)]
    [(node val left right)
     (cond [is-left (pair
                     (and (<= (snd (sorted?-raw left #t)) val)
                          (>= (snd (sorted?-raw right #f)) val))
                     (snd (sorted?-raw right #f)))]
           [else (pair
                  (and (<= (snd (sorted?-raw left #t)) val)
                       (>= (snd (sorted?-raw right #f)) val))
                  (snd (sorted?-raw left #t)))]
           )
     ]
    )
  )

(define (sorted? [tree : Tree]) : Boolean
  (type-case Tree tree
    [(leaf val) #t]
    [(node val left right) (and (and (and (fst (sorted?-raw left #t))
                                          (fst (sorted?-raw right #f)))
                                     (<= (snd (sorted?-raw left #t)) val))
                                (>= (snd (sorted?-raw right #f)) val))]
    )
  )

(test (sorted? (leaf 1))
      #t)

(test (sorted? (node 1 (leaf 0) (leaf 2)))
      #t)

(test (sorted? (node 1 (leaf 1) (leaf 2)))
      #t)

(test (sorted? (node 1 (leaf 2) (leaf 2)))
      #f)

(test (sorted? (node 6 (node 2 (leaf 1) (leaf 6)) (leaf 7)))
      #t)

(test (sorted? (node 6
                     (node 4 (node 2 (leaf 1) (leaf 3)) (leaf 5))
                     (node 8 (leaf 7) (leaf 9))))
      #t)

(test (sorted? (node 6
                     (node 4 (node 2 (leaf 1) (leaf 3)) (leaf 3))
                     (node 8 (leaf 7) (leaf 9))))
      #f)

(test (sorted? (node 6
                     (node 4 (node 2 (leaf 1) (leaf 3)) (leaf 7))
                     (node 8 (leaf 7) (leaf 9))))
      #f)

(test (sorted? (node 6
                     (node 4 (node 2 (leaf 1) (leaf 3)) (leaf 5))
                     (node 8 (leaf 4) (leaf 9))))
      #f)
