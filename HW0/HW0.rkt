#lang plait

; Part 1
(define (3rd-power [num : Number]) : Number
  (* (* num num) num)
  )

(test (3rd-power 17)
      4913)

(test (3rd-power 0)
      0)

(test (3rd-power -2)
      -8)


; Part 2
(define (42nd-power [num : Number]) : Number
  (* (* (* (3rd-power (3rd-power (3rd-power num))) ; 27
  (3rd-power (3rd-power num))) ; 9
  (3rd-power num)) ; 3
  (3rd-power num)) ; 3
  )
 
(test (42nd-power 17)
      4773695331839566234818968439734627784374274207965089)

(test (42nd-power 0)
      0)

(test (42nd-power -2)
      4398046511104)


; Part 3
(define (plural [str : String]) : String
  (local [(define last-index (sub1 (string-length str)))]
    (cond [(eq? (string-ref str last-index) #\y)
           (string-append (substring str 0 last-index) "ies")]
          [else (string-append str "s")]
          )
    )
  )

(test (plural "baby")
      "babies")

(test (plural "fish")
      "fishs")


; Part 4
(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

(define (energy-usage [light : Light]) : Number
  (type-case Light light
    [(bulb watts technology) (/ (* watts 24) 1000)]
    [(candle inches) 0.0]
    )
  )

(test (energy-usage (bulb 100.0 'halogen))
      2.4)

(test (energy-usage (candle 10.0))
      0.0)


; Part 5
(define (use-for-one-hour [light : Light]) : Light
  (type-case Light light
    [(bulb watts technology) light]
    [(candle inches) (cond [(< inches 1.0) (candle 0.0)]
                           [else (candle (sub1 inches))]
                           )
                     ]
    )
  )

(test (use-for-one-hour (bulb 100.0 'halogen))
     (bulb 100.0 'halogen)
     )

(test (use-for-one-hour (candle 10.0))
      (candle 9.0)
      )

(test (use-for-one-hour (candle 1.0))
      (candle 0.0)
      )

(test (use-for-one-hour (candle 0.9))
      (candle 0.0)
      )

(test (use-for-one-hour (candle 0.0))
      (candle 0.0)
      )
