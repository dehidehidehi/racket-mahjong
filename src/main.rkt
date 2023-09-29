(module mahjong racket
  (provide (all-defined-out))


  (module+ test 
    (require rackunit)
    (define (mk-t val)
      (mj-tile val)))


  (struct mj-tile [value]
		 #:transparent)
  (module+ test
    (check-false (mj-tile=? (mj-tile "P1") (mj-tile "P2")))
    (check-true (mj-tile=? (mj-tile "P1") (mj-tile "P1"))))


  (define (mj-tile-suit t)
    (substring (mj-tile-value t) 0 1))
  (module+ test
    (check-equal? (mj-tile-suit (mk-t "P1")) "P")
    (check-equal? (mj-tile-suit (mk-t "DG")) "D"))


  (define (mj-tile-rank t)
    (substring (mj-tile-value t) 1 2))


  (define (mj-tile->string t)
    (mj-tile-value t))
  (module+ test 
    (check-equal? "P1" (mj-tile->string (mk-t "P1"))))


  (define (mj-tile-honour? t)
    (set-member? (set "W" "D") (mj-tile-suit t)))
  (module+ test
    (check-false (mj-tile-honour? (mk-t "P1")))
    (check-false (mj-tile-honour? (mk-t "S1")))
    (check-false (mj-tile-honour? (mk-t "M1")))
    (check-true (mj-tile-honour? (mk-t "WW")))
    (check-true (mj-tile-honour? (mk-t "WS")))
    (check-true (mj-tile-honour? (mk-t "WN")))
    (check-true (mj-tile-honour? (mk-t "WE")))
    (check-true (mj-tile-honour? (mk-t "DR")))
    (check-true (mj-tile-honour? (mk-t "DW")))
    (check-true (mj-tile-honour? (mk-t "DG"))))


  (define (mj-tile-terminal? t)
    (set-member? (set "1" "9") (mj-tile-rank t)))
  (module+ test
    (check-false (mj-tile-terminal? (mk-t "P2")))
    (check-false (mj-tile-terminal? (mk-t "S8")))
    (check-false (mj-tile-terminal? (mk-t "GD")))
    (check-false (mj-tile-terminal? (mk-t "NW")))
    (check-true (mj-tile-terminal? (mk-t "M1")))
    (check-true (mj-tile-terminal? (mk-t "M9")))
    (check-true (mj-tile-terminal? (mk-t "S1")))
    (check-true (mj-tile-terminal? (mk-t "S9")))
    (check-true (mj-tile-terminal? (mk-t "P1")))
    (check-true (mj-tile-terminal? (mk-t "P9"))))


  (define (mj-tile-suit=? t1 t2)
    (and (mj-tile? t1)
	 (mj-tile? t2)
	 (string=? (mj-tile-suit t1) (mj-tile-suit t2))
	 (not (or (mj-tile-honour? t1) (mj-tile-honour? t2)))))
  (module+ test
    (check-false (mj-tile-suit=? (mk-t "DG") (mk-t "DR"))) ; not suited tile
    (check-false (mj-tile-suit=? (mk-t "DG") (mk-t "S1")))
    (check-false (mj-tile-suit=? (mk-t "DG") (mk-t "WS")))
    (check-false (mj-tile-suit=? (mk-t "M1") (mk-t "P2")))
    (check-false (mj-tile-suit=? (mk-t "P1") (mk-t "M1")))
    (check-false (mj-tile-suit=? (mk-t "S1") (mk-t "M1")))
    (check-false (mj-tile-suit=? (mk-t "WN") (mk-t "DG")))
    (check-false (mj-tile-suit=? (mk-t "WN") (mk-t "S1")))
    (check-false (mj-tile-suit=? (mk-t "WN") (mk-t "WS")))
    (check-true (mj-tile-suit=? (mk-t "M1") (mk-t "M3")))
    (check-true (mj-tile-suit=? (mk-t "P1") (mk-t "P1")))
    (check-true (mj-tile-suit=? (mk-t "P1") (mk-t "P2"))))


  (struct mj-hand [concealed melds])


  (define (mj-hand-tiles hand)
    (append (mj-hand-concealed hand) (mj-hand-melds hand)))


  (define (mj-hand->string h)
    (define htiles (mj-hand-tiles h))
    (string-join (map mj-tile->string htiles) ","))
  (module+ test
    (define (mk-h tiles)
      (mj-hand tiles empty))
    (check-equal? "P1,P2" (mj-hand->string (mk-h (list (mk-t "P1") (mk-t "P2"))))))


  (define (mj-tile=? t1 t2)
    (and (mj-tile? t1)
	 (mj-tile? t2)
	 (string=? (mj-tile-value t1) (mj-tile-value t2))))


  (define (mj-tile<? t1 t2)
    (and (mj-tile? t1)
	 (mj-tile? t2)
	 (and 
	   (number? (string->number (substring (mj-tile-value t1) 1)))
	   (number? (string->number (substring (mj-tile-value t2) 1))))
	 (string<? (mj-tile-value t1) (mj-tile-value t2))))
  (module+ test
    (check-false (mj-tile<? (mj-tile "P2") (mj-tile "P1")))
    (check-false (mj-tile<? (mj-tile "GD") (mj-tile "P1")))
    (check-false (mj-tile<? (mj-tile "P1") (mj-tile "GD")))
    (check-true (mj-tile<? (mj-tile "P1") (mj-tile "P2"))))


  (define (mj-sequence? tiles)
    (define sorted-tiles (sort tiles mj-tile<?))
    (define suits (map mj-tile-suit tiles))
    (and 
      (= 3 (length tiles))
      (not (mj-tile-honour? (first tiles)))
      (not (mj-tile-honour? (second tiles)))
      (not (mj-tile-honour? (third tiles)))
      (mj-tile-suit=? (first tiles) (second tiles))
      (mj-tile-suit=? (second tiles) (third tiles))
      (mj-tile<? (first sorted-tiles) (second sorted-tiles))
      (mj-tile<? (second sorted-tiles) (third sorted-tiles))))
  (module+ test
    (check-false (mj-sequence? empty))
    (check-false (mj-sequence? (list (mk-t "P1") (mk-t "P2"))))
    (check-false (mj-sequence? (list (mk-t "GD") (mk-t "P2"))))
    (check-false (mj-sequence? (list (mk-t "P1") (mk-t "P2") (mk-t "P1"))))
    (check-false (mj-sequence? (list (mk-t "P1") (mk-t "P2") (mk-t "P3") (mk-t "P4"))))
    (check-false (mj-sequence? (list (mk-t "GD") (mk-t "M1") (mk-t "M2"))))
    (check-false (mj-sequence? (list (mk-t "GD") (mk-t "M1") (mk-t "P3"))))
    (check-false (mj-sequence? (list (mk-t "M1") (mk-t "M2") (mk-t "P1"))))
    (check-true (mj-sequence? (list (mk-t "P1") (mk-t "P2") (mk-t "P3"))))
    (check-true (mj-sequence? (list (mk-t "P1") (mk-t "P3") (mk-t "P2")))))


  (define (mj-triplet? tiles)
    (and
      (= 3 (length tiles)) 
      (mj-tile=? (first tiles) (second tiles))
      (mj-tile=? (second tiles) (third tiles))))
  (module+ test
    (check-false (mj-triplet? empty))
    (check-false (mj-triplet? (list (mk-t "P1") (mk-t "P1"))))
    (check-false (mj-triplet? (list (mk-t "P1") (mk-t "P1") (mk-t "P2"))))
    (check-false (mj-triplet? (list (mk-t "M1") (mk-t "M2") (mk-t "P1"))))
    (check-false (mj-triplet? (list (mk-t "P1") (mk-t "P1") (mk-t "P1") (mk-t "P1"))))
    (check-true (mj-triplet? (list (mk-t "P1") (mk-t "P1") (mk-t "P1")))))


  (define (mj-pair? tiles)
    (cond
      [(not (= 2 (length tiles))) #f]
      [(mj-tile=? (first tiles) (second tiles)) #t]
      [else #f]))
  (module+ test
    (check-false (mj-pair? empty))
    (check-false (mj-pair? (list (mk-t "P1"))))
    (check-false (mj-pair? (list (mk-t "P1")(mk-t "P2"))))
    (check-false (mj-pair? (list (mk-t "M1")(mk-t "M2"))))
    (check-false (mj-pair? (list (mk-t "GD")(mk-t "RD"))))
    (check-false (mj-pair? (list (mk-t "M1")(mk-t "RD"))))
    (check-true (mj-pair? (list (mk-t "P1")(mk-t "P1")))))


  (define (block-it unordered-tiles)
    (define tiles (sort unordered-tiles mj-tile<?))
    (define nb (min 3 (length tiles)))
    (define taken (take tiles nb))
    (cond 
      [(empty? tiles) (append empty)]
      [(= 1 (length taken)) (cons taken empty)]
      [(= 2 (length taken)) (cond 
			      [(mj-pair? taken) (cons taken (block-it (drop tiles 2)))]
			      [else (cons (take tiles 1) (block-it (drop tiles 1)))])]
      [(= (length taken) 3) (cond
			      [(mj-sequence? taken) (cons taken (block-it (drop tiles 3)))]
			      [(mj-triplet? taken) (cons taken (block-it (drop tiles 3)))]
			      [(mj-pair? (take taken 2)) (cons (take taken 2) (block-it (drop tiles 2)))]
			      [else (cons (take tiles 1) (block-it (drop tiles 1)))])]))
  (module+ test
  (define (h-parse str)
    (map mk-t (string-split str ",")))

  ;mj-hand-to-blocks
  ; blocked ready hand
  (define readyhand-1 (h-parse "P1,S3,M8,P2,S2,M9,P3,S1,M7,WW,WW,WW,RD,RD"))
  (define readyhand-1-blocked (block-it readyhand-1))
  (check-match 
    readyhand-1-blocked
    (list-no-order (list _ _ _)(list _ _ _)(list _ _ _)(list _ _ _)(list _ _)))
  (check-true
    (mj-4-blocks-1-pair? readyhand-1-blocked)
    "Proper ready hand should be detected.")

  ; blocked tenpai hand
  (define tenpai-hand-1 (h-parse "P1,P1,P1,P2,P2,P2,S1,S2,S3,M1,M2,GD,RD"))
  (define tenpai-hand-1-blocked (block-it tenpai-hand-1))
  (check-match 
    tenpai-hand-1-blocked
    (list-no-order (list _ _ _)(list _ _ _)(list _ _ _)(list _)(list _)(list _)(list
										 _))))


  (define (mj-4-blocks-1-pair? blocked-tiles)
    (match blocked-tiles
      [(list-no-order (list _ _ _) (list _ _ _) (list _ _ _) (list _ _ _) (list _ _)) #t]
      [else #f]))


  (define (mj-ready-hand? tiles)
    (mj-4-blocks-1-pair? (block-it tiles)))


  (define (mj-ready-hand?* blocked-tiles)
    (mj-4-blocks-1-pair? blocked-tiles))


  (define (yaku-tanyao? tiles)
    (and 
      (not (ormap mj-tile-honour? tiles))
      (not (ormap mj-tile-terminal? tiles))
      (mj-ready-hand? tiles)))
  (module+ test
  (check-true (yaku-tanyao? (h-parse "P2,P3,P4,S5,S6,S7,M6,M6,M6,M5,M5,M5,S8,S8")))
  (check-false (yaku-tanyao? (h-parse "P1,P2,P3,S5,S6,S7,M6,M6,M6,M5,M5,M5,S8,S8")))
  (check-false (yaku-tanyao? (h-parse "DG,DG,DG,S5,S6,S7,M6,M6,M6,M5,M5,M5,S8,S8"))))


  (struct yaku [name hans]
		 #:transparent)


  (define ALL_SIMPLES (yaku "All simples" 1))
  (define THREE_COLORED_STRAIGHT (yaku "Three colored straight" 2))

  )
