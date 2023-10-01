;#lang racket
#lang debug racket


(module+ test
  (require rackunit)
  (require racket/trace))


(define P 'Pin)
(define S 'Sou)
(define M 'Manzu)
(define NW 'NorthWind)
(define SW 'SouthWind)
(define EW 'EastWind)
(define WW 'WestWind)
(define RD 'RedDragon)
(define GD 'GreenDragon)
(define WD 'WhiteDragon)


(struct mj-honour [face] #:prefab)
(struct mj-ranked [suit rank] #:prefab)
(define (mj-ranked->string t)
  (define sym-str 
    (match (symbol->string (mj-ranked-suit t))
      ["Pin" "P"]
      ["Sou" "S"]
      ["Manzu" "M"]))
  (string-append sym-str (number->string (mj-ranked-rank t))))

(module+ test
  (check-equal? "P1" (mj-ranked->string (mj-ranked P 1)))
  (check-equal? "S9" (mj-ranked->string (mj-ranked S 9)))
  (check-equal? "M5" (mj-ranked->string (mj-ranked M 5))))

(module+ test
  (check-false (equal? (mj-ranked P 1) (mj-ranked P 2)))
  (check-false (equal? (mj-ranked P 1) (mj-ranked P 2)))
  (check-false (equal? (mj-ranked P 1) (mj-honour NW)))
  (check-true (equal? (mj-ranked P 1) (mj-ranked P 1)))
  (check-true (equal? (mj-honour NW) (mj-honour NW))))


(define (tile . params)
  (if (and (symbol? (first params))
	   (or (< (length params) 2) (number? (second params))))
    (cond
      [(set-member? (set P S M) (first params)) (mj-ranked (first params) (second params))]
      [else (mj-honour (first params))])
    (error "Invalid tile symbols")))


(module+ test
  (check-equal? (tile P 1) (mj-ranked P 1))
  (check-equal? (tile M 2) (mj-ranked M 2))
  (check-equal? (tile S 3) (mj-ranked S 3))
  (check-equal? (tile NW) (mj-honour NW))
  (check-equal? (tile WW) (mj-honour WW))
  (check-equal? (tile SW) (mj-honour SW))
  (check-equal? (tile EW) (mj-honour EW))
  (check-equal? (tile GD) (mj-honour GD))
  (check-equal? (tile RD) (mj-honour RD))
  (check-equal? (tile WD) (mj-honour WD)))



(define (mj-tile-terminal? t)
  (and (mj-ranked? t)
       (or (= 1 (mj-ranked-rank t)) (= 9 (mj-ranked-rank t)))))


(module+ test
  (check-false (mj-tile-terminal? (tile GD)))
  (check-false (mj-tile-terminal? (tile NW)))
  (check-false (mj-tile-terminal? (tile P 2)))
  (check-false (mj-tile-terminal? (tile S 8)))
  (check-true (mj-tile-terminal?  (tile M 1)))
  (check-true (mj-tile-terminal?  (tile M 9)))
  (check-true (mj-tile-terminal?  (tile P 1)))
  (check-true (mj-tile-terminal?  (tile P 9)))
  (check-true (mj-tile-terminal?  (tile S 1)))
  (check-true (mj-tile-terminal?  (tile S 9))))


(define (mj-ranked-suit=? t1 t2)
  (and (and (mj-ranked? t1) (mj-ranked? t2))
       (equal? (mj-ranked-suit t1) (mj-ranked-suit t2))))


(module+ test
  (check-false (mj-ranked-suit=? (tile GD) (tile RD))) ; not suited tile
  (check-false (mj-ranked-suit=? (tile GD) (tile S 1)))
  (check-false (mj-ranked-suit=? (tile GD) (tile SW)))
  (check-false (mj-ranked-suit=? (tile M 1) (tile P 2)))
  (check-false (mj-ranked-suit=? (tile P 1) (tile M 1)))
  (check-false (mj-ranked-suit=? (tile S 1) (tile M 1)))
  (check-false (mj-ranked-suit=? (tile NW) (tile GD)))
  (check-false (mj-ranked-suit=? (tile NW) (tile S 1)))
  (check-false (mj-ranked-suit=? (tile NW) (tile SW)))
  (check-true (mj-ranked-suit=?  (tile M 1) (tile M 3)))
  (check-true (mj-ranked-suit=?  (tile P 1) (tile P 1)))
  (check-true (mj-ranked-suit=?  (tile P 1) (tile P 2))))


(define (mj-ranked-suit=?* . p-tiles)
  (define tiles (flatten p-tiles))
  (cond
    [(empty? tiles) #t]
    [(= 2 (length tiles)) (mj-ranked-suit=? (first tiles) (second tiles))]
    [else (eq? #t (andmap (lambda (t) (mj-ranked-suit=? t (first tiles))) (rest tiles)))]))


(module+ test
  (check-true (mj-ranked-suit=?* empty))
  (check-true (mj-ranked-suit=?* (tile P 1) (tile P 2)))
  (check-true (mj-ranked-suit=?* (tile M 1) (tile M 1) (tile M 1)))
  (check-false (mj-ranked-suit=?* (tile P 1) (tile S 1)))
  (check-false (mj-ranked-suit=?* (tile P 1) (tile S 1) (tile S 2)))
  (check-false (mj-ranked-suit=?* (tile GD) (tile GD)))
  (check-false (mj-ranked-suit=?* (tile NW) (tile SW))))


(struct mj-hand [concealed melds])


(define (mj-hand-tiles hand)
  (append (mj-hand-concealed hand) (mj-hand-melds hand)))


(module+ test
  (define (mk-h tiles)
    (mj-hand tiles empty)))


(define (mj-tile<? t1 t2)
  (and (or (mj-ranked? t1) (mj-honour? t1))
       (or (mj-ranked? t2) (mj-honour? t2))
       (cond
	 [(and (mj-ranked? t1) (mj-ranked? t2))
	  (string<? (mj-ranked->string t1) (mj-ranked->string t2))]
	 [(and (mj-honour? t1) (mj-honour? t2)) (symbol<? (mj-honour-face t1) (mj-honour-face t2))]
	 [(and (mj-honour? t1) (mj-ranked? t2)) #f]
	 [(and (mj-ranked? t1) (mj-honour? t2)) #t]
	 [else (error "Illegal state")])))


(module+ test
  (check-true  (mj-tile<? (tile P 1) (tile P 2)))
  (check-false (mj-tile<? (tile P 2)  (tile P 1)))
  (check-true  (mj-tile<? (tile P 1)  (tile GD)))
  (check-false (mj-tile<? (tile GD)   (tile P 1))))


(module+ test
  (check-equal? (sort (list (tile P 2) (tile S 1) (tile P 1) (tile P 3)) mj-tile<?)
		(list (tile P 1) (tile P 2) (tile P 3) (tile S 1))))


(define (mj-pair? . tiles)
  (define ftiles (flatten tiles))
  (and (= 2 (length ftiles))
       (equal? (first ftiles) (second ftiles))))


(module+ test
  (check-false (mj-pair? empty))
  (check-false (mj-pair? (tile P 1)))
  (check-false (mj-pair? (tile P 1)(tile P 2)))
  (check-false (mj-pair? (tile M 1)(tile M 2)))
  (check-false (mj-pair? (tile GD)(tile RD)))
  (check-false (mj-pair? (tile M 1)(tile RD)))
  (check-true (mj-pair?  (list (tile P 1)(tile P 1))))
  (check-true (mj-pair?  (tile P 1)(tile P 1))))


(define (mj-triplet? . tiles)
  (define ftiles (flatten tiles))
  (and (= 3 (length ftiles))
       (and (equal? (first ftiles) (second ftiles))
	    (equal? (first ftiles) (third ftiles)))))


(module+ test
  (check-false (mj-triplet? empty))
  (check-false (mj-triplet? (tile P 1) (tile P 1)))
  (check-false (mj-triplet? (tile P 1) (tile P 1) (tile P 2)))
  (check-false (mj-triplet? (tile M 1) (tile M 2) (tile P 1)))
  (check-false (mj-triplet? (tile P 1) (tile P 1) (tile P 1) (tile P 1)))
  (check-true  (mj-triplet? (tile P 1) (tile P 1) (tile P 1)))
  (check-true  (mj-triplet? (tile WW) (tile WW) (tile WW)))
  (check-true  (mj-triplet? (list (tile WW) (tile WW) (tile WW)))))


(define (arithmetic-sequence? numbers [step-size 1])
  (define (compare-last-2-numbers) 
    (= step-size 
       (- (last numbers) 
	  (list-ref numbers (- (length numbers) 2)))))
  (cond
    [(empty? numbers) #f]
    [(= 1 (length numbers)) #t]
    [(= 2 (length numbers)) (compare-last-2-numbers)]
    [else (and (compare-last-2-numbers) (arithmetic-sequence? (drop-right numbers 1) step-size))]))


(module+ test
  (check-false (arithmetic-sequence? empty))
  (check-true (arithmetic-sequence? (list 1)))
  (check-true (arithmetic-sequence? (list 1 2)))
  (check-false (arithmetic-sequence? (list 1 2 4)))
  (check-false (arithmetic-sequence? (list 1 3 4)))
  (check-true (arithmetic-sequence? (list 2 3 4)))
  (check-true (arithmetic-sequence? (list 2 4 6) 2))
  (check-false (arithmetic-sequence? (list 3 2 1)))
  (check-false (arithmetic-sequence? (list 6 4 2) 2)))


(define (mj-ranked-sequence? p-tiles)
  (define tiles (flatten p-tiles))
  (arithmetic-sequence? (map mj-ranked-rank tiles)))


(define (mj-sequence? . tiles)
  (define ftiles (flatten tiles))
  (define sorted-tiles (sort ftiles mj-tile<?))
  (and 
    (= 3 (length ftiles))
    (andmap mj-ranked? ftiles)
    (mj-ranked-suit=?* ftiles)
    (mj-ranked-sequence? sorted-tiles)))


(module+ test
  (check-true (mj-sequence?  (tile P 1) (tile P 2) (tile P 3)))
  (check-true (mj-sequence?  (tile P 1) (tile P 3) (tile P 2)))
  (check-true (mj-sequence?  (list (tile P 1) (tile P 3) (tile P 2))))
  (check-false (mj-sequence? empty))
  (check-false (mj-sequence? (tile P 1) (tile P 2)))
  (check-false (mj-sequence? (tile GD) (tile P 2)))
  (check-false (mj-sequence? (tile P 1) (tile P 2) (tile P 1)))
  (check-false (mj-sequence? (tile P 1) (tile P 2) (tile P 3) (tile P 4)))
  (check-false (mj-sequence? (tile GD) (tile M 1) (tile M 2)))
  (check-false (mj-sequence? (tile GD) (tile M 1) (tile P 3)))
  (check-false (mj-sequence? (tile M 1) (tile M 2) (tile P 1))))


(define (mj-hand->blocks unordered-tiles)
  (define tiles (sort unordered-tiles mj-tile<?))
  (define nb (min 3 (length tiles)))
  (define taken (take tiles nb))
  (cond 
    [(empty? tiles) (append empty)]
    [(= 1 (length taken)) (cons taken empty)]
    [(= 2 (length taken)) (cond 
			    [(mj-pair? taken) (cons taken (mj-hand->blocks (drop tiles 2)))]
			    [else (cons (take tiles 1) (mj-hand->blocks (drop tiles 1)))])]
    [(= (length taken) 3) (cond
			    [(mj-sequence? taken) (cons taken (mj-hand->blocks (drop tiles 3)))]
			    [(mj-triplet? taken) (cons taken (mj-hand->blocks (drop tiles 3)))]
			    [(mj-pair? (take taken 2)) (cons (take taken 2) (mj-hand->blocks (drop tiles 2)))]
			    [else (cons (take tiles 1) (mj-hand->blocks (drop tiles 1)))])]))


(module+ test
  (define ns (variable-reference->namespace (#%variable-reference)))
  (define (h-parse str)
    (define split-tiles (string-split str ","))
    (define (ranked? str) (number? (string->number (substring str 1 2))))
    (define (str->tile str) (eval (string->symbol str) ns))
    (define (convert str)
      (cond
	[(ranked? str) (tile (str->tile (substring str 0 1)) (string->number (substring str 1 2)))]
	[else (tile (str->tile str))]))
    (map convert split-tiles))

  ;mj-hand-to-blocks
  ; blocked ready hand
  (define readyhand-1 (h-parse "P1,S3,M8,P2,S2,M9,P3,S1,M7,WW,WW,WW,RD,RD"))
  (define readyhand-1-blocked (mj-hand->blocks readyhand-1))
  (check-match 
    readyhand-1-blocked
    (list-no-order (list _ _ _)(list _ _ _)(list _ _ _)(list _ _ _)(list _ _)))
  (check-true
    (mj-4-blocks-1-pair? readyhand-1-blocked)
    "Proper ready hand should be detected.")

  ; blocked tenpai hand
  (define tenpai-hand-1 (h-parse "P1,P1,P1,P2,P2,P2,S1,S2,S3,M1,M2,GD,RD"))
  (define tenpai-hand-1-blocked (mj-hand->blocks tenpai-hand-1))
  (check-match 
    tenpai-hand-1-blocked
    (list-no-order (list _ _ _)(list _ _ _)(list _ _ _)(list _)(list _)(list _)(list _))))


(define (mj-4-blocks-1-pair? blocked-tiles)
  (match blocked-tiles
    [(list-no-order (list _ _ _) (list _ _ _) (list _ _ _) (list _ _ _) (list _ _)) #t]
    [else #f]))


(define (mj-ready-hand? tiles)
  (or (chiitoitsu? tiles)
      (mj-4-blocks-1-pair? (mj-hand->blocks tiles))))


(define (chiitoitsu? tiles)
  (match (mj-hand->blocks tiles)
    [(list-no-order (list _ _) (list _ _)(list _ _)(list _ _)(list _ _)(list _ _)(list _ _)) #t]
    [_ #f]))


(module+ test
  (check-true (chiitoitsu? (h-parse "P1,P1,S1,S1,M1,M1,P2,P2,S2,S2,M2,M2,GD,GD")))
  (check-false (chiitoitsu? (h-parse "P1,P1,S1,S1,M1,M1,P2,P2,S2,S2,M2,M2,GD,RD"))))


(define (tanyao? tiles)
  (and 
    (not (ormap mj-honour? tiles))
    (not (ormap mj-tile-terminal? tiles))
    (mj-ready-hand? tiles)))


(module+ test
  (check-true  (tanyao? (h-parse "P2,P3,P4,S5,S6,S7,M6,M6,M6,M5,M5,M5,S8,S8")))
  (check-false (tanyao? (h-parse "P1,P2,P3,S5,S6,S7,M6,M6,M6,M5,M5,M5,S8,S8")))
  (check-false (tanyao? (h-parse "GD,GD,GD,S5,S6,S7,M6,M6,M6,M5,M5,M5,S8,S8"))))


(struct yaku [name hans]
  #:transparent)


(define ALL_SIMPLES (yaku "All simples" 1))


(define THREE_COLORED_STRAIGHT (yaku "Three colored straight" 2))


(module mahjong racket
  (provide all-defined-out))

