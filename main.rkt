;#lang racket
#lang debug racket
(require debug/repl)


; Ideas for snippets
; module+ test (...)
; htdf template 
; htdd enum template 
; htdd composite data template


(module+ test
  (require rackunit))


(module* templates #f


  (define t/... (empty))


  ;; Suit is one of:
  ;; - M (manzu)
  ;; - S (sou)
  ;; - P (pin)
  ;; interp. the suit of a tile.
  (define (template-process-suit suit)
    (cond 
      [(eq? suit 'M) (t/...)]
      [(eq? suit 'S) (t/...)]
      [(eq? suit 'P) (t/...)]
      [else (t/...)]))


  ;; Rank is Integer
  ;; interpr. the rank of a tile, restricted to [-1, 9], -1 corresponds to a aka 5 and should equal? 5.
  (define (template-process-rank rank)
    (cond 
      [(or (= rank 5) (= -1 rank)) (t/... rank)]
      [else (t/... rank)]))


  ;; s/tile is (make-s/tile Symbol Integer)
  ;; interp. a sequence tile, with a suit and a rank.
  (define (template-process-s/tile tile)
    (let* ([suit (s/tile-suit tile)]
	   [rank (s/tile-rank tile)]
	   [handle-suit (cond 
			  [(eq? suit 'M) (t/...)]
			  [(eq? suit 'S) (t/...)]
			  [(eq? suit 'P) (t/...)]
			  [else (t/...)])]
	   [handle-rank (cond 
			  [(or (= rank 5) (= -1 rank)) (t/... rank)]
			  [else (t/... rank)])])
      (t/... handle-suit handle-rank)))


  ;; Honor is one of:
  ;; - WW (west wind)
  ;; - NW (north wind)
  ;; - EW (east wind)
  ;; - SW (south wind)
  ;; - GD (green dragon)
  ;; - RD (red dragon)
  ;; - WD (white dragon)
  ;; interp. a tile which is an honor.
  (define (template-process-h/tile honor)
    (cond
      [(not (valid-honor? honor)) #f]
      [(eq? honor 'WW) (t/...)]
      [(eq? honor 'NW) (t/...)]
      [(eq? honor 'EW) (t/...)]
      [(eq? honor 'SW) (t/...)]
      [(eq? honor 'GD) (t/...)]
      [(eq? honor 'RD) (t/...)]
      [(eq? honor 'WD) (t/...)]
      [(eq? honor 'GD/aka) (t/...)]
      [(eq? honor 'RD/aka) (t/...)]
      [(eq? honor 'WD/aka) (t/...)]
      [else (t/...)]))


  ;; Hand is (make-hand (ListOf (Union s/tile Symbol)))
  ;; interp. a hand of 0 to 14 tiles, which are either s/tile or honor tiles.
  (define (template-process-hand tiles)
    (let ([tile (first tiles)])
      ((cond
	 [(not (list? tiles)) t/...]
	 [(empty? tiles) t/...]
	 [(s/tile? tile) 
	  (cons ((let* ([suit (s/tile-suit tile)]
			[rank (s/tile-rank tile)]
			[handle-suit (cond 
				       [(eq? suit 'M) (t/...)]
				       [(eq? suit 'S) (t/...)]
				       [(eq? suit 'P) (t/...)]
				       [else (t/...)])]
			[handle-rank (cond 
				       [(or (= rank 5) (= -1 rank)) (t/... rank)]
				       [else (t/... rank)])])
		   (t/... handle-suit handle-rank))
		 (template-process-hand (rest tiles))))]
	 [(eq? tile 'WW) (t/...)]
	 [(eq? tile 'NW) (t/...)]
	 [(eq? tile 'EW) (t/...)]
	 [(eq? tile 'SW) (t/...)]
	 [(eq? tile 'GD) (t/...)]
	 [(eq? tile 'RD) (t/...)]
	 [(eq? tile 'WD) (t/...)]
	 [(eq? tile 'GD/aka) (t/...)]
	 [(eq? tile 'RD/aka) (t/...)]
	 [(eq? tile 'WD/aka) (t/...)]
	 [else (t/...)]))
      )
    )

  (define (template-hand-complete? hand) (t/... hand))
  )


;; Predicate (ListOf Any) -> (Union #f (ListOf Any))
;; Utility function.
;; Reduces a list applying a predicate which has a fixed arity of 1.
;; Returns #f if any application of the predicate returns #f.
(define (all? pred lst)
  (if (andmap pred lst) lst #f))


;; Predicate (ListOf Any) -> (Union #f (ListOf Any))
;; Utility function.
;; Reduces a list applying a bi-predicate which has a fixed arity of 2.
;; Returns #f if any application of the predicate returns #f.
(define (all/bi? bipred lst)
  (if (andmap (λ (x) (bipred x (first lst))) (rest lst)) lst #f))


;; Any Any (ListOf Any) -> (ListOf Any)
;; Replaces a value in a list by another value when the the equal? procedure returns #t
(define (replace v1 v2 lst)
  (map (λ (e) (if (equal? v1 e) v2 e)) lst))
(module+ test
  (check-equal? (replace 1 2 (list 2 1 2)) (list 2 2 2)))


(list 'WW 'NW 'EW 'SW 'GD 'RD 'WD 'RD/aka 'GD/aka 'WD/aka)
(list 'CHINIISOU)
(define-struct s/tile (suit rank) #:transparent)
(define-struct hand (tiles) #:transparent)


(define (s/tile-same-suit? tiles) 
  (if (all/bi? equal? (map s/tile-suit tiles)) tiles #f))
(module+ test
  (define (m n) (make-s/tile 'M n))
  (define (s n) (make-s/tile 'S n))
  (check-equal? (s/tile-same-suit? (list (m 1) (m 2) (m 3))) (list (m 1) (m 2) (m 3)))
  (check-false (s/tile-same-suit? (list (m 1) (s 2) (m 3))) #f))


(define (s/tile-same-rank? tiles) 
  (if (all/bi? = (map s/tile-rank tiles)) tiles #f))
(module+ test
  (check-equal? (s/tile-same-rank? (list (m 1) (m 1) (m 1))) (list (m 1) (m 1) (m 1)))
  (check-false (s/tile-same-rank? (list (m 1) (m 2) (m 1)))))


;; Symbol -> (Union Symbol #f)
;; Returns the honor if the symbol is considered a valid honor, else #f.
(define (valid-honor? honor)
    (cond
      [(eq? honor 'WW) honor]
      [(eq? honor 'NW) honor]
      [(eq? honor 'EW) honor]
      [(eq? honor 'SW) honor]
      [(eq? honor 'GD) honor]
      [(eq? honor 'RD) honor]
      [(eq? honor 'WD) honor]
      [(eq? honor 'GD/aka) honor]
      [(eq? honor 'RD/aka) honor]
      [(eq? honor 'WD/aka) honor]
      [else #f]))
(module+ test
  (check-eq? (valid-honor? 'WW) 'WW)
  (check-eq? (valid-honor? 'NW) 'NW)
  (check-eq? (valid-honor? 'EW) 'EW)
  (check-eq? (valid-honor? 'SW) 'SW)
  (check-eq? (valid-honor? 'GD) 'GD)
  (check-eq? (valid-honor? 'RD) 'RD)
  (check-eq? (valid-honor? 'WD) 'WD)
  (check-eq? (valid-honor? 'GD/aka) 'GD/aka)
  (check-eq? (valid-honor? 'RD/aka) 'RD/aka)
  (check-eq? (valid-honor? 'WD/aka) 'WD/aka))


(define (all-valid-honors? honors)
  (if (andmap valid-honor? honors) honors #f))
(module+ test
  (check-equal? (all-valid-honors? (list 'NW 'WW 'GD/aka)) (list 'NW 'WW 'GD/aka))
  (check-false (all-valid-honors? (list 'NW 'WW 'GD/aka 'WHAT?))))
	

;; ListOf s/tiles -> (Union (ListOf s/tiles) #f)
;; If all s/tile ranks are equal then returns the list of tiles.
;; This takes into account red fives.
(define (all-s/tile-ranks=? tiles)  ; ListOf s/tile
  (define ranks (map s/tile-rank tiles))
  (if (and (s/tile-same-suit? tiles) 
       (or (s/tile-same-rank? tiles)
	   (equal? (list 5 5 5) (replace -1 5 ranks))))
    tiles
    #f))
(module+ test 
  (check-equal? (all-s/tile-ranks=? (list (m 5) (m 5) (m 5))) (list (m 5) (m 5) (m 5)))
  (check-equal? (all-s/tile-ranks=? (list (m -1) (m 5) (m 5))) (list (m -1) (m 5) (m 5)))
  (check-false (all-s/tile-ranks=? (list (m 1) (m 5) (m 5)))))


;; ListOf Symbol -> (Union (ListOf Symbol) #f)
;; If all honor tiles are equal then returns the list of honors.
;; This takes into account red dragons.
(define (all-honors-equal? honors)
  (cond
    [(not (all-valid-honors? honors)) #f]
    [(all/bi? eq? honors) honors]
    [(and (member 'GD/aka honors) (all/bi? eq? (replace 'GD/aka 'GD honors))) honors]
    [(and (member 'RD/aka honors) (all/bi? eq? (replace 'RD/aka 'RD honors))) honors]
    [(and (member 'WD/aka honors) (all/bi? eq? (replace 'WD/aka 'WD honors))) honors]
    [else #f]))
(module+ test 
  (check-equal? (all-honors-equal? (list 'WW 'WW 'WW)) (list 'WW 'WW 'WW))
  (check-equal? (all-honors-equal? (list 'GD 'GD 'GD)) (list 'GD 'GD 'GD))
  (check-equal? (all-honors-equal? (list 'RD/aka 'RD 'RD)) (list 'RD/aka 'RD 'RD))
  (check-false (all-honors-equal? (list 'WW 'WW 'NW))))


;; tile ListOf tile -> Union #f ListOf tile
;; Returns a completed pair or #f where a pair are 2 identical tiles.
;; A red dragon is the same as it's corresponding dragon.
;; A red five is the same as it's corresponding five.
;; tile ListOf tile -> Union #f ListOf tile
;; Returns a completed triplet or #f.
(define (completes-triplet tile b)
  (define blocked (list* tile b))
  (cond 
    [(not (= 3 (length blocked))) #f]
    [(and (all? s/tile? blocked) 
	  (all/bi? equal? (map s/tile-suit blocked))
	  (all-s/tile-ranks=? blocked))
     blocked]
    [(and (all-honors-equal? blocked)) blocked]
    [else #f]))
(module+ test
  (check-false (completes-triplet 'WW (list 'WW)))
  (check-false (completes-triplet 'WW (list 'SW 'WW)))
  (let ([m2 (make-s/tile 'M 2)])
    (check-equal? (completes-triplet m2 (list m2 m2)) (list m2 m2 m2)))
  (check-equal? (completes-triplet 'WW (list 'WW 'WW)) (list 'WW 'WW 'WW))
  (check-equal? (completes-triplet 'GD (list 'GD 'GD)) (list 'GD 'GD 'GD))
  (let ([m5/aka (make-s/tile 'M -1)]
	[m5 (make-s/tile 'M 5)])
    (check-equal? (completes-triplet m5/aka (list m5 m5)) (list m5/aka m5 m5)))
  (check-equal? (completes-triplet 'GD/aka (list 'GD 'GD)) (list 'GD/aka 'GD 'GD))
  (check-equal? (completes-triplet 'RD/aka (list 'RD 'RD)) (list 'RD/aka 'RD 'RD))
  (check-equal? (completes-triplet 'WD/aka (list 'WD 'WD)) (list 'WD/aka 'WD 'WD))
  (check-equal? (completes-triplet 'WD (list 'WD 'WD/aka)) (list 'WD 'WD 'WD/aka)))


;; tile ListOf tile -> Union #f ListOf tile
;; Given two tiles and one more tile, returns a completed sequence or #f.
(define (completes-sequence tile block)
  (let ([blocked (list* tile block)])
    (cond
      [(not (= 3 (length blocked))) #f]
      [(not (andmap s/tile? blocked)) #f]
      [(not (all/bi? equal? (map s/tile-suit blocked))) #f]
      [(check-duplicates (map s/tile-rank blocked)) #f]
      [(let* ([ranks (map s/tile-rank blocked)]
	      [normalized-ranks (replace -1 5 ranks)]
	      [sorted (sort normalized-ranks <=)])
	 ( = (- (last sorted) (first sorted)) 2)) blocked]
      [else #f])))
(module+ test
  (define m1 (make-s/tile 'M 1))
  (define m2 (make-s/tile 'M 2))
  (define m4 (make-s/tile 'M 4))
  (define m5 (make-s/tile 'M 5))
  (define m5/aka (make-s/tile 'M -1))
  (define m6 (make-s/tile 'M 6))
  (define m7 (make-s/tile 'M 7))
  (define s7 (make-s/tile 'S 7))
  (check-false (completes-sequence 'WW (list 'WW 'WW)))
  (check-false (completes-sequence m4 (list 'WW 'WW)))
  (check-false (completes-sequence m4 (list 'WW 'WW)))
  (check-false (completes-sequence m4 (list m5 m7)))
  (check-false (completes-sequence m4 (list m6 m7)))
  (check-false (completes-sequence m4 (list m4 m4)))
  (check-false (completes-sequence m4 (list m4 m7)))
  (check-false (completes-sequence m4 (list m4 m6)))
  (check-false (completes-sequence m7 (list m5 m5)))
  (check-false (completes-sequence s7 (list m5 m5)))
  (check-false (completes-sequence m5/aka (list m1 m2)))
  (check-equal? (completes-sequence m4 (list m5 m6)) (list m4 m5 m6))
  (check-equal? (completes-sequence m4 (list m6 m5)) (list m4 m6 m5))
  (check-equal? (completes-sequence m5 (list m6 m4)) (list m5 m6 m4))
  (check-equal? (completes-sequence m5/aka (list m6 m4)) (list m5/aka m6 m4))
  (check-equal? (completes-sequence m5/aka (list m7 m6)) (list m5/aka m7 m6)))



;; TODO completes pair 

;; TODO compose hand

;; hand blocks -> ListOf ListOf tile
;; divides the hand into sequence, pair, or honor blocks.
;(define (block-hand hand [blocks empty])
;  (define tile (first hand))
;  (cond
;    [(not (list? hand)) empty]
;    [(empty? hand) blocks]
;    [(s/tile? tile)
;     (let ([suit (s/tile-suit tile)]
;	   [rank (s/tile-rank tile)])
;       (cond
;	 [(empty? blocks) (block-hand (rest hand) (cons (list tile) empty))]
;	 [] ; try adding to existing blocks
;	 [else ()]
;	 [(ormap (λ (t) (andmap eq? )) 
;		 [; for each block check if adding this tile creates any triplet]
;
;		  (t/... (cond
;			   [(eq? suit 'M) suit]
;			   [(eq? suit 'S) suit]
;			   [(eq? suit 'P) suit]
;			   [else #f])
;			 (t/... rank)))]
	; [(eq? honor 'WW) (t/...)]
	; [(eq? honor 'NW) (t/...)]
	; [(eq? honor 'EW) (t/...)]
	; [(eq? honor 'SW) (t/...)]
	; [(eq? honor 'GD) (t/...)]
	; [(eq? honor 'RD) (t/...)]
	; [(eq? honor 'WD) (t/...)]
	; [(eq? honor 'GD/aka) (t/...)]
	; [(eq? honor 'RD/aka) (t/...)]
	; [(eq? honor 'WD/aka) (t/...)]
;	  [else (t/...)]]))])))
;
;(module+ test
;  ; malformed input
;  (check-eq? #f (block-hand (make-hand empty) ))
;  (check-eq? #f (block-hand (make-hand "abc") ))
;  (check-eq? #f (block-hand (make-hand (list 'S1) )))
;  (check-eq? #f (block-hand (make-hand
;			      (list
;				(make-s/tile 'M 9)
;				(make-s/tile 'S 1)
;				(make-s/tile 'S 1)
;				(make-s/tile 'S 2)
;				(make-s/tile 'S 2)
;				(make-s/tile 'S 2)
;				(make-s/tile 'S 3)
;				(make-s/tile 'S 3)
;				(make-s/tile 'S 3)
;				(make-s/tile 'S 4)
;				(make-s/tile 'S 4)) ))) ; chiniisou tenpai
;  (let ([h (make-hand
;	     (list
;	       (make-s/tile 'S 1)
;	       (make-s/tile 'S 1)
;	       (make-s/tile 'S 1)
;	       (make-s/tile 'S 2)
;	       (make-s/tile 'S 2)
;	       (make-s/tile 'S 2)
;	       (make-s/tile 'S 3)
;	       (make-s/tile 'S 3)
;	       (make-s/tile 'S 3)
;	       (make-s/tile 'S 4)
;	       (make-s/tile 'S 4)))])
;    (check-eq? h (block-hand h ))) ; chiniisou tenpai
;  )
;
;
;;; hand -> ListOf yaku
;;; determines the yaku the hand is eligible for
;
;
;

