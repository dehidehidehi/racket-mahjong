#lang typed/racket
; Functional types
(define-type Predicate (-> (Any -> Boolean)))
(define-type BiPredicate(-> (Any Any -> Boolean)))

; util
(: str->int (-> String Integer))
(define (str->int str)
  (define maybeNumber (string->number str))
   (assert maybeNumber exact-integer?))
                   
(: remove-n (-> Any (Listof Any) Integer (Listof Any)))
(define (remove-n v list n) 
  (cond
    [(or (empty? list) (= n 0)) list]
    [else (remove-n v (remove v list) (- n 1))]))

(: take-safe (All(A) (-> (Listof A) Integer (Listof A))))
(define (take-safe lst i) (take lst (if (> i (length lst)) (length lst) i)))

;; tile units

(struct Tile ([value : String]))

(struct Wind ([value : String])
  #:guard (lambda([s : String])
            (unless (member s '("W" "S" "N" "E"))
              (error (string-append "Bad wind was used as input ->" s)))))

(struct PrevailingWind ([value : Wind]))

(struct Suit ([value : String])
  #:guard (lambda(s)
            (unless (member s '("P" "S" "M"))
              (error (string-append "Bad suit value -> " s)))))

(struct Rank ([value : Integer])
  #:guard (lambda(i)
            (unless (and (>= 1 i)
                         (<= 9 i))
              (error("Bad rank value")))))

(struct SuitedTile ([suit : Suit] [rank : Rank]))

(: suited-tile? (-> Tile Boolean))
(define (suited-tile? tile)
  (exact-integer? (str->int (substring (Tile-value tile) 1 2))))

(: suited-tiles? (-> (Listof Tile) Boolean))
(define (suited-tiles? tiles)
  (andmap suited-tile? tiles))

(: mk-suited-tile-str (-> String SuitedTile))
(define (mk-suited-tile-str str)
  (define rank-value (str->int (substring str 1 2)))
  (cond
    [(not (suited-tile? (Tile str))) (error "Invalid rank value")]
    [else (SuitedTile (Suit (substring str 0 1)) (Rank rank-value))]))

(: mk-suited-tile (-> Tile SuitedTile))
(define (mk-suited-tile tile)
  (mk-suited-tile-str (Tile-value tile)))


;; blocks

(struct TileBlock ([tiles : (Listof Tile)])
  #:guard (lambda(t)
            (unless (and (>= 2 (length t)) (<= 4 (length t)))
              (error "Tiles not between 2 and 4 length."))))

(struct SequenceBlock([tiles : (Listof SuitedTile)])
  #:guard (lambda(t)
            (unless (and (>= 2 (length t)) (<= 4 (length t)))
              (error "Tiles not between 2 and 4 length."))))

(: paired? (-> TileBlock Boolean))
(define (paired? block)
  (and (= 2 (length (TileBlock-tiles block))) (= 1 (set-count (set (TileBlock-tiles block))))))

(: triplet? (-> TileBlock Boolean))
(define (triplet? block)
   (and (= 3 (length (TileBlock-tiles block))) (= 1 (set-count (set (TileBlock-tiles block))))))

(: sequence? (-> (Listof SuitedTile) Boolean))
(define (sequence? tiles)
  (define (sort-ranks [tiles : (Listof SuitedTile)]) 
    (sort (map (compose Rank-value SuitedTile-rank) tiles) <=))
  (: consecutive? (-> (Listof Number) Boolean))
  (define (consecutive? lst) ; must be ordered
        (and (= 1 (- (cadr lst) (car lst)))
             (consecutive? (cdr lst))))
  (and (= 3 (length tiles))
       (consecutive? (sort-ranks tiles))))

(struct Sequence ([block : SequenceBlock])
  #:guard (lambda(b) 
             (unless (sequence? (SequenceBlock-tiles b))
               (error "Provided tiles do not constitute a sequence."))))

(struct Triplet ([block : TileBlock])
  #:guard (lambda(b)
            (unless (triplet? b)
              (error ("Provided tiles do not constitute a triplet.")))))

(struct Paired ([block : TileBlock])
  #:guard (lambda(b)
            (unless (paired? b)
              (error ("Provided tiles are not pairs")))))

(: compose-blocks (-> (Listof Tile) (Listof (U Triplet Sequence Paired Tile))))
(define (compose-blocks tiles)
  (define sorted-tiles (sort (map Tile-value tiles) string<?))
  (cond
    [(triplet? (TileBlock (take-safe tiles 3))) (append (list (Triplet (TileBlock (take-safe tiles 3)))) (compose-blocks (drop tiles 3)))]
    [(sequence? (map mk-suited-tile (take-safe tiles 3))) (append (list (Sequence (SequenceBlock (map mk-suited-tile (take-safe tiles 3))))) (compose-blocks (drop tiles 3)))]
    [(paired? (TileBlock (take-safe tiles 2))) (append (list (Paired (TileBlock(take-safe tiles 2)))) (compose-blocks (drop tiles 2)))]
    [else (append (list (car tiles)) (compose-blocks (cdr tiles)))]))
 
;;parsing
;parse-tile
(: parse-tile (-> String Tile))
(define (parse-tile t) (Tile t))

;parse-tiles
(: parse-tiles (-> String (Listof Tile)))
(define (parse-tiles tiles)
 (if(string=? "" tiles) '()
 (cons (parse-tile (substring tiles 0 2)) (parse-tiles (substring tiles 2 (string-length tiles))))))

; composing hands
(: compose-hand (-> (Listof Tile) Any))
(define (compose-hand tiles)
      (compose-blocks tiles))

(: read-hand (-> String String (Listof Tile) Any))
(define (read-hand seat-wind round-wind tiles)
 (list (PrevailingWind (Wind seat-wind)) (PrevailingWind (Wind round-wind)) (compose-hand tiles)))

; Identify 3 triplets and one Paired
(print (list (PrevailingWind (Wind "W")) (PrevailingWind (Wind "S"))))
;(print (read-hand "S" "W" (parse-tiles "P0P1P1P2P2P2P3P3P3P4P4")))