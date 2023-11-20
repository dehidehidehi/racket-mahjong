#lang racket

;; What is this ?
;; A simple mahjong hand parser.
;; To help me learn Racket.

(struct tile ())

(struct tile/honor (honor)
  #:super tile
  #:guard (λ (honor name)
	    (let ([valid-honors (list 'RD 'GD 'WD 'WW 'SW 'NW 'EW)])
	      (unless (member honor valid-honors)
		;(error "Invalid honor")
		(println "Invalid honor")
		(honor)))))

(struct tile/ranked (suit rank)
  #:super tile
  #:guard (λ (suit rank name)
	    (let ([valid-suits (list 'P 'S 'M)])
	      (and 
		(unless (member suit valid-suits) 
		  ;(error "Invalid suit ")
		(println "Invalid suit")
		  )
		(unless (and (number? rank) (>= rank 1) (<= rank 9)) 
		  ;(error "Invalid rank")
		(println "Invalid rank")
		  )
		(values suit rank)))))

(define (mj/hand tiles-str)
  (define (parse-symbol t) (λ () (compose string->symbol string (λ (s) (string-ref s 0))) t))
  (define (parse-tval t) (λ () (compose string->number string (λ (s) (string-ref s 1))) t))
  (let ([tiles (string-split tiles-str)])
    (for ([suit (map parse-symbol tiles)]
	  [rank (map parse-tval tiles)]
	  [full tiles])
      (if (rank)
	(tile/ranked suit rank)
	(tile/honor full)))))

(define ok (mj/hand "P1 P1"))
(ok)
;(define ok (mj/hand P1 P1 P1 M1 M2 M3 S5 S6 S7 WW WW))


