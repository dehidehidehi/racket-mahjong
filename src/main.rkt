#lang racket

(struct tile ())

(struct tile/honor (honor)
  #:super tile
  #:guard (λ (honor name)
	    (let ([valid-honors (list 'RD 'GD 'WD 'WW 'SW 'NW 'EW)])
	      (unless (member honor valid-honors)
		(error "Invalid honor")
		(honor)))))

(struct tile/ranked (suit rank)
  #:super tile
  #:guard (λ (suit rank name)
	    (let ([valid-suits (list 'P 'S 'M)])
	      (and 
		(unless (member suit valid-suits) 
		  (error "Invalid suit "))
		(unless (and (number? rank) (>= rank 1) (<= rank 9)) 
		  (error "Invalid rank"))
		(values suit rank)))))


(define-syntax-rule (mj/hand . varargs)
  ;(unless (list? tiles-abbr) (error "Invalid param : not a list"))
  (let ([tiles-abbr (flatten varargs)])
    ;(for ([suit (map (compose string->symbol (λ s (string-ref s 0)) symbol->string) tiles-abbr)]
;	  [tval (map (compose (λ s (string-ref s 1)) symbol->string) tiles-abbr)])
      ;(match (cons suit tval)
      (match (tiles-abbr)
	;['(_ number?) (tile/ranked suit tval)]
	[else "unknown!"])))


(define ok (mj/hand 'P1))
(ok)
;(define ok (mj/hand P1 P1 P1 M1 M2 M3 S5 S6 S7 WW WW))


