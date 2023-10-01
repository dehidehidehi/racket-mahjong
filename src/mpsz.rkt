#lang debug racket
(provide all-defined-out)

(module+ test
  (require rackunit))


;(define (string->mpsz notation)
;  (match notation
;    [1d1c (let ([rank (string->number (substring notation 0 1))]
;		[suit (string->symbol (substring notation 1 2))])
;	    (mpsz-tile rank suit))]
;    [2d1c (cons (string->mpsz ))]
;    [3d1c (cons (string->mpsz ))]
;    [4d1c (cons (string->mpsz ))]))


(define (expand-mpsz notation)
  (match notation
    [(regexp #px"^[\\d]{1}[mpsz]") (expand-mpsz-group (substring notation 0 2))]
    [(regexp #px"^[\\d]{2}[mpsz]") (expand-mpsz-group (substring notation 0 3))]
    [(regexp #px"^[\\d]{3}[mpsz]") (expand-mpsz-group (substring notation 0 4))]
    [(regexp #px"^[\\d]{4}[mpsz]") (expand-mpsz-group (substring notation 0 5))]
    [_ ""]))


(define (expand-mpsz-group group)
  (define grp-len (string-length group))
  (define digits (substring group 0 (- grp-len 1)))
  (define suit (substring group (- grp-len 1) grp-len))
  (define digits-intstream (build-vector (string-length digits) add1))
  (define expanded-list (for/list ([i digits-intstream]) (string-append (substring digits (- i 1) i) suit)))
  (apply string-append expanded-list))


(module+ test
  (check-equal? (expand-mpsz "1m") "1m")
  (check-equal? (expand-mpsz "11m") "1m1m")
  (check-equal? (expand-mpsz "12s") "1s2s")
  (check-equal? (expand-mpsz "122p") "1p2p2p")
  (check-equal? (expand-mpsz "444m") "4m4m4m")
  (check-equal? (expand-mpsz "5555s") "5s5s5s5s"))

;;(module+ test
;  (check-equal? (string->mpsz "1m") (list (mpsz-tile 1 m)))
;  (check-equal? (string->mpsz "2m") (list (mpsz-tile 2 m)))
;  (check-equal? (string->mpsz "3s") (list (mpsz-tile 3 s)))
;  (check-equal? (string->mpsz "4p") (list (mpsz-tile 4 p)))
;  (check-equal? (string->mpsz "0p") (list (mpsz-tile 0 p)))
;  (check-equal? (string->mpsz "1z") (list (mpsz-tile 1 z)))
;  (check-equal? (string->mpsz "7z") (list (mpsz-tile 7 z))))


;(module+ test
;  (check-equal? (string->mpsz "1m1m")   (list (mpsz-tile 1 m) (mpsz-tile 1 m)))
;  (check-equal? (string->mpsz "3s4s5s") (list (mpsz-tile 3 s) (mpsz-tile 4 s) (mpsz-tile 5 s))))


;(module+ test
;  (check-equal? (string->mpsz "345s") (list (mpsz-tile 3 s) (mpsz-tile 5 s) (mpsz-tile 5 s))))


(define (string-group str step)
  (cond
    [(<= (string-length str) step) (cons str empty)]
    [else (cons (substring str 0 step) (string-group (substring str step) step))]))


(module+ test
  (check-equal? (string-group "abc" 1)  (list "a" "b" "c"))
  (check-equal? (string-group "abcd" 2) (list "ab" "cd"))
  (check-equal? (string-group "abc" 2)  (list "ab" "c")))


(struct mpsz-tile (digit suit) #:prefab)


(define M 'm)
(define m 'm)
(define P 'p)
(define p 'p)
(define S 's)
(define s 's)
(define Z 'z)
(define z 'z)
