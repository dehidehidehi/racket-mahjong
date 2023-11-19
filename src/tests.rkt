(module mahjong-tests typed/racket
  (require "main.rkt")
  (require typed/rackunit)
  (require typed/rackunit/text-ui)

  (define (T [val : String]) (Tile val 0))

  (define t1 (T "1"))
  (define t2 (T "2"))
  (define t3 (T "3"))
  (define t4 (T "4"))
  (define t5 (T "5"))

  (define tile-paired?-test
    (test-suite "tile-paired?"
                (check-false (tile-paired? t1 t2))
                (check-true (tile-paired? t3 t3))))

  (define pair-tiles-test
    (test-suite "pair-tiles"
                (check-equal? 'empty (pair-tiles empty))
                (check-equal? 'empty (pair-tiles (list t1)))
                ))

  (define count-pairs-test
    (test-suite "count-pairs"
                ;;		(check-equal? 0 (count-pairs '()))
                ;		(check-equal? 0 (count-pairs (list (T "a"))))
                ;		(check-equal? 0 (count-pairs (list (T "a") (T "b"))))
                ;		(check-equal? 1 (count-pairs (list (T "a") (T "a"))))
                ;		(check-equal? 1 (count-pairs (list (T "a") (T "a") (T "a"))))
                ;		(check-equal? 2 (count-pairs (list (T "a") (T "a") (T "a") (T "a"))))
                (check-equal? 2 (count-pairs (list (T "a") (T "a") (T "b") (T "b"))))
                (check-equal? 2 (count-pairs (list (T "b") (T "a") (T "a") (T "b"))))
                ))

  (run-tests tile-paired?-test)
  (run-tests pair-tiles-test)
  ;(run-tests count-pairs-test)
  )
