#lang racket

(struct maze (N M tbl))
(define (connections tbl c) (dict-ref tbl c '()))
(define (connect! tbl c n) 
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))
(define (connected? tbl a b) (member a (connections tbl b)))
(define (build-maze N M)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter 
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  (let move-to-cell ([c (list (random N) (random M))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      (connect! tbl c n)
      (move-to-cell n)))
  (maze N M tbl))

(define (show-maze m)
  (match-define (maze N M tbl) m)
  (for ([i N]) (display "+---"))
  (displayln "+")
  (for ([j M])
    (display "|")
    (for ([i (- N 1)])
      (if (connected? tbl (list i j) (list (+ 1 i) j))
          (display "    ")
          (display "   |")))
    (display "   |")
    (newline)
    (for ([i N])
      (if (connected? tbl (list i j) (list i (+ j 1)))
          (display "+   ")
          (display "+---")))
    (displayln "+"))
  (newline))


;; Returns a path connecting two given cells in the maze
;; find-path :: Maze Cell Cell -> (Listof Cell)
(define (find-path m p1 p2)
  (match-define (maze N M tbl) m)
  (define (alternatives p prev) (remove prev (connections tbl p)))
  (define (dead-end? p prev) (empty? (alternatives p prev)))
  (define ((next-turn route) p)
    (define prev (car route))
    (cond
      [(equal? p p2) (cons p2 route)]
      [(dead-end? p prev) '()]
      [else (append-map (next-turn (cons p route)) 
                        (alternatives p prev))])) 
  (reverse 
   (append-map (next-turn (list p1)) 
               (alternatives p1 (list p1)))))


;; Shows a maze with a path connecting two given cells
(define (show-path m p1 p2)  
  (match-define (maze N M tbl) m)
  (define route (find-path m p1 p2 ))
  (for ([i N]) (display "+---"))
  (displayln "+")
  (for ([j M])
    (display "|")
    (for ([i (- N 0)])
      (if (member (list i j) route)
          (display " *")
          (display "  "))
      (if (connected? tbl (list i j) (list (+ 1 i) j))
          (display "  ")
          (display " |")))
    (newline)
    (for ([i N])
      (if (connected? tbl (list i j) (list i (+ j 1)))
          (display "+   ")
          (display "+---")))
    (displayln "+"))
  (newline))


(define m (build-maze 10 10))
(show-maze m)
(show-path m '(0 0) '(9 9))

;
;(match-define (maze N M tbl) m)
;
;(define (alternatives p prev) (remove prev (connections tbl p)))
;
;(define (dead-end? p prev) (empty? (alternatives p prev)))
;
;(define p1 '(0 0))
;(define p2 '(2 2))
;
;(define ((next-turn route) p)
;  (define prev (car route))
;  (cond
;    [(equal? p p2) (cons p2 route)]
;    [(dead-end? p prev) '()]
;    [else (append-map (next-turn (cons p route)) 
;                      (alternatives p prev))]))
;
;   (append-map (next-turn (list p1)) 
;               (alternatives p1 (list p1)))


