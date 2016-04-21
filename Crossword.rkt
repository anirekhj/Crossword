;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;The following line is REQUIRED (do not remove)
(require "puzlib.rkt")

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz09 (read-puzzle "puzzle09.txt"))
(define puzz05 (read-puzzle "puzzle05.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid-singles '((#\A) (#\B) (#\C)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g) consumes a Grid 'g' and swaps the the row
;; and column of each element in the Grid.
;; transpose: Grid -> Grid

;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))

(define (transpose g)
  (local [(define (modify new old)
            (cond [(empty? g) empty]
                  [(empty? (first old)) new]
                  [else (modify (cons (foldr (lambda (x v)
                                               (cons (first x) v)) empty old) new)
                                (map (lambda (x) (rest x)) old))]))]
    (reverse (modify empty g))))

;; Tests:
(check-expect (transpose empty) empty)
(check-expect (transpose grid-singles) '((#\A #\B #\C)))
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose (transpose grid-abc)) grid-abc)

;;****************************************************************************************************************************************************

;; (find-wpos loc row) takes a row 'loc' and the row number 'row and
;; produces a list of all horizontal WPos that occur in that row.
;; find-wpos: (listof Char) Nat -> (listof WPos)

;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list "##.#") 0)
              (list (make-wpos 0 0 true 2)))

(define (find-wpos loc row)
  (local [(define (word-info newlist grid-row col at-word?)
            (cond [(empty? grid-row) newlist]
                  [(and at-word? (char=? (first grid-row) #\#))
                   (word-info (cons (list (first (first newlist))
                                          (add1 (second (first newlist))))
                                    (rest newlist))
                              (rest grid-row)(add1 col) true)]
                  [(and (not at-word?) (char=? (first grid-row) #\#))
                   (word-info (cons (list col 1) newlist)
                              (rest grid-row) (add1 col) true)]
                  [else (word-info newlist (rest grid-row) (add1 col) false)]))]
    (filter (lambda (x) (not (empty? x))) (map (lambda (x) (cond [(< (second x) 2) empty]
                                          [else (make-wpos row (first x) true (second x))]))
                        (word-info empty loc 0 false)))))


;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
(check-expect (find-wpos (string->list "..##.#.##..") 4)
              (list (make-wpos 4 7 true 2) (make-wpos 4 2 true 2)))
(check-expect (find-wpos (string->list ".") 1) empty)

;; the order does not matter: here is an example
;; that uses lists-equiv?

(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)

;;****************************************************************************************************************************************************

;; (initial-state puzzle) consumes a Puzzle and
;; produces the initial State to start searching from.
;; initial-state: Puzzle -> State
;; requires: a non empty puzzle
;; Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))

(define (initial-state puzzle)
  (local [(define grid (map string->list (first puzzle)))
          (define (make-horiz lst-wpos)
            (map (lambda (x) (make-wpos (wpos-col x) (wpos-row x) false (wpos-len x))) lst-wpos))
          (define (all-wpos grid row horiz?)
            (cond [(empty? grid) empty]
                  [(not horiz?) (append (make-horiz (find-wpos (first grid) row))
                                (all-wpos (rest grid) (add1 row) horiz?))]
                  [else (append (find-wpos (first grid) row)
                                (all-wpos (rest grid) (add1 row) horiz?))]))]              
    (make-state grid (append (all-wpos grid 0 true)
                             (all-wpos (transpose grid) 0 false)) (second puzzle))))
  
;; Tests:
(check-expect (initial-state puzz03)
              (make-state
 (list (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
 (list
  (make-wpos 0 0 true 5)
  (make-wpos 1 0 true 5)
  (make-wpos 2 0 true 5)
  (make-wpos 3 0 true 5)
  (make-wpos 4 0 true 5)
  (make-wpos 0 0 false 5)
  (make-wpos 0 1 false 5)
  (make-wpos 0 2 false 5)
  (make-wpos 0 3 false 5)
  (make-wpos 0 4 false 5))
 (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS")))
(check-expect (initial-state puzz05)
              (make-state
 (list
  (list #\# #\# #\# #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.)
  (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
  (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
  (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
  (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
  (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
  (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
  (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
  (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
  (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
  (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
  (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
  (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
  (list #\. #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\# #\#))
 (list
  (make-wpos 0 0 true 7)
  (make-wpos 1 6 true 7)
  (make-wpos 2 0 true 7)
  (make-wpos 3 6 true 7)
  (make-wpos 4 0 true 7)
  (make-wpos 5 6 true 7)
  (make-wpos 6 0 true 7)
  (make-wpos 7 7 true 7)
  (make-wpos 8 1 true 7)
  (make-wpos 9 7 true 7)
  (make-wpos 10 1 true 7)
  (make-wpos 11 7 true 7)
  (make-wpos 12 1 true 7)
  (make-wpos 13 7 true 7)
  (make-wpos 0 0 false 7)
  (make-wpos 6 1 false 7)
  (make-wpos 0 2 false 7)
  (make-wpos 6 3 false 7)
  (make-wpos 0 4 false 7)
  (make-wpos 6 5 false 7)
  (make-wpos 0 6 false 7)
  (make-wpos 7 7 false 7)
  (make-wpos 1 8 false 7)
  (make-wpos 7 9 false 7)
  (make-wpos 1 10 false 7)
  (make-wpos 7 11 false 7)
  (make-wpos 1 12 false 7)
  (make-wpos 7 13 false 7))
 (list
  "ABALONE"
  "ACREAGE"
  "ADMIRER"
  "AIRHEAD"
  "APPOINT"
  "AWKWARD"
  "DYNASTY"
  "EARLOBE"
  "EXTRACT"
  "EXTREME"
  "EYEBROW"
  "FANFARE"
  "FRECKLE"
  "INNARDS"
  "MAESTRO"
  "PATTERN"
  "RAFTERS"
  "RAPPORT"
  "REFEREE"
  "REMORSE"
  "RESTART"
  "ROTUNDA"
  "SMOLDER"
  "SUNSPOT"
  "TERRACE"
  "THEATRE"
  "TORNADO"
  "TRAMWAY"
  "YARDAGE")))

;;****************************************************************************************************************************************************

;; (extract-wpos g wp) consumes a Grid 'g' and a Wpos 'wp' and
;; produces the list of characters corresponding to that word position.
;; extract-wpos: Grid WPos -> (listof Char)

;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local [(define (find-word row col length grid)
            (cond [(empty? grid) empty]
                  [(= 0 length) empty]
                  [(> row 0) (find-word (sub1 row) col length (rest grid))]
                  [(= row 0) (find-word -1 col length (first grid))]
                  [(> col 0) (find-word row (sub1 col) length (rest grid))]
                  [else (cons (first grid)
                              (find-word row col (sub1 length) (rest grid)))]))]
    (cond [(wpos-horiz? wp)(find-word
                            (wpos-row wp) (wpos-col wp) (wpos-len wp) g)]
          [else (find-word (wpos-col wp)
                           (wpos-row wp) (wpos-len wp) (transpose g))])))
                           
;; Tests:
(check-expect (extract-wpos grid-abc (make-wpos 0 10 false 2)) empty)
(check-expect (extract-wpos grid-singles (make-wpos 0 0 false 2))
              (list #\A #\B))
(check-expect (extract-wpos grid-singles (make-wpos 0 0 false 3))
              (list #\A #\B #\C))
(check-expect (extract-wpos empty (make-wpos 0 10 false 2)) empty)

;;****************************************************************************************************************************************************

;; (replace-wpos g wp loc) consumes a Grid 'g', a WPos 'wp'
;; and a list of characters 'loc' and produces the Grid with the
;; word position replaced by the word represented by the loc.
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)

;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local [(define (insert-word-v g row col loc)
            (cond [(empty? g) empty]
                  [(empty? loc) g]
                  [(> row 0) (cons (first g)
                                   (insert-word-v (rest g) (sub1 row) col loc))]
                  [(= row 0) (insert-word-v g -1 col loc)]
                  [else (cons (insert-in-lst col (first g) (first loc))
                                   (insert-word-v (rest g) row col (rest loc)))]))
          (define (insert-in-lst col lst char)
            (cond
              [(empty? lst) empty]
              [(> col 0) (cons (first lst) (insert-in-lst (sub1 col) (rest lst) char))]
              [else (cons char (rest lst))]))
          (define (insert-word g row col loc)
            (cond [(empty? g) empty]
                  [(empty? loc) g]
                  [(> row 0) (cons (first g)
                                   (insert-word (rest g) (sub1 row) col loc))]
                  [(= row 0) (cons (insert-word (first g) -1 col loc) (rest g))]
                  [(> col 0) (cons (first g)
                                   (insert-word (rest g) row (sub1 col) loc))]
                  [else (cons (first loc) (insert-word (rest g) row col (rest loc)))]))]
  (cond [(wpos-horiz? wp) (insert-word g (wpos-row wp) (wpos-col wp) loc)]
          [else (insert-word-v g (wpos-row wp) (wpos-col wp) loc)])))
             

;; Tests:
(check-expect (replace-wpos empty (make-wpos 0 0 true 2) '(#\J #\K))
              empty)
(check-expect (replace-wpos grid-singles (make-wpos 0 2 false 2) '(#\J #\K))
              (list (list #\A) (list #\B) (list #\C)))
(check-expect (replace-wpos grid-singles (make-wpos 0 0 true 0) '(#\J #\K))
              (list (list #\J) (list #\B) (list #\C)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 1) '(#\K))
              (list (list #\K #\B #\C) (list #\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 1) '(empty #\K))
              (list (list 'empty #\B #\C) (list #\K #\Y #\Z)))

;;****************************************************************************************************************************************************

;; (fit? word cells) consumes a word and a list of character
;; that represents the word position in the puzzle.
;; fit?: (listof Char) (listof Char) -> Bool

;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (cond [(and (empty? word) (empty? cells)) true]
        [(and (empty? word) (not (empty? cells))) true]
        [(and (not (empty? word)) (empty? cells)) false]
        [(char=? (first word) (first cells))
         (fit? (rest word) (rest cells))]
        [(char=? #\# (first cells))
         (fit? (rest word) (rest cells))]
        [else false]))

;; Tests:
(check-expect (fit? (string->list "apple") (string->list "####e")) true)
(check-expect (fit? (string->list "apple") (string->list "#.##e")) false)
(check-expect (fit? (string->list "hey how are you?") (string->list "hey how are you?")) true)
(check-expect (fit? (string->list " ") (string->list " ")) true)
(check-expect (fit? (string->list "qwerty") (string->list " ")) false)
 
;;****************************************************************************************************************************************************

;; (neighbours s) consumes a State 's' and produces a list of states
;; that represent valid neighbour states with one additional word
;; placed in the puzzle.
;; neighbours: State -> (listof State)

;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local [(define (current-wpos-fast lowp max-char current)
            (cond [(empty? lowp) current]
                  [(> (count-char (state-grid s) (first lowp)) max-char)
                   (current-wpos-fast (rest lowp) (count-char (state-grid s) (first lowp)) (first lowp))]
                  [else (current-wpos-fast (rest lowp) max-char current)]))
          (define (count-char grid wp)
            (foldr (lambda (a b) (cond [(char=? a #\#) b]
                                       [else (add1 b)])) 0 (extract-wpos grid wp))) 
          (define current-wpos (current-wpos-fast (state-positions s) -1 empty)) ;;(first (state-positions s))
          (define wpos-char (extract-wpos (state-grid s) current-wpos))
          (define (word-fits? word)
            (fit? (string->list word) wpos-char))
          (define (remove-not-fit low)
            (filter (lambda (x) (word-fits? x)) low))]
    (map (lambda (v)
           (make-state
            (replace-wpos (state-grid s) current-wpos (string->list v))
            (filter (lambda (y) (not (equal? y current-wpos))) (state-positions s))
            (filter (lambda (z) (not (string=? z v))) (state-words s))))            
         (remove-not-fit (state-words s)))))
            

;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

 (check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

;(disp (criss-cross (read-puzzle "puzzle03.txt")))