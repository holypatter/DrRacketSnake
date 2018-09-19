;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |0.1 Snake|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Snake Game

;; =================
;; Constants:
(define SIZE 10) ;pixel size of the player body and enemies
(define WIDTH 50)
(define HEIGHT 50)
(define MTS (empty-scene (* SIZE WIDTH) (* SIZE HEIGHT)))
(define BODY-COLOR "black")
(define FOOD-COLOR "black")
(define BODY-PART (rectangle SIZE SIZE "solid" BODY-COLOR))
(define FOOD-PART (rectangle SIZE SIZE "outline" FOOD-COLOR))
(define SCORETEXT-COLOR "blue")
(define SCORETEXT-X 25)
(define SCORETEXT-Y 10)
(define SCORETEXT-SIZE 10)
(define TICK/SEC 10)



;; =================
;; Data definitions:

;; Direction is Natural[0,3]
;; interp. a direction
;;  - 0: right
;;  - 1: down
;;  - 2: left
;;  - 3: up
(define RIGHT 0)
(define DOWN 1)
(define LEFT 2)
(define UP 3)


(define-struct position (x y))
;; position is Compound(Natural, Natural)
;; interp. the x and y positions of an object
(define P1 (make-position 10 10))
(define P2 (make-position 20 10))

#;
(define (fn-for-pos pos)
  (... (position-x pos)
       (position-y pos))
  )

;; ListOfPositions is one of:
;;  - empty
;;  - (cons Position ListOfPositions)

#;
(define (fn-for-lop lop)
  (cond
    [(empty? lop) (...)]
    [else (... (first lop)
          (fn-for-lop (rest lop)))]
    )
  )


(define-struct player (dir body))
;; player is Compound(direction ListOfPositions)
;; interp. a player with direction dir
;;                   and ListOfPositions body

#;
(define (fn-for-player player)
  (... (player-dir player)
       (player-body player))
  )

(define-struct game (player score enemy))
;; Game is Compound(ListOfPositions Natural[0,inf) Position)
;; interp.
;;  player is ListOfPositions
;;  score is Natural[0,inf) (score in the game)
;;  enemy is Position of the enemy

#;
(define (fn-for-game game)
  (... (game-player game)
       (game-score game)
       (game-enemy game))
  )

;; =================
;; Functions:

;; Game -> Game
;; Snake Game
;; 
(define (main g)
  (big-bang g                    ; Game
            (on-tick   tock (/ 1 TICK/SEC))     ; Game -> Game
            (to-draw   render)   ; Game -> Image
            (on-key    keyed)))    ; Game KeyEvent -> Game

;; Game -> Game
;; move to next game state
;; player moves forward
;;  - onto enemy
;;    - extend body length
;;    - spawn next enemy
;;  - onto self
;;    - reset game
;;  - onto wall
;;    - reset game
;;  - onto nothing
;;    - move body forwards
(define BUFFER 0)
(define (tock g)
  (cond
    ;if eat an enemy, grow longer, increment score
    [(position=? (next-pos (game-player g)) (game-enemy g))
     (make-game (make-player (player-dir (game-player g)) (cons (next-pos (game-player g)) (player-body (game-player g)))) (+ 1 (game-score g)) (spawn-position (player-body (game-player g))))
     ]
    ;if passes X or Y bounds, reset game
    [(or (< (position-x (next-pos (game-player g))) BUFFER) (> (position-x (next-pos (game-player g))) (- WIDTH BUFFER))
         (< (position-y (next-pos (game-player g))) BUFFER) (> (position-y (next-pos (game-player g))) (- HEIGHT BUFFER)))
     INITIAL-GAME]
    ;if touches itself, reset game
    [(contains-pos? (next-pos (game-player g)) (player-body (game-player g)))
     INITIAL-GAME]
    ;otherwise, just move forwards
    [else
     (make-game (make-player (player-dir (game-player g)) (cons (next-pos (game-player g)) (push (player-body (game-player g))))) (game-score g) (game-enemy g))
     ]
    )
  )

;; Game -> Image
;; render ... 
;;
(define (render g)
  ;; get body using render-body, place enemy over body, place score over result
  (place-image (render-score (game-score g)) SCORETEXT-X SCORETEXT-Y
  (place-image FOOD-PART (* SIZE (position-x (game-enemy g))) (* SIZE (position-y (game-enemy g))) (render-body (player-body (game-player g))))
  ))

;; Number -> Image
;; returns the score# as a a text image
(define (render-score score)
  (text (string-append "Score: " (number->string score)) SCORETEXT-SIZE SCORETEXT-COLOR)
  )

;; ListOfPositions -> Image
;; return an image of all positions over MTS
(define (render-body body)
  (if (empty? body)
      MTS
      (place-image BODY-PART (* SIZE (position-x (first body))) (* SIZE (position-y (first body))) (render-body (rest body)))
  )
)

;; Game KeyEvent -> Game
;; check keys, set player direction to new Key, can't be exact opposite
(define (keyed g ke)
  (cond
    [(key=? ke "d")
     (if (= (modulo (player-dir (game-player g)) 2) 0) ;player is moving horizontal
         g
         (make-game (make-player RIGHT (player-body (game-player g))) (game-score g) (game-enemy g)))]  ;right
    [(key=? ke "s")
     (if (= (modulo (player-dir (game-player g)) 2) 1) ;player is moving vertical
         g
         (make-game (make-player DOWN (player-body (game-player g))) (game-score g) (game-enemy g)))]  ;down
    [(key=? ke "a")
     (if (= (modulo (player-dir (game-player g)) 2) 0) ;player is moving horizontal
         g
         (make-game (make-player LEFT (player-body (game-player g))) (game-score g) (game-enemy g)))]  ;left
    [(key=? ke "w")
     (if (= (modulo (player-dir (game-player g)) 2) 1) ;player is moving vertical
         g
         (make-game (make-player UP (player-body (game-player g))) (game-score g) (game-enemy g)))]  ;up
    [else g] ;otherwise don't do anything
    )
  )


;; Player Position -> Boolean
;; return true if the player's next position is equal to pos
(define (next-pos? player pos)
  (position=? pos (get-pos player))
  )

;; List -> List
;; remove the last element in a list, keep in the same order
(define (push list)
  (if (or (empty? list) (= (cons-length list) 1)) ;if list has 1 or less elements
      empty
      (cons (first list) (push (rest list)))
      )
  )

;; Player -> Position
;; return the current position of a player
(define (get-pos player)
  (first (player-body player))
  )

;; Player -> Position
;; return the next position of a player
(define (next-pos player)
  (cond
    [(= (player-dir player) RIGHT) (make-position (+ 1 (position-x (get-pos player))) (position-y (get-pos player)))]
    [(= (player-dir player) UP)    (make-position (position-x (get-pos player)) (- (position-y (get-pos player)) 1))]
    [(= (player-dir player) LEFT)  (make-position (- (position-x (get-pos player)) 1) (position-y (get-pos player)))]
    [(= (player-dir player) DOWN)  (make-position (position-x (get-pos player)) (+ (position-y (get-pos player)) 1))]
    )
  )

;; Position Position -> Boolean
;; return true if the positions are equal
(define (position=? pos1 pos2)
  (and (= (position-x pos1) (position-x pos2)) (= (position-y pos1) (position-y pos2)))
  )

;; Position -> Number
;; turn a 2d matrix into 1d based on width and height
(define (position->number pos)
  (+ (position-x pos) (* (position-y pos) WIDTH))
  )

;; ListOfPositions -> ListOfNumbers
;; converts all positions into numbers, BUT in reverse order
(define (lop->lon lop)
  (if (empty? lop)
      empty
      (cons (position->number (first lop)) (lop->lon (rest lop))))
  )

;; Number -> Position
;; turn a 1d matrix into 2d based on width and height
(define (number->position num)
  (make-position (modulo num WIDTH) (floor (/ num WIDTH)))
  )

;; Number -> ListOfNumbers
;; produce a list of numbers from 0 to Number-1 excluding a list of numbers
(check-expect (num-to-list 1 empty) (cons 1 empty))
(check-expect (num-to-list 2 empty) (cons 2 (cons 1 empty)))

(define (num-to-list n exclude)
  (if (= 0 n)
      empty
      (if (contains-num? n exclude)
          (num-to-list (- n 1) exclude)
          (cons n (num-to-list (- n 1) exclude))
          )
      )
  )

;; Number ListOfNumbers -> Boolean
;; return true if a list of numbers contains number
(define (contains-num? n lon)
  (if (empty? lon)
      false
      (if (= (first lon) n)
          true
          (contains-num? n (rest lon))
          )
      )
  )

;; Position ListOfPositions -> Boolean
;; returns true if a list of positions contains a position
(define (contains-pos? p lop)
  (if (empty? lop)
      false
      (if (position=? (first lop) p)
          true
          (contains-pos? p (rest lop))
          )
      )
  )

;; ListOfNumbers Number -> Number
;; return the i'th number in the list, i < size of ListOfNumbers
(define (cons-ith lon n)
  (if (= n 0)
      (first lon)
      (cons-ith (rest lon) (- n 1)))
  )

;; List -> Number
;; return the size of a list
(define (cons-length list)
  (if (empty? list)
      0
      (+ 1 (cons-length (rest list))))
  )

;; ListOfPositions -> Position
;; returns a random position that is not in the list
;; exclude list is the positions of the player snake
(define (spawn-position excludeList)
;; convert excludeList into numbers with (lop->lon excludeList)
;; generate all positions with           (num-to-list (* WIDTH HEIGHT) lonexcludeList)
;; select one at random with             (random n)
;; convert selected # back to pos with   (number->position)
;; return the position
;  (lop->lon excludeList)
;  (num-to-list (* WIDTH HEIGHT) (lop->lon excludeList))
;  (random (num-to-list (* WIDTH HEIGHT) (lop->lon excludeList)) (- (* WIDTH HEIGHT) (cons-length excludeList)))
  ;(make-position (+ 1 (random (- WIDTH 2)) 1) (+ 1 (random (- HEIGHT 2)) 1))
  (number->position (cons-ith  (num-to-list (* WIDTH HEIGHT) (lop->lon excludeList)) (random (- (* (- WIDTH 2) (- HEIGHT 2)) (cons-length excludeList)))))
)

(define INITIAL-POSITION (make-position (round (/ WIDTH 2)) (round (/ HEIGHT 2))))
(define INITIAL-SCORE 0)
(define INITIAL-PLAYER (make-player RIGHT
                                    (cons INITIAL-POSITION
                                    (cons INITIAL-POSITION
                                    (cons INITIAL-POSITION
                                    (cons INITIAL-POSITION empty))
                                    ))))
(define INITIAL-GAME (make-game INITIAL-PLAYER INITIAL-SCORE
                                (spawn-position (player-body INITIAL-PLAYER))
                                ;(make-position 40 40)
                                ))

(main INITIAL-GAME)