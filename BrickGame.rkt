;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |pong real 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

#|--- DATA DEFINITIONS ---|#
;;A Ball is a (make-ball Number Number Number Number)
(define-struct ball (x y vx vy))
; - where the first Number is the ball's x-coordinate
; - the second Number is the ball's y-coordinate
; - the third Number is the ball's x-velocity
; - the fourth Number is the ball's y-velocity

#;(define (ball-temp ball)
    (...(ball-x ball)...(ball-y ball)...(ball-vx ball)...(ball-vy ball)...))

;; A Brick is a (make-brick (Number Number))
(define-struct brick[x y])
; - the first Number is the brick's x position
; - the second Number is the bricks's y position

#;(define (brick-temp brick)
    (...(brick-x brick)...(brick-y brick)...))


;; A LoB is one of
; -'()
; -(cons Brick LoB)
#;(define (lob-temp lob)
    (cond
      [(empty? lob)...]
      [(cons? lob)
       (...(brick-temp (first lob))...(lob-temp (rest lob))...)]))

;; A LLOB is one of
; -'()
; -(cons LoB LLoB)

#; (define (llob-temp llob)
     (cond
       [(empty? llob)...]
       [(cons? llob)
        (...(lob-temp (first llob))...(llob-temp (rest llob))...)]))


;; A World is one of:
; - "lost"
; - "win"
; - (make-world Number Ball LLOB Lives)
(define-struct world[paddle ball bricks lives])
; -where paddle is a Number which represents the x-coordinate of the paddle
; -where ball is the Ball
; -where bricks is the List of List of Bricks
; -where lives is a Number indicating the lives the player has

#; (define (world-temp w)
     (...(world-paddle w)...(ball-temp (world-ball w))...
         (llob-temp (world-bricks w))...(world-lives w)))



#|--- CONSTANTS ---|# 

(define WIDTH 200)
(define HEIGHT 200)

(define BALL-COLOR 'blue)
(define BALL-RADIUS 6)
(define BALL-SPEED 4)

(define BRICK-COLOR 'red)
(define BRICK-WIDTH 30)
(define BRICK-HEIGHT 10)
(define BRICK-PADDING 10)
(define ROWS 3)
(define COLUMNS 5)

(define PADDLE-COLOR 'purple)
(define PADDLE-WIDTH 40)
(define PADDLE-HEIGHT BRICK-HEIGHT)
(define PADDLE-Y (- HEIGHT (/ PADDLE-HEIGHT 2)))
(define PADDLE-SPEED 5)

(define INITIAL-BALL (make-ball (/ WIDTH 2)
                                (- HEIGHT PADDLE-HEIGHT BALL-RADIUS)
                                0 BALL-SPEED))
(define brick1 (make-brick 50 40))
(define paddle1 (/ WIDTH 2))
(define ROW1 (list (make-brick 10 40)
                   (make-brick 50 40)
                   (make-brick 90 40)
                   (make-brick 130 40)))
(define ROW-COLUMN (list
                    (list (make-brick 10 40)
                          (make-brick 50 40)
                          (make-brick 90 40))
                    (list (make-brick 10 60)
                          (make-brick 50 60)
                          (make-brick 90 60))))
(define BALL (circle BALL-RADIUS "solid" BALL-COLOR))
(define BG (empty-scene WIDTH HEIGHT))
(define LIVES 5)
(define LOST-SCREEN (text "you lost" 33 "pink"))
(define WIN-SCREEN (text "you won" 33 "pink"))
(define LOST "lost")
(define WIN "win")
(define BRICK (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR))
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR))


#|--- FUNCTIONS ---|#
;; make-row: Brick Number -> LOB
;; draw the bricks in a row on the screen
(check-expect (make-row brick1 2)
              (cons brick1
                    (cons (make-brick 90 40) '())))
(check-expect (make-row brick1 0) '())

(define (make-row b n)
  (if (not (= n 0))
      (cons b (make-row (make-brick (+ BRICK-WIDTH BRICK-PADDING (brick-x b)) (brick-y b))
                        (sub1 n)))
      '()))

;; make-grid: Brick Number Number -> LLOB
; draws the grid of the bricks on the screen
(check-expect (make-grid brick1 2 2) (cons (cons brick1 (cons (make-brick 90 40) '()))
                                           (cons (cons (make-brick 50 60)
                                                       (cons (make-brick 90 60) '())) '())))
(check-expect (make-grid brick1 0 0) '())

(define (make-grid b x y)
  (if (not (= y 0))
      (cons (make-row b x)
            (make-grid (make-brick (brick-x b) (+ BRICK-HEIGHT BRICK-PADDING (brick-y b)))
                       x (sub1 y))) '()))

;; draw-all: World -> Image
;; given a ball, a paddle, bricks, and lives render as a large image
(check-expect (draw-all "lost") (place-image LOST-SCREEN (/ WIDTH 2) (/ HEIGHT 2) BG))
(check-expect (draw-all "win") (place-image WIN-SCREEN (/ WIDTH 2) (/ HEIGHT 2) BG))
(check-expect
 (draw-all (make-world paddle1 INITIAL-BALL (make-grid brick1 2 2) LIVES))
 (place-image
  PADDLE 100 195
  (place-image BRICK 50 40
               (place-image BRICK 90 40
                            (place-image BRICK 50 60
                                         (place-image BRICK 90 60
                                                      (place-image BALL 100 184
                                                                   (place-image
                                                                    (text
                                                                     (number->string LIVES) 20
                                                                     "green") 170 20 BG))))))))

(define (draw-all w)
  (cond
    [(string? w)
     (if (string=? "lost" w)
         (place-image LOST-SCREEN (/ WIDTH 2) (/ HEIGHT 2) BG)
         (place-image WIN-SCREEN (/ WIDTH 2) (/ HEIGHT 2) BG))] 
    [else (draw-paddle (world-paddle w)
                       (draw-bricks-grid (world-bricks w)
                                         (draw-ball (world-ball w)
                                                    (draw-lives (world-lives w) BG))))]))


;; draw-paddle: Number Image -> Image
;; draws the paddle on the screen
(check-expect (draw-paddle (/ WIDTH 2) BG) (place-image PADDLE 100 195 BG)) 
(check-expect (draw-paddle 50 BG) (place-image PADDLE 50 195 BG)) 

(define (draw-paddle x img)
  (place-image PADDLE x PADDLE-Y img))

;; draw-ball: Ball Image -> Image
;; given a ball and an image draws a ball on the screen
(check-expect (draw-ball INITIAL-BALL BG) (place-image BALL 100 184 BG))
(check-expect (draw-ball (make-ball 10 10 3 3) BG) (place-image BALL 10 10 BG))

(define (draw-ball ball img)
  (place-image BALL (ball-x ball) (ball-y ball) img))

;; draw-bricks-row: LOB Image-> Image
;; draw the brick on the screen
(check-expect (draw-bricks-row ROW1 BG)
              (place-image BRICK 10 40
                           (place-image BRICK 50 40
                                        (place-image BRICK 90 40 (place-image BRICK 130 40 BG)))))
(check-expect (draw-bricks-row '() BG) BG)

(define (draw-bricks-row lob img)
  (cond
    [(empty? lob) img]
    [else
     (place-image BRICK (brick-x (first lob)) (brick-y (first lob))
                  (draw-bricks-row (rest lob) img))]))


;; draw-bricks-grid LLOB Image-> Image
;; draws the list of list of bricks on the screen
(check-expect
 (draw-bricks-grid ROW-COLUMN BG)
 (place-image BRICK 10 40
              (place-image BRICK 50 40
                           (place-image BRICK 90 40
                                        (place-image BRICK 10 60
                                                     (place-image BRICK 50 60
                                                                  (place-image BRICK 90 60 BG)
                                                                  ))))))
(define (draw-bricks-grid llob img)
  (cond
    [(empty? llob) img]
    [(cons? llob)
     (place-image BRICK (brick-x (first (first llob))) (brick-y (first (first llob)))
                  (draw-bricks-grid (rest llob) (draw-bricks-row (first llob) img)))]))

;; draw-lives: Number Image -> Image
;; draws the number representing lives on the screen 
(check-expect (draw-lives 4 BG) (place-image (text "4" 20 "green") 170 20 BG))
(check-expect (draw-lives 3 BG) (place-image (text "3" 20 "green") 170 20 BG))
              
(define (draw-lives l img) 
  (place-image (text (number->string l) 20 "green") 170 20 img))


;; change-paddle: World KeyEvent -> World
;; changes the world state
(check-expect (change-paddle W0 "right")
              (make-world 105 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) LIVES))
(check-expect (change-paddle W0 "left")
              (make-world 95 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) LIVES))
(check-expect (change-paddle W1 "left") W1)
(check-expect (change-paddle W2 "right") W2)
(check-expect (change-paddle "lost" "right") "lost")

(define (change-paddle w k)
  (cond
    [(string? w) w]
    [else (make-world (move-paddle (world-paddle w) k)
                      (world-ball w) (world-bricks w) (world-lives w))]))

;; move-paddle: Number KeyEvent -> Number
;; when the left and right arrow keys are pressed, a new paddle with the new y coordinates
;; is created
(check-expect (move-paddle 100 "left") 95)
(check-expect (move-paddle 100 "right") 105)
(check-expect (move-paddle 20 "left") 20)
(check-expect (move-paddle 180 "right") 180)

(define (move-paddle n k)
  (cond
    [(and (key=? k "left") (not (border? n k))) (- n PADDLE-SPEED)]
    [(and (key=? k "right") (not (border? n k))) (+ n PADDLE-SPEED)] 
    [else n]))

;; border?: Number KeyEvent -> Boolean
;; determines if the paddle is out of bounds
(check-expect (border? 100 "left") #false)
(check-expect (border? 100 "right") #false)
(check-expect (border? 20 "left") #true)
(check-expect (border? 180 "right") #true)
(check-expect (border? 180 "help") #false)

(define (border? n k)
  (cond
    [(key=? k "left") (< (- n PADDLE-SPEED) (/ PADDLE-WIDTH 2))]
    [(key=? k "right") (> (+ n PADDLE-SPEED) (- WIDTH (/ PADDLE-WIDTH 2)))]
    [else #false])) 


;; change-world: World -> World
;; changes the state of the world
(define (change-world w)
  (cond
    [(lost w) "lost"]
    [(win w) "win"]
    [else (collision w (move-ball (world-ball w)))]))

(check-expect (change-world (make-world paddle1 INITIAL-BALL (make-grid brick1 1 1) 0)) "lost")
(check-expect (change-world (make-world paddle1 INITIAL-BALL (make-grid brick1 0 0) 2)) "win")
(check-expect (change-world (make-world paddle1 (make-ball 170 10 3 3) (make-grid brick1 1 1) 2))
              (make-world paddle1 (make-ball 173 13 3 3) (make-grid brick1 1 1) 2))

;; stop: World -> Boolean
;; gives the game a static screen when player wins or loses
(define (stop w)
  (string? w))

(check-expect (stop "win") #true)
(check-expect (stop W1) #false)


; move-ball: Ball -> Ball
; given a ball, makes a new ball with the new x and y positions
(check-expect (move-ball (make-ball 5 5 5 5)) (make-ball 10 10 5 5))
(check-expect (move-ball (make-ball 25 25 -5 5)) (make-ball 20 30 -5 5))
(check-expect (move-ball (make-ball 25 25 5 -5)) (make-ball 30 20 5 -5))
(check-expect (move-ball (make-ball 25 25 0 0)) (make-ball 25 25 0 0))

(define (move-ball b)
  (make-ball (+ (ball-x b) (ball-vx b)) (+ (ball-y b) (ball-vy b)) (ball-vx b) (ball-vy b)))


;; collision : World Ball-> World
;; given a world and a ball, check to see if the ball collides, and how it collides and outputs
;; the appropriate world, relating to the way it collided with something
(define (collision w b)
  (cond
    [(collision-ground? b) (reset w)]
    [(collision-row? (world-bricks w) b) (less-bricks w b)]
    [(paddle-side (world-paddle w) b) (change-ball-vx w b)]
    [(collision-paddle? (world-paddle w) b)
     (make-world (world-paddle w)
                 (launch-ball b (world-paddle w)) (world-bricks w) (world-lives w))]
    [(collision-wall? b) (off-wall w b)]
    [else (make-world (world-paddle w) b (world-bricks w) (world-lives w))]))

(check-expect (collision W0 (make-ball 10 194 3 3))
              (make-world paddle1 (make-ball 100 184 3 3) (make-grid brick1 3 5) 4))
(check-expect (collision (make-world paddle1 (make-ball 20 40 3 3)
                                     (list (list (make-brick 40 40) (make-brick 80 40))
                                           (list (make-brick 40 80) (make-brick 80 80))) 4)
                         (make-ball 20 40 3 3))
              (make-world 100 (make-ball 20 40 -3 3)
                          (list (list (make-brick 80 40)) (list (make-brick 40 80)
                                                                (make-brick 80 80))) 4))
(check-expect (collision (make-world 100 (make-ball 100 184 0 3)
                                     (list (list (make-brick 40 40) (make-brick 80 40))
                                           (list (make-brick 40 80) (make-brick 80 80))) 4)
                         (make-ball 100 184 0 3))
              (make-world 100 (make-ball 100 181 0 -3)
                          (list (list (make-brick 40 40) (make-brick 80 40))
                                (list (make-brick 40 80) (make-brick 80 80))) 4))
(check-expect (collision (make-world 100 (make-ball 110 193 3 3)
                                     (list (list (make-brick 40 40) (make-brick 80 40))) 4)
                         (make-ball 110 193 3 3))
              (make-world 100 (make-ball 110 193 -3 3)
                          (list (list (make-brick 40 40) (make-brick 80 40))) 4))
(check-expect (collision (make-world 100 (make-ball 100 6 4 5)
                                     (list (list (make-brick 40 40) (make-brick 80 40))) 4)
                         (make-ball 100 6 4 5))
              (make-world 100 (make-ball 100 6 4 -5)
                          (list (list (make-brick 40 40) (make-brick 80 40))) 4))
(check-expect (collision (make-world 100 (make-ball 10 10 3 3)
                                     (list (list (make-brick 40 40) (make-brick 80 40))) 4)
                         (make-ball 10 10 3 3))
              (make-world 100 (make-ball 10 10 3 3)
                          (list (list (make-brick 40 40) (make-brick 80 40))) 4))

;; collision-row?: LoB Ball -> Boolean
;; determines if the ball hit any bricks in the given row of bricks
(check-expect (collision-row? (list (list (make-brick 40 40) (make-brick 80 40))
                                    (list (make-brick 40 80) (make-brick 80 80)))
                              (make-ball 20 40 3 3))
              #true)
(check-expect (collision-row?
               (list (list (make-brick 40 40) (make-brick 80 40))
                     (list (make-brick 40 80) (make-brick 80 80)))
               (make-ball 100 40 3 3))
              #true)
(check-expect (collision-row? (list (list (make-brick 40 40) (make-brick 80 40))
                                    (list (make-brick 40 80) (make-brick 80 80)))
                              (make-ball 120 70 3 3))
              #false)

(define (collision-row? llob b)
  (cond
    [(empty? llob) #false]
    [(cons? (first llob))
     (or (collision-row-helper (first llob) b) (collision-row? (rest llob) b))]))

;; collision-row-helper: LoB Ball -> Boolean
;; checks if the ball hits any brick in ONE row 
(define (collision-row-helper lob b)
  (cond
    [(empty? lob) #false]
    [(brick? (first lob))
     (or (brick-collide (first lob) b) (collision-row-helper (rest lob) b))]))

(check-expect (collision-row-helper (list) (make-ball 30 30 3 3)) #false)
(check-expect (collision-row-helper (list (make-brick 40 40) (make-brick 80 40))
                                    (make-ball 40 40 3 3)) #true)
(check-expect (collision-row-helper (list (make-brick 40 40) (make-brick 80 40))
                                    (make-ball 100 100 3 3)) #false)

;; brick-collide: Brick Ball -> Boolean
;; determines if the ball hit the given brick
(check-expect (brick-collide (make-brick 40 40) (make-ball 20 40 3 3)) #true)
(check-expect (brick-collide (make-brick 40 40) (make-ball 30 30 3 3)) #true)
(check-expect (brick-collide (make-brick 40 40) (make-ball 10 10 3 3)) #false)
(check-expect (brick-collide (make-brick 40 40) (make-ball 35 50 3 3)) #true)
(check-expect (brick-collide (make-brick 40 40) (make-ball 60 44 3 3)) #true)
(check-expect (brick-collide (make-brick 40 40) (make-ball 60 55 3 3)) #false)
(check-expect (brick-collide (make-brick 40 40) (make-ball 40 10 3 3)) #false)
(check-expect (brick-collide (make-brick 40 40) (make-ball 60 49 3 3)) #true)

(define (brick-collide br ball)
  (and (hit-brick-x br ball) (hit-brick-y br ball)))
 

;; hit-brick-x: Brick Ball -> Boolean
;; determines if the ball touches the given brick along its width
(check-expect (hit-brick-x (make-brick 40 40) (make-ball 10 40 3 3)) #false)
(check-expect (hit-brick-x (make-brick 40 40) (make-ball 20 40 3 3)) #true)
(check-expect (hit-brick-x (make-brick 40 40) (make-ball 30 40 3 3)) #true)
(check-expect (hit-brick-x (make-brick 40 40) (make-ball 60 40 3 3)) #true)
(check-expect (hit-brick-x (make-brick 40 40) (make-ball 80 40 3 3)) #false)

(define (hit-brick-x br ball)
  (and (>= (+ (ball-x ball) BALL-RADIUS) (- (brick-x br) (/ BRICK-WIDTH 2)))
       (<= (- (ball-x ball) BALL-RADIUS) (+ (brick-x br) (/ BRICK-WIDTH 2)))))

;; hit-brick-y: Brick Ball -> Boolean
;; determines if the ball touches the given brick along its height
(check-expect (hit-brick-y (make-brick 40 40) (make-ball 30 10 3 3)) #false)
(check-expect (hit-brick-y (make-brick 40 40) (make-ball 30 30 3 3)) #true)
(check-expect (hit-brick-y (make-brick 40 40) (make-ball 30 40 3 3)) #true)
(check-expect (hit-brick-y (make-brick 40 40) (make-ball 30 50 3 3)) #true)
(check-expect (hit-brick-y (make-brick 40 40) (make-ball 30 70 3 3)) #false)

(define (hit-brick-y br ball)
  (and (>= (+ (ball-y ball) BALL-RADIUS) (- (brick-y br) (/ BRICK-HEIGHT 2)))
       (<= (- (ball-y ball) BALL-RADIUS) (+ (brick-y br) (/ BRICK-HEIGHT 2)))))

;; collision-paddle?: Number Ball -> Boolean
;; given a x coordinate of the paddle and a ball, determine if the ball hit the paddle 
(check-expect (collision-paddle? 100 (make-ball 10 10 3 3)) #false)
(check-expect (collision-paddle? 100 (make-ball 100 184 3 3)) #true)
(check-expect (collision-paddle? 100 (make-ball 100 184 3 3)) #true)
(check-expect (collision-paddle? 100 (make-ball 100 190 3 3)) #true)
(check-expect (collision-paddle? 100 (make-ball 190 184 3 3)) #false)

(define (collision-paddle? p b)
  (and (paddle-x-collision p b) (paddle-y-collision p b)))

;; paddle-x-collision: Number Ball -> Boolean
;; determines if the ball is within the width of the paddle
(check-expect (paddle-x-collision 100 (make-ball 10 10 3 3)) #false)
(check-expect (paddle-x-collision 100  (make-ball 100 170 3 3)) #true)
(check-expect (paddle-x-collision 100  (make-ball 80 170 3 3)) #true)
(check-expect (paddle-x-collision 100 (make-ball 120 170 3 3)) #true)

(define (paddle-x-collision wp b)
  (and (>= (ball-x b) (- wp (/ PADDLE-WIDTH 2))) (<= (ball-x b) (+ wp (/  PADDLE-WIDTH 2)))))

;; paddle-y-collision: Number Ball -> Boolean
;; determines if part of the ball is below the top of the paddle
(check-expect (paddle-y-collision 100 (make-ball 10 10 3 3)) #false)
(check-expect (paddle-y-collision 100 (make-ball 100 184 3 3)) #true)
(check-expect (paddle-y-collision 100 (make-ball 100 187 3 3)) #true)

(define (paddle-y-collision wp b)
  (>= (+ (ball-y b) BALL-RADIUS) 190))

;; paddle-side: Number Ball -> Boolean
;; given the x coordinate of the paddle and a ball, determine if the ball hit the left or right
;; side of the ball
(define (paddle-side n b)
  (or (and (<= (- (ball-x b) BALL-RADIUS) (+ n (/ PADDLE-WIDTH 2)))
           (>= (ball-y b) (- PADDLE-Y (/ PADDLE-HEIGHT 2))))
      (and (>= (+ (ball-x b) BALL-RADIUS) (- n (/ PADDLE-WIDTH 2)))
           (>= (ball-y b) (- PADDLE-Y (/ PADDLE-HEIGHT 2))))))

(check-expect (paddle-side 100 (make-ball 90 194 3 3)) #true)
(check-expect (paddle-side 100 (make-ball 110 194 3 3)) #true)
(check-expect (paddle-side 100 (make-ball 10 10 3 3)) false)

;;collision-wall?: Ball -> Boolean
;; returns true if the ball hits a wall by checking if it is out of bounds
;; returns false if the ball is within bounds, meaning it did not hit a wall
(check-expect (collision-wall? (make-ball 160 100 40 40)) #false)
(check-expect (collision-wall? (make-ball 160 300 40 40)) #true)
(check-expect (collision-wall? (make-ball 250 140 40 40)) #true)
(check-expect (collision-wall? (make-ball 230 300 40 40)) #true)

(define (collision-wall? b)
  (not (and (ball-in-bound? (ball-x b) WIDTH) (ball-in-bound? (ball-y b) HEIGHT))))

;; ball-in-bound?: Number Number -> Boolean
;; takes the position of the x coordinate of ball and compares to the bounds of screen
(define (ball-in-bound? x n)
  (and (< (+ BALL-RADIUS x) n)
       (> (- x BALL-RADIUS) 0)))

(check-expect (ball-in-bound? 10 WIDTH) #true)
(check-expect (ball-in-bound? 10 HEIGHT) #true)
(check-expect (ball-in-bound? 0 WIDTH) #false)
(check-expect (ball-in-bound? 0 HEIGHT) #false)

;;collision-ground?: Ball -> Boolean
(define (collision-ground? b)
  (>= (+ BALL-RADIUS (ball-y b)) HEIGHT))

(check-expect (collision-ground? (make-ball 10 10 3 3)) #false)
(check-expect (collision-ground? (make-ball 10 194 3 3)) #true)

;; less-bricks: World Ball -> World
;; given a world and a ball, returns a world without the brick that was hit
(define (less-bricks w b)
  (make-world (world-paddle w) (new-ball-velocity (removed-brick (world-bricks w) b) b)
              (new-bricks (removed-brick (world-bricks w) b) (world-bricks w) b) (world-lives w)))

(check-expect (less-bricks (make-world 100 (make-ball 40 29 3 3) (list (list (make-brick 40 40)
                                                                             (make-brick 80 40))
                                                                       (list (make-brick 40 80)
                                                                             (make-brick 80 80)))4)
                           (make-ball 40 29 3 3))
              (make-world 100 (make-ball 40 29 3 -3) (list (list (make-brick 80 40))
                                                           (list (make-brick 40 80)
                                                                 (make-brick 80 80))) 4))
(check-expect (less-bricks (make-world 100 (make-ball 51 45 3 3) (list (list (make-brick 40 40)
                                                                             (make-brick 80 40))
                                                                       (list (make-brick 40 80)
                                                                             (make-brick 80 80))) 4)
                           (make-ball 51 45 3 3))
              (make-world 100 (make-ball 51 45 -3 3) (list (list (make-brick 80 40))
                                                           (list (make-brick 40 80)
                                                                 (make-brick 80 80)))4))

(check-expect (less-bricks (make-world 100 (make-ball 24 51 3 3)(list (list (make-brick 40 40)
                                                                            (make-brick 80 40))
                                                                      (list (make-brick 40 80)
                                                                            (make-brick 80 80))) 4)
                           (make-ball 24 51 3 3))
              (make-world 100 (make-ball 24 51 -3 -3) (list (list (make-brick 80 40))
                                                            (list (make-brick 40 80)
                                                                  (make-brick 80 80)))4))
         

;; removed-brick: LLoB Ball -> Brick
;; gets the brick that was hit
(define (removed-brick llob ball)
  (if (collision-row-helper (first llob) ball)
      (get-brick (first llob) ball)
      (removed-brick (rest llob) ball)))

(check-expect (removed-brick (list (list (make-brick 40 40) (make-brick 80 40))
                                   (list (make-brick 40 80) (make-brick 80 80)))
                             (make-ball 20 40 3 3)) (make-brick 40 40))

(check-expect (removed-brick (list (list (make-brick 40 40) (make-brick 80 40))
                                   (list (make-brick 40 80) (make-brick 80 80)))
                             (make-ball 20 80 3 3)) (make-brick 40 80))

(check-expect (removed-brick (list (list (make-brick 40 40) (make-brick 80 40))
                                   (list (make-brick 40 80) (make-brick 80 80)))
                             (make-ball 74 80 3 3)) (make-brick 80 80))


;; get-brick: LoB Ball -> Brick
;; gets the brick in the given row that was hit
(define (get-brick lob b)
  (if (brick-collide (first lob) b)
      (first lob)
      (get-brick (rest lob) b))) 

(check-expect (get-brick (list (make-brick 40 40) (make-brick 80 40)) (make-ball 20 40 3 3))
              (make-brick 40 40))
(check-expect (get-brick (list (make-brick 40 40) (make-brick 80 40)) (make-ball 101 40 3 3))
              (make-brick 80 40))

;; new-ball-velocity: Brick Ball -> Ball
;; changes the velocity of the ball depending on the side of that brick that it hit
(define (new-ball-velocity br b)
  (cond
    [(hit-brick-horizontally? br b)
     (make-ball (ball-x b) (ball-y b) (* -1 (ball-vx b)) (ball-vy b))]
    [(hit-brick-vertically? br b)
     (make-ball (ball-x b) (ball-y b) (ball-vx b) (* -1 (ball-vy b)))]
    [(hit-brick-corner? br b)
     (make-ball (ball-x b) (ball-y b) (* -1 (ball-vx b)) (* -1 (ball-vy b)))]))

(check-expect (new-ball-velocity (make-brick 40 40) (make-ball 20 40 3 3)) (make-ball 20 40 -3 3)) 
(check-expect (new-ball-velocity (make-brick 40 40) (make-ball 61 40 -3 3)) (make-ball 61 40 3 3))
(check-expect (new-ball-velocity (make-brick 40 40) (make-ball 30 29 3 -3)) (make-ball 30 29 3 3))
(check-expect (new-ball-velocity (make-brick 40 40) (make-ball 30 51 3 3)) (make-ball 30 51 3 -3)) 
(check-expect (new-ball-velocity (make-brick 40 40) (make-ball 23 49 3 3)) (make-ball 23 49 -3 -3))

;; hit-brick-horizontally?: Brick Ball -> Boolean
;; determines if the ball hit the left or right side of the given brick
(define (hit-brick-horizontally? br b)
  (and (hit-brick-x br b) (and (>= (ball-y b) (- (brick-y br) (/ BRICK-HEIGHT 2)))
                               (<= (ball-y b) (+ (brick-y br) (/ BRICK-HEIGHT 2))))))

(check-expect (hit-brick-horizontally? (make-brick 40 40) (make-ball 30 40 3 3)) #true)
(check-expect (hit-brick-horizontally? (make-brick 40 40) (make-ball 40 29 3 3)) #false)
(check-expect (hit-brick-horizontally? (make-brick 40 40) (make-ball 45 51 3 3)) #false)
(check-expect (hit-brick-horizontally? (make-brick 40 40) (make-ball 20 35 3 3)) #true)
(check-expect (hit-brick-horizontally? (make-brick 40 40) (make-ball 51 45 3 3)) #true)
(check-expect (hit-brick-horizontally? (make-brick 40 40) (make-ball 20 60 3 3)) #false)

;; hit-brick-vertically?: Brick Ball -> Boolean
;; determines if the ball hit the top or bottom side of the given brick
(define (hit-brick-vertically? br b)
  (and (hit-brick-y br b) (and (>= (ball-x b) (- (brick-x br) (/ BRICK-WIDTH 2)))
                               (<= (ball-x b) (+ (brick-x br) (/ BRICK-WIDTH 2))))))

(check-expect (hit-brick-vertically? (make-brick 40 40) (make-ball 40 19 3 3)) #false)
(check-expect (hit-brick-vertically? (make-brick 40 40) (make-ball 40 29 3 3)) #true)
(check-expect (hit-brick-vertically? (make-brick 40 40) (make-ball 45 51 3 3)) #true)
(check-expect (hit-brick-vertically? (make-brick 40 40) (make-ball 20 35 3 3)) #false)
(check-expect (hit-brick-vertically? (make-brick 40 40) (make-ball 51 60 3 3)) #false)
(check-expect (hit-brick-vertically? (make-brick 40 40) (make-ball 20 60 3 3)) #false)

;; hit-brick-corner?: Brick Ball -> Boolean
;; determines if the ball hit a corner of the given brick
(define (hit-brick-corner? br b)
  (and (hit-brick-x br b) (hit-brick-y br b)))

(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 51 60 3 3)) #false)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 51 30 3 3)) #true)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 20 60 3 3)) #false)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 20 30 3 3)) #true)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 20 100 3 3)) #false)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 56 51 3 3)) #true)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 24 29 3 3)) #true)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 56 29 3 3)) #true)
(check-expect (hit-brick-corner? (make-brick 40 40) (make-ball 24 51 3 3)) #true)


;; new-bricks : Brick LLoB Ball  -> LLoB
;; if the ball hits any brick, remove that brick from the list
(define (new-bricks br llob b)
  (cond
    [(empty? llob) '()]
    [(cons? llob)
     (if (collision-row-helper (first llob) b)
         (if (empty? (new-bricks-helper (first llob) br))
             (rest llob)
             (cons (new-bricks-helper (first llob) br) (rest llob)))
         (cons (first llob) (new-bricks br (rest llob) b)))]))

(check-expect (new-bricks (make-brick 40 40) (list (list (make-brick 40 40) (make-brick 80 40))
                                                   (list (make-brick 40 80) (make-brick 80 80)))
                          (make-ball 40 40 3 3))
              (list (list (make-brick 80 40)) (list (make-brick 40 80) (make-brick 80 80))))
(check-expect (new-bricks (make-brick 40 80) (list (list (make-brick 40 40) (make-brick 80 40))
                                                   (list (make-brick 40 80)))
                          (make-ball 40 80 3 3))
              (list (list (make-brick 40 40) (make-brick 80 40))))
         
;; new-bricks-helper: LoB Brick -> LoB
;; returns the given list of bricks with the given brick removed
(define (new-bricks-helper lob br)
  (cond
    [(empty? lob) '()]
    [else (if (and (= (brick-x (first lob)) (brick-x br)) (= (brick-y (first lob)) (brick-y br)))
              (new-bricks-helper (rest lob) br)
              (cons (first lob) (new-bricks-helper (rest lob) br)))]))

(check-expect (new-bricks-helper (list (make-brick 40 40) (make-brick 80 40)) (make-brick 80 40))
              (list (make-brick 40 40)))
(check-expect (new-bricks-helper (list (make-brick 40 40) (make-brick 80 40)) (make-brick 40 40))
              (list (make-brick 80 40)))


;; reset: World -> World
;; creates a new world which restarts the position of the paddle and ball as well as
;; reduces the lives by one
(define (reset w)
  (make-world paddle1 (make-ball 100 184 3 3) (world-bricks w) (- (world-lives w) 1)))

(check-expect (reset W2) (make-world paddle1 (make-ball 100 184 3 3) (make-grid brick1 3 5) 4))
(check-expect (reset (make-world paddle1 (make-ball 10 10 3 3) (make-grid brick1 3 5) 4))
              (make-world paddle1 (make-ball 100 184 3 3) (make-grid brick1 3 5) 3))


;; off-wall: World Ball -> World
;; determines if the ball hit the top, corners, or sides of the wall and changes
;; velocity accordingly

(check-expect (off-wall W1 (make-ball 100 6 4 5))
              (make-world (world-paddle W1)
                          (make-ball 100 6 4 -5) (world-bricks W1) 5))
(check-expect (off-wall W1 (make-ball 6 60 -4 5))
              (make-world (world-paddle W1)
                          (make-ball 6 60 4 5) (world-bricks W1) 5))
(check-expect (off-wall W1 (make-ball 194 60 4 5))
              (make-world (world-paddle W1)
                          (make-ball 194 60 -4 5) (world-bricks W1) 5))

(define (off-wall w b)
  (cond
    [(hit-corner-wall? b) (make-world (world-paddle w)
                                      (make-ball (ball-x b)
                                                 (ball-y b) (* -1 (ball-vx b)) (* -1 (ball-vy b)))
                                      (world-bricks w) (world-lives w))]
    [(<= (- (ball-y b) BALL-RADIUS) 0)
     (change-ball-vy w b)]
    [(hit-side? b) (change-ball-vx w b)]))

(check-expect (off-wall (make-world 100 (make-ball 6 194 3 3)
                                    (list (list (make-brick 40 40) (make-brick 80 40))) 4)
                        (make-ball 6 194 3 3))
              (make-world 100 (make-ball 6 194 -3 -3)
                          (list (list (make-brick 40 40) (make-brick 80 40))) 4))
(check-expect (off-wall (make-world 100 (make-ball 10 6 3 3)
                                    (list (list (make-brick 40 40) (make-brick 80 40))) 4)
                        (make-ball 10 6 3 3))
              (make-world 100 (make-ball 10 6 3 -3)
                          (list (list (make-brick 40 40) (make-brick 80 40))) 4))
(check-expect (off-wall (make-world 100 (make-ball 6 30 -3 3)
                                    (list (list (make-brick 40 40) (make-brick 80 40))) 4)
                        (make-ball 6 30 -3 3))
              (make-world 100 (make-ball 6 30 3 3)
                          (list (list (make-brick 40 40) (make-brick 80 40))) 4))

;; hit-corner-wall? Ball -> Boolean
;; determines if the ball hit the corner of the boundaries

(check-expect (hit-corner-wall? (make-ball 6 194 3 3)) #true)
(check-expect (hit-corner-wall? (make-ball 6 6 3 3)) #true)
(check-expect (hit-corner-wall? (make-ball 194 6 3 3)) #true)
(check-expect (hit-corner-wall? (make-ball 194 194 3 3)) #true)
(check-expect (hit-corner-wall? (make-ball 194 100 3 3)) #false)

(define (hit-corner-wall? b)
  (cond
    [(and (>= (+ BALL-RADIUS (ball-y b)) HEIGHT) (<= (- (ball-x b) BALL-RADIUS) 0)) #true]
    [(and (>= (+ BALL-RADIUS (ball-y b)) HEIGHT) (>= (+ (ball-x b) BALL-RADIUS) WIDTH)) #true] 
    [(and (<= (- (ball-y b) BALL-RADIUS) 0) ( <= (- (ball-x b) BALL-RADIUS) 0)) #true]
    [(and (<= (- (ball-y b) BALL-RADIUS) 0) (>= (+ (ball-x b) BALL-RADIUS) WIDTH)) #true]
    [else #false]))


;; hit-side?: Ball -> Boolean
;; determines if ball hits the left or right side of screen
(define (hit-side? b)
  (or (<= (- (ball-x b) BALL-RADIUS) 0) (>= (+ (ball-x b) BALL-RADIUS) WIDTH)))

(check-expect (hit-side? (make-ball 50 50 3 3)) #false)
(check-expect (hit-side? (make-ball 6 30 -3 3)) #true)
(check-expect (hit-side? (make-ball 194 30 3 3)) #true)

;; change-ball-vx : World Ball -> World
;; changes x velocity of ball
(define (change-ball-vx w b)
  (make-world (world-paddle w) (make-ball (ball-x b) (ball-y b) (* -1 (ball-vx b)) (ball-vy b))
              (world-bricks w) (world-lives w)))

(check-expect (change-ball-vx W5 (make-ball 5 5 5 5))
              (make-world 100 (make-ball 5 5 -5 5) (make-grid brick1 ROWS COLUMNS) 2))
(check-expect (change-ball-vx W0 (world-ball W0))
              (make-world 100 (make-ball 100 184 0 4) (make-grid brick1 ROWS COLUMNS) 5))
(check-expect (change-ball-vx (make-world 100 (make-ball 40 40 3 3) (list (list (make-brick 40 40)
                                                                                (make-brick 80 40))
                                                                          (list (make-brick 40 80)
                                                                                (make-brick 80 80)))
                                          4)
                              (make-ball 40 40 3 3))
              (make-world 100 (make-ball 40 40 -3 3) (list (list (make-brick 40 40)
                                                                 (make-brick 80 40))
                                                           (list (make-brick 40 80)
                                                                 (make-brick 80 80))) 4))

;; change-ball-vy: World Ball -> World
;; changes y velocity of ball
(define (change-ball-vy w b)
  (make-world (world-paddle w) (make-ball (ball-x b) (ball-y b) (ball-vx b) (* -1 (ball-vy b)))
              (world-bricks w) (world-lives w)))

(check-expect (change-ball-vy W5 (make-ball 5 5 5 5))
              (make-world 100 (make-ball 5 5 5 -5) (make-grid brick1 ROWS COLUMNS) 2))
(check-expect (change-ball-vy (make-world 100 (make-ball 40 40 3 3) (list (list (make-brick 40 40)
                                                                                (make-brick 80 40))
                                                                          (list (make-brick 40 80)
                                                                                (make-brick 80 80)))
                                          4)
                              (make-ball 40 40 3 3))
              (make-world 100 (make-ball 40 40 3 -3) (list (list (make-brick 40 40)
                                                                 (make-brick 80 40))
                                                           (list (make-brick 40 80)
                                                                 (make-brick 80 80))) 4))
(check-expect (change-ball-vy (make-world 100 (make-ball 100 100 0 4)
                                          (list (make-brick 40 80) (make-brick 80 80)) 4)
                              (make-ball 100 100 0 4))
              (make-world 100 (make-ball 100 100 0 -4)
                          (list (make-brick 40 80) (make-brick 80 80)) 4))

;; lost : World -> Boolean
;; determines if the amount of lives is equal to 0
(define (lost w)
  (if (= (world-lives w) 0)
      #true
      #false))

(check-expect (lost (make-world paddle1 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) 0)) #true)
(check-expect (lost (make-world paddle1 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) 5)) #false)

; win: World -> Boolean
;; determines if the bricks is an empty list
(define (win w)
  (if (empty? (world-bricks w))
      #true
      #false))

(check-expect (win (make-world paddle1 INITIAL-BALL (make-grid brick1 0 0) LIVES)) #true)
(check-expect (win W0) #false)


;; speed: Ball -> Number
;; compute the speed of the ball
(check-expect (speed INITIAL-BALL) 4)
(define (speed ball)
  (sqrt (+ (sqr (ball-vx ball))
           (sqr (ball-vy ball)))))

;;new-x-velocity : Ball Number -> Number
;;Produces the new x velocity of a ball that launched off a paddle with this x-coordinate
(define (new-x-velocity ball x)
  (inexact->exact
   (* .95
      (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS))
      (speed ball))))
(check-expect (new-x-velocity INITIAL-BALL 100) 0)
(check-expect (new-x-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* 4.75 -40/26)))

;;new-y-velocity : Ball Number -> Number
;;Produces the new y velocity of a ball that launched off a paddle with this x-coordinate
(define (new-y-velocity ball x)
  (inexact->exact
   (* (- (sqrt (- 1 (sqr (* .95
                            (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS)))))))
      (speed ball))))
(check-expect (new-y-velocity INITIAL-BALL 100) -4)
(check-expect (new-y-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))

;;launch-ball : Ball Number -> Ball
;;Launch ball off paddle with this x-coordinate
(define (launch-ball ball x)
  (make-ball (+ (ball-x ball) (new-x-velocity ball x))
             (+ (ball-y ball) (new-y-velocity ball x))
             (new-x-velocity ball x) (new-y-velocity ball x)))
(check-expect (launch-ball INITIAL-BALL 100)
              (make-ball 100 180 0 -4))
(check-expect (launch-ball (make-ball 60 190 3 4) 100)
              (make-ball (+ 60 (inexact->exact (* 4.75 -40/26)))
                         (+ 190 (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))
                         (inexact->exact (* 4.75 -40/26))
                         (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26))))))))

(define W0 (make-world paddle1 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) LIVES))
(define W1 (make-world 20 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) LIVES))
(define W2 (make-world 180 INITIAL-BALL (make-grid brick1 ROWS COLUMNS) LIVES))
(define W5 (make-world paddle1 (make-ball 100 184 3 3) (make-grid brick1 ROWS COLUMNS) 2))
(define W3 "lost")
(define W4 "win")

(define (main x)
  (big-bang W0
    [to-draw draw-all]
    [on-key change-paddle]
    [on-tick change-world]
    [stop-when stop draw-all]))

(main 0)
