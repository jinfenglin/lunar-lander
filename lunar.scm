;; this is the code for problem set -- Lunar Lander
#lang scheme
;stragy block
(define (full-burn ship-state) 1)
(define (no-burn ship-state) 0)
(define (ask-user ship-state) (get-burn-rate))
;problem 3
(define (random-choice . stragy) ; we assume there is only 2 stragy so the random number is 0-1
  (let ([roll (random (length stragy))]) ;get a random number according to the length of parameter
    (cond ((= roll 0) (list-ref stragy 0)) ;if random is 0 use first stragy
          ((= roll 1) (list-ref stragy 1)))
    ;regist new stragy here
    )
  )
;Problem 4
(define (height-choice stragy-1 stragy-2 alert-height)
  (lambda (ship-state) 
    (cond ((<= alert-height (height ship-state)) (stragy-1 ship-state))
    (else (stragy-2 ship-state))
    )
  ))
;problem 5
(define (choice stragy-1 stragy-2 condition) ;condition give number from 0-1 to decide.
  (lambda (ship-state) (if (= (condition ship-state) 1) 
                           (stragy-1 ship-state)
                           (stragy-2 ship-state)
    )))
(define (new-random-choice strategy-1 strategy-2)
(choice strategy-1
        strategy-2
        (lambda (ship-state) (= (random 2) 0))))

(define (new-height-choice stragy-1 stragy-2 alert-height) 
  (choice stragy-1
          stragy-2
          (lambda (ship-state) (cond ((>= (height ship-state) alert-height) 0)
                                     (else 1)
                                     ))))
;problem 6
(define compund-choice
  (lambda (ship-state)
  (if (> (height ship-state) 40)
      (no-burn ship-state)
      (random-choice (full-burn ship-state) (ask-user ship-state))
      )
  )
)
;problem 8
(define square (lambda (x) (* x x)))
(define get-acc (lambda (ship-state) (/(square (velocity ship-state))(* 2 (height ship-state)))))
(define constant-acc 
  (lambda (ship-state)
    (/ 
     (+ (get-acc ship-state) 
        gravity) 
     engine-strength)
   )
  )
;problem 11 
(define optimal-constant-acc
  (lambda (ship-state)
    ( cond ((< (- 1 (constant-acc ship-state)) 0.005) (constant-acc ship-state))
           (else (no-burn ship-state))
     )))
;problem 12
;super-engine serial procedure is defined for removing the constrain on the fuel burn rate while 
;at the same time not affect the result of previous problems.
;please (play-super-engine (fuel-cal-simulation 10)) to test this part 10 is the delt-h, don't use (play (fuel-cal-simulation 10)),remember to set dt=0.001
(define (play-super-engine stragy) (super-engine-lander-loop (initial-ship-state) stragy))
(define (super-engine-lander-loop ship-state stragy)
  (show-ship-state ship-state)
  (if (landed? ship-state)
      (end-game ship-state)
      (lander-loop (super-engine-update ship-state (stragy ship-state)) stragy)))
(define (super-engine-update ship-state fuel-burn-rate)
  (if (>= (- (fuel ship-state) (* fuel-burn-rate dt)) 0) ; if the fuel is larger than 0 then update all the state, otherwise only update the height and velocity which is only the -gravity
      (make-ship-state
       (+ (height ship-state) (* (velocity ship-state) dt)) ; height
       (+ (velocity ship-state)
          (* (- (* engine-strength  fuel-burn-rate) gravity)
             dt))                                           ; velocity
       (- (fuel ship-state) (*  fuel-burn-rate dt)))        ; fuel
       (make-ship-state
        (+ (height ship-state) (* (velocity ship-state) dt))
        (+ (velocity ship-state)
          (* (- (* engine-strength 0) gravity) ;if fuel is 0 then the fuel-burn-rate is always 0
             dt))
        (fuel ship-state)
        ); case that fuel is not enough
       ))
(define (sqrt x) (expt x 1/2))
(define (fuel-cal-simulation delt-h)  
  (lambda (ship-state)
    ( if(> (- delt-h (- 50 (height ship-state))) 0.01) ; if the distance dropped is less than delt-h do no-burn
        (no-burn ship-state)
        (constant-acc ship-state)
  )))
(define (fuel-cal delt-h height)
  (/ (*(sqrt (* 2 gravity delt-h)) height) (* delt-h engine-strength))
  )
;problem 1 & 10
(define (engine-protection-gear fuel-burn-rate)    ;make sure the fuel burn rate is no larger than 1
  (cond ((> fuel-burn-rate 1) 1)
        (else fuel-burn-rate)
        )) 
(define (update ship-state fuel-burn-rate)
  (if (>= (- (fuel ship-state) (* (engine-protection-gear fuel-burn-rate) dt)) 0) ; if the fuel is larger than 0 then update all the state, otherwise only update the height and velocity which is only the -gravity
      (make-ship-state
       (+ (height ship-state) (* (velocity ship-state) dt)) ; height
       (+ (velocity ship-state)
          (* (- (* engine-strength (engine-protection-gear fuel-burn-rate)) gravity)
             dt))                                           ; velocity
       (- (fuel ship-state) (* (engine-protection-gear fuel-burn-rate) dt)))        ; fuel
       (make-ship-state
        (+ (height ship-state) (* (velocity ship-state) dt))
        (+ (velocity ship-state)
          (* (- (* engine-strength 0) gravity) ;if fuel is 0 then the fuel-burn-rate is always 0
             dt))
        (fuel ship-state)
        ); case that fuel is not enough
       ))
;problem 2
(define (lander-loop ship-state stragy)
  (show-ship-state ship-state)
  (if (landed? ship-state)
      (end-game ship-state)
      (lander-loop (update ship-state (stragy ship-state)) stragy)))

(define (show-ship-state ship-state)
  (write-line 
    (list 'height (height ship-state)
          'velocity (velocity ship-state)
          'fuel (fuel ship-state))))

(define (landed? ship-state)
  (<= (height ship-state) 0))

(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
       (write-line final-velocity)
       (cond ((>= final-velocity safe-velocity)
               (write-line "good landing")
               'game-over)
             (else
               (write-line "you crashed!")
               'game-over))))

(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))

(define (play stragy) (lander-loop (initial-ship-state) stragy))

(define (initial-ship-state)
  (make-ship-state 50       ; 50 km high
                   0        ; not moving (0 km/sec)
                   20))     ; 20 kg of fuel left

(define dt 0.001)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define (player-input) 
  (char->integer (prompt-for-command-char " action: "))) 

(define burn-key 32)   ;space key

; You'll learn about the stuff below here in Chapter 2.
; For now, think of make-ship-state, height, velocity, and fuel
; as primitive procedures built in to Scheme.

(define (make-ship-state height velocity fuel)
  (list 'HEIGHT   height
        'VELOCITY velocity
        'FUEL     fuel))

(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))

(define (second l) (cadr l))
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))

; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...

; for input and output

(define (write-line x)
  (display x)
  (newline))

(define (get-one-key)
  (let ((x (read-char)))
    (if (eq? x #\newline)
        x
        (empty-buffer x))))

(define (empty-buffer x)
  (if (eq? (read-char) #\newline)
      x
      (empty-buffer x)))

(define (prompt-for-command-char prompt)
  (display prompt)
  (get-one-key)) 

; for random number generation

(#%require (only racket/base random))

; a ridiculous addendum  (you'll need this for the exercises)

(define (1+ x) (+ 1 x))
