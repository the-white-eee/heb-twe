#lang racket
(define make-named-strategy
  (lambda (strategy name author)
    (lambda (message)
      (cond ((equal? message "name") name)
            ((equal? message "author") author)
            ((list? message) (strategy message))
            (else "bad message for strategy")))))
(define PTF
  (make-named-strategy (lambda (x) "c")
                       "Poor Trusting Fool"
                       "I'm dead"))

(define DA
  (make-named-strategy (lambda (x) "d")
                       "Defect Always"
                       "You will be dead soon"))

(define grudger-intern
  (lambda ()
    (letrec ((cooperate? #t))
      (lambda (list)
        (cond ((empty? list) (begin (set! cooperate? #t) "c"))
              ((not cooperate?) "d")
              ((equal? (get-last-enemy-move list) "d")
               (begin (set! cooperate? #f)
                      "d"))
              (else "c"))))))

(define G
  (make-named-strategy (grudger-intern) 
                       "grudger" 
                       "me"))

(define get-last-enemy-move
  (lambda (list)
    (cond ((empty? list) "bad List - its empty!")
          ((empty? (rest list)) (first (rest (first list))))
          (else (get-last-enemy-move (rest list))))))

(define TfT
  (make-named-strategy (lambda (x)
                        (if (empty? x)
                            "c"
                            (get-last-enemy-move x)))
                       "Tit-for-Tat"
                       "you suck!"))

(define TftT
  (make-named-strategy ((lambda ()
                          (letrec ((last-enemy-move "0")
                                   (my-last-move "0"))
                            (lambda (x)
                              (cond ((empty? x) (begin (set! my-last-move "c")
                                                       "c"))
                                    ((equal? (get-last-enemy-move x) last-enemy-move) (begin (set! my-last-move last-enemy-move)
                                                                                             last-enemy-move))
                                    (else (begin (set! last-enemy-move (get-last-enemy-move x))
                                                 my-last-move)))))))
                       "Tit-for-two-Tats"
                       "stupid fool"))

(define NP
  (make-named-strategy (lambda (x)
                         (cond ((empty? x) "c")
                               ((equal? (get-last-enemy-move x) "d") "d")
                               (else (if (< (random) 0.1)
                                         "d"
                                         "c"))))
                       "Naive Prober"
                       "gambling is NOT a crime"))
    
(define to-the-end
  (lambda (list elem)
    (if (empty? list)
        (cons elem empty)
        (cons (first list) (to-the-end (rest list) elem)))))

(define play-n-rounds
  (lambda (strat1 strat2 rounds)
    (letrec ((history1 empty)
             (history2 empty)
             (last-move1 "")
             (last-move2 "")
             (do (lambda ()
                   (if (= rounds 0)
                       history1
                       (begin (set! rounds (- rounds 1))
                              (set! last-move1 (strat1 history1))
                              (set! last-move2 (strat2 history2))
                              (set! history1 (to-the-end history1 (cons last-move1 (cons last-move2 empty))))
                              (set! history2 (to-the-end history2 (cons last-move2 (cons last-move1 empty))))
                              (do))))))
      (do)
      )))


(define compute-points-single-game
  (lambda (game strat1 strat2 rounds)
    (letrec ((pplayer1 0)
             (pplayer2 0)
             (do (lambda (list)
                   (if (empty? list)
                       (cons pplayer1 (cons pplayer2 empty))
                       (letrec ((points (game (first list))))
                         (begin (set! pplayer1 (+ pplayer1 (first points)))
                                (set! pplayer2 (+ pplayer2 (first (rest points))))
                                (do (rest list)))
                         )))))
      (do (play-n-rounds strat1 strat2 rounds)))))

(define distribute-points-bonnie-clyde
  (lambda (list)
    (cond ((empty? list) (write-string "bad list for game"))
          ((empty? (rest list)) (write-string "bad list for game"))
          (else (letrec ((player1 (first list))
                         (player2 (first (rest list))))
                  (cond ((and (equal? player1 "c") (equal? player2 "c")) (cons 1 (cons 1 empty)))
                        ((and (equal? player1 "c") (equal? player2 "d")) (cons 20 (cons 0 empty)))
                        ((and (equal? player1 "d") (equal? player2 "c")) (cons 0 (cons 20 empty)))
                        ((and (equal? player1 "d") (equal? player2 "d")) (cons 10 (cons 10 empty)))))))))
        

(define winner-in-single-game
  (lambda (game strat1 strat2 rounds)
    (letrec ((results (compute-points-single-game game strat1 strat2 rounds)))
      (cond ((= (first results) (first (rest results))) "tie")
            ((< (first results) (first (rest results))) strat1)
            (else strat2)))))

(define winner-in-n-games
  (lambda (game strat1 strat2 games rounds)
    (letrec ((strat1-wins 0)
             (strat2-wins 0)
             (winner strat1)
             (do (lambda (round)
                   (if (= (- rounds round) 0)
                       (cond ((> strat1-wins strat2-wins) strat1)
                             ((= strat1-wins strat2-wins) (begin (write-string "tie in n games between ")
                                                                 (write-string (strat1 "name"))
                                                                 (write-string " and ")
                                                                 (write-string (strat2 "name"))
                                                                 (newline)
                                                                 strat1))
                             (else strat2))
                       (begin (set! winner (winner-in-single-game game strat1 strat2 rounds))
                              (cond ((equal? winner "tie") "")
                                    ((equal? winner strat1) (begin (set! strat1-wins (+ strat1-wins 1))))
                                    (else (begin (set! strat2-wins (+ strat2-wins 1)))))
                              (do (+ round 1)))))))
      (do 0))))

(define winner-strategy-knockout-system
  (lambda (game strats games rounds)
    (letrec ((winner empty)
             (next-round empty)
             (do (lambda (strats)
                   (cond ((and (empty? strats) (empty? (rest winner))) (first winner))
                         ((empty? strats) (begin (set! next-round winner)
                                                 (set! winner empty)
                                                 (do next-round)))
                         ((empty? (rest strats)) (begin (set! next-round winner)
                                                        (set! winner empty)
                                                        (do (cons (first strats) next-round))))
                         (else (begin (set! winner (cons (winner-in-n-games game (first strats) (first (rest strats)) games rounds) winner))
                                      (do (rest (rest strats)))))))))
      (do strats))))

(define strat&points
  (lambda (strat)
    (letrec ((point 0))
      (lambda (m)
        (cond ((equal? m "get") point)
              ((equal? m "set") (lambda (x) (begin (set! point x))))
              ((equal? m "add") (lambda (x) (begin (set! point (+ point x)))))
              ((equal? m "strat") strat)
              (else (write-string "bad message in strat&points")))))))

(define winner-strategy-lowest-score
  (lambda (game strats rounds)
    (letrec ((strats-with-points empty)
             (convert (lambda (strats)
                        (if (empty? strats)
                            empty
                            (cons (strat&points (first strats)) (convert (rest strats))))))
             (one-vs-all (lambda (strat strats)
                           (if (empty? strats)
                               (cons strat empty)
                               (letrec ((result (compute-points-single-game game (strat "strat") ((first strats) "strat") rounds)))
                                 (begin ((strat "add") (first result))
                                        (((first strats) "add") (first (rest result)))
                                              (cons (first strats) (one-vs-all strat (rest strats))))))))
             (do (lambda (strat strats)
                   (if (empty? strats)
                       empty
                       (begin (write-string ((strat "strat") "name"))
                              (write-string " is playing")
                              (newline)
                              (set! strats (one-vs-all strat strats))
                              (print strats)
                              (cons strat (do (first strats) (rest strats)))))))
             (print (lambda (strats-with-points)
                      (begin (write-string "Die Strategie ")
                             (write-string (((first strats-with-points) "strat") "name"))
                             (write-string " hat insgesamt ")
                             (write-string (number->string ((first strats-with-points) "get")))
                             (write-string " Punkte erreicht!")
                             (newline)
                             (if (empty? (rest strats-with-points))
                                 ""
                                 (print (rest strats-with-points))))))
             (get-best-strategy (lambda (strats-with-points)
                                  (if (empty? (rest strats-with-points)) 
                                      (first strats-with-points)
                                      (letrec ((best-after (get-best-strategy (rest strats-with-points)))
                                               (my-points ((first strats-with-points) "get")))
                                        (if (< my-points (best-after "get"))
                                            (first strats-with-points)
                                            best-after))))))
      (begin (set! strats-with-points (convert strats))
             (letrec ((result-of (cond ((empty? strats) (write-string "stupid bastards gave me not one strategy"))
                                       ((empty? (rest strats)) (write-string "got online one strategie, you stupid cunts!"))
                                       (else (do (first strats-with-points) (rest strats-with-points))))))
               (print result-of)
               ((get-best-strategy result-of) "strat"))))))
             
          
                   
    

(define strategies (cons DA (cons G (cons PTF (cons TfT (cons TftT (cons NP empty)))))))

((winner-strategy-lowest-score distribute-points-bonnie-clyde strategies 100) "name")