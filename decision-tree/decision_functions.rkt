#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)
(provide list-of-values)

(define y1
  (cons "feature1" (lambda (x) (car x) ))) ; returns the value of feature 1 for a given test sample
(define y2
  (cons "feature2" (lambda (x) (cadr x) )))
(define y3
  (cons "feature3" (lambda (x) (caddr x) )))
(define y4>62 
  (cons "feature4>62" (lambda (x) (if (> (cadddr x) 62) 1 0)))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define pclass
  (cons "pclass" (lambda (x) (list-ref x 0)) )) ; returns the value of pclass for a given test sample
(define sex 
  (cons "sex" (lambda (x) (list-ref x 1)) ))
(define age>25 
  (cons "age>25" (lambda (x) (if (> (list-ref x 2) 25) 1 0))))
(define sibsp 
  (cons "sibsp" (lambda (x) (list-ref x 3))))
(define parch 
  (cons "parch" (lambda (x) (list-ref x 4))))
(define fare>50 
  (cons "fare>50" (lambda (x) (if (> (list-ref x 5) 50) 1 0))))
(define emb 
  (cons "emb" (lambda (x) (list-ref x 6))))
;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)
(provide mylist)
(define cshape
  (cons "cshape" (lambda (x) (list-ref x 0))))
(define csurf 
  (cons "csurf" (lambda (x) (list-ref x 1))))
(define bruise 
  (cons "bruise" (lambda (x) (list-ref x 2))))
(define odor 
  (cons "odor" (lambda (x) (list-ref x 3))))
(define gatch 
  (cons "gatch" (lambda (x) (list-ref x 4))))
(define gspace 
  (cons "gspace" (lambda (x) (list-ref x 5))))
(define gsize 
  (cons "gsize" (lambda (x) (list-ref x 6))))
(define sshape 
  (cons "sshape" (lambda (x) (list-ref x 7))))
(define nring 
  (cons "nring" (lambda (x) (list-ref x 8))))
(define pop 
  (cons "pop" (lambda (x) (list-ref x 9))))
(define hab 
  (cons "hab" (lambda (x) (list-ref x 10))))
(define mylist (list cshape  csurf bruise  odor gatch gspace gsize  sshape nring nring pop hab))

(define (list-of-values f)
  (cond 
    [(equal? f (cdr y1)) (list 0 1)]
    [(equal? f (cdr y2)) (list 0 1)]
    [(equal? f (cdr y3)) (list 0 1 2)]
    [(equal? f (cdr y4>62)) (list 0 1)]
    [(equal? f (cdr pclass)) '(1 2 3) ]
    [(equal? f (cdr sex)) '(0 1)]
    [(equal? f (cdr age>25)) '(0 1)]
    [(equal? f (cdr sibsp)) '(0 1 2 3 4 5 6 7 8)]
    [(equal? f (cdr parch)) '(0 1 2 3 4 5)]
    [(equal? f (cdr fare>50)) '(0 1)]
    [(equal? f (cdr emb)) '(0 1 2)]
    [(equal? f (cdr cshape)) '(0 1 2 3 4 5)]
    [(equal? f (cdr csurf)) '(0 1 2)]
    [(equal? f (cdr bruise)) '(0 1)]
    [(equal? f (cdr odor)) '(0 1 2 3 4 5 6 7 8)]
    [(equal? f (cdr gatch)) '(0 1)]
    [(equal? f (cdr gspace)) '(0 1)]
    [(equal? f (cdr gsize)) '(0 1)]
    [(equal? f (cdr sshape)) '(0 1)]
    [(equal? f (cdr nring)) '(0 1 2)]
    [(equal? f (cdr pop)) '(0 1 2 3 4 5)]
    [(equal? f (cdr hab)) '(0 1 2 3 4 5 6)]))
  
