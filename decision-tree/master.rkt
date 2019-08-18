#lang racket
(require 2htdp/batch-io)
(require "decision_functions.rkt")
;;;Don't give someone mushroom
;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")
(define titanicout "../output/titanic-decision-tree.dot")
(define mushroomout "../output/mushroom-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw (cdr (read-csv-file "toy_train.csv") ))

(provide titanic-raw)
(define titanic-raw (map (lambda (x) (cddr x))
                         (cdr (read-csv-file "titanic_train.csv")))) 

(provide mushroom-raw)
(define mushroom-raw (cdr (read-csv-file "mushrooms_train.csv")))  

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data)
  (let* ([init (string->number (car data))]
         [tails (cdr data)]
         [c-data (map (lambda (x) (string->number x)) tails )])
    (cons c-data init)))

;list of (features . result)
(provide toy)
(define toy
  (map (lambda (x) (format x)) toy-raw))

(provide titanic)
(define titanic
  (map (lambda (x) (format x)) titanic-raw))

(provide mushroom)
(define mushroom
  (map (lambda (x) (format x)) mushroom-raw))


;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (let ([n (length data)])
    (define (collect data a p)
      (cond [(> a n) p]
            [(null? (car data)) 0]
            [(= 1 (cdr (car data))) (collect (cdr data) (+ 1 a) (+ 1 p))]
            [else (collect (cdr data) (+ 1 a) p)]))
    (if(null? data) 0
       (/ (collect data 1 0) n)))
  )

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (let* ([p (get-leaf-prob data)]
         [n (- 1 p)])
    (cond [(or (= 1 p) (= p 0)) 0] 
          [else (- 0 (+ (* p (log p 2)) (* n (log n 2))))])))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)

(define (search-and-add f lst acc)
  (define (loop result lst acc bcc)
    (cond [(null? acc) (append bcc (list (list result lst)))]
          [(equal? result (caar acc)) (append bcc (list (append (car acc) (list lst))) (cdr acc))]
          [else (loop result lst (cdr acc) (append bcc (list (car acc))))]))
  (let* ([result (f (car lst))]) 
    (loop result lst acc '())))

(define (make-consistent data-grps list-val acc)
  (cond [(null? list-val) acc]
        [(null? data-grps) (make-consistent data-grps
                                            (cdr list-val)
                                            (append acc (list (list (car list-val) '()))))]
        [(equal? (car list-val) (caar data-grps )) (make-consistent
                                                    (cdr data-grps)
                                                    (cdr list-val)
                                                    (append acc (list (car data-grps))))]
        [else (make-consistent
               data-grps
               (cdr list-val)
               (append acc (list (list (car list-val) '()))))]))

;;;;build-data-sorted takes cdr of decision function
(define (build-data-sorted f data)
  (define (build-data-helper f data acc)
    (cond [(null? data) acc]
          [else (build-data-helper f (cdr data) (search-and-add f (car data) acc))])) 
  (let* ([partial-sorted-data (sort  (build-data-helper f data '())
                                     (lambda (x y) (< (car x) (car y))))]
         [final-data (make-consistent partial-sorted-data (list-of-values f) '())])
    (map (lambda (x) (cdr x)) final-data )))

;(define (build-data-sorted-v2 f data)
;  (define (build-data-helper f data acc)
;    (cond [(null? data) acc]
;          [else (build-data-helper f (cdr data) (search-and-add f (car data) acc))]))
;  (let* ([partial-sorted-data (sort  (build-data-helper f data '())
;                                     (lambda (x y) (< (car x) (car y))))]
;         [final-data (begin (displayln (list-of-values f)) (make-consistent partial-sorted-data (list-of-values f) '()))])
;    final-data ))
;(build-data-sorted (cdr y1) (cadr (build-data-sorted (cdr y3) (car (build-data-sorted (cdr y4>62) toy)))))
;(build-data-sorted-v2 (cdr y1) (cadr (build-data-sorted (cdr y3) (car (build-data-sorted (cdr y4>62) toy)))))

;  (map (lambda (x) (cdr x)) (sort (build-data-helper f data '())
;                                  (lambda (x y) (< (car x) (car y))))))
;                                  

(define (entropy-diff f data)
  (cond [(null? data) 0]
        [(null? (car data)) 0]
        [else (let* ([entropy-init (get-entropy data)]
                     [sorted-data (build-data-sorted f data)]
                     [n (length data)]
                     [ent-lst (map (lambda (x) (* (/ (length x) n) (get-entropy x)))
                                   sorted-data)])
                (- entropy-init (foldr (lambda (x y) (+ x y)) 0 ent-lst)))]))

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (caar (sort (map (lambda (x) (cons x (entropy-diff (cdr x) data))) candidates)
              (lambda (x y) (> (cdr x) (cdr y))))))

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
 (cond [(equal? data (list '())) (DTree "0" '() '())]
       [(or (= 1 (length candidates)) (= 1 depth)) (let* ([func (choose-f candidates data)]
                           [d (car func)]
                           [f (cdr func)]
                           [my-lst (build-data-sorted f data)]
                           [my-lst1 (map (lambda (x) (DTree x '() '()))
                                         (map (lambda (x) (number->string (get-leaf-prob x))) my-lst))])
                      (DTree d f my-lst1))]
       [else (let* ([func (choose-f candidates data)]
                    [rem-candidates (remove* (list func) candidates)]
                    [d (car func)]
                    [f (cdr func)]
                    [my-lst (build-data-sorted f data)])
               (DTree d f (map (lambda (x) (build-tree rem-candidates x (- depth 1))) my-lst)))]))

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(define (search lst num a)
  (cond [(null? lst) "not yet"]
        [(equal? (car lst) num) a]
        [(search (cdr lst) num (+ 1 a))]))
(define (length lst)
  (foldr (lambda (x y) (+ 1 y)) 0 lst))
;(length '(1 2 3 4 ))

(provide make-decision)
(define (make-decision tree test)
  (match tree
    [(DTree d f c) (cond [(null? c) (string->number d)]
                         [else (let* ([result (f test)]
                                      [index (search (list-of-values f) result 0)])
                                 (if (equal? "not yet" index) 0
                                     (make-decision (list-ref c index) test)))])]))
;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;(define yourlist (list pclass sex age>25 sibsp parch fare>50 emb))
;(define dotfile
;  (display-tree (build-tree yourlist titanic 5) titanicout))
;(define mylist (list cshape  csurf bruise  odor gatch gspace gsize  sshape nring nring pop hab))
;(define yourlist (list pclass sex age>25 sibsp parch fare>50 emb))


;(define myfile
;  (display-tree (build-tree mylist mushroom 5) mushroomout))
;(write-file "mushroomfile.txt" (read-file myfile))
;(define yourfile
;  (display-tree (build-tree yourlist titanic 7) titanicout))
;(write-file "titanic.txt" (read-file yourfile))
;(write-file "toy.txt" (read-file dotfile))

;============================================================================================================
;============================================================================================================
;============================================================================================================
