(define null 
  (lambda 
    (x) (eq x (quote))))
(define and 
  (lambda (x y) 
    (cond (x (cond (y #t)
                   (#t #f)))
          (#t #f))))
(define not
  (lambda (x)
    (cond (x #f)
          (#t #t))))
(define append
  (lambda (x y)
    (cond ((null x) y)
          (#t (cons (car x) (append (cdr x) y))))))
(define list
  (lambda (x y)
    (cons x (cons y (quote)))))
(define pair
  (lambda (x y)
    (cond ((and (null x) (null y)) (quote))
          ((and (not (atom x)) (not (atom y)))
           (cons (list (car x) (car y))
                 (pair (cdr x) (cdr y)))))))

(define caar
  (lambda (x) (car (car x))))
(define cadr
  (lambda (x) (car (cdr x))))
(define cadar
  (lambda (x) (car (cdr (car x)))))
(define caddr
  (lambda (x) (car (cdr (cdr x)))))
(define caddar
  (lambda (x) (car (cdr (cdr (car x))))))

(define assoc
  (lambda (x y)
    (cond ((eq (caar y) x) (cadar y))
          (#t (assoc x (cdr y))))))
(define eval 
  (lambda (e a)
    (cond 
      ((atom e) (assoc e a))
      ((atom (car e))
       (cond
         ((eq (car e) quote)    (cadr e))
         ((eq (car e) atom)     (atom   (eval (cadr e) a)))
         ((eq (car e) eq)       (eq     (eval (cadr e) a)
                                        (eval (caddr e) a)))
         ((eq (car e) car)      (car    (eval (cadr e) a)))
         ((eq (car e) cdr)      (cdr    (eval (cadr e) a)))
         ((eq (car e) cons)     (cons   (eval (cadr e) a)
                                                (eval (caddr e) a)))
         ((eq (car e) cond)     (evcon  (cdr e) a))
         (#t (eval (cons (assoc (car e) a)
                         (cdr e))
                   a))))
      ((eq (caar e) (car (quote define))
           (eval (cons (caddar e) (cdr e))
                 (cons (list (cadar e) (car e)) a))))
      ((eq (caar e) (car (quote lambda))
           (eval (caddar e)
                 (append (pair (cadar e) (evlis (cdr e) a))
                         a)))))))


(define evcon
  (lambda (c a)
    (cond ((eval (caar c) a)
           (eval (cadar c) a))
          (#t (evcon (cdr c) a)))))

(define evlis
  (lambda (m a)
    (cond ((null m) (quote))
          (#t (cons (eval   (car m) a)
                    (evlis  (cdr m) a))))))

(eval (quote (quote (1 2 3))) ())