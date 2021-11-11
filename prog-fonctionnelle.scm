;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; quad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define quad
  (lambda (x)
    (let ((y (* x x)))
      (* y y))))

(quad 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cercle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cercle
  (lambda (r)
    (let ( (c (* 2 (* 3.14 r))) (s (* 3.14 (* r r))) )
      (cons c (cons s ())))))

(cercle 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fac
  (lambda (n)
    (if (= 0 n)
        0
        (if (= 1 n)
            1
            (* n (fac (- n 1)))))))

(fac 0)
(fac 1)
(fac 5)
(fac 6)
(fac 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; som-int
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define som-int
  (lambda (n)
    (if (= 0 n)
        0
        (if (= 1 n)
            1
            (+ n (som-int (- n 1)))))))

(som-int 0)
(som-int 1)
(som-int 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; long
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define long
  (lambda (L)
    (if (null? L)
        0
        (+ 1 (long (cdr L))))))

(long ())
(long '(1))
(long '(1 2 3))
(long '('(a b c) 2 3 '(89 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; miroir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define miroir-append
  (lambda (L)
    (if (null? L)
        ()
        (append (miroir (cdr L)) (list (car L))))))

(define miroir
  (lambda (L)
    (if (null? L)
        ()
        (letrec ((myAppend (lambda (x lst)
                             (if (null? lst)
                                 (cons x ())
                                 (cons (car lst) (myAppend x (cdr lst)))))))
          (myAppend (car L) (miroir (cdr L)))))))

(miroir ())
(miroir '(1))
(miroir '(1 2 3))
(miroir '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; carre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define carre
  (lambda (L)
    (if (null? L)
        ()
        (let ((head (car L)))
          (cons (* head head) (carre (cdr L)))))))

(carre ())
(carre '(1))
(carre '(1 2 3))
(carre '(10 12 32 48))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nbpos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nbpos
  (lambda (L)
    (if (null? L)
        0
        (if (< (car L) 0)
            (nbpos (cdr L))
            (+ 1 (nbpos (cdr L)))))))

(nbpos ())
(nbpos '(1 2 3))
(nbpos '(-1 -2 -3))
(nbpos '(-1 2 -3))
(nbpos '(1 -2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; membre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define membre
  (lambda (x L)
    (if (null? L)
        #f
        (if (= x (car L))
            #t
            (membre x (cdr L))))))

(membre 3 ())
(membre 3 '(1 2 3))
(membre 3 '(1 2 3 4))
(membre 3 '(3 1 2 4))
(membre 4 '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; epure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define epure
  (lambda (L)
    (if (null? L)
        ()
        (if (membre (car L) (cdr L))
            (epure (cdr L))
            (cons (car L) (epure (cdr L)))))))

(epure ())
(epure '(1 2 3))
(epure '(1 2 3 2 5 3 3 1 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nieme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nieme
  (lambda (n L)
    (if (null? L)
        "error"
        (if (= 1 n)
            (car L)
            (nieme (- n 1) (cdr L))))))

(nieme 1 ())
(nieme 1 '(1 2 3))
(nieme 2 '(1 2 3))
(nieme 3 '(1 2 3))
(nieme 4 '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; insere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define insere
  (lambda (n x L)
    (if (null? L)
        (if (= 1 n)
            (cons x ())
            ())
        (if (= 1 n)
            (cons x L)
            (cons (car L) (insere (- n 1) x (cdr L)))))))

(insere 3 1 ())
(insere 2 6 '(1 2 3))
(insere 3 6 '(1 2 3))
(insere 4 6 '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; union
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define union
  (lambda (L1 L2)
    (epure (append L1 L2))))

(union () ())
(union () '(1 2 3))
(union '(1 2 3) ())
(union '(1 2 3) '(4 5 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; inter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inter
  (lambda (L1 L2)
    (if (null? L1)
        ()
        (if (membre (car L1) L2)
            (cons (car L1) (inter (cdr L1) L2))
            (inter (cdr L1) L2)))))

(inter () ())
(inter '(1 2 3) ())
(inter () '(1 2 3))
(inter '(1 2 3) '(1 2 3))
(inter '(1 2 3) '(1 3))
(inter '(1 3) '(1 2 3))
(inter '(5 1 3 4) '(1 2 4 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; met-a-plat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define met-a-plat
  (lambda (L)
    (if (null? L)
        ()
        (if (list? (car L))
            (append (met-a-plat (car L)) (met-a-plat (cdr L)))
            (cons (car L) (met-a-plat (cdr L)))))))

(met-a-plat ())
(met-a-plat '(1 2 3))
(met-a-plat '(1 (2 5 7) 3))
(met-a-plat '(1 (2 5 7) (2 (12 55 77))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; zip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
        ()
        (cons (list (car L1) (car L2)) (zip (cdr L1) (cdr L2))))))

(zip () ())
(zip '(1 2 3) ())
(zip () '(1 2 3))
(zip '(1 2 3) '(1 2 3))
(zip '(1 2 3) '(1 3))
(zip '(1 3) '(1 2 3))
(zip '(5 1 3 4) '(1 2 4 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prod
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define prod
  (lambda (L1 L2)
    (letrec ((prodfac
              (lambda (n lst)
                (if (null? lst)
                    ()
                    (cons (list n (car lst)) (prodfac n (cdr lst)))))))
      (if (null? L1)
          ()
          (append (prodfac (car L1) L2) (prod (cdr L1) L2))))))

(prod () ())
(prod () '(4 5 6))
(prod '(1 2 3) ())
(prod '(1 2 3) '(4 5 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; som_list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define som_list
  (lambda (L)
    (letrec ((aux (lambda (lst)
                    (cond
                     ((null? lst) 0)
                     ((number? (car lst)) (+ (car lst) (aux (cdr lst))))
                     (else (aux (cdr lst))))))
             ;;(s (aux L))
             )
      (if (= 0 (aux L))
          "error"
          (aux L)))))

(som_list ())
(som_list '(1 2 3))
(som_list '(1 a 3))
(som_list '(a b 3))
(som_list '(a b c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; triang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define triang
  (lambda (n)
    (letrec ((left
              (lambda (l)
                (if (= l n)
                    ()
                    (cons l (left (+ l 1))))))
             (right
              (lambda (r)
                (if (= r 0)
                    ()
                    (cons r (right (- r 1)))))))
      (append (left 1) (right n)))))

(triang 0)
(triang 10)
(triang 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fibo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fibo
  (lambda (n)
    (if (or (= 1 n) (= 0 n))
        1
        (+ (fibo (- n 1)) (fibo (- n 2))))))

(fibo 0)
(fibo 1)
(fibo 5)
(fibo 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ies
  (lambda (n L)
    (if (null? L)
        '(() () ())
        (let* ((t (ies n (cdr L)))
              (inf (car t)) (eq (cadr t)) (sup (caddr t)) (x (car L)))
          (cond
           ((< x n) (list (cons x inf) eq sup))
           ((> x n) (list inf eq (cons x sup)))
           (else (list inf (cons x eq) sup)))))))


(ies 5 '(0 1 2 3 4 5 6 7 1 5 4 6 7 2 8 9 0 8 9))
(ies 5 '(0 1 2 3 4 5 6 7 8 9))
(ies 5 ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tri-ins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tri-ins
  (lambda (L)
    (letrec ((insert
              (lambda (n lst)
                (if (null? lst)
                    (list n)
                    (if (< n (car lst))
                        (cons n lst)
                        (cons (car lst) (insert n (cdr lst))))))))
      (if (null? l)
          ()
          (insert (car L) (tri-ins (cdr L)))))))

(tri-ins ())
(tri-ins '(1))
(tri-ins '(1 4 2 5 7 8))
(tri-ins '(4 3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tri-sel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tri-sel
  (lambda (L)
    (letrec ((min-and-rest
              (lambda (L)
                (if (null? L)
                    (list "end" ())
                    (let ((s (min-and-rest (cdr L))))
                      (if (and (not (equal? "end" (car s))) (> (car L) (car s)))
                          (list (car s) (cons (car L) (cadr s)))
                          (list (car L) (cons (car s) (cadr s)))))))))
      (if (null? L)
          ()
          (let ((r (min-and-rest L)))
            (if (equal? "end" (car r))
                ()
                (cons (car r) (tri-sel (cadr r)))))))))

(tri-sel ())
(tri-sel '(6))
(tri-sel '(6 4))
(tri-sel '(6 5 8 9 4 1 2 3 7))
(tri-sel '(6 5 8 9 4 1 2 3 7 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tri-bul
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tri-fus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tri-fusion
  (lambda (L)
    (if (null? L)
        ()
        (letrec* ((split ;; coupe une liste en 2
                   (lambda (L)
                     (cond
                      ((null? L) (list () ()))
                      ((null? (cdr L)) (list L ()))
                      (else (let ((h1 (car L))
                                  (h2 (cadr L))
                                  (lsts (split (cddr L))))
                              (list (cons h1 (car lsts))
                                    (cons h2 (cadr lsts))))))))
                 ;; fusionne 2 listes triées
                 (fusion
                  (lambda (L1 L2)
                    (cond
                     ((null? L1) L2)
                     ((null? L2) L1)
                     ((< (car L1) (car L2)) (cons (car L1) (fusion (cdr L1) L2)))
                     (else (cons (car L2) (fusion L1 (cdr L2)))))))
                 ;; variables
                 (splited (split L))
                 (L1 (car splited))
                 (L2 (cadr splited)))
          (fusion (if (and (not (null? L1)) (not (null? (cdr L1))))
                      (tri-fusion L1) L1)
                  (if (and (not (null? L2)) (not (null? (cdr L2))))
                      (tri-fusion L2) L2))))))


(tri-fusion ())
(tri-fusion '(6))
(tri-fusion '(6 4))
(tri-fusion '(6 5 8 9 4 1 2 3 7))
(tri-fusion '(6 5 8 9 4 1 2 3 7 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tri-rap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tri-rap
  (lambda (L)
    (if (null? L)
        ()
        (let ((return-ies (ies (car L) L)))
          (append (tri-rap (car return-ies))
                  (cadr return-ies)
                  (tri-rap (caddr return-ies)))))))

(tri-rap ())
(tri-rap '(1))
(tri-rap '(1 4 2 5 7 8))
(tri-rap '(4 3 2 1))
