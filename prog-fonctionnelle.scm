;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Feuille 1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(fibo 8)
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
(define tri-bul
  (lambda (L)
    (letrec ((parcour-echange
              (lambda (L)
                (cond
                 ((null? L) ())
                 ((null? (cdr L)) (list (car L) ()))
                 (else
                  (let ((r (parcour-echange (cdr L))))
                    (if (< (car L) (car r))
                        (list (car L) (cons (car r) (cadr r)))
                        (list (car r) (cons (car L) (cadr r))))))))))
      (if (null? L)
          ()
          (let ((s (parcour-echange L)))
            (cons (car s) (tri-bul (cadr s))))))))

(tri-bul ())
(tri-bul '(1))
(tri-bul '(2 4 3 9 6 7))
(tri-bul '(4 3 2 1))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Feuille 2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exist?
  (lambda (p L)
    (if (null? L)
        #f
        (or (p (car L)) (exist? p (cdr L))))))

(define pour-tout?
  (lambda (p L)
                (if (null? L)
                    #t
                    (and (p (car L)) (pour-tout? p (cdr L))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tous-egaux (existanciel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tous-egaux-e
  (lambda (L)
    (if (null? L)
        #t
        (not (exist? (lambda (x) (not (= x (car L)))) L)))))

(tous-egaux-e ())
(tous-egaux-e '(1))
(tous-egaux-e '(1 1 1 1))
(tous-egaux-e '(1 1 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tous-egaux (universel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tous-egaux-u
  (lambda (L)
    (if (null? L)
        #t
        (pour-tout? (lambda (x) (= x (car L))) L))))

(tous-egaux-u ())
(tous-egaux-u '(1))
(tous-egaux-u '(1 1 1 1))
(tous-egaux-u '(1 1 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tous-diff (existanciel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tous-diff-e
  (lambda (L)
    (if (null? L)
        #t
        (and (not (exist? (lambda (x) (= x (car L))) (cdr L)))
             (tous-diff-e (cdr L))))))

(tous-diff-e ())
(tous-diff-e '(1))
(tous-diff-e '(1 1 1 1))
(tous-diff-e '(5 1 2 1))
(tous-diff-e '(4 2 1 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tous-diff (universel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tous-diff-u
  (lambda (L)
    (if (null? L)
        #t
        (and (pour-tout? (lambda (x) (not (= x (car L)))) (cdr L))
             (tous-diff-u (cdr L))))))

(tous-diff-u ())
(tous-diff-u '(1))
(tous-diff-u '(1 1 1 1))
(tous-diff-u '(1 1 2 1))
(tous-diff-u '(4 2 1 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema de recurrences simples:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define schema-rec
  (lambda (f arret neutre n)
    (if (arret n)
        neutre
        (f (schema-rec f arret neutre (- n 1)) n))))

(schema-rec (lambda (x y) (+ x y)) (lambda (x) (= x 0)) 0 10)
(schema-rec (lambda (x y) (+ x (* y y))) (lambda (x) (= x 0)) 0 10)
(schema-rec (lambda (x y) (* x y)) (lambda (x) (= x 1)) 1 5)
(schema-rec (lambda (x y) (list (cadr x) (+ (car x) (cadr x))))
            (lambda (x) (= x 0))
            '(1 1)
            10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema de recurrences double:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sh-rec-double
  (lambda (f init0 init1 n)
    (cond
     ((= 0 n) init0)
     ((= 1 n) init1)
     (else
      (f (sh-rec-double f init0 init1 (- n 1))
         (sh-rec-double f init0 init1 (- n 2))
         n)))))

(sh-rec-double (lambda (u1 u2 n) (+ u1 u2)) 1 1 5)
(sh-rec-double (lambda (u1 u2 n) (+ u1 u2)) 1 1 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 3:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reflexive
  (lambda (E R)
    (pour-tout? (lambda (x) (R x x)) E)))

(reflexive () (lambda (x y) (= x y)))
(reflexive '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= x y)))
(reflexive '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= (* x y) 0)))
(reflexive '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= (modulo x y) 0)))

(define symetrique
  (lambda (E R)
    (pour-tout? (lambda (x)
       (pour-tout? (lambda (y)
          (if (R x y)
              (R y x)
              #t))
        E)) E)))

(symetrique () (lambda (x y) (= x y)))
(symetrique '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= x y)))
(symetrique '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= (* x y) 0)))
(symetrique '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= (modulo x y) 0)))

(define transitive
  (lambda (E R)
    (pour-tout? (lambda (x)
       (pour-tout? (lambda (y)
          (pour-tout? (lambda (z)
             (if (and (R x y) (R y z))
                 (R x z)
                 #t))
           E)) E)) E)))

(transitive () (lambda (x y) (= x y)))
(transitive '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= x y)))
(transitive '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= (* x y) 0)))
(transitive '(1 2 3 1 4 5 3 6 7 4 8 9) (lambda (x y) (= (modulo x y) 0)))

 
;; E '(0 1 2 3 4 5 6)) 
;; R (lambda (x y) (= (modulo (- x y) 3) 0)))
;; Résultat:
;; ((0 3 6) (1 4) (2 5)). 

(define quotient
  (lambda (E R)
    (if (null? E)
        ()
        (letrec ((find-group (lambda (e l)
                               (if (null? l)
                                   (list (list e))
                                   (if (R e (caar l))
                                          (cons (cons e (car l)) (cdr l))
                                          (cons (car l) (find-group e (cdr l))))))))
          (find-group (car E) (quotient (cdr E) R))))))

(quotient '(0 1 2 3 4 5 6) (lambda (x y) (= (modulo (- x y) 3) 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 4:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PC
  (lambda (E n)
    (if (= 0 n)
        '(())
        (let ((prev (PC E (- n 1))))
          (append-map (lambda (x)
                 (map (lambda (y)
                        (cons x y)) prev)) E)))))

(PC '(0 1) 0)
(PC '(0 1) 1)
(PC '(0 1) 2)
(PC '(0 1) 3)
(PC '(0 1) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 5:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define chemin
  (lambda (dep but G)
    (if (equal? dep but)
        (list but)
        (letrec* ((find (lambda (d g) ;; find d and remove it from g
                         (if (null? g)
                             '(() ("null" ()))
                             (if (equal? d (caar g))
                                 (list (cdr g) (car g))
                                 (let ((t (find d (cdr g))))
                                   (list (cons (car g) (car t)) (cadr t)))))))
                  (sd (find dep G)))
          (map (lambda (x) (cons dep x))
               (if (null? cadadr)
                   ()
                   (append-map (lambda (y) (chemin y but (car sd))) (cadadr sd))))))))


(chemin 'D 'D '((D (A C)) (A (D C B)) (C (B)) (B ())))
(chemin 'D 'A '((D (A C)) (A (D C B)) (C (B)) (B ())))
(chemin 'D 'C '((D (A C)) (A (D C B)) (C (B)) (B ())))
(chemin 'D 'B '((D (A C)) (A (D C B)) (C (B)) (B ())))
(chemin 'B 'D '((D (A C)) (A (D C B)) (C (B)) (B ())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 6:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define trace
  (lambda (M)
    (if (null? M)
        0
        (+ (caar M) (trace (map cdr (cdr M)))))))

(trace ())
(trace '((1 2 3) (4 5 6) (7 8 9)))

(define transp
  (lambda (M)
    (if (or (null? M) (null? (car M)))
        ()
        (cons (map car M) (transp (map cdr M))))))

(transp ())
(transp '((1 2 3) (4 5 6) (7 8 9)))

(define MV
  (lambda (M V)
    (if (null? M)
        ()
        (map (lambda (x) (apply + (map * x V))) M))))

(MV () '(1 2 3))
(MV '((1 2 3) (4 5 6) (7 8 9)) '(1 1 1))
(MV '((1 2 3) (4 5 6) (7 8 9)) '(1 0 0))

(define AL
  (lambda (M)
    (lambda (x)
      (MV M x))))

(AL '((1 2 3) (4 5 6) (7 8 9)))
((AL '((1 2 3) (4 5 6) (7 8 9))) '(1 2 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 7:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

((compose (lambda (x) (+ x 2)) (lambda (y) (* y 2))) 3)

(define trace-fn
  (lambda (f n)
    (letrec ((composeN (lambda (F N)
                         (if (= N 0)
                             (lambda (x) x)
                             (compose F (composeN F (- N 1)))))))
      (if (= n 0)
          (list (composeN f 0))
          (append (trace-fn f (- n 1)) (list (composeN f n)))))))

(map (lambda (f) (f 1)) (trace-fn (lambda (x) (* x 2)) 4))

(define applique
  (lambda (Lf x)
    (map (lambda (f) (f x)) Lf)))

(applique (trace-fn (lambda (x) (* x x)) 3) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 8:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define P
  (lambda (E)
    (if (null? E)
        '(())
        (let ((s (P (cdr E))))
          (append s (map (lambda (x) (cons (car E) x)) s))))))

;; (append (map (lambda (x) append (car E)) (P (cdr E))) (P (cdr E)))
(P '(1 2 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 9:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define P2
  (lambda (E)
    (if (null? (cddr E))
        (list (list (list (car E)) (cdr E)))
        (let ((r (P2 (cdr E))))
          (append (map (lambda (x) (list (cons (car E) (car x)) (cadr x))) r)
                  (map (lambda (x) (list (car x) (cons (car E) (cadr x)))) r)
                  (list (cons (list (car E)) (list (cdr E)))))))))

(P2 '(1 2))
(P2 '(1 2 3))
(P2 '(1 2 3 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 10:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1-a
(define fct
  (lambda (d a)
    (lambda (x)
      (if (null? d)
          ()
          (if (equal? x (car d))
              (car a)
              ((fct (cdr d) (cdr a)) x))))))

((fct '(a b c) '(x y z)) 'b)
((fct '(a b c d) '(x y z t)) 'a)
((fct '(a b c d) '(x y z t)) 'd)

;; 1-b
(define LF
  (lambda (E F)
    (let* ((n (length E))
          (pf (PC F n)))
      (map (lambda (x)
             (fct x E))
           pf))))

(LF '(a b c) '(x y z))

;; 2-a
(define SREX10
  (lambda (L init f)
    (if (null? L)
        init
        (f (car L) (SREX10 (cdr L) init f)))))

(define filter
  (lambda (E P)
    (SREX10 E () (lambda (u1 u2)
                   (if (P u1)
                       (cons u1 u2)
                       u2)))))

(filter '(1 a 2 b) integer?)

;; 2-b
(define qqs?
  (lambda (L P)
    (if (null? L)
        #t
        (and (P (car L)) (qqs? (cdr L) P)))))

(define stabilise?
  (lambda (f P)
    (letrec ((appartien? (lambda (l n)
                           (if (null? l)
                               #f
                               (or (equal? (car l) n) (appartien? (cdr l) n))))))
      (qqs? P (lambda (x) (appartien? P (f x)))))))

(stabilise? (lambda (x) x) '(1 2 3 4))
(stabilise? (lambda (x) (+ 1 x)) '(1 2 3 4))
