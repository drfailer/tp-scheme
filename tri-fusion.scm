;; Fusionne deux listes triées (retourne une liste triée):
;; Complexité: O(n1 + n2) ~ O(n) (dans la fonction principale)
;; Profondeur de recursion: n1 + n2
(define fusion
  (lambda (L1 L2)
    (cond
     ((null? L1) L2)
     ((null? L2) L1)
     ((< (car L1) (car L2)) (cons (car L1) (fusion (cdr L1) L2)))
     (else (cons (car L2) (fusion L1 (cdr L2)))))))

;; Sépare une liste en deux sous liste:
;; retour: (sous-liste1 sous-liste2)
;; Complexité: O(n/2)
;; Profondeur de récursion: n/2
(define split
  (lambda (L)
    (cond
     ((null? L) (list () ()))
     ((null? (cdr L)) (list L ()))
     (else (let ((h1 (car L))
                 (h2 (cadr L))
                 (lsts (split (cddr L))))
             (list (cons h1 (car lsts)) (cons h2 (cadr lsts))))))))

;; Trie une liste de nombres:
;; Complexité: O(n*log(n)):
;; split: n/2 ; fusion: n
;; split + fusion: n/2 + n ~ n
;; tri des sous-listes: log(n) (division successives de la taille de l'entrée
;; par 2)
(define tri-fusion
  (lambda (L)
    (if (null? L)
        ()
        (let* ((splited (split L))
               (L1 (car splited))
               (L2 (cadr splited)))
          (fusion (if (not (null? (cdr L1))) (tri-fusion L1) L1)
                  (if (not (null? (cdr L2))) (tri-fusion L2) L2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fusion:
(fusion () ())
(fusion '(1) ())
(fusion '(1 2 3) ())
(fusion () '(1 2 3))
(fusion '(1 3 5) '(2 4 6))

;; split:
(split ())
(split '(1))
(split '(1 2 3 4 5))
(split '(1 2 3 4 5 6))

;; tri-fusion:
(tri-fusion ())
(tri-fusion '(6))
(tri-fusion '(6 5 8 9 4 1 2 3 7))
(tri-fusion '(6 5 8 9 4 1 2 3 7 0))




;; Vérifications sur opérations de bases:
(cadr '(1 2 3 4))
(cddr '(1 2 3 4))
(cadr '((1 2 3) (4 5 6)))
(cdr ())
