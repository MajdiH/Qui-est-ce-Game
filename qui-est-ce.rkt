; Constructeur de la classe personne
(define	make-personne	(lambda (prenom0 sexe0 barbe0 moustache0 lunette0 colorHair0 colorEye0 hat0)
                          (let	((prenom	prenom0)
                                 (sexe	sexe0)
                                 (barbe barbe0)
                                 (moustache moustache0)
                                 (lunette lunette0)
                                 (colorHair colorHair0)
                                 (colorEye colorEye0)
                                 (hat hat0)
                                 )
                            (lambda	(message	.	params)
                              (cond	((eqv?	message	'getPrenom)	prenom)
                                        ((eqv?	message	'getSexe)	sexe)
                                        ((eqv?	message	'getBarbe)	barbe)
                                        ((eqv?	message	'getMoustache)	moustache)
                                        ((eqv?	message	'getLunette)	lunette)
                                        ((eqv?	message	'getCheveux)	colorHair)
                                        ((eqv?	message	'getYeux)	colorEye)
                                        ((eqv?	message	'getChapeau)	hat)
                                        ((eqv?	message	'affiche)	(list prenom sexe barbe moustache lunette colorHair colorEye hat))                                        
                                        (else
                                         (print	(list  "Je ne comprends pas"	message))
                                         )
                                        )
                              )
                            )
                          )
  )

; Constructeur de la classe jeu
(define	make-jeu	(lambda ()
                          (letrec ((suspects0 (list    Sam Alex Peter Maria Anne Claire Philip Alfred Bill Herman Anita Paul Franz Max Richard Bernard Robert Susan Charles David Tom Joe George Eric ))
                                   ;Les suspects 
                                   (lesSuspects suspects0)
                                   ;Le coupable est un random
                                   (leCoupable	(random-liste lesSuspects))
                                   ;Affiche les suspects
                                   (afficherSuspects (lambda (l)
                                                       (if (null? l)
                                                           ()
                                                           (cons ((car l) 'affiche)
                                                                 (afficherSuspects (cdr l))
                                                                 )
                                                           )
                                                       )
                                                     )
                                   ;Poser les questions
                                   (poserQuestion (lambda (l params)
                                                    (cond ((null? l)
                                                           ())
                                                          ((or (eqv? (car params) cheveux?) (eqv? (car params) yeux?))
                                                           (if (eqv? ((car params) (car l) (cadr params)) ((car params) leCoupable (cadr params)))
                                                               (cons (car l) (poserQuestion (cdr l) params))
                                                               (poserQuestion (cdr l) params)
                                                               ))
                                                          (else (if (eqv? ((car params) (car l)) ((car params) leCoupable))
                                                                    (cons (car l) (poserQuestion (cdr l) params))
                                                                    (poserQuestion (cdr l) params)
                                                                    )
                                                                )
                                                          )
                                                    )
                                                  )
                                   )
                            (lambda	(message	.	params)
                              (cond	((eqv?	message	'afficheSuspects) (afficherSuspects lesSuspects))
                                        ((eqv?	message	'afficheCoupable) (leCoupable 'affiche))
                                        ((eqv?	message	'poseQuestion) (set! lesSuspects (poserQuestion lesSuspects params))
                                                                      (cond ((eqv? (longueur lesSuspects) 1)
                                                                             (format "Coupable trouve. ~a"(leCoupable 'getPrenom)))
                                                                            (else
                                                                             (afficherSuspects lesSuspects))))
                                        (else
                                         (print	(list  "Je ne comprends pas"	message))
                                         )
                                        )
                              )
                            )
                          )
  )



; Les suspects  
(define Sam (make-personne 'Sam 'homme 'pas-barbe 'moustache 'lunettes 'chauve 'marron 'pas-chapeau))
(define Alex (make-personne 'Alex 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'noir 'marron 'pas-chapeau))
(define Peter (make-personne 'Peter 'homme 'pas-barbe 'moustache 'pas-lunettes 'blanc 'bleu 'pas-chapeau))
(define Maria (make-personne 'Maria 'femme 'pas-barbe 'pas-moustache 'pas-lunettes 'chatain 'marron 'chapeau))
(define Anne (make-personne 'Anne 'femme 'pas-barbe 'pas-moustache 'pas-lunettes 'noir 'marron 'pas-chapeau))
(define Claire (make-personne 'Claire 'femme 'pas-barbe 'pas-moustache 'lunettes 'roux 'vert 'chapeau))
(define Philip (make-personne 'Philip 'homme 'barbe 'moustache 'pas-lunettes 'noir 'marron 'pas-chapeau))
(define Alfred (make-personne 'Alfred 'homme 'barbe 'moustache 'pas-lunettes 'roux 'bleu 'pas-chapeau))
(define Bill (make-personne 'Bill 'homme 'barbe 'moustache 'pas-lunettes 'chauve 'marron 'pas-chapeau))
(define Herman (make-personne 'Herman 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'chauve 'bleu 'pas-chapeau))
(define Anita (make-personne 'Anita 'femme 'pas-barbe 'pas-moustache 'pas-lunettes 'blond 'bleu 'pas-chapeau))
(define Paul (make-personne 'Paul 'homme 'pas-barbe 'pas-moustache 'lunettes 'blanc 'marron 'pas-chapeau))
(define Franz (make-personne 'Franz 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'roux 'vert 'pas-chapeau))
(define Max (make-personne 'Max 'homme 'barbe 'moustache 'pas-lunettes 'noir 'marron 'pas-chapeau))
(define Richard (make-personne 'Richard 'homme 'barbe 'moustache 'lunettes 'chauve 'marron 'pas-chapeau))
(define Bernard (make-personne 'Bernard 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'chatain 'marron 'chapeau))
(define Robert (make-personne 'Robert 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'chatain 'bleu 'pas-chapeau))
(define Susan (make-personne 'Susan 'femme 'pas-barbe 'pas-moustache 'pas-lunettes 'blanc 'bleu 'pas-chapeau))
(define Charles (make-personne 'Charles 'homme 'pas-barbe 'moustache 'pas-lunettes 'blond 'marron 'pas-chapeau))
(define David (make-personne 'David 'homme 'barbe 'moustache 'pas-lunettes 'blond 'marron 'pas-chapeau))
(define Tom (make-personne 'Tom 'homme 'pas-barbe 'pas-moustache 'lunettes 'chauve 'bleu 'pas-chapeau))
(define Joe (make-personne 'Joe 'homme 'pas-barbe 'pas-moustache 'lunettes 'blond 'vert 'pas-chapeau))
(define George (make-personne 'George 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'blanc 'vert 'chapeau))
(define Eric (make-personne 'Eric 'homme 'pas-barbe 'pas-moustache 'pas-lunettes 'blond 'marron 'chapeau))


; prédicat homme?  
(define homme? (lambda (p)
                 (eqv? (p 'getSexe) 'homme)
                 )
  )
; prédicat femme?
(define femme? (lambda (p)
                 (eqv? (p 'getSexe) 'femme)
                 ))

; prédicat barbe?  
(define barbe? (lambda (p)
                 (eqv? (p 'getBarbe) 'barbe)
                 ))

; prédicat moustache?  
(define moustache? (lambda (p)
                     (eqv? (p 'getMoustache) 'moustache)
                     ))
; prédicat lunette?  
(define lunettes? (lambda (p)
                    (eqv? (p 'getLunette) 'lunettes)
                    ))

; prédicat cheveux?  )
(define cheveux? (lambda (p couleur)
                   (eqv? (p 'getCheveux) couleur)
                   ))
; prédicat yeux?  
(define yeux? (lambda (p couleur)
                (eqv? (p 'getYeux) couleur)
                ))

; prédicat chapeau?  
(define chapeau? (lambda (p)
                   (eqv? (p 'getChapeau) 'chapeau)
                   ))

; Des prédicats donnés
(define longueur (lambda (l)
                   (if (null? l)
                       0
                       (+ 1 (longueur (cdr l)))
                       )
                   )
  )

(define n-ieme (lambda (l n)
                 (if (= n 1)
                     (car l)
                     (n-ieme (cdr l) (- n 1))
                     )
                 )
  )

(define random-liste (lambda (l)
                       (letrec ((n (longueur l))
                                (r (+ 1 (random n)))
                                )
                         (n-ieme l r)
                         )
                       )
  )


;creer le jeu
(define leJeu (make-jeu))


(leJeu 'afficheSuspects)
(print "Poser les questions")
;(leJeu 'afficheCoupable)