#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2 [res '()])
  (if (and (not (collection-empty? w1))
           (not (collection-empty? w2))
           (equal? (collection-first w1) (collection-first w2)))
      (longest-common-prefix (collection-rest w1)
                             (collection-rest w2)
                             (append res (list (collection-first w1))))
      (list res w1 w2)))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words [prefix '()])
  (cond
    ([collection-empty? words] prefix)
    ([empty? prefix] (longest-common-prefix-of-collection
                      (collection-rest (collection-rest words))
                      (car (longest-common-prefix (collection-first words)
                                                  (collection-first (collection-rest words))))))
    (else (longest-common-prefix-of-collection
           (collection-rest words)
           (car (longest-common-prefix (collection-first words) prefix))))))


(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern)))
         (label (if (equal? branch #f) '() (get-branch-label branch)))
         (words (longest-common-prefix (list->stream label) pattern))
         (prefix (car words)))
    (cond
      ([equal? prefix pattern] #t)
      ([equal? branch #f] (list #f '()))
      ([and (not (equal? prefix pattern)) (not (equal? prefix label))] (list #f prefix))
      (else (list label (caddr words) (get-branch-subtree branch))))))
  
  
(define (st-has-pattern? st pattern)
  (let* ((result (match-pattern-with-label st pattern)))
    (if (list? result)
        (if (equal? (car result) #f)
            #f
            (st-has-pattern? (caddr result) (cadr result)))
        #t)))
 

(define (get-suffixes text)
  (cond
    ([collection-empty? text] empty-collection)
    (else (collection-cons text (get-suffixes (cdr text))))))


(define (get-ch-words words ch)
  (collection-filter (λ (word) (and (not (null? word)) (equal? (car word) ch))) words))


(define (ast-func suffixes)
  (if (collection-empty? suffixes)
      suffixes
      (cons (list (collection-first (collection-first suffixes))) (collection-map stream-rest suffixes))))


(define (cst-func suffixes)
  (cond
    ([collection-empty? suffixes] suffixes)
    ([collection-empty? (collection-rest suffixes)] (cons (collection-first suffixes) empty-collection))
    (else (let* ((prefix (longest-common-prefix-of-collection suffixes)))
            (cons prefix (collection-map (λ (word) (caddr (longest-common-prefix prefix word))) suffixes))))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (let* ((partial-st-with-null (collection-map (λ (chr) (labeling-func
                                                         (get-ch-words suffixes chr)))
                                               alphabet))
         (partial-st (collection-filter (λ (x) (pair? x))
                                        partial-st-with-null)))
    (collection-map (λ (branch)
                      (if (collection-empty? (cdr branch))
                          branch
                          (cons (car branch) (suffixes->st labeling-func (cdr branch) alphabet))))
                    partial-st)))



; nu uitați să convertiți alfabetul într-un flux
(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define (text->st labeling-func)
  (λ (text)
    (let* ((suffixes (get-suffixes (append text '(#\$))))
           (alphabet (list->stream (sort (remove-duplicates (map car (stream->list suffixes))) char<?))))
      (suffixes->st labeling-func suffixes alphabet))))


(define text->ast
  (text->st ast-func))


(define text->cst
  (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (truncate-list list len [new-list '()])
  (if (zero? len)
      (reverse new-list)
      (truncate-list (cdr list) (sub1 len) (cons (car list) new-list))))

(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    (let iter-branches ((st st) (substring '()))
      (if (st-empty? st)
          #f
          (let* ((first-br (first-branch st))
                 (br-subtree (get-branch-subtree first-br))
                 (other-br (other-branches st)))
            (cond
              ([and (st-empty? br-subtree) (st-empty? other-br)] #f)
              ([>= (length substring) len] (truncate-list substring len))
              (else (let ((child-search (iter-branches br-subtree (append substring (get-branch-label first-br))))
                          (brother-search (iter-branches other-br substring)))
                      (if (list? child-search)
                          child-search
                          brother-search)))))))))
