#lang racket

;fitness proportionate selection, also known as roulette wheel selection
(define (roulette-wheel-selection items) 
  
  (define (select random-number items)
    (let ((random-number (- random-number (caar items))))
      (if (> random-number 0) (select random-number (cdr items)) (car items))))
  
  (let ((sumw (foldl + 0 (map car items))))
    (cond ((null? items) '())
          ((<= sumw 0) (pick-random items))
          (else (select (* (random) sumw) items)))))

(define (fifty-fifty . something) (<= (random) 0.5))

(define (build-initial-population population-size chromosome-size)
  
  (define (build-population population) 
    (if (<= population-size (length population))
        population        
        (let ((vertices (filter-map (lambda(x) (if (<= (random) 0.2);(/ (sqrt chromosome-size) chromosome-size)) 
          (arithmetic-shift 1 x) #f)) (range chromosome-size))))
          (build-population (cons (foldl bitwise-ior 0 vertices) population)))))
  
  (build-population '()))

;get vertices from chromosome, e.g. 10101 -> (1 3 4)
(define (extract-vertices chromosome chromosome-size)  
  (filter-map (lambda(vertice) (if (bitwise-bit-set? chromosome vertice) vertice #f)) (range chromosome-size)))

(define (extract-vertices-2 chromosome chromosome-size)  
  (extract-vertices (bitwise-not chromosome) chromosome-size))

;return true, if given chromosome is clique in graph, false otherwise 
(define (is-clique? chromosome chromosome-size graph-matrix)
  
  (andmap (lambda(x) (or (not (bitwise-bit-set? chromosome x)) 
                         (equal? chromosome (bitwise-and chromosome (vector-ref graph-matrix x))))) 
          (range chromosome-size)))

(define (is-clique?-2 chromosome chromosome-size graph-matrix)
  
  (foldl + 0 (filter-map (lambda(x) (if (not (bitwise-bit-set? chromosome x)) 
                                        #f 
                                        (integer-count (bitwise-and chromosome (bitwise-not (vector-ref graph-matrix x)))))) 
                         (range chromosome-size))))

;f(chromosome) = number of vertices in given chromosome, if it's a clique, 0 otherwise
(define (get-fitness chromosome chromosome-size graph-matrix)
  (if (is-clique? chromosome chromosome-size graph-matrix) (integer-count chromosome) 0))

;f(chromosome) = N_vertices * exp(- N_missing_edges / c)
;so if given chromosome is a clique, f(chromosome) = N_vertices
(define (get-fitness-2 chromosome chromosome-size graph-matrix)
  (/ (/ (* (integer-count chromosome) (integer-count chromosome)) 2) (add1 (is-clique?-2 chromosome chromosome-size graph-matrix))))
  ;(* (integer-count chromosome) 
     ;(exp (/ (* 1.0 (- (is-clique?-2 chromosome chromosome-size graph-matrix))) 100))))

;uniform crossover
(define (ux-crossover father mother chromosome-size)
    
    (let* ((intersection (bitwise-and father mother))
           (difference (bitwise-xor father mother))
           (cross (filter-map 
                  (lambda(x) (if (and (fifty-fifty) (bitwise-bit-set? difference x)) (arithmetic-shift 1 x) #f)) 
                  (range chromosome-size)))
           (crossed (foldl bitwise-ior 0 cross)))
        
        (list (bitwise-ior intersection crossed) (bitwise-ior intersection (bitwise-xor difference crossed)))))

;x, y = random, random; chromosome[x], chromosome[y] = chromosome[y], chromosome[x]
(define (swap-mutatio chromosome chromosome-size)
  
  (let ((x (random chromosome-size)) (y (random chromosome-size)))
    
    (set-bit (set-bit chromosome x (bitwise-bit-set? chromosome y)) y (bitwise-bit-set? chromosome x))))

;return number of vertices in given chromosome
(define (integer-count chromosome)
  (count (lambda(x) (bitwise-bit-set? chromosome x)) (range (integer-length chromosome))))

;x = random; chromosome[x] = not chromosome[x]
(define (inverse-mutatio chromosome chromosome-size)
  (flip-bit chromosome (random chromosome-size)))

;chromosome[idx] = 0
(define (down-bit chromosome idx) 
  (bitwise-and chromosome (bitwise-not (arithmetic-shift 1 idx))))

;chromosome[idx] = 1
(define (up-bit chromosome idx) 
  (bitwise-ior chromosome (arithmetic-shift 1 idx)))

;chromosome[idx] = not chromosome[idx]
(define (flip-bit chromosome idx) 
  (if (bitwise-bit-set? chromosome idx) (down-bit chromosome idx) (up-bit chromosome idx)))

;chromosome[idx] = value
(define (set-bit chromosome idx value)
  (if value (up-bit chromosome idx) (down-bit chromosome idx)))

;sort vertices by order
(define (sort-vertices vertices graph-order graph-matrix)    
  (sort (if (null? vertices) (range graph-order) vertices)
        #:key (lambda(vertice) (integer-count (vector-ref graph-matrix vertice))) <))

;step0:relax
(define (relax chromosome graph-matrix graph-order) 
  
  (define (remove chromosome vertices)
    
    (if (null? vertices) chromosome
        (if (and (bitwise-bit-set? chromosome (car vertices)) (< (random) 0.5))
            (remove (down-bit chromosome (car vertices)) (cdr vertices))
            (remove chromosome (cdr vertices)))))
  
  (define (add chromosome vertices)
    
    (if (null? vertices) chromosome
        (if (and (not (bitwise-bit-set? chromosome (car vertices))) (< (random) 0.1))
            (add (up-bit chromosome (car vertices)) (cdr vertices))
            (add chromosome (cdr vertices)))))
  
  ;chromosome)
  ;(let ((vertices (sort-vertices 
  ;                 (build-list (quotient graph-order 20) (lambda(x) (random graph-order))) 
  ;                 graph-order graph-matrix)))
  ;  (add (remove chromosome vertices) (reverse vertices))))
  
  (let ((vertices (sort-vertices ;(range graph-order)
                   (build-list (quotient graph-order 10) ;(inexact->exact (round (* 2 (sqrt graph-order)))) 
                               (lambda(x) (random graph-order))) 
                   graph-order graph-matrix)))
    (add (remove chromosome (take vertices (quotient (length vertices) 2))) 
         (take-right vertices (quotient (length vertices) 2)))))
  

(define (pick-random list)
  (list-ref list (random (length list))))

;E.Marchiore heorustic first step : repair
(define (repair chromosome graph-matrix graph-order) 
  
  (define (repair-proc chromosome vertices)    
    (if (null? vertices) 
        chromosome
        (let ((i (pick-random vertices)))          
          (if (< (random) 0.1)
              (repair-proc (down-bit chromosome i) (remove i vertices))
              (let ((new-chromosome (bitwise-and chromosome (vector-ref graph-matrix i))))
                (repair-proc new-chromosome (remove i (extract-vertices new-chromosome graph-order))))))))
  
  (repair-proc chromosome (extract-vertices chromosome graph-order)))

;step2 extend
(define (extend chromosome graph-matrix graph-order)
  
  (define (extend-proc chromosome vertices)
    
    (cond ((null? vertices) chromosome)
          ((bitwise-bit-set? chromosome (car vertices)) chromosome)
          (else (let ((new-chromosome (up-bit chromosome (car vertices))))
                  (extend-proc (if (is-clique? new-chromosome graph-order graph-matrix) new-chromosome chromosome) (cdr vertices))))))
  
  (extend-proc chromosome (shuffle (extract-vertices-2 chromosome graph-order))))


(define (HGA-MCP output-file task graph)

  (define (best-fitness population) (car (argmax car population)))
  (define (worst-fitness population) (car (argmin car population)))
  (define (average-fitness population) (inexact->exact (round (/ (foldl + 0 (map car population)) 1.0 (length population)))))
  
  (define (apply-HA population graph-matrix graph-order)
    ;population)
    (map (lambda(chromosome) (extend (repair (relax chromosome graph-matrix graph-order) graph-matrix graph-order) graph-matrix graph-order)) population))
  
  (define (apply-crossover population cross-rate chromosome-size)
    
    (define (build-new-population new-population population-size)      
      (if (<= population-size (length new-population))
          new-population
          (let ((father (roulette-wheel-selection population)) (mother (roulette-wheel-selection population)))
            (build-new-population (append new-population (ux-crossover (cdr father) (cdr mother) chromosome-size)) population-size))))
    
    (build-new-population '() (* cross-rate (length population))))
  
  (define (apply-mutatio population mutatio-probability chromosome-size)    
    (map (lambda(chromosome) (if (< (random) mutatio-probability) 
                                 (inverse-mutatio chromosome chromosome-size) 
                                 chromosome))  
         population))
  
  (define (apply-selection population select-number)
    
    (define (make-selection new-population)
      (if (<= select-number (length new-population)) 
          new-population
          (make-selection (cons (roulette-wheel-selection population) new-population))))
    
    ;(printf "selection0:") (print (length (remove-duplicates population))) (newline)
    ;(printf "selection1:") (print (length (remove-duplicates (make-selection '())))) (newline)
    (make-selection (list (argmax car population))))
  
  (define (map-fitness chromosome-size population graph-matrix)
    
    ;(print (length (remove-duplicates population))) (newline)
    ;(for-each (lambda(x) (printf "~b ~a\n" x (get-fitness-2 x chromosome-size graph-matrix))) population )
    
    (map (lambda(chromosome) (cons (get-fitness chromosome chromosome-size graph-matrix) chromosome)) population))
  
  (define initial-size 100)
  (define cross-rate 1)
  (define mutatio-probability 0.1) 
  ;(define elites 2)
  (define ga-steps 200)
  
  (define (find-maximum-clique graph-order population steps graph-matrix fitness-array) 
    
    (cond ((or (< steps 0) (>= (car (argmax car population)) task))

           ;(printf "best:~a found:~a steps:~a\n" task (argmax car population) steps)
           
           (with-output-to-file (string-append "out/" output-file ".out") 
            
            (lambda()
              (printf "~a\n" (list initial-size mutatio-probability (* initial-size cross-rate 1/2)))
              (printf "~a\n" (extract-vertices (cdr (argmax car population)) graph-order)) 
              (printf "~a\n" fitness-array)
              (printf "~a\n" graph))           
            #:mode 'text #:exists 'replace)
           
           (cons steps (extract-vertices (cdr (argmax car population)) graph-order)) 
           ;(print (argmax car population))(newline)
           )
          (else 
           ;(printf "begin:") (print (length (remove-duplicates population))) (newline)
           (let ((new-cross-population (append 
                                        (apply-crossover population cross-rate graph-order) 
                                        (build-initial-population (/ initial-size 2) graph-order))))
             ;(printf "cross:") (print (length (remove-duplicates new-cross-population))) (newline)
             ;(print new-cross-population)(newline)
             (let ((new-mut-population ;(apply-mutatio new-cross-population mutatio-probability graph-order)))
                   (apply-HA (apply-mutatio new-cross-population mutatio-probability graph-order) graph-matrix graph-order)))
               
               (let ((new-fit-population (map-fitness graph-order 
                                                      new-mut-population
                                                      ;(append new-mut-population 
                   ;(apply-HA (build-initial-population initial-size graph-order) graph-matrix graph-order))
                                                      graph-matrix)))
                 ;(printf "mutatio:") (print (length (remove-duplicates new-mut-population))) (newline)
                 ;(print (append population new-mut-population))(newline)
                 (let ((new-sel-population (apply-selection (append population new-fit-population) initial-size)))
                   ;(printf "selection:") (print (length (remove-duplicates new-sel-population))) (newline)
                   
                   ;(print (apply-selection (append population new-mut-population) initial-size))
                   ;(print new-sel-population)
                   (printf "~a :: best: ~a worst: ~a total: ~a different: ~a\n" 
                           steps (car (argmax car new-sel-population)) ;(caar (sort new-sel-population #:key car >)) 
                           (car (argmin car new-sel-population)) ;(car (last (sort new-sel-population #:key car >)))
                           (foldl + 0 (map car new-sel-population))
                           (length (remove-duplicates new-sel-population)))
                   
                   (find-maximum-clique graph-order new-sel-population (sub1 steps) graph-matrix
                                         ;(append fitness-array (list (best-fitness population) 
                                         ; (average-fitness population) (worst-fitness population)))  )))))))) 
                                        (append fitness-array (list (map car new-sel-population)))  ))))))))
  
  (define (build-graph-matrix graph-set graph-order)
    
    (define (make-line i) 
      
      (foldl bitwise-ior 
             0 
             (filter-map (lambda(j) (if (or (equal? i j) 
                                            (set-member? graph-set (list i j)) 
                                            (set-member? graph-set (list j i)))  
                                        (arithmetic-shift 1 j)
                                        #f)) 
                         (range graph-order))))
    
    (build-vector graph-order make-line)) 
  
  (time (let* ((graph-order (length (remove-duplicates (flatten graph))))    
         (initial-population (build-initial-population initial-size graph-order))
         (graph-matrix (build-graph-matrix (list->set graph) graph-order))
         (init-pop-fitness (map-fitness graph-order initial-population graph-matrix)))
      
      (find-maximum-clique graph-order init-pop-fitness ga-steps graph-matrix '()))))

;(HGA-MCP 100000 '((0 1) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))

(provide HGA-MCP)

;(let ((pop (map (lambda(x) (cons (random) x)) (build-initial-population 500 10))))
;  
;  (print (roulette-wheel-selection pop))
;  (print (roulette-wheel-selection pop))
;  (print (roulette-wheel-selection pop))
  
;  )
