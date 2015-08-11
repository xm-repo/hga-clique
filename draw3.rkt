
#lang racket/gui

(require racket/gui/base)
(require racket/draw)
(require racket/dict)
(require plot)
(require plot/utils)

;main window size
(define window_width 950)
(define window_height 700)
;canvas size
(define picture_width 950)
(define picture_height 3500)
;graph picture size
(define graph_width 900)
(define graph_height 600)
;plot picture size
(define plot_width 800)
(define plot_height 550)

;this is main window proc
(define (main-window)
  
  (define frame (new frame% [label "Visualization"] [style '(no-resize-border toolbar-button)] 
                     [width window_width] [height window_height])) ;make main window
  
  (send frame create-status-line) ;make status line
  (send frame set-status-text "Click menu File -> Load and open data file!")
  
  (define canvas (new canvas% [parent frame] [style '(vscroll border)] 
                      [paint-callback (lambda(canvas dc) 
                                        (send dc set-smoothing 'smoothed) 
                                        (send dc draw-bitmap picture 0 0))])) ;make main canvas
  (send canvas init-auto-scrollbars picture_width picture_height 0.0 0.0) ;show vertical scroll-bar
  
  (define picture (send canvas make-bitmap picture_width picture_height)) ;make main bitmap picture
  (define dc (new bitmap-dc% [bitmap picture])) ;get drawing context from main bitmap picture
  (send dc set-smoothing 'smoothed)
  (send dc set-background "white")
  
  (define menu-bar (new menu-bar% [parent frame] [demand-callback (lambda(m)(void))])) ;make menu bar  
  (define file-menu (new menu% [label "File"] [parent menu-bar] [help-string "Fain menu"])) ;make menu-item "File"
  
  (define (on-menu-save m-item c-event) ;save current picture to file
    (let ((filepath (put-file "Save picture!" frame "." "picture" "jpg")))
      (if filepath 
          (begin
            (send picture save-file filepath 'jpeg 100) 
            (send frame set-status-text (string-append "Picture saved to " (path->string filepath))))
          #f)))
  (define menu-save (new menu-item% [label "Save"] [parent file-menu] 
                         [callback on-menu-save] [help-string "Save picture on disk!"])) ;make menu item File->Save 
  
  (define (on-menu-load m-item c-event) ;load data from file and begin drawing on picture
    (let ((filepath (get-file "Load data file!" frame "." "data" "txt")))
      (define (call-load-and-draw) (load-and-draw frame misc-menu dc filepath))
      (if filepath (thread call-load-and-draw) #f)))
  (define menu-load (new menu-item% [label "Load"] [parent file-menu] 
                         [callback on-menu-load] [help-string "Load data from disk!"])) ;make menu item File->Load

  (define misc-menu (new menu% [label "Miscellaneous"] [parent menu-bar] [help-string "Miscellaneous menu"])) ;make menu-item "Miscellaneous"

  (define about-string (string-append "Visualization of a heuristic based genetic\n"
                                      "algorithm for the maximum clique problem.\n"
                                      "\nFaculty of Computational Mathematics and Cybernetics\n"
                                      "Moscow State University"))
  (define (on-menu-about m-item c-event) (message-box "About..." about-string frame '(ok no-icon)))
  (define menu-misc-about (new menu-item% [label "About..."] [parent misc-menu] 
                         [callback on-menu-about] [help-string "Show about message box!"])) ;make menu item Misc->About...
  
  (define misc-menu-sep (new separator-menu-item% [parent misc-menu]))

  (define (on-menu-2dplot m-item c-event) (send canvas scroll #f (/ 350 picture_height)))
  (define menu-misc-2dplot (new menu-item% [label "Show 2d generation-fitness plot"] [parent misc-menu] 
                         [callback on-menu-2dplot] [help-string "Scroll to 2d plot!"])) ;make menu item Misc->Show 2d plot
  (define (on-menu-3dplot m-item c-event) (send canvas scroll #f (/ 1150 picture_height)))
  (define menu-misc-3dplot (new menu-item% [label "Show 3d generation-fitness plot"] [parent misc-menu] 
                         [callback on-menu-3dplot] [help-string "Scroll to 3d plot!"])) ;make menu item Misc->Show 3d plot
  (define (on-menu-splot m-item c-event) (send canvas scroll #f (/ 2050 picture_height)))
  (define menu-misc-splot (new menu-item% [label "Show solution plot"] [parent misc-menu] 
                         [callback on-menu-splot] [help-string "Scroll to solution plot!"])) ;make menu item Misc->Show solution plot

  (send frame show #t))

(define (load-and-draw frame misc-menu drawing-context filepath) 
  (with-input-from-file filepath #:mode 'text 
    (lambda () 
      
      (send frame set-status-text (string-append "Loading data from " (path->string filepath)))
      (send drawing-context set-background "white")
      (send drawing-context clear)
      
      (let ((header (read)) (clique (read)) (fitness-array (map (lambda(f-arr) (sort f-arr <)) (read))) (graph (read)))  
        
        (send frame set-status-text 
              (string-append "Loading data from " (path->string filepath) ":   header"))
        (draw-header drawing-context filepath header clique fitness-array graph)
        (send frame refresh)
        
        (send frame set-status-text 
              (string-append "Loading data from " (path->string filepath) ":   2d plot"))
        (simple-plot drawing-context fitness-array plot_width plot_height)
        (send frame refresh)
        
        (send frame set-status-text 
              (string-append "Loading data from " (path->string filepath) ":   3d plot"))
        (smart-plot drawing-context fitness-array plot_width plot_height)
        (send frame refresh)
        
        (send frame set-status-text 
              (string-append "Loading data from " (path->string filepath) ":   solution plot"))
        (solution-plot drawing-context frame filepath clique graph graph_width graph_height)
        (send frame refresh)

        (define 3dplot-frame (smart-plot-2 drawing-context fitness-array plot_width plot_height))
        (define (on-menu-zzz m-item c-event) (send 3dplot-frame show #t))
        (define menu-sep3dplot (new menu-item% [label "Show 3d generation-fitness plot in separate window"] [parent misc-menu] 
                                  [callback on-menu-zzz] [help-string "Show 3d plot in separate window!"])) ;make menu item Misc->Show separate 3d plot
        
        (send frame set-status-text 
              (string-append "Loading data from " (path->string filepath) ":   loaded successfully"))))))

;draw summary, e.g. graph size, clique size, ga steps, etc.
(define (draw-header drawing-context filepath header clique fitness-array graph) 
  ;(send drawing-context set-brush "white" 'solid)
  ;(send drawing-context draw-rectangle 0 0 picture_width picture_height)
  (send drawing-context set-font (make-font #:size 20 #:family 'system #:weight 'normal #:underlined? #t #:smoothing 'partly-smoothed))
  (send drawing-context draw-text (string-append "Summary for file " (path->string filepath)) 20 20)
  (send drawing-context set-font (make-font #:size 16 #:family 'system #:weight 'normal #:smoothing 'partly-smoothed))
  (send drawing-context draw-text 
        (string-append "Graph order: |V(G)| = " (number->string (length (remove-duplicates (flatten graph))))) 50 60) 
  (send drawing-context draw-text (string-append "Graph size: |E(G)| = " (number->string (length (remove-duplicates graph)))) 50 80)
  
  (define (makeclique-string)
    (define str (string-join (map number->string (sort (set->list clique) <)) ","))
    (list->string (map (lambda(x y) (if (and (equal? x #\,) (zero? (modulo y 10))) #\linefeed x)) 
      (string->list str) (range (string-length str)))))

  (send drawing-context draw-text 
        (string-append "Clique size: |V(C)| = " (number->string (set-count clique))) 50 110)
  (send drawing-context draw-text 
        (string-append "Clique: V(C) = { " (string-join (map number->string (sort (set->list clique) <)) ",") " }") 50 130)
  (send drawing-context draw-text 
        (string-append "GA steps: " (number->string (length fitness-array))) 50 160)
  (send drawing-context draw-text (string-append "Population size: " (number->string (first header))) 50 180)
  (send drawing-context draw-text (string-append "Mutation probability: " (number->string (second header))) 50 200)
  (send drawing-context draw-text (string-append "Crossovers: " (number->string (third header))) 50 220))


;2d fitness plot
(define (simple-plot drawing-context fitness-array width height) 
  
  (send drawing-context set-font (make-font #:size 20 #:family 'system #:weight 'normal #:underlined? #t #:smoothing 'partly-smoothed))
  (send drawing-context draw-text "Generation-fitness 2d plot" 20 300)
  
  (define (best-fitness step) (if (>= step (length fitness-array)) -1 (argmax values (list-ref fitness-array step))))
  (define (worst-fitness step) (if (>= step (length fitness-array)) -1 (argmin values (list-ref fitness-array step))))
  (define (average-fitness step) (if (>= step (length fitness-array)) -1 
                                     (/ (foldl + 0 (list-ref fitness-array step)) 1.0 (length (list-ref fitness-array step)))))
  
  (define x-max0 (sub1 (length fitness-array)))
  (define x-mids0 (linear-seq 0 x-max0 (add1 x-max0)))
  (define y-max0 (foldl max -1 (flatten fitness-array)))
  (define y-min0 (foldl min y-max0 (flatten fitness-array)))
  
  (plot-x-label "generation")
  (plot-y-label "fitness")
  (plot-font-size 16)
  (label-point-size 0.00001)
  
  (plot/dc (list (tick-grid)
                 
                 (lines-interval (for/list ([x (in-list x-mids0)]) (vector x (best-fitness x)))
                                 (for/list ([x (in-list x-mids0)]) (vector x (worst-fitness x)))
                                 #:x-min -2 #:x-max (add1 x-max0)
                                 #:y-min y-min0 #:y-max (* y-max0 1.2)
                                 #:color 6 #:alpha 0.7 #:style 'crossdiag-hatch)
                 
                 (lines (for/list ([x (in-list x-mids0)]) (vector x (best-fitness x)))                 
                        #:x-min 0 #:x-max (add1 x-max0)
                        #:y-min y-min0 #:y-max y-max0
                        #:color "black" #:width 0.8 #:label "best fitness" #:style 'solid)
                 
                 (lines (for/list ([x (in-list x-mids0)]) (vector x (worst-fitness x)))                 
                        #:x-min 0 #:x-max (add1 x-max0)
                        #:y-min y-min0 #:y-max y-max0
                        #:color "red" #:width 0.8 #:label "worst fitness")
                 
                 (lines (for/list ([x (in-list x-mids0)]) (vector x (average-fitness x)))                 
                        #:x-min 0 #:x-max (add1 x-max0)
                        #:y-min y-min0 #:y-max y-max0
                        #:color "blue" #:width 0.8 #:label "average fitness")
                 
                 #|(points (append (for/list ([x (in-list x-mids0)]) (vector x (best-fitness x)))
                                 (for/list ([x (in-list x-mids0)]) (vector x (average-fitness x)))
                                 (for/list ([x (in-list x-mids0)]) (vector x (worst-fitness x))))                    
                         #:x-min -1 #:x-max (add1 x-max0)
                         #:y-min (sub1 y-min0) #:y-max (add1 y-max0)
                         #:fill-color "black" #:size 1.5 #:sym '4star #:color "black" #:line-width 1)|#
                 
                 (for/list ([x (in-list (filter (lambda(x) (zero? (remainder x 100))) x-mids0))]) 
                   (point-label (vector x (best-fitness x)) #:alpha 0.3 #:color "black" #:size 10 
                                #:angle (if (< (best-fitness x) (best-fitness (add1 x))) (- 0.2) (+ 0.2))
                                #:anchor (if (< (best-fitness x) (best-fitness (add1 x))) 'top 'bottom)))
                 
                 (for/list ([x (in-list (filter (lambda(x) (zero? (remainder (+ 30 x) 100))) x-mids0))]) 
                   (point-label (vector x (worst-fitness x)) #:alpha 0.3 #:color "black" #:size 10
                                #:angle (if (< (worst-fitness x) (worst-fitness (add1 x))) (- 0.2) (+ 0.2))
                                #:anchor (if (< (worst-fitness x) (worst-fitness (add1 x))) 'top 'bottom)))
                 
                 (for/list ([x (in-list (filter (lambda(x) (zero? (remainder (+ 90 x) 100))) x-mids0))]) 
                   (point-label (vector x (average-fitness x)) #:alpha 0.3 #:color "black" #:size 10
                                #:angle (if (< (average-fitness x) (average-fitness (add1 x))) (- 0.2) (+ 0.2))
                                #:anchor (if (< (average-fitness x) (average-fitness (add1 x))) 'top 'bottom)))
                                ) drawing-context 50 370 width height))

;3d fitness plot in separate window
(define (smart-plot-2 drawing-context fitness-array width height)
  
  (define frame (new frame% [label "Visualization"] [style '(no-resize-border toolbar-button)] 
                     [width width] [height height])) ;make main window
  
  (define (average-fitness step) (if (>= step (length fitness-array)) -1 
                                     (/ (foldl + 0 (list-ref fitness-array step)) 1.0 (length (list-ref fitness-array step)))))
  
  (define x-max (length fitness-array))
  (define y-max (length (car fitness-array)))
  (define z-max (foldl max -1 (flatten fitness-array)))
  (define z-min (foldl min z-max (flatten fitness-array)))
  
  (define (norm2 x y) (list-ref (list-ref fitness-array x) y))
  (define x-ivls (bounds->intervals (linear-seq 0 x-max (add1 x-max))))
  (define y-ivls (bounds->intervals (linear-seq 1 (add1 y-max) (add1 y-max))))
  (define x-mids (linear-seq 0 x-max (add1 x-max)))
  (define y-mids (linear-seq 0 y-max (add1 y-max)))
  
  (plot-title "Generation-fitness 3d plot")
  (plot-x-label "generation")
  (plot-y-label "individual")
  (plot-z-label "fitness")
  (plot-z-far-label "fitness")
  (plot-font-size 16)
  (plot3d-angle 30)
  (plot3d-altitude 35)
  (plot-legend-anchor 'top-right)
  ;(plot3d-ambient-light 1)
  (plot3d-frame (list (rectangles3d (append*
                                  (for/list ([y-ivl (in-list y-ivls)] [y (in-list y-mids)])
                                    (for/list ([x-ivl (in-list (filter-map (lambda(x y) (if (even? y) x #f)) x-ivls (range x-max)))]
                                               [x (in-list (filter-map (lambda(x y) (if (even? y) x #f)) x-mids (range (add1 x-max))))])
                                      (define z (norm2 x y))
                                      (vector x-ivl y-ivl (ivl 0 z)))))
                                 #:x-min 0 #:x-max x-max
                                 #:y-min 0 #:y-max (add1 y-max)
                                 #:z-min z-min #:z-max (* 2 z-max)
                                 #:alpha 1  #:style 'solid
                                 #:color "gray" #:line-color "black" #:line-width 0.5 ;#:line-style '                  
                                 #:label "fitness")
                   (rectangles3d (append*
                                  (for/list ([y-ivl (in-list y-ivls)] [y (in-list y-mids)])
                                    (for/list ([x-ivl (in-list (filter-map (lambda(x y) (if (odd? y) x #f)) x-ivls (range x-max)))]
                                               [x (in-list (filter-map (lambda(x y) (if (odd? y) x #f)) x-mids (range (add1 x-max))))])
                                      (define z (norm2 x y))
                                      (vector x-ivl y-ivl (ivl 0 z)))))
                                 #:x-min 0 #:x-max x-max
                                 #:y-min 0 #:y-max (add1 y-max)
                                 #:z-min z-min #:z-max (* 2 z-max)
                                 #:alpha 1  #:style 'solid
                                 #:color 4 #:line-color "black" #:line-width 0.5 ;#:line-style '                  
                                 #:label "fitness")
                   (rectangles3d (append
                                  (for/list ([x-ivl (in-list x-ivls)]
                                             [x (in-list x-mids)])
                                    (vector x-ivl (ivl 0 1) (ivl 0 (average-fitness x)))))
                                 #:x-min 0 #:x-max x-max
                                 #:y-min 0 #:y-max (add1 y-max)
                                 #:z-min z-min #:z-max (* 2 z-max)
                                 #:alpha 2/4  #:style 'solid
                                 #:color 2 #:line-color "black" #:line-width 0.5 ;#:line-style '                  
                                 #:label "average fitness"))))

;3d fitness plot
(define (smart-plot drawing-context fitness-array width height)
  
  (send drawing-context set-font (make-font #:size 20 #:family 'system #:weight 'normal #:underlined? #t #:smoothing 'partly-smoothed))
  (send drawing-context draw-text "Generation-fitness 3d plot" 20 950)
  
  (define (average-fitness step) (if (>= step (length fitness-array)) -1 
                                     (/ (foldl + 0 (list-ref fitness-array step)) 1.0 (length (list-ref fitness-array step)))))
  
  (define x-max (length fitness-array))
  (define y-max (length (car fitness-array)))
  (define z-max (foldl max -1 (flatten fitness-array)))
  (define z-min (foldl min z-max (flatten fitness-array)))
  
  (define (norm2 x y) (list-ref (list-ref fitness-array x) y))
  (define x-ivls (bounds->intervals (linear-seq 0 x-max (add1 x-max))))
  (define y-ivls (bounds->intervals (linear-seq 1 (add1 y-max) (add1 y-max))))
  (define x-mids (linear-seq 0 x-max (add1 x-max)))
  (define y-mids (linear-seq 0 y-max (add1 y-max)))
  
  ;(plot-title "Evolution")
  (plot-x-label "generation")
  (plot-y-label "individual")
  (plot-z-label "fitness")
  (plot-z-far-label "fitness")
  (plot-font-size 16)
  (plot3d-angle 30)
  (plot3d-altitude 35)
  (plot-legend-anchor 'top-right)
  ;(plot3d-ambient-light 1)
  (plot3d/dc (list (rectangles3d (append*
                                  (for/list ([y-ivl (in-list y-ivls)] [y (in-list y-mids)])
                                    (for/list ([x-ivl (in-list (filter-map (lambda(x y) (if (even? y) x #f)) x-ivls (range x-max)))]
                                               [x (in-list (filter-map (lambda(x y) (if (even? y) x #f)) x-mids (range (add1 x-max))))])
                                      (define z (norm2 x y))
                                      (vector x-ivl y-ivl (ivl 0 z)))))
                                 #:x-min 0 #:x-max x-max
                                 #:y-min 0 #:y-max (add1 y-max)
                                 #:z-min z-min #:z-max (* 2 z-max)
                                 #:alpha 1  #:style 'solid
                                 #:color "gray" #:line-color "black" #:line-width 0.5 ;#:line-style '	                 
                                 #:label "fitness")
                   (rectangles3d (append*
                                  (for/list ([y-ivl (in-list y-ivls)] [y (in-list y-mids)])
                                    (for/list ([x-ivl (in-list (filter-map (lambda(x y) (if (odd? y) x #f)) x-ivls (range x-max)))]
                                               [x (in-list (filter-map (lambda(x y) (if (odd? y) x #f)) x-mids (range (add1 x-max))))])
                                      (define z (norm2 x y))
                                      (vector x-ivl y-ivl (ivl 0 z)))))
                                 #:x-min 0 #:x-max x-max
                                 #:y-min 0 #:y-max (add1 y-max)
                                 #:z-min z-min #:z-max (* 1.5 z-max)
                                 #:alpha 1  #:style 'solid
                                 #:color 4 #:line-color "black" #:line-width 0.5 ;#:line-style '                  
                                 #:label "fitness")
                   (rectangles3d (append
                                  (for/list ([x-ivl (in-list x-ivls)]
                                             [x (in-list x-mids)])
                                    (vector x-ivl (ivl 0 1) (ivl 0 (average-fitness x)))))
                                 #:x-min 0 #:x-max x-max
                                 #:y-min 0 #:y-max (add1 y-max)
                                 #:z-min z-min #:z-max (* 1.5 z-max)
                                 #:alpha 2/4  #:style 'solid
                                 #:color 2 #:line-color "black" #:line-width 0.5 ;#:line-style '                  
                                 #:label "average fitness")) drawing-context 50 1000 width height))

(define (solution-plot drawing-context frame filepath clique graph graph_width graph_height)
  
  (define (force-directed width height graph)
    
    (define (integer-count chromosome)
      (count (lambda(x) (bitwise-bit-set? chromosome x)) (range (integer-length chromosome))))
    
    (define (build-graph-matrix graph-set graph-order)
      
      (define (make-line i) 
        
        (foldl bitwise-ior 
               0 
               (filter-map (lambda(j) (if (or (equal? i j) (set-member? graph-set (list i j)) (set-member? graph-set (list j i)))  
                                          (arithmetic-shift 1 j)
                                          #f)) 
                           (range graph-order))))
      
      (build-vector graph-order make-line))
    
    (define (make-random-points points graph-order)
      
      (if (< graph-order 0) 
          points
          (make-random-points (dict-set points graph-order (list (cons (random width) (random height)) '(0 . 0))) (sub1 graph-order))))
    
    (define (force-directed-drawing graph-matrix points graph-order)
      
      (define (vec-mod v) (sqrt (+ (sqr (car v)) (sqr (cdr v)))))
      (define (vec-norm v) (vec-div v (vec-mod v)))
      (define (vec-sub v1 v2) (cons (- (car v1) (car v2)) (- (cdr v1) (cdr v2))))
      (define (vec-add v1 v2) (cons (+ (car v1) (car v2)) (+ (cdr v1) (cdr v2))))
      (define (vec-div v x) (cons (/ (car v) (+ x 0.0001)) (/ (cdr v) (+ x 0.0001))))
      (define (vec-mul v x) (cons (* (car v) x) (* (cdr v) x)))
      (define (vec-min v x) (if (< (vec-mod v) x) (vec-mod v) x))
      
      (define t 20.0)
      (define cooling 0.3)
      (define Ck 1.0)
      (define k (* Ck (sqrt (/ (* width height 1.0) graph-order))))
      
      (define CR 0.1)
      (define (f-r x) (/ (* CR k k) x))
      (define CA 1.0)
      (define (f-a x) (/ (* CA x x) k))
      
      (define (repulsive-forces points)
        
        (define (zero-disp points vertices)
          (if (null? vertices) 
              points 
              (zero-disp (dict-update points (car vertices) (lambda(value) (list (car value) '(0 . 0)))) (cdr vertices))))
        
        (define (check-forces points v vertices)
          
          (cond ((null? vertices) points)
                (equal? v (car vertices) (check-forces points v (cdr vertices)))
                (else 
                 (let* ((v-pos (car (dict-ref points v)))
                        (v-disp (cdr (dict-ref points v))) 
                        (u-pos (car (dict-ref points (car vertices))))
                        (delta (vec-sub v-pos u-pos)) ;!!!!
                        (v-new-disp (vec-add v-disp (vec-mul (vec-norm delta) (f-r (vec-mod delta))))))
                   
                   (check-forces (dict-set points v (list v-pos v-new-disp) v (cdr vertices)))))))
        
        (define (loop points vertices)
          
          (if (null? vertices)
              points
              (loop (check-forces points (car vertices) (range graph-order)) (cdr vertices))))
        
        (loop (zero-disp points (range graph-order)) (range graph-order)))
      
      (define (attractive-forces points)
        
        (define (check-forces points graph)
          (if (null? graph)
              points
              (let* ((v (first (first graph)))
                     (u (second (first graph)))
                     (v-pos (car (dict-ref points v)))
                     (u-pos (car (dict-ref points u)))
                     (delta (vec-sub v-pos u-pos))
                     (disp-vec (vec-mul (vec-norm delta) (f-a (vec-mod delta))))
                     (v-disp (vec-sub (second (dict-ref points v)) disp-vec))
                     (u-disp (vec-add (second (dict-ref points u)) disp-vec)))
                
                ;(if (not (bitwise-bit-set? (vector-ref graph-matrix v) u)) (check-forces points (cdr graph))
                    (check-forces (dict-set (dict-set points u (list u-pos u-disp)) v (list v-pos v-disp)) (cdr graph)))));)
        
        (check-forces points graph))
      
      (define (limit-displacement graph-order points t)
        
        (define (limit-proc points vertices)
          
          (if (null? vertices)
              points           
              (let* ((v-pos (first (dict-ref points (car vertices))))
                     (v-disp (second (dict-ref points (car vertices))))
                     (v-new-pos (vec-add v-pos (vec-mul (vec-norm v-disp) (vec-min v-disp t)))))
                
                (limit-proc (dict-set points (car vertices) (list (cons (max 0 (min width (car v-new-pos))) (max 0 (min height (cdr v-new-pos)))) '(0 . 0))) 
                            (cdr vertices)))))
        
        (limit-proc points (range graph-order)))
      
      (define (fd-loop step t points)
        
        ;(printf "~a\n" step)
        (send frame set-status-text 
              (string-append "Loading data from " (path->string filepath) ":   force-directed placement (" (number->string step) " steps left)")) 
        
        (if (<= step 0) points
            (fd-loop (sub1 step) (* cooling t) (limit-displacement graph-order (attractive-forces (repulsive-forces points)) t))))
      
      (fd-loop 50 t points))
    
    (let* ((graph-order (length (remove-duplicates (flatten graph))))
           (graph-matrix (build-graph-matrix (list->set graph) graph-order)))
      
      (force-directed-drawing graph-matrix (make-random-points #hash() (sub1 graph-order)) graph-order)))
  
  
  (define (sol-plot points x-offset y-offset)
    
    (send frame set-status-text (string-append "Loading data from " (path->string filepath) ":   solution plot"))
    (send drawing-context set-font (make-font #:size 20 #:family 'system #:weight 'normal #:underlined? #t #:smoothing 'partly-smoothed))
    (send drawing-context draw-text "Solution plot" 20 1670)
    
    (for-each (lambda(edge) 
                
                (if (and (set-member? clique (first edge)) (set-member? clique (second edge)))
                    (send drawing-context set-pen "red" 0.2 'solid)
                    (send drawing-context set-pen (make-color 105 105 105 0.9) 0.06 'solid))
                (send drawing-context draw-line 
                      (+ x-offset (car (first (dict-ref points (first edge)))))
                      (+ y-offset (cdr (first (dict-ref points (first edge)))))
                      (+ x-offset (car (first (dict-ref points (second edge)))))
                      (+ y-offset (cdr (first (dict-ref points (second edge)))))))
              graph)
    
    (dict-for-each points (lambda(key value)
                            (if (set-member? clique key)
                                (send drawing-context set-pen "red" 9 'solid)
                                (send drawing-context set-pen "blue" 7 'solid))
                            (send drawing-context draw-point (+ x-offset (car (first value))) (+ y-offset (cdr (first value)))))))
  
  (sol-plot (force-directed graph_width graph_height graph) 5 1700))

(main-window)





















