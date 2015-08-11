
#lang racket

(require racket/dict)
(require rackunit)
(require rackunit/text-ui)
;(require rackunit/gui)

;(require "HGA-clique.rkt")
(require "HGA-clique.rkt")

;образуют ли выданные вершины клику?
(define (is-clique? vertices graph)
  
  ;построим полный граф на данных вершинах
  (define (build-complete-graph complete-graph vertices)
    (if (null? vertices) 
        complete-graph
        (build-complete-graph (append (map (lambda(vertice) (list (car vertices) vertice)) (cdr vertices)) complete-graph) (cdr vertices))))
  
  ;проверим, что в исходном графе есть все ребра полного графа на данных вершинах (в set поиск побыстрее)
  (let ((graph-set (list->set graph)))
    (andmap (lambda(edge) (or (set-member? graph-set edge) (set-member? graph-set (reverse edge)))) (build-complete-graph '() vertices))))

;читаем файл с набором тестов
;return: список путей к файлам с данными для тестов
(define (read-tests-file file-path)
  
  ;построчное чтение файла
  ;пустые строки и строки, начинающиеся с $ (комментарии) игнорируются
  ;return: список строк файла
  (define (read-file test-cases)
    
    (let ((next-line (read-line (current-input-port) 'any)))
      
      (if (eof-object? next-line) 
          (filter-not (lambda(str) (or (zero? (string-length str)) (equal? #\$ (string-ref str 0)))) test-cases)
          (read-file (cons (string-trim next-line) test-cases)))))
  
  (with-input-from-file file-path #:mode 'text (lambda () (read-file '()))))

;читаем данные для теста из файла
;формат файла:
;;description
;;K - требуемый размер клики
;;граф в виде ((a b) (b c) ...)
(define (read-test-data test-file)
  
  #|(define (make-map edges)
    
    (define (make-map-proc n v-map edges)
      (cond ((null? edges) v-map)
            ((dict-has-key? v-map (car edges)) (make-map-proc n v-map (cdr edges)))
            (else (make-map-proc (add1 n) (dict-set v-map (car edges) n) (cdr edges)))))
    
    (make-map-proc 0 #hash() (remove-duplicates (flatten edges))))|#
  
  #|(define (map-graph edges)
    
    (let ((v-map (make-map edges)))  
      (map (lambda(edge) (list (dict-ref v-map (first edge) -1) (dict-ref v-map (second edge) -1))) edges)))|#
  
  (with-input-from-file test-file #:mode 'text 
    (lambda () (list (read-line (current-input-port) 'any) (read) (read)))))

(define (proc x) (length x))

;создаем тесты
(define (do-test file-path)
  
  ;читаем из файла данные для теста, запускаем тест
  ;проверяем результат - выданный список вершин образует клику размера не меньше, чем требовалось
  (define (create-test test-file)
    
    (let ((test-data (read-test-data test-file))) 
      
      (lambda() 
        
        (let* ((answer (HGA-MCP (first test-data) (second test-data) (third test-data)))
               (clique (cdr answer))
               (ga-steps (car answer))
               (clique-size (length (remove-duplicates clique)))
               (vertices (length (remove-duplicates (flatten (third test-data)))))
               (edges (length (remove-duplicates (third test-data))))) 

          (define test-desc (string-append (first test-data) " (" (number->string vertices) " vertices, " (number->string edges) " edges" ")"
              " best known: " (number->string (second test-data))))
          
          (test-check 
            test-desc
            (lambda(x y) 
              (printf "test: ~a \n" test-desc)
              (printf "steps: ~a \n" (number->string ga-steps))
              (printf "found: ~a is-clique: ~a \n" clique-size x)) 
            (is-clique? clique (third test-data)) (<= (second test-data) clique-size))))))
  
  (define ga-mcp-tests
    (test-suite 
     "Genetic algorithm for maximum clique problem."     
     (for-each (lambda(test-file) ((create-test test-file))) (read-tests-file file-path))))
  
  ;запуск тестов с отображением результатов в текстовом режиме
  (run-tests ga-mcp-tests 'verbose))
  
  ;запуск тестов с отображением результатов в gui
  ;(test/gui ga-mcp-tests))

(do-test "clique-test/test.txt")
