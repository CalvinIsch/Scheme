;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ManipulatingData&ReadingGraphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This file works with various data types to manipulate and sift through
; data.
; Calvin Isch

; Purpose: A function that takes a DNA string, (letters ACTG repeated randomly
; in any order), and converts it to a run-length encoding so if there are long
; repitetions of any letter it will record it in a smaller amount of space.
; signature: (rle-encode dna:String)-> String
(define (rle-encode dna)
 (local [ ; A helper function that adds up the amount of repeated code
          ; String String Number String -> String
          ; dna: the dna sequence
          ; current: the character we currently have
          ; amount: the number of repetitions so far
          ; new: our accumulated new string
    (define (helper dna current amount new)
      (cond
        [(equal? "" dna) (string-append new current (number->string amount))]
        [(equal? (substring dna 0 1) current)
          (helper (substring dna 1) current (+ 1 amount) new)]
        [else (helper (substring dna 1) (substring dna 0 1) 1
                      (string-append new current (number->string amount)))]))]
   (cond [(equal? "" dna) dna]
         [else (helper dna (substring dna 0 1) 0 "")])))
(check-expect (rle-encode "AAGCCCTTAAAAAAAAAA") "A2G1C3T2A10")
(check-expect (rle-encode "") "")
 
; Purpose: A function that takes a run-length encoding of DNA and transforms it
; back into the regular DNA string
; signature: (rle-decode rldna:string) -> string
(define (rle-decode rldna)
  (local [ ; String String Number String String
           ; this is breaks down the dna sequence so you can get the numbers
           ; from the letters and pieces it all together into one final
           ; regularly sequence dna
           ;
           ; current: the accumulated string that will be produced
           ; dna: the encoded dna sequence we're working with
           ; n: a number representing the character in the dna sequence we're on
           ; letter: the current letter of dna
           ; number: the number mixed inbetween the letters
    (define (helper current dna n letter number)
      (local [ ; This is used multiple times, so I made a local to host it.
         (define s (substring dna (- n 1) n))]
      (cond
        [(equal? (string-length dna)  n)
            (string-append current (replicate
             (string->number (string-append number s)) letter))]
        [(or (equal? s "A")
             (equal? s "T")
             (equal? s "C")
             (equal? s "G"))
         (helper (string-append current (replicate (string->number number) letter))
                 dna (+ n 1) s "0")]
        [else (helper current dna (+ n 1)
                  letter (string-append number s))])))]
    (cond
      [(equal? "" rldna) ""]
      [else (helper "" (substring rldna 1) 1 (substring rldna 0 1) "0")])))
(check-expect (rle-decode "A2G1C3T2A10")"AAGCCCTTAAAAAAAAAA")
(check-expect (rle-decode "") "")
(check-expect (rle-decode "A11T11C11G11")
    "AAAAAAAAAAATTTTTTTTTTTCCCCCCCCCCCGGGGGGGGGGG")

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (insert (first l) (rest l) 0) (sort> (rest l)))]))
(check-expect (sort> '(1 2 3 4)) 6)
(check-expect (sort> '()) 0)

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l x)
  (cond
    [(empty? l) x]
    [else (if (>= n (first l))
              (+ x 1)
             (insert n (rest l) (+ x 1)))]))
(check-expect (insert 4 '(3 2 1) 0) 1)
(check-expect (insert 3 '() 0) 0)

; Purpose: A function that determines the amount of times a comparison expression
; is used whenever sorting a [ListOf Number]
; signature: (num-comps lon:[ListOf Numbers]) -> number
(define (num-comps lon)
  (sort> lon))
(check-expect (num-comps '(1 2 3 4)) 6)
(check-expect (num-comps '(4 3 2 1)) 3)
(check-expect (num-comps '()) 0)
(check-expect (num-comps '(1)) 0)
(check-expect (num-comps '(2 1)) 1)
(check-expect (num-comps '(1 2)) 1)

; Graph's represented sa lists 
(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

; Purpose: A function that given a graph, and a symbol, returns the "neighbors"
; of that symbol which are the symbols it connects to
; signature: (neighbors g:[Listof [ListOf Symbols]] s:Symbol)->[ListOf Symbol]
(define (neighbors s g)
  (cond
    [(empty? g) "Error: Make sure you chose a letter on the graph"]
    [(equal? (first (first g)) s) (rest (first g))]
    [else (neighbors s (rest g))]))
(check-expect (neighbors 'A sample-graph) (list 'B 'E))
(check-expect (neighbors 'B sample-graph)(list 'E 'F))
(check-expect (neighbors 'pony sample-graph)
              "Error: Make sure you chose a letter on the graph")

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local (
           (define next (neighbors origination G))
           (define candidate
           (find-path/list next destination G)))
          (cond
            [(boolean? candidate) #false]
            [else (cons origination candidate)]))]))
(check-expect (find-path 'C 'D sample-graph) (list 'C 'D))
(check-expect (find-path 'C 'G sample-graph) #false)

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))
(check-expect (find-path 'A 'G sample-graph) (list 'A 'B 'E 'F 'G))

; Whenever I use find-path from 'A to 'G it find the path ('A 'B 'E 'F 'G)
; it goes through this path because the function currently works by chosing
; the first node from each of the neighbor sections, so even though it would
; be faster for the function to work by going from 'E to 'F to 'G it starts
; with 'B


; Purpose: A function that consumes a graph and determines if there is a path
; between any pair of nodes. Returns true if there is any path, and false if
; there isn't any.
; signature: (test-on-all-nodes graph:[ListOf [ListOf Symbols]])-> boolean
(define (test-on-all-nodes graph)
  (cond
    [(empty? graph) #false]
    [(empty? (rest (first graph))) (test-on-all-nodes (rest graph))]
    [else #true]))
(check-expect (test-on-all-nodes sample-graph) #true)
(check-expect (test-on-all-nodes '()) #false)
(check-expect (test-on-all-nodes  '((A) (C) (D) (B E F))) #true)

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path-in-one origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ( ; [List-of Node] -> [Maybe Path] 
                   ; finds a path from some node on lo-Os to destination
                   ; if there is no path, the function produces #false
           (define (list-path lo-Os)
             (cond
               [(empty? lo-Os) #false]
               [else 
                 (cond
                   [(boolean? (find-path-in-one (first lo-Os) destination G))
                     (list-path (rest lo-Os))]
                   [else (find-path (first lo-Os) destination G)])]))
           (define candidate (list-path (neighbors origination G))))
      (cond
        [(boolean? candidate) #false]
        [else (cons origination candidate)]))]))
(check-expect (find-path-in-one 'C 'D sample-graph) (list 'C 'D))
(check-expect (find-path-in-one 'C 'G sample-graph) #false)