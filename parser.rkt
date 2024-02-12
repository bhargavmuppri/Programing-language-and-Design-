#lang racket

;; skip all whitespace except for newlines
(define (skip-whitespace in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) ch]
      [(and (char-whitespace? ch) (not (char=? ch #\newline)))
       (begin
         (read-char in)
         (skip-whitespace in))]
      [else ch])))

;; returns true if the string is found in the input, false otherwise
(define (match-str str in)
  (let* [(len (string-length str))
         (in-str (peek-string len 0 in))]
    (if (eof-object? in-str)
        #f
        (if (string-ci=? str in-str)
            (let [(ch (peek-char in len))]
              (or (eof-object? ch) (not (char-alphabetic? ch))))
            #f
            ))))

;; helper functions to handle return values for the parser
;
(define (ok n)
  (list #t n))
;
(define (err n)
  (list #f n))
;
(define (ok? res)
  (car res))
;
(define (nline res)
  (cadr res))

;; parses a non zero digit
;;FIRST(nonzero-digit) = {1..9}
;;FOLLOW(nonzero-digit) = {0..9, a..zA..Z, if, read, write, goto, gosub, return, :, EOL, +, -, =}
;;nonzero_digit -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
(define (parse-nonzero-digit n in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (err n)]
      [(and (char-numeric? ch) (not (char=? ch #\0))) (ok n)]
      [else (err n)])))

;; parses a digit
;;FIRST(digit) = {0..9}
;;FOLLOW(digit) = {0..9, a..zA..Z, if, read, write, goto, gosub, return, :, EOL, +, -, =}
;;digit -> 0 | nonzero_digit
(define (parse-digit n in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (err n)]
      [(char-numeric? ch) (ok n)]
      [else (err n)])))

;; parse zero or more digits: (digit*)
(define (parse-digits n in)
  (let [(r (parse-digit n in))]
    (if (ok? r)
        (begin
          (read-char in)
          (parse-digits n in))
        (ok n))))

;; parse idx
;;FIRST(idx) = {1..9}
;;FOLLOW(idx) = {if, read, write, goto, gosub, return, a..zA..Z, :, EOL}
;;idx -> nonzero_digit digit* 
(define (parse-idx n in)
  (let [(r (parse-nonzero-digit n in))]
    (if (ok? r)
        (begin
          (read-char in)
          (parse-digits n in))
        r)))

;; parse id
;;FIRST(id) = {a..zA..Z}
;;FOLLOW(id) = {+, -, =, ), then, :, EOL}
;;id -> [a-zA-Z]+ 
(define (parse-id n in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (err n)]
      [(char-alphabetic? ch)
       (begin
         (read-char in)
         (parse-id n in)
         (ok n))]
      [else (err n)])))

;; parse numsign
;;FIRST(numsign) = {+, -, epsilon}
;;FOLLOW(numsign) = {0..9}
;;numsign -> + | - | epsilon 
(define (parse-numsign n in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (ok n)]     
      [(or (char=? ch #\+) (char=? ch #\-))
       (begin
         (read-char in)
         (ok n))]
      [else (ok n)])))

;; parse num
;;FIRST(num) = {+, -, 0..9}
;;FOLLOW(num) = {+, -, =, ), then, :, EOL}
;;num -> numsign digit digit* 
(define (parse-num n in)
  (parse-numsign n in)
  (let [(r (parse-digit n in))]
    (if (ok? r)
        (begin
          (read-char in)
          (parse-digits (nline r) in))
        r)))

;; parse etail
;;FIRST(etail) = {+ , - , = , epsilon}
;;FOLLOW(etail) = {), then, :, EOL}
;;etail -> + expr | - expr | = expr | epsilon
(define (parse-etail n in)
  (skip-whitespace in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (err n)]     
      [(or (char=? ch #\+) (char=? ch #\-) (char=? ch #\=))
       (begin
         (read-char in)
         (parse-expr n in))]
      [else (ok n)])))

;; parse expr
;;FIRST(expr) = {a..zA..Z , + , - , 0..9 , ( }
;;FOLLOW(expr) = {), then, :, EOL}
;;expr -> id etail | num etail | (expr) 
(define (parse-expr n in)
  (skip-whitespace in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (err n)]
      [(char-alphabetic? ch)
       (let [(r (parse-id n in))]
         (if (ok? r)
             (parse-etail (nline r) in)
             r)
         )]
      [(or (char=? ch #\+) (char=? ch #\-) (char-numeric? ch))
       (let [(r (parse-num n in))]
         (if (ok? r)
             (parse-etail (nline r) in)
             r)
         )]
      [(char=? ch #\()
       (begin
         (read-char in)
         (let [(r (parse-expr n in))]
           (if (ok? r)
               (begin
                 (skip-whitespace in)
                 (let [(cc (peek-char in))]
                   (if (char=? cc #\))
                       (begin
                         (read-char in)
                         r)
                       (err n))))
               r))
         )]
      [else (err n)])))

;; parse stmt
;;FIRST(stmt) = {a..zA..Z, if , read , write , goto , gosub , return}
;;FOLLOW(stmt) = {:, EOL}
;;stmt -> id = expr | if expr then stmt | read id | write expr | goto idx | gosub idx | return 
(define (parse-stmt n in)
  (skip-whitespace in)
  (cond
    [(match-str "if" in)
     (begin
       (read-string 2 in)
       (let [(r (parse-expr n in))]
         (if (ok? r)    
             (begin
               (skip-whitespace in)
               (if (match-str "then" in)
                   (begin
                     (read-string 4 in)
                     (parse-stmt (nline r) in))
                   (err (nline r))))
             r)))]
    [(match-str "read" in)
     (begin
       (read-string 4 in)
       (skip-whitespace in)
       (parse-id n in))]
    [(match-str "write" in)
     (begin
       (read-string 5 in)
       (parse-expr n in))]
    [(match-str "goto" in)
     (begin
       (read-string 4 in)
       (skip-whitespace in)
       (parse-idx n in))]
    [(match-str "gosub" in)
     (begin
       (read-string 5 in)
       (skip-whitespace in)
       (parse-idx n in))]
    [(match-str "return" in)
     (begin
       (read-string 6 in)
       (ok n))]
    [else
     (let [(r (parse-id n in))]
       (if (ok? r)
           (begin
             (skip-whitespace in)
             (let [(cc (peek-char in))]
               (if (char=? cc #\=)
                   (begin
                     (read-char in)
                     (parse-expr n in))
                   r)))
           r))]
    ))

;; parse linetail
;;FIRST(linetail) = {:, epsilon}
;;FOLLOW(linetail) = {EOL}
;;linetail -> :stmt | epsilon 
(define (parse-linetail n in)
  (skip-whitespace in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (err n)]
      [(char=? ch #\:)
       (begin
         (read-char in)
         (parse-stmt n in))]
      [else (err n)])))

;; parse zero or more linetails (linetail*)
(define (parse-linetails n in)
  (let [(r (parse-linetail n in))]
    (if (ok? r)
        (parse-linetails n in)
        (ok n))))

;; parse line
;;FIRST(line) = {1..9}
;;FOLLOW(line) = {1..9, $$}
;;line -> idx stmt linetail* [EOL] 
(define (parse-line n in)
  (let [(r1 (parse-idx n in))]
    (if (ok? r1)
        (let [(ch (peek-char in))]
          (if (and (not (eof-object? ch)) (char=? ch #\space))
              (let [(r2 (parse-stmt (nline r1) in))]
                (if (ok? r2)
                    (let [(r3 (parse-linetails (nline r2) in))]
                      (if (ok? r3)
                          (begin
                            (skip-whitespace in)
                            (let [(eol (read-char in))]
                              (if (and (not (eof-object? eol)) (char=? eol #\newline))
                                  (ok (+ (nline r3) 1))
                                  (err (nline r3)))))
                          r3))
                    r2))
              (err (nline r1))))
        r1)))

;; parse linelist
;;FIRST(linelist) = {1..9, epsilon}
;;FOLLOW(linelist) = {$$}
;;linelist -> line linelist | epsilon 
(define (parse-linelist n in)
  (let [(ch (peek-char in))]
    (cond
      [(eof-object? ch) (ok n)]
      [(and (char-numeric? ch) (not (char=? ch #\0)))
       (begin
         (let [(res (parse-line n in))]
           (if (ok? res)
               (parse-linelist (nline res) in)
               res)))]
      [else (ok n)])))

;; parse program
;;FIRST(program) = {1..9, $$}
;;FOLLOW(program) = {$}
;;program -> linelist $$ 
(define (parse-program n in)
  (skip-whitespace in)
  (let [(result (parse-linelist n in))]
    (if (ok? result)
        (if (match-str "$$" in)
            result
            (err (nline result)))
        result)))

;; parse a program file
(define (parse progfile)
  (define in (open-input-file progfile))
  (let [(r (parse-program 1 in))]
    (if (ok? r)
        (displayln "Accept")
        (printf "Syntax error found on line ~a\n" (nline r))
        ))
  (close-input-port in)
  )
