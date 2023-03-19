#lang racket

;;; Simple parser for infix calculations
;;; 
;;; [https://matt.might.net/articles/lexers-in-racket/]

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define (wrap-lexer input n)
  (define calc-lexer
    (lexer
      [(:+ (char-range #\a #\z) (char-range #\A #\Z))
       (cons `(ID ,(string->symbol lexeme))
            (wrap-lexer input-port n))]
    
     [(:: (:? #\-) (:+ (char-range #\0 #\9)))
      (cons `(INT ,(string->number lexeme))
             (wrap-lexer input-port n))]
    
     [(:or #\+ #\- #\/ #\* #\^)
      (cons `(OP ,(string->symbol lexeme))
            (wrap-lexer input-port n))]
    
     [#\(
      (cons `(LPAREN ,n)
       (wrap-lexer input-port (+ 1 n)))]
    
     [#\)
      (cons `(RPAREN ,(- n 1))
       (wrap-lexer input-port (- n 1)))]

     [whitespace
      (wrap-lexer input-port n)]

     [(eof)
      '()]))
  (calc-lexer input))

(define (infix->prefix exp)
  (match exp
    [(? symbol?) exp]

    [`((INT ,x)) x]

    [`((INT ,x) (OP ,y) ,z ...)
     `(,y ,x ,(infix->prefix `(,@z)))]

    [`((ID ,x)) x]

    [`((ID ,x) (OP ,y) ,z ...)
     `(,y ,x ,(infix->prefix `(,@z)))]
    
    [`((LPAREN ,n) ,x ... (RPAREN ,n) (OP ,y) ,z ...)
     `(,y ,(infix->prefix `(,@x)) ,(infix->prefix `(,@z)))]
    
    [`((LPAREN ,n) ,x ... (RPAREN ,n))
     (infix->prefix `(,@x))]
    
    [else 
     (begin (display (format "exp unknown: ~A\n" exp))
      exp)]))

(define (print-lex-test str)
  (display 
   (format "~A => ~%  ~A~%~%" str (wrap-lexer (open-input-string str) 0))))

(define (print-calc-test str)
  (display 
   (format "~A => ~%  ~A~%~%" str (infix->prefix 
                                   (wrap-lexer (open-input-string str) 0)))))

(define (lex-test-all)
  (begin (print-lex-test "1 + (4 - 3)")
   (print-lex-test "1 + (4 - 3) / 7")
   (print-lex-test "1 + (4 - 3) / (7 ^ 3)")
   (print-lex-test "(1 + (4 - 3) / (7 ^ 3))")))

(define (calc-test-all)
  (begin (print-calc-test "1 + (4 - 3)")
   (print-calc-test "1 + (4 - 3) / 7")
   (print-calc-test "1 + (4 - 3) / (7 ^ 3)")
   (print-calc-test "(1 + (4 - 3) / (7 ^ 3))")))
