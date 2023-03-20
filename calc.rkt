#lang racket

;;; Simple parser for infix calculations
;;; 
;;; [https://matt.might.net/articles/lexers-in-racket/]

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;; Top level lexer proc
(define (calc-lexer input)

  ;; A wrapper for the core lexer that tracks paren indentation
  ;;  Param `n` increases as lparens are lexed, and decremented on rparens
  (define (calc-lexer-wrapper input n)

    ;; The core lexer itself
    (define calc-lexer-core
      (lexer

       ;; Symbolic values
       [(:+ (char-range #\a #\z) (char-range #\A #\Z))
        (cons `(ID ,(string->symbol lexeme))
             (calc-lexer-wrapper input-port n))]
    
       ;; Integer literals
       [(:: (:? #\-) (:+ (char-range #\0 #\9)))
        (cons `(INT ,(string->number lexeme))
               (calc-lexer-wrapper input-port n))]
    
       ;; Operands + - / * ^
       [(:or #\+ #\- #\/ #\* #\^)
        (cons `(OP ,(string->symbol lexeme))
              (calc-lexer-wrapper input-port n))]
    
       ;; Opening parens
       [#\(
        (cons `(LPAREN ,n)
         (calc-lexer-wrapper input-port (+ n 1)))]
    
       ;; Closing parens
       [#\)
        (cons `(RPAREN ,(- n 1))
         (calc-lexer-wrapper input-port (- n 1)))]

       ;; Ignore whitespace
       [whitespace
        (calc-lexer-wrapper input-port n)]

       ;; End list with empty list
       [(eof)
        '()]))
    (calc-lexer-core input))
  (calc-lexer-wrapper input 0))

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
   (format "~A => ~%  ~A~%~%" str (calc-lexer (open-input-string str)))))

(define (print-calc-test str)
  (display 
   (format "~A => ~%  ~A~%~%" str (infix->prefix 
                                   (calc-lexer (open-input-string str))))))

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
