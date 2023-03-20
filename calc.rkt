#lang racket/base

;;; Simple infix math lexer and rearranger into prefix math.
;;; 
;;; [https://matt.might.net/articles/lexers-in-racket/]

(require racket/match) ;; Not included in racket/base
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;; Top level lexer proc.
(define (calc-lexer input)

  ;; A wrapper for the core lexer that tracks paren indentation.
  ;;  Param `n` increases as lparens are lexed, and decremented on rparens.
  (define (calc-lexer-wrapper input n)

    ;; The core lexer itself.
    (define calc-lexer-core
      (lexer

       ;; Symbolic values.
       [(:+ (char-range #\a #\z) (char-range #\A #\Z))
        (cons `(ID ,(string->symbol lexeme))
             (calc-lexer-wrapper input-port n))]
    
       ;; Integer literals.
       [(:: (:? #\-) (:+ (char-range #\0 #\9)))
        (cons `(INT ,(string->number lexeme))
               (calc-lexer-wrapper input-port n))]
    
       ;; Operands + - / * ^.
       [(:or #\+ #\- #\/ #\* #\^)
        (cons `(OP ,(string->symbol lexeme))
              (calc-lexer-wrapper input-port n))]
    
       ;; Opening parens.
       [#\(
        (cons `(LPAREN ,n)
         (calc-lexer-wrapper input-port (+ n 1)))]
    
       ;; Closing parens.
       [#\)
        (cons `(RPAREN ,(- n 1))
         (calc-lexer-wrapper input-port (- n 1)))]

       ;; Ignore whitespace.
       [whitespace
        (calc-lexer-wrapper input-port n)]

       ;; End lex list with '().
       [(eof)
        '()]))
    (calc-lexer-core input))
  (calc-lexer-wrapper input 0))

;; Takes the output tree from a lexed infix expression and rearranges into prefix
;;  that can (hopefully) be `eval`ed. Note for matching parenthesised expressions
;;  then `n` is used to track nesting but is ultimately discarded.
(define (infix->prefix exp)
  (match exp

    ;; De-tokenise integer literals.
    [`((INT ,x)) x]

    ;; De-tokenise integers, pull operand to front of expression,
    ;;  operate on rest.
    [`((INT ,x) (OP ,op) ,rest ...)
     `(,op ,x ,(infix->prefix `(,@rest)))]

    ;; De-tokenise symbol literals.
    [`((ID ,x)) x]

    ;; De-tokenise symbols, pull operand to front of expression,
    ;;  operate on rest.
    [`((ID ,x) (OP ,op) ,rest ...)
     `(,op ,x ,(infix->prefix `(,@rest)))]
    
    ;; Rearrange parenthesised expressions, convert expression inside parens
    ;;  and then expressions after.
    [`((LPAREN ,n) ,exp ... (RPAREN ,n) (OP ,op) ,rest ...)
     `(,op ,(infix->prefix `(,@exp)) ,(infix->prefix `(,@rest)))]
    
    ;; Rearrange parenthesised expressions, convert expression inside parens.
    [`((LPAREN ,n) ,exp ... (RPAREN ,n))
     (infix->prefix `(,@exp))]
    
    ;; Catch erroneous expressions.
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
