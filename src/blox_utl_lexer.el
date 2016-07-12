;; -*- lexical-binding: t; coding: prefer-utf-8-unix; -*-

;; (define-lex-simple-regex-analyzer semantic-lex-punctuation
;;   "Detect and create punctuation tokens."
;;   "\\(\\s.\\|\\s$\\|\\s'\\)" 'punctuation)

(define-lex-simple-regex-analyzer utl-start-token
  "Detect the token that denotes the start of a UTL code block"
  "[%" 'utl-start)

(define-lex blox_utl_lex
  "A lexer for Townnews' UTL template language."
  utl-start-token
  )
