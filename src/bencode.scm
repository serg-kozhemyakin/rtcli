(declare (unit bencode)
         (export bencode
                 bdecode))

(define (bencode obj)
  (cond ((symbol? obj)
         (let ((str (symbol->string obj)))
           (string-append (number->string (string-length str)) ":" str)))
        ((number? obj)
         (string-append "i" (number->string obj) "e"))
        ((string? obj)
         (string-append (number->string (string-length obj)) ":" obj))
        ((list? obj)
         (string-append "l" (apply string-append (map bencode obj)) "e"))
        ((hash-table? obj)
         (string-append "d" (apply string-append
                                   (map (lambda (key) (string-append (bencode key) (bencode (hash-table-ref obj key))))
                                        (sort (hash-table-keys obj) string<)))
                        "e"))
        ))

(define (read-till-delimiter port delimiter)
  (define (read-till-delimiter-internal port delimiter str)
    (let ((char (read-char port)))
      (if (or (eof-object? char)
              (char=? char delimiter))
          str
          (read-till-delimiter-internal port delimiter (cons char str)))))
  (reverse-list->string (read-till-delimiter-internal port delimiter '())))

(define (bdecode port)
  (letrec ((bdecode-list (lambda (port decoded-list)
                           (let ((next-char (peek-char port)))
                             (if (or (eof-object? next-char)
                                     (char=? next-char #\e))
                                 (begin
                                   (read-char port)
                                   (reverse decoded-list))
                                 (bdecode-list port (cons (bdecode port) decoded-list))))))
           (bdecode-hash (lambda (port hash)
                           (let ((next-char (peek-char port)))
                             (if (or (eof-object? next-char)
                                     (char=? next-char #\e))
                                 (begin
                                   (read-char port)
                                   hash)
                                 (begin
                                   (hash-table-set! hash (bdecode port) (bdecode port))
                                   (bdecode-hash port hash))))))
           (next-char (read-char port)))
    (if (eof-object? next-char)
        #f
        (case next-char
          [(#\i) (string->number (read-till-delimiter port #\e))]
          [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (let ((len (string->number (string-append (make-string 1 next-char) (read-till-delimiter port #\:)))))
             (read-string len port))]
          [(#\l) (bdecode-list port '())]
          [(#\d) (bdecode-hash port (make-hash-table))]
          (else #f)))))
