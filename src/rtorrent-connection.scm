(declare (unit rtorrent-connection)
         (uses bencode
               rtorrent-commands))

(use coops)
(use xml-rpc-client xml-rpc-lolevel)
(use ssax)
(use sxpath-lolevel)
(use socket)
(use intarweb)
(use simple-sha1)

(define xml-rpc-parsers-with-i8
  (make-parameter `((i8 . ,xml-rpc-int->number)
                    ,@(xml-rpc-parsers))))
(set! xml-rpc-parsers xml-rpc-parsers-with-i8)

(define-class <rtorrent-connection> ()
  ((client initform: #f
           accessor: connection-client)))

(define-class <scgi-connection> (<rtorrent-connection>)
  ((in-port initform: #f
            accessor: connection-in-port)
   (out-port initform: #f
             accessor: connection-out-port)))

(define-class <scgi-socket-connection> (<scgi-connection>)
  ((socket initform: #f
           accessor: connection-socket)))

(define-class <scgi-tcp-connection> (<scgi-connection>)
  ((host initform: "localhost"
         accessor: connection-host)
   (port initform: 5000
         accessor: connection-port)))

(define-class <xml-rpc-connection> (<rtorrent-connection>)
  ((url initform: "http://localhost/RPC2"
        accessor: connection-url)))

(define-generic (init-connection connection))
(define-method (init-connection (con <xml-rpc-connection>))
  (set! (connection-client con) (xml-rpc-server (connection-url con)))
  con)

(define-method (init-connection before: (con <scgi-tcp-connection>))
  (condition-case
   (let ((addr (address-information
                (connection-host con)
                (connection-port con))))
     (set! (connection-client con) (socket-connect/ai addr))
     con)
   (var () (print "Can't connect to rtorrent at '" (connection-host con) ":" (connection-port con))
        (exit 1))))

(define-method (init-connection before: (con <scgi-socket-connection>))
  (condition-case
   (let ((so (socket af/unix sock/stream)))
     (socket-connect so (unix-address (connection-socket con)))
     (set! (connection-client con) so)
     con)
   (var () (print "Can't connect to rtorrent at '" (connection-socket con))
        (exit 1))))

(define-method (init-connection (con <scgi-connection>))
  (define-values (i o) (socket-i/o-ports (connection-client con)))
  (set! (connection-in-port con) i)
  (set! (connection-out-port con) o)
  con)

(define-generic (close-connection con))
(define-method (close-connection (con <xml-rpc-connection>)) #f)
(define-method (close-connection (con <scgi-connection>))
  (socket-close (connection-client con)))

(define (make-connection url)
  (validate-connection
   (call/cc
     (lambda (return)
       (if (substring=? url "http" 0 0 4)
           (return (make <xml-rpc-connection> 'url url)))
       (if (file-exists? url)
           (return (make <scgi-socket-connection> 'socket url)))
       (let* ((parts (string-split url ":"))
              (_ (if (not (= 2 (length parts))) (return #f)))
              (port (string->number (cadr parts))))
         (if (and port
                  (> port 1024)
                  (< port 64565))
             (return (make <scgi-tcp-connection> 'host (car parts) 'port port))))
       #f))))

(define (validate-connection con)
  (if (not con)
      (begin
        (print "Can't create connection with specified parameters")
        (exit 1))
      (let ((rt-methods (vector->list (rt:cmd con 'system.listMethods))))
        (register-xml-rpc-commands rt-methods)
        con)))

(define-generic (send connection rest))

(define-method (send before: con . rest)
  (init-connection con))

(define-method (send after: con . rest)
  (close-connection con))

(define-method (send (con <xml-rpc-connection>) request . parameters)
  (condition-case
   (apply ((connection-client con) request) parameters)
   (var (exn xml-rpc) (print "rtorrent(" request "): " ((condition-property-accessor 'exn 'message) var)))
   (var (exn i/o net) (print "Can't connect to rtorrent at '" (connection-url con) "'") (exit 1))))

(define-method (send (con <scgi-connection>) request . parameters)
  (condition-case
   (apply send-scgi-request con request parameters)
   (var (exn xml-rpc) (print "rtorrent(" request "): " ((condition-property-accessor 'exn 'message) var)))))

(define (sxml->string sxml)
  (string-concatenate (flatten (sxml:sxml->xml sxml))))

(define (send-scgi-request con method . parameters)
  (let* ((call (sxml->string (xml-rpc-methodcall method parameters)))
         (call-len (number->string (string-length call)))
         (header (string-append "CONTENT_LENGTH\x00" call-len "\x00"
                                "SCGI\x001\x00"
                                "REQUEST_METHOD\x00POST"))
         (out-port (connection-out-port con))
         (in-port (connection-in-port con))
         (_ (begin
              (display (string-append (number->string (string-length header)) ":"
                                      header ","
                                      call) out-port)
              (flush-output out-port)))
         (response (read-response in-port)))
    (if (eq? (response-code response) 200)
        (let* ((headers (read-headers in-port))
               (resp-len (header-value 'content-length headers))
               (response-body (read-string resp-len in-port)))
          (xml-rpc-response->values (ssax:xml->sxml (open-input-string response-body) '())))
        #f)))

(define (calculate-torrent-hash tor)
  (cond
   ((and (hash-table? tor)
         (hash-table-ref tor "info"))
    (string->sha1sum (bencode (hash-table-ref tor "info"))))
   ((and (string? tor)
         (= 40 (string-length tor))) tor)
   ((file-exists? tor)
    (let* ((torrent (call-with-input-file tor (lambda (p) (bdecode p))))
           (_ (if (not torrent)
                  (begin
                    (print "File '" tor "' doesn't looks like valid torrent file")
                    (exit 1)))))
      (calculate-torrent-hash torrent)))
   (else #f)))

