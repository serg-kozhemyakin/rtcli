(declare (uses rtorrent-connection rtorrent-commands bencode))

(use posix)
(use vector-lib)
(use srfi-13)
(use srfi-69)
(use args)
(use trace)

(define mode #f)

(define opts
  (list (args:make-option (c host)      (required: "HOST") (string-append "Host where rtorrent running or rtorrent's unix socket\n"
                                                                          (make-string 26 #\space)
                                                                          "(http://host:port/path | host:port | path)."))
        (args:make-option (t torrent)   (required: "FILE") "Torrent filename or hash string")
        (args:make-option (d dest)      (required: "DIR")  "Destination dir")
        (args:make-option (a add)       #:none     "Add specified torrent"
                          (set! mode 'add))
        (args:make-option (r remove)    #:none     "Remove specified torrent"
                          (set! mode 'remove))
        (args:make-option (l list)      #:none     "List registered torrents"
                          (set! mode 'list))
        (args:make-option (p pause)     #:none     "Pause specified torrent"
                          (set! mode 'pause))
        (args:make-option (c continue)  #:none     "Continue specified torrent"
                          (set! mode 'resume))
        (args:make-option (s stop)      #:none     "Stop specified torrent"
                          (set! mode 'stop))
        (args:make-option (x calc)      #:none     "Calculate hash for torrent file"
                          (set! mode 'calc))
        (args:make-option (z call)      #:none     "Call specified xml-rpc function with parameters and print results"
                          (set! mode 'call))
        (args:make-option (v version)   #:none     "Display version"
                          (print "rtcli $Revision: 1.3 $")
                          (exit))
        (args:make-option (h help)      #:none     "Display this text"
                          (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " -c host -t torrent-or-hash operation")
      (newline)
      (print (args:usage opts))
      (print "Report bugs to serg.kozhemyakin at gmail.")))
  (exit 1))

(define (validate-parameters options . params)
  (let ((have-missed-params #f))
   (for-each (lambda (p)
               (if (not (alist-ref p options))
                   (begin
                     (set! have-missed-params #t)
                     (print "Parameter '" p "' required for current operation")
                     ))) params)
   (if have-missed-params (exit 1))))

(define (calc-torrent-hash options)
  (validate-parameters options 'torrent)
  (let ((tor (alist-ref 'torrent options)))
    (print (calculate-torrent-hash tor))))

(define (add-torrent options)
  (validate-parameters options 'torrent 'host)
  (let* ((tor  (alist-ref 'torrent options))
         (dest (alist-ref 'dest options))
         (_ (if (not (file-exists? tor))
                (begin
                  (print "Can't find file '" tor "'.")
                  (exit 1))))
         (torrent (call-with-input-file tor
                      (lambda (p)
                        (bdecode p))))
         (th (calculate-torrent-hash torrent))
         (connection (make-connection (alist-ref 'host options))))
    (if (and th
             connection)
        (begin
          (print "Adding torrent file '" tor "'" (if dest (string-append " to " dest) ""))
          (rt:load_raw connection (string->blob (bencode torrent)))
          (if dest (rt:d.set_directory connection th dest))
          (rt:d.start connection th))
        (begin
          (print "Invalid connection url specified '" url "'")
          (exit 1)))))

(define (remove-torrent options)
  (validate-parameters options 'torrent 'host)
  (let ((th (calculate-torrent-hash (alist-ref 'torrent options)))
        (connection (make-connection (alist-ref 'host options))))
    (if (and th connection)
        (begin
          (print "Removing torrent with hash '" th "'")
          (rt:d.stop connection th)
          (rt:d.erase connection th)))))

(define (list-torrents options)
  (define (print-torrent-info c t)
    (let* ((left-bytes (rt:d.get_left_bytes c t))
           (down-bytes (rt:d.completed_bytes c t)))
      (print t " [" down-bytes "/" (+ down-bytes left-bytes) "]")))
  (validate-parameters options 'host)
  (let ((connection (make-connection (alist-ref 'host options))))
    (if connection
        (let ((tl (rt:download_list connection "")))
          (if (vector? tl)
              (begin
                (print "List of all torrents:")
                (for-each (lambda (t) (print-torrent-info connection t))
                          (vector->list tl))))))))

(define (call-rtorrent options operands)
  (validate-parameters options 'host)
  (let ((connection (make-connection (alist-ref 'host options))))
    (if connection
        (begin
         (let ((result (apply send connection operands)))
           (if (vector? result)
               (for-each print (vector->list result))
               (print result)))))))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments) opts)
    (cond
     ((not mode) (usage))
     ((eq? mode 'calc) (calc-torrent-hash options))
     ((eq? mode 'add) (add-torrent options))
     ((eq? mode 'remove) (remove-torrent options))
     ((eq? mode 'call) (call-rtorrent options operands))
     ((eq? mode 'list) (list-torrents options))
     )))

(main)

