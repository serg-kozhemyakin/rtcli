(declare (uses rtorrent-connection
               rtorrent-commands
               rtcli-commands
               bencode
               version))

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
                          (print "rtcli: " version:date "(" version:id ")")
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

