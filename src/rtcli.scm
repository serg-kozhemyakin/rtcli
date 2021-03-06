(declare (uses rtorrent-connection
               rtorrent-commands
               rtcli-commands
               bencode
               version))

(use args)

(define mode #f)

(define opts
  (list (args:make-option (c host)      (required: "HOST") (string-append "Host where rtorrent running or rtorrent's unix socket\n"
                                                                          (make-string 26 #\space)
                                                                          "(http://host:port/path | host:port | path)."))
        (args:make-option (t torrent)   (required: "FILE") "Torrent filename or hash string")
        (args:make-option (a add)       #:none     "Add specified torrent"
                          (set! mode 'add))
        (args:make-option (d dest)      (required: "DIR")  "Destination dir for add operation")
        (args:make-option (e erase)     #:none     "Remove specified torrent"
                          (set! mode 'erase))
        (args:make-option (o open)      #:none     "Open specified torrent"
                          (set! mode 'open))
        (args:make-option (q close)     #:none     "Close specified torrent"
                          (set! mode 'close))
        (args:make-option (l list)      #:none     "List registered torrents"
                          (set! mode 'list))
        (args:make-option (format)      (required: "FORMAT") (string-append "Format output for list command\n"
                                                                            (make-string 26 #\space)
                                                                            "Default format:\n"
                                                                            (make-string 26 #\space)
                                                                            "~d.name/~d.hash [~d.completed_bytes/~d.get_size_bytes] (~d.up.rate/~d.down.rate) {~d.up.total/~d.down.total} ~d.ratio"))
        (args:make-option (r resume)    #:none     "Resume specified torrent"
                          (set! mode 'resume))
        (args:make-option (p pause)     #:none     "Pause specified torrent"
                          (set! mode 'pause))
        (args:make-option (x calc)      #:none     "Calculate hash for torrent file"
                          (set! mode 'calc))
        (args:make-option (dump)        #:none     "Dump all information from torrent file"
                          (set! mode 'dump))
        (args:make-option (dump-files)  #:none     "Dump files information only from torrent file"
                          (set! mode 'dump-files))
        (args:make-option (z call)      #:none     "Call specified xml-rpc function with parameters and print results"
                          (set! mode 'call))
        (args:make-option (shutdown)    #:none     "Shutdown rTorrent"
                          (set! mode 'shutdown))
        (args:make-option (v version)   #:none     "Display version"
                          (print "rtcli: " rtcli:version)
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
    (let* ((defined-commands `((calc . ,calc-torrent-hash)
                               (add . ,add-torrent)
                               (erase . ,erase-torrent)
                               (list . ,list-torrents)
                               (pause . ,pause-torrent)
                               (resume . ,resume-torrent)
                               (open . ,open-torrent)
                               (close . ,close-torrent)
                               (dump . ,dump-torrent-info)
                               (dump-files . ,dump-files-info)))
           (code (alist-ref mode defined-commands)))
      (cond
       ((not mode) (usage))
       ((eq? mode 'call) (call-rtorrent options operands))
       (code (code options))
       (else (print "Unimplemented operation '" mode "' called"))))))

(main)

