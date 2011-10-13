(declare (unit rtcli-commands))
(declare (uses rtorrent-connection
               rtorrent-commands))

(define (validate-parameters options . params)
  (let ((have-missed-params #f))
    (for-each (lambda (p)
                (if (not (alist-ref p options))
                    (begin
                      (set! have-missed-params #t)
                      (print "Parameter '" p "' required for current operation")
                      ))) params)
    (if have-missed-params (exit 1))))

(define (generic-torrent-command options validation-parameters code)
  (apply validate-parameters options validation-parameters)
  (let ((th (calculate-torrent-hash (alist-ref 'torrent options)))
        (connection (make-connection (alist-ref 'host options))))
    (if (and th connection)
        (code connection th))))

(define (oneline-torrent-command options validation-parameters cmd)
  (generic-torrent-command options validation-parameters
                           (lambda (c t)
                             (rt:cmd c cmd t))))

(define (calc-torrent-hash options)
  (validate-parameters options 'torrent)
  (let ((tor (alist-ref 'torrent options)))
    (print (calculate-torrent-hash tor))))

(define (erase-torrent options)
  (generic-torrent-command options '(torrent host)
                           (lambda (c t)
                             (print "Removing torrent with hash '" t "'")
                             (rt:cmd c 'd.stop t)
                             (rt:cmd c 'd.erase t))))

(define (pause-torrent options)
  (oneline-torrent-command options '(torrent host) 'd.pause))

(define (resume-torrent options)
  (oneline-torrent-command options '(torrent host) 'd.resume))

(define (open-torrent options)
  (oneline-torrent-command options '(torrent host) 'd.open))

(define (close-torrent options)
  (oneline-torrent-command options '(torrent host) 'd.close))

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
         (_ (if (not torrent)
                (begin
                  (print "File '" tor "' doesn't looks like valid torrent file")
                  (exit 1))))
         (th (calculate-torrent-hash torrent))
         (connection (make-connection (alist-ref 'host options))))
    (if (and th
             connection)
        (begin
          (print "Adding torrent file '" tor "'" (if dest (string-append " to " dest) ""))
          (rt:cmd connection 'load_raw (string->blob (bencode torrent)))
          ;; now it's time to sleep few secs and double check that rTorrent now knows about new file
          (if (call/cc
               (lambda (break)
                 (do ((count 0 (+ 1 cont)))
                     ((> count 5) #f)
                   (let ((tmp (rt:cmd connection 'd.state th)))
                     (if tmp
                         (break #t)
                         (sleep 1))))))
              (begin
                (if dest (rt:cmd connection 'd.set_directory th dest))
                (rt:cmd connection 'd.start th))
              (begin
                (print "Hm. Seems torrent file wasn't added correctly. Aborting.")
                (exit 1))
              ))
        (begin
          (print "Invalid connection url specified '" url "'")
          (exit 1)))))

(define (list-torrents options)
  (define (print-torrent-info c t)
    (let* ((left-bytes (rt:cmd c 'd.get_left_bytes t))
           (down-bytes (rt:cmd c 'd.completed_bytes t))
           (down-rate (rt:cmd c 'd.down.rate t))
           (up-rate (rt:cmd c 'd.up.rate t))
           (down-total (rt:cmd c 'd.down.total t))
           (up-total (rt:cmd c 'd.up.total t))
           (ratio (rt:cmd c 'd.ratio t))
           (name (rt:cmd c 'd.name t))
           (state (rt:cmd c 'd.state t)))
      (print name "/" t " [" down-bytes "/" (+ down-bytes left-bytes) "]"
             " (" up-rate "/" down-rate ")"
             " {" up-total "/" down-total "}"
             " " ratio
             " " (if state "Started" "Stopped"))))
  (validate-parameters options 'host)
  (let ((connection (make-connection (alist-ref 'host options))))
    (if connection
        (let ((tl (rt:cmd connection 'download_list "")))
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
