(declare (unit rtcli-commands))
(declare (uses rtorrent-connection
               rtorrent-commands))

(use format)
(use numbers)

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
          ;; check that torrent was added
          (if (rt:cmd connection 'd.state th)
              (begin
                (if dest (rt:cmd connection 'd.set_directory th dest))
                (rt:cmd connection 'd.start th))
              (begin
                (print "Hm. Seems torrent file wasn't added correctly. Aborting.")
                (exit 1))))
        (begin
          (print "Invalid connection url specified '" (alist-ref 'host options) "'")
          (exit 1)))))

(define (list-torrents options)
  (validate-parameters options 'host)
  (let ((connection (make-connection (alist-ref 'host options))))
    (if connection
        (let ((tl (rt:cmd connection 'download_list "")))
          (if (vector? tl)
              (letrec ((of (alist-ref 'format options eqv?
                                      "~d.name/~d.hash [~d.completed_bytes/~d.get_size_bytes] (~d.up.rate/~d.down.rate) {~d.up.total/~d.down.total} ~d.ratio"))
                       (fmt of)
                       (collect-fields (lambda (rest lst)
                                         (if (null? rest)
                                             lst
                                             (let* ((m (car rest))
                                                    (mf (string-append "~" m))
                                                    (ml (string-length mf))
                                                    (pos (string-contains of mf)))
                                               (if pos
                                                   (let ((fmt-pos (string-contains fmt mf)))
                                                     (set! fmt (string-replace fmt "~A"  fmt-pos (+ fmt-pos ml)))
                                                     (collect-fields (cdr rest) (alist-cons pos m lst)))
                                                   (collect-fields (cdr rest) lst))))))
                       (flds (alist->hash-table
                              (collect-fields
                               (sort registered-xml-rpc-methods
                                     (lambda (a b) (> (string-length a) (string-length b))))
                               '())))
                       (ordered-methods (map (lambda (k)
                                               (hash-table-ref flds k)) (sort (hash-table-keys flds) <))))
                (for-each
                 (lambda (t)
                   (let ((vals (map (lambda (m)
                                      (rt:cmd connection m t)) ordered-methods)))
                     (print (apply format fmt vals))))
                 (vector->list tl)))))
        )))

(define (call-rtorrent options operands)
  (validate-parameters options 'host)
  (let ((connection (make-connection (alist-ref 'host options))))
    (if connection
        (begin
          (let ((result (apply send connection operands)))
            (if (vector? result)
                (for-each print (vector->list result))
                (print result)))))))

