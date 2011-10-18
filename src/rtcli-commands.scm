(declare (unit rtcli-commands))
(declare (uses rtorrent-connection
               rtorrent-commands))

(use format)
(use numbers)
(use srfi-19)

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
         (new-top-dir (alist-ref 'new-top-dir options))
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
         (files (hash-table-ref/default (hash-table-ref torrent "info") "files" #f))
         (_ (if (and new-top-dir
                     files)
                (hash-table-set! (hash-table-ref torrent "info") "name" new-top-dir)))
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
                                                    (pos (string-contains of mf))
                                                    (fmt-pos (string-contains fmt mf)))
                                               (if (and pos fmt-pos)
                                                   (begin
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

(define (dump-torrent-info options)
  (validate-parameters options 'torrent)
  (let* ((tor  (call-with-input-file (alist-ref 'torrent options) bdecode))
         (cdt (hash-table-ref/default tor "creation date" #f))
         (al (hash-table-ref/default tor "announce-list" #f)))
    (print "Announce to: " (hash-table-ref tor "announce"))
    (if cdt
        (print "Created at: " (seconds->string cdt) " (" cdt ")")
        (print "Created at: <undef>"))
    (print "Created by: " (hash-table-ref/default tor "created by" "<undef>"))
    (print "Comment: " (hash-table-ref/default tor "comment" "<undef>"))
    (if al
        (begin
          (print "Additional announce list:")
          (let print-al ((l al))
            (if (not (null? l))
                (begin
                  (map (lambda (a) (print a)) (car l))
                  (print-al (cdr l)))))))
    (print "Encoding: " (hash-table-ref/default tor "encoding" "<undef>"))
    (dump-files-info options tor)))

(define (dump-files-info options . rest)
  (let* ((tor (if (null? rest)
                  (begin
                    (validate-parameters options 'torrent)
                    (call-with-input-file (alist-ref 'torrent options) bdecode))
                  (car rest)))
         (info (hash-table-ref tor "info"))
         (files (hash-table-ref/default info "files" #f)))
    (if files
        (let* ((dir (hash-table-ref/default info "name" #f))
               (_ (if (and (not (null? rest))
                           dir)
                      (begin
                        (print "Top level directory: " dir)
                        (print "Files:")))))
          (let print-file ((l files))
            (if (not (null? l))
                (let ((f (car l)))
                  (print (if dir (string-append dir "/"))
                         (string-join (hash-table-ref f "path") "/") " Size: " (hash-table-ref f "length"))
                  (print-file (cdr l)))))
          )
        (print "File:" (hash-table-ref tor "name") "Size: "(hash-table-ref tor "length")))))


