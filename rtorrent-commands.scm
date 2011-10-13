(declare (unit rtorrent-commands)
         (uses rtorrent-connection)
         (local registered-xml-rpc-methods)
         (export rt:cmd
                 register-xml-rpc-commands))

;; list of some preconfigured methods
(define registered-xml-rpc-methods
  '("system.listMethods"
    "system.hostname"
    "system.pid"
    "system.client_version"
    "system.library_version"
    "load_raw"
    "d.get_directory"
    "d.set_directory"
    "d.state"
    "d.start"
    "d.stop"
    "d.erase"))

(define (register-xml-rpc-commands list)
  (set! registered-xml-rpc-methods (append list registered-xml-rpc-methods)))

(define (rt:cmd connection request . parameters)
  (let* ((method (if (symbol? request) (symbol->string request) request))
         (registered (member method registered-xml-rpc-methods)))
    (if registered
        (apply send connection method parameters)
        (begin
          (print "Method '" method "' not registered.")
          #f))))
