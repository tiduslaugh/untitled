;; Contains code that gets executed immediately after functions are registered but before
;; the display is initialized.
(define-module (lib prelude)
    #:use-module ((config)
                  #:prefix config:)
    #:use-module ((system repl server))
    #:use-module ((system repl coop-server))
)
(define coop-server #f)
;; Running top level repl server code in a module is no bueno, apparently...
(define-public (init-debug-server)
    (if config:launch-debug-server
        ;;(set! coop-server (spawn-coop-repl-server (make-tcp-server-socket #:port config:debug-server-port)))))
        (spawn-server (make-tcp-server-socket #:port config:debug-server-port))))
(define-public (poll-repl-server)
    (poll-coop-repl-server coop-server))
