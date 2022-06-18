;; Contains code that gets executed immediately after functions are registered but before
;; the display is initialized.
(define-module (lib prelude))
(use-modules ((config) #:prefix config:) ;; ((ice-9 readline))
             ((system repl server))
             ((system repl coop-server))
             ((srfi srfi-18))
             ((lib clog) #:prefix lib/clog:))

;; (activate-readline)
;; Running top level repl server code in a module is no bueno, apparently...
(define (init-debug-server)
    (if config:launch-debug-server
        (spawn-server (make-tcp-server-socket #:port config:debug-server-port))))

(define-public (call-main-protected main)
    (with-exception-handler lib/clog:clog-exception-handler main #:unwind? #t))
