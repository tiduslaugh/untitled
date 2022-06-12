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

;; eg (generate-prefix '(lib fancy-pants)) -> 'lib--fancy-pants:
(define (generate-prefix module-list)
    (string->symbol
      (string-append
        (string-join (map symbol->string module-list) "/")
        ":"
      )))

(define-syntax use-prefixed-module
  (syntax-rules ()
    ((use-prefixed-module module-list)
     (primitive-eval `(use-modules (,'module-list #:prefix ,(generate-prefix 'module-list)))))))
(export use-prefixed-module)

(define-public (call-main-protected main)
    (with-exception-handler lib/clog:clog-exception-handler main #:unwind? #t))
