(define-module (config))
;; Whether to launch the debug server, and what port to launch it on.
(define-public launch-debug-server #t)
(define-public debug-server-port 7000)
(define-public log-path '(string-append root-dir "/" "untitled.log"))