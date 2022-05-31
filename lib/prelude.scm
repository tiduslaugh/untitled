;; Contains code that gets executed immediately after functions are registered but before
;; the display is initialized.
;;(define-module (lib prelude)
;;    #:use-module ((config) #:prefix config:)
;;    #:use-module ((system repl server))
;;    #:use-module ((system repl coop-server))
;;)
(use-modules ((config) #:prefix config:) ;; ((ice-9 readline))
             ((system repl server))
             ((system repl coop-server))
             ((srfi srfi-18))
             ((c-bindings))
             ((lib clog)))

;; (activate-readline)
;; Running top level repl server code in a module is no bueno, apparently...
(define (init-debug-server)
    (if config:launch-debug-server
        (spawn-server (make-tcp-server-socket #:port config:debug-server-port))))

;; eg (interleave (list 1 2 3) "and") -> (list 1 "and" 2 "and" 3)
(define (interleave lst sep)
  (if (nil? lst)
    '()
    (letrec ((interleave-internal 
             (lambda (in sep out)
               (if (null? (cdr in))
                 (reverse (cons (car in) out))
                 (interleave-internal (cdr in) sep (cons sep (cons (car in) out)))))))
      (interleave-internal lst sep '()))))

;; eg (generate-prefix '(lib fancy-pants)) -> 'lib--fancy-pants:
(define (generate-prefix module-list)
    (string->symbol
      (string-append
        (string-join (map symbol->string module-list) "--")
        ":"
      )))

(define-syntax use-prefixed-module
  (syntax-rules ()
    ((use-prefixed-module module-list)
     (primitive-eval `(use-modules (,'module-list #:prefix ,(generate-prefix 'module-list)))))))
(export use-prefixed-module)

(define (clog-exception-handler exc)
    (begin
        (display "We messed up")
        (clog 'error "Caught scheme exception: ~S" exc)))

(define (call-main-protected main)
    (with-exception-handler clog-exception-handler main))
