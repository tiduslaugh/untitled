(define-module (lib clog)
    #:use-module (ice-9 control)
    #:use-module (ice-9 exceptions)
    #:use-module (ice-9 format)
    #:use-module (c-bindings))

;; Use our c based logging to log a message
;; e.g. (clog 'error "We screwed up this bad: ~S" 5)
(define-syntax clog
  (lambda (x)
    (define severities '(trace debug info warning error fatal))
    (define (symbol/string->string val)
      (if (symbol? val)
        (symbol->string val)
        val))
    (syntax-case x (quote)
     ((clog (quote level) fmt arg ...) 
      (memq (syntax->datum #'level) severities)
      (let ((loc (syntax-source x)))
        #`(log-level
          (quote level)
          #,(assq-ref loc 'filename)
          #,(symbol/string->string (assq-ref loc 'line))
          (with-output-to-string 
            (lambda () 
              (simple-format (current-output-port) fmt arg ...)))))))))

(define (get-backtrace-str stack)
  (with-output-to-string
    (lambda ()
      (display-backtrace stack (current-output-port)))))

(define (get-exception-str exc)
  (string-trim-right 
    (with-output-to-string 
      (lambda () (print-exception (current-output-port) #f (exception-kind exc) (exception-args exc))))
    char-set:whitespace))

(define (clog-exception-handler exc) 
  (let*
   ((stack (make-stack #t 3))
    (bt (get-backtrace-str stack))
    (exception-str (get-exception-str exc)))
   (clog 'error "~A" bt)
   (clog 'error "~A" exception-str)))

;; Note: the below doesn't work and I'm not sure why. Just use the clog macro for now

;; (define (make-logging-port level)
;;   (let ((buf ""))
;;     (make-soft-port
;;       (vector
;;         (lambda (c) (set! buf (string-append buf (string c)))) ;;putch
;; 	    (lambda (s) (set! buf (string-append buf s))) ;;putstr
;; 	    (lambda () (display (simple-format #f "buf is ~A" buf)) (clog level "~A" buf) (set! buf "")) ;;flush
;; 	    #f ;; getch
;; 	    (lambda () (set! buf "")) ;;close
;;       )
;; 	"w")))
;; 
;; (define-syntax-rule (logging-port-template level)
;;     (primitive-eval 
;;       `(define-public 
;;          ,(string->symbol (format #f "log-~A-port" level)) 
;;          (make-logging-port (quote level)))))
;; 
;; (map (lambda (sev) (logging-port-template sev)) severities)

(define (call-main-protected main)
  (let/ec 
    cancel
    (with-exception-handler 
      (lambda (exn)
        (clog-exception-handler exn)
        (cancel #f))
      (lambda ()
        (start-stack 'main (main))))))

(export clog clog-exception-handler call-main-protected)
