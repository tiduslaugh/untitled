(define-module (lib clog))

(define (symbol/string->string val)
  (if (symbol? val)
    (symbol->string val)
    val))

(define severities '(trace debug info warning error fatal))
;; Use our c based logging to log a message
;; e.g. (clog 'error "We screwed up this bad: ~S" 5)
(define-syntax-rule (clog level fmt arg ...)
 (let ((loc (current-source-location)))
     (log-level level 
                (assq-ref loc 'filename)
                (symbol/string->string (assq-ref loc 'line))
                (with-output-to-string 
                  (lambda () (simple-format (current-output-port) fmt arg ...))))))

(define (make-logging-port level)
  (let ((buf ""))
    (make-soft-port
      (vector
        (lambda (c) (set! buf (string-append buf (string c)))) ;;putch
	(lambda (s) (set! buf (string-append buf s))) ;;putstr
	(lambda () (begin (clog level "~S" buf) (set! buf "")))
	#f
	(lambda () (set! buf ""))
	"w"))))

(export clog make-logging-port)
