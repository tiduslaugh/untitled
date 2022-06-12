(use-modules ((lib prelude))
             ((c-bindings))
             ((ice-9 control)))

(use-prefixed-module (lib clog))

(define (main game return)
  (define-values (ctrl input-char) (getch))
  (lib/clog:clog 'info "~S ~S" ctrl input-char)
  (if (and (eq? input-char #\q) ctrl)
    (return #f))
  (lib/clog:clog 'info "Char was ~S" input-char)
  (main game return))

(define (main-wrap)
  (let ((return-value
         (call/ec (lambda (return) (main '() return)))))
    (lib/clog:clog 'info "Return value was ~S" return-value)))

(define (call-main-protected main)
    (with-exception-handler lib/clog:clog-exception-handler main #:unwind? #t))

(call-main-protected main-wrap)
