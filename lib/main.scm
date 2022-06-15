(use-modules ((lib prelude))
             ((c-bindings))
             ((ice-9 control)))

(use-prefixed-module (lib clog))

(define (main game return)
  (define-values (ctrl input-char) (getch))
  (lib/clog:clog 'info "~S ~S" ctrl input-char)
  (mvaddch 0 0 #\0)
  (cond
    ((and (eq? input-char #\q) ctrl) (return #f))
    ((and (char? input-char) (char>? #\40) (char<? #\177)) (mvaddch 0 0 input-char))
    ((eq? input-char 'curses-error) (return input-char)))
  (error "foo")
  (main game return))

(define (main-wrap)
  (let ((return-value
         (call/ec (lambda (return) (main '() return)))))
    (lib/clog:clog 'info "Return value was ~S" return-value)))

(lib/clog:call-main-protected main-wrap)
