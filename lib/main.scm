(use-modules (lib prelude)
             (c-bindings)
             (ice-9 control)
             ((lib game) #:prefix lib/game:))

(define (handle-input game return)
  (define-values (ctrl input-char) (getch))
  (cond
    ((and (eq? input-char #\q) ctrl) 
     (return #f))
    ((and (char? input-char) (char>? #\40) (char<? #\177)) 
     (let* ((cursor (lib/game:game-cursor game))
            (y (car cursor))
            (x (cadr cursor)))
       (mvaddch y x input-char)
       (lib/game:set-game-cursor game (list y (1+ x)))))
    ((eq? input-char 'curses-error) 
     (return input-char))
    (else game)))

(define (main game return)
  (mvaddstr 0 0 "Hello there.")
  (define game2 (handle-input game return))
  (main game2 return))

(define (new-game)
  (lib/game:game (list 1 0)))

(define (main-wrap)
  (let ((return-value
          (call/ec 
            (lambda (return) (main (new-game) return)))))
    (lib/clog:clog 'info "Return value was ~S" return-value)))

(lib/clog:call-main-protected main-wrap)
