;;  Losing unix doesn't know about the -real- control bit

;; there should be some way to conditionalize this on the basis
;; of %TOFCI -- except that the existing supdup server loses this information!
;; It isn't clear-cut what to do in the server, as %tofci means that the user
;; can generate full 9-bit MIT characters, which isn't what the `km' termcap
;; flag means.  On the other hand, being able to generate 8-bit characters
;; (which is sort of what `km' is) isn't the same as %tofci.
;; I think the problem is fundamental and cultural and irresolvable.

;; unix supdup server uses 0237 as a control escape.
;; c-a		001
;; m-a		341
;; c-m-a	201
;; c-1		237 061
;; m-1		261
;; c-m-1	237 261
;; c-m-_	237 237

(defvar supdup-control-map (make-keymap))
(fillarray supdup-control-map 'ascii-loses)
(defvar supdup-control-meta-map (make-keymap))
(fillarray supdup-control-meta-map 'ascii-loses)
(define-key supdup-control-meta-map "\C-_" nil) ; this is c-m-_
(define-key supdup-control-map "\e" supdup-control-meta-map)
(define-key global-map "\e\C-_" supdup-control-map)
(let ((n ?0))
  (while (<= n ?9)
    (define-key supdup-control-map (char-to-string n) 'supdup-digit-argument)
    (define-key supdup-control-meta-map (char-to-string n) 'supdup-digit-argument)
    (setq n (1+ n)))
  (define-key supdup-control-map "-" 'supdup-digit-argument)
  (define-key supdup-control-meta-map "-" 'supdup-digit-argument))

(defun ascii-loses ()
  (interactive)
  (if (= (aref (this-command-keys) 0) meta-prefix-char)
      ;; loser typed <esc> c-_ <char>
      (error "Undefined command: %s"
	     (mapconcat 'text-char-description (this-command-keys) " "))
    ;; Get here from m-c-_ <char> for c-<char> or m-c-_ m-<char>
    (error "Ascii loses: c-%s%c"
	   (if (> last-input-char ?\200) "m-" "")
	   (logand last-input-char ?\177))))


(defun supdup-digit-argument (p)
  (interactive "P")
  (let ((n last-input-char))
    (if (and (<= (+ ?\200 ?0) n) (<= n (+ ?\200 ?9)))
	(setq n (- n ?\200)))
    (cond ((or (= n ?-) (= n ?\M--))
	   (message "Arg: %s" (setq prefix-arg '-)))
	  ((or (< n ?0) (> n ?9))
	   (error "Lossage: %s" (this-command-keys)))
	  (t
	   (setq n (- n ?0))
	   (message "Arg: %d"
		    (setq prefix-arg
			  (cond ((listp p)
				 n)
				((eq p '-)
				 (- n))
				((>= p 0)
				 (+ (* p 10) n))
				(t
				 (- (* p 10) n)))))))))

;; Attempt to detect slimebollix machine serving as terminal.
(if (let ((termcap (getenv "TERMCAP")))
      (and termcap
	   (string-match ":co#131:li#52:\\|:co#135:li#50:" termcap)))
    (message "In doing business with Symbolics, you are rewarding a wrong."))


;; Mouse support works with Lambdas.
;(autoload 'sup-mouse-report "sup-mouse"
;  "This command is sent by a special version of Supdup on the LMI Lambda
;when the mouse is clicked." t)
;(global-set-key "\C-x\C-@" 'sup-mouse-report)

