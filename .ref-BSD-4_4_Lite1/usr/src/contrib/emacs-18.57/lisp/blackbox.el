;  Blackbox game in Emacs Lisp

;  by F. Thomas May
;  uw-nsr!uw-warp!tom@beaver.cs.washington.edu

(defvar blackbox-mode-map nil "")

(if blackbox-mode-map
    ()
  (setq blackbox-mode-map (make-keymap))
  (suppress-keymap blackbox-mode-map t)
  (define-key blackbox-mode-map "\C-f" 'bb-right)
  (define-key blackbox-mode-map "\C-b" 'bb-left)
  (define-key blackbox-mode-map "\C-p" 'bb-up)
  (define-key blackbox-mode-map "\C-n" 'bb-down)
  (define-key blackbox-mode-map "\C-e" 'bb-eol)
  (define-key blackbox-mode-map "\C-a" 'bb-bol)
  (define-key blackbox-mode-map " " 'bb-romp)
  (define-key blackbox-mode-map "\C-m" 'bb-done))


;; Blackbox mode is suitable only for specially formatted data.
(put 'blackbox-mode 'mode-class 'special)

(defun blackbox-mode ()
  "Major mode for playing blackbox.

SPC -- send in a ray from point, or toggle a ball
RET -- end game and get score

Precisely,\\{blackbox-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map blackbox-mode-map)
  (setq truncate-lines t)
  (setq major-mode 'blackbox-mode)
  (setq mode-name "Blackbox"))

(defun blackbox (num)
  "Play blackbox.  Arg is number of balls."
  (interactive "P")
  (switch-to-buffer "*Blackbox*")
  (blackbox-mode)
  (setq buffer-read-only t)
  (buffer-flush-undo (current-buffer))
  (setq bb-board (bb-init-board (or num 4)))
  (setq bb-balls-placed nil)
  (setq bb-x -1)
  (setq bb-y -1)
  (setq bb-score 0)
  (setq bb-detour-count 0)
  (bb-insert-board)
  (bb-goto (cons bb-x bb-y)))

(defun bb-init-board (num-balls)
  (random t)
  (let (board pos)
    (while (>= (setq num-balls (1- num-balls)) 0)
      (while
	  (progn
	    (setq pos (cons (logand (random) 7) (logand (random) 7)))
	    (bb-member pos board)))
      (setq board (cons pos board)))
    board))

(defun bb-insert-board ()
  (let (i (buffer-read-only nil))
    (erase-buffer)
    (insert "                     \n")
    (setq i 8)
    (while (>= (setq i (1- i)) 0)
      (insert "   - - - - - - - -   \n"))
    (insert "                     \n")))

(defun bb-right ()
  (interactive)
  (if (= bb-x 8)
      ()
    (forward-char 2)
    (setq bb-x (1+ bb-x))))

(defun bb-left ()
  (interactive)
  (if (= bb-x -1)
      ()
    (backward-char 2)
    (setq bb-x (1- bb-x))))

(defun bb-up ()
  (interactive)
  (if (= bb-y -1)
      ()
    (previous-line 1)
    (setq bb-y (1- bb-y))))

(defun bb-down ()
  (interactive)
  (if (= bb-y 8)
      ()
    (next-line 1)
    (setq bb-y (1+ bb-y))))

(defun bb-eol ()
  (interactive)
  (setq bb-x 8)
  (bb-goto (cons bb-x bb-y)))

(defun bb-bol ()
  (interactive)
  (setq bb-x -1)
  (bb-goto (cons bb-x bb-y)))

(defun bb-romp ()
  (interactive)
  (cond
   ((and
     (or (= bb-x -1) (= bb-x 8))
     (or (= bb-y -1) (= bb-y 8))))
   ((bb-outside-box bb-x bb-y)
    (bb-trace-ray bb-x bb-y))
   (t
    (bb-place-ball bb-x bb-y))))

(defun bb-place-ball (x y)
  (let ((coord (cons x y)))
    (cond
     ((bb-member coord bb-balls-placed)
      (setq bb-balls-placed (bb-delete coord bb-balls-placed))
      (bb-update-board "-"))
     (t
      (setq bb-balls-placed (cons coord bb-balls-placed))
      (bb-update-board "O")))))

(defun bb-trace-ray (x y)
  (let ((result (bb-trace-ray-2
		 t
		 x
		 (cond
		  ((= x -1) 1)
		  ((= x 8) -1)
		  (t 0))
		 y
		 (cond
		  ((= y -1) 1)
		  ((= y 8) -1)
		  (t 0)))))
    (cond
     ((eq result 'hit)
      (bb-update-board "H")
      (setq bb-score (1+ bb-score)))
     ((equal result (cons x y))
      (bb-update-board "R")
      (setq bb-score (1+ bb-score)))
     (t
      (setq bb-detour-count (1+ bb-detour-count))
      (bb-update-board (format "%d" bb-detour-count))
      (save-excursion
	(bb-goto result)
	(bb-update-board (format "%d" bb-detour-count)))
      (setq bb-score (+ bb-score 2))))))

(defun bb-trace-ray-2 (first x dx y dy)
  (cond
   ((and (not first)
	 (bb-outside-box x y))
    (cons x y))
   ((bb-member (cons (+ x dx) (+ y dy)) bb-board)
    'hit)
   ((bb-member (cons (+ x dx dy) (+ y dy dx)) bb-board)
    (bb-trace-ray-2 nil x (- dy) y (- dx)))
   ((bb-member (cons (+ x dx (- dy)) (+ y dy (- dx))) bb-board)
    (bb-trace-ray-2 nil x dy y dx))
   (t
    (bb-trace-ray-2 nil (+ x dx) dx (+ y dy) dy))))

(defun bb-done ()
  (interactive)
  (let (bogus-balls)
    (if (not (= (length bb-balls-placed) (length bb-board)))
	(message "Spud!  You have only %d balls in the box."
		 (length bb-balls-placed))
      (setq bogus-balls (bb-show-bogus-balls bb-balls-placed bb-board))
      (if (= bogus-balls 0)
	  (message "Right!  Your score is %d." bb-score)
	(setq bb-score (+ bb-score (* 5 bogus-balls)))
	(message "Veg!  You missed %d balls.  Your score is %d."
		 bogus-balls bb-score))
      (bb-goto '(-1 . -1)))))

(defun bb-show-bogus-balls (balls-placed board)
  (bb-show-bogus-balls-2 balls-placed board "x")
  (bb-show-bogus-balls-2 board balls-placed "o"))

(defun bb-show-bogus-balls-2 (list-1 list-2 c)
  (cond
   ((null list-1)
    0)
   ((bb-member (car list-1) list-2)
    (bb-show-bogus-balls-2 (cdr list-1) list-2 c))
   (t
    (bb-goto (car list-1))
    (bb-update-board c)
    (1+ (bb-show-bogus-balls-2 (cdr list-1) list-2 c)))))

(defun bb-outside-box (x y)
  (or (= x -1) (= x 8) (= y -1) (= y 8)))

(defun bb-goto (pos)
  (goto-char (+ (* (car pos) 2) (* (cdr pos) 22) 26)))

(defun bb-update-board (c)
  (let ((buffer-read-only nil))
    (backward-char (1- (length c)))
    (delete-char (length c))
    (insert c)
    (backward-char 1)))
  
(defun bb-member (elt list)
  "Returns non-nil if ELT is an element of LIST.  Comparison done with equal."
  (eval (cons 'or (mapcar (function (lambda (x) (equal x elt))) list))))

(defun bb-delete (item list)
  "Deletes ITEM from LIST and returns a copy."
  (cond
   ((equal item (car list)) (cdr list))
   (t (cons (car list) (bb-delete item (cdr list))))))



