;
; hanoi - towers of hanoi in GNUmacs
;
; Author (a) 1985, Damon Anton Permezel
;

;;;
;;; hanoi-topos - direct cursor addressing
;;;
(defun hanoi-topos (row col)
  (goto-line row)
  (beginning-of-line)
  (forward-char col))

;;;
;;; hanoi - user callable Towers of Hanoi
;;;
(defun hanoi (nrings)
  "Towers of Hanoi diversion.  Argument is number of rings."
  (interactive
   (list (if (null current-prefix-arg)
	     3
	     (prefix-numeric-value current-prefix-arg))))  
  (if (<= nrings 0) (error "Negative number of rings"))
  (let (pole-spacing
	floor-row
	fly-row
	(window-height (window-height (selected-window)))
	(window-width (window-width (selected-window))))
    (let ((h (+ nrings 2))
	  (w (+ (* (1- nrings) 6) 2 5)))
      (if (not (and (>= window-width h)
		    (> window-width w)))
	  (progn
	    (delete-other-windows)
	    (if (not (and (>= (setq window-height
				    (window-height (selected-window))) h)
			  (> (setq window-width
				   (window-width (selected-window))) w)))
		(error "Screen is too small (need at least %dx%d)" w h))))
      (setq pole-spacing (/ window-width 6))
      (if (not (zerop (logand pole-spacing 1)))
	  ;; must be even
	  (setq pole-spacing (1+ pole-spacing)))
      (setq floor-row (if (> (- window-height 3) h)
			  (- window-height 3) window-height)))
    (let ((fly-row (- floor-row nrings 1))
	  ;; pole: column . fill height
	  (pole-1 (cons pole-spacing floor-row))
	  (pole-2 (cons (* 3 pole-spacing) floor-row))
	  (pole-3 (cons (* 5 pole-spacing) floor-row))
	  (rings (make-vector nrings nil)))
      ;; construct the ring list
      (let ((i 0))
	(while (< i nrings)
	  ;; ring: [pole-number string empty-string]
	  (aset rings i (vector nil
				(make-string (+ i i 3) (+ ?0 i))
				(make-string (+ i i 3) ?\  )))
	  (setq i (1+ i))))
      ;;
      ;; init the screen
      ;;
      (switch-to-buffer "*Hanoi*")
      (setq buffer-read-only nil)
      (buffer-flush-undo (current-buffer))
      (erase-buffer)
      (let ((i 0))
	(while (< i floor-row)
	  (setq i (1+ i))
	  (insert-char ?\  (1- window-width))
	  (insert ?\n)))
      (insert-char ?= (1- window-width))

      (let ((n 1))
	(while (< n 6)
	  (hanoi-topos fly-row (* n pole-spacing))
	  (setq n (+ n 2))
	  (let ((i fly-row))
	    (while (< i floor-row)
	      (setq i (1+ i))
	      (next-line 1)
	      (insert ?\|)
	      (delete-char 1)
	      (backward-char 1)))))
      ;(sit-for 0)
      ;;
      ;; now draw the rings in their initial positions
      ;;
      (let ((i 0)
	    ring)
	(while (< i nrings)
	  (setq ring (aref rings (- nrings 1 i)))
	  (aset ring 0 (- floor-row i))
	  (hanoi-topos (cdr pole-1)
		       (- (car pole-1) (- nrings i)))
	  (hanoi-draw-ring ring t nil)
	  (setcdr pole-1 (1- (cdr pole-1)))
	  (setq i (1+ i))))
      (setq buffer-read-only t)
      (sit-for 0)
      ;;
      ;; do it!
      ;;
      (hanoi0 (1- nrings) pole-1 pole-2 pole-3)
      (goto-char (point-min))
      (message "Done")
      (setq buffer-read-only t)
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0))))

;;;
;;; hanoi0 - work horse of hanoi
;;;
(defun hanoi0 (n from to work)
  (cond ((input-pending-p)
	 (signal 'quit (list "I can tell you've had enough")))
	((< n 0))
	(t
	 (hanoi0 (1- n) from work to)
	 (hanoi-move-ring n from to)
	 (hanoi0 (1- n) work to from))))

;;;
;;; hanoi-move-ring - move ring 'n' from 'from' to 'to'
;;;
;;;
(defun hanoi-move-ring (n from to)
  (let ((ring (aref rings n))		; ring <- ring: (ring# . row)
	(buffer-read-only nil))
    (let ((row (aref ring 0))		; row <- row ring is on
	  (col (- (car from) n 1))	; col <- left edge of ring
	  (dst-col (- (car to) n 1))	; dst-col <- dest col for left edge
	  (dst-row (cdr to)))		; dst-row <- dest row for ring
      (hanoi-topos row col)
      (while (> row fly-row)		; move up to the fly row
	(hanoi-draw-ring ring nil t)	; blank out ring
	(previous-line 1)		; move up a line
	(hanoi-draw-ring ring t nil)	; redraw
	(sit-for 0)
	(setq row (1- row)))
      (setcdr from (1+ (cdr from)))	; adjust top row
      ;;
      ;; fly the ring over to the right pole
      ;;
      (while (not (equal dst-col col))
	(cond ((> dst-col col)		; dst-col > col: right shift
	       (end-of-line 1)
	       (delete-backward-char 2)
	       (beginning-of-line 1)
	       (insert ?\  ?\  )
	       (sit-for 0)
	       (setq col (1+ (1+ col))))
	      ((< dst-col col)		; dst-col < col: left shift
	       (beginning-of-line 1)
	       (delete-char 2)
	       (end-of-line 1)
	       (insert ?\  ?\  )
	       (sit-for 0)
	       (setq col (1- (1- col))))))
      ;;
      ;; let the ring float down
      ;;
      (hanoi-topos fly-row dst-col)
      (while (< row dst-row)		; move down to the dest row
	(hanoi-draw-ring ring nil (> row fly-row)) ; blank out ring
	(next-line 1)			; move down a line
	(hanoi-draw-ring ring t nil)	; redraw ring
	(sit-for 0)
	(setq row (1+ row)))
      (aset ring 0 dst-row)
      (setcdr to (1- (cdr to))))))	; adjust top row

;;;
;;; draw-ring -	draw the ring at point, leave point unchanged
;;;
;;; Input:
;;;	ring
;;;	f1	-	flag: t -> draw, nil -> erase
;;;	f2	-	flag: t -> erasing and need to draw ?\|
;;;
(defun hanoi-draw-ring (ring f1 f2)
  (save-excursion
    (let* ((string (if f1 (aref ring 1) (aref ring 2)))
	   (len (length string)))
      (delete-char len)
      (insert string)
      (if f2
	  (progn
	    (backward-char (/ (+ len 1) 2))
	    (delete-char 1) (insert ?\|))))))

