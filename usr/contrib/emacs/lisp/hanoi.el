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
(defun hanoi (n-rings)
  "Towers of Hanoi diversion.  Arg is number of rings."
  (interactive "p")
  (if (or (> n-rings 9) (<= n-rings 0))
      (error "Funny number of rings"))

  (let ((floor-row 21)
	(fly-row (- 21 n-rings 1))
	(pole-1 (cons 20  20))		; pole: column . fill height
	(pole-2 (cons 40  20))		; (these must be consed, not '(x . y)
	(pole-3 (cons 60  20))		;  otherwise we are not reentrant)
	(rings '(t (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)
		   (6 . 0) (7 . 0) (8 . 0) (9 . 0))))
    ;;
    ;; init the screen
    ;;
    (switch-to-buffer " *Hanoi*")
    (delete-other-windows)
    (erase-buffer)

    (let ((i 1))			; 21 copies of a line of 78 blanks
      (while (< i floor-row)
	(setq i (1+ i))
	(insert "                                                                              \n")))

    (let ((i 0))			; draw the base
      (while (< i 78)
	(setq i (1+ i))
	(insert ?=)))

    (mapcar (function (lambda (x)	; draw the towers
			      (hanoi-topos fly-row x)
			      (let ((i fly-row))
				(while (< i floor-row)
				  (setq i (1+ i))
				  (next-line 1)
				  (insert ?|)
				  (delete-char 1)
				  (backward-char 1)))))
	    '(20 40 60))
    (sit-for 0)
    ;;
    ;; now init the rings
    ;;
    (let ((where (1- floor-row))
	  (i n-rings)
	  r)
      (while (> i 0)
	(setq r (car (nthcdr i rings)))	; extract desired ring
	(setcdr r where)		; indicate ring row
	(setq where (1- where))
	(hanoi-move-ring i pole-1 pole-1)
	(setq i (1- i))))
    (sit-for 0)
    (hanoi0 n-rings pole-1 pole-2 pole-3))
  (message "Done"))

;;;
;;; hanoi0 - work horse of hanoi
;;;
(defun hanoi0 (n from to work)
  (if (> n 0)
      (progn
	(hanoi0 (1- n) from work to)
	(hanoi-move-ring n from to)
	(hanoi0 (1- n) work to from))))

;;;
;;; hanoi-move-ring - move ring 'n' from 'from' to 'to'
;;;
;;; from and to are dotted pairs consisting of (pole col . fill height)
;;;
(defun hanoi-move-ring (n from to)
  (let ((r (car (nthcdr n rings))))	; r <- ring: (ring# . row)
    (if (eq from to)		; must change poles?
	;;
	;; ring on same pole - used for initialization
	;;
	(progn
	  (hanoi-topos (cdr to) (- (car to) n))
	  (hanoi-draw-ring n t nil)
	  (setcdr to (1- (cdr to))))
      (let ((row (cdr r))		; row <- row ring is on
	    (col (- (car from) n))	; col <- left edge of ring
	    (dst-col (- (car to) n))	; dst-col <- dest col for left edge
	    (dst-row (cdr to)))		; dst-row <- dest row for ring
	(hanoi-topos row col)
	(while (> row fly-row)		; move up to the fly row
	  (hanoi-draw-ring n nil nil)	; blank out ring
	  (previous-line 1)		; move up a line
	  (hanoi-draw-ring n t nil)	; redraw
	  (sit-for 0)
	  (setq row (1- row)))
	(setcdr from (1+ (cdr from)))	; adjust top row
	;;
	;; fly the ring over to the right pole
	;;
	(while (not (equal dst-col col))
	  (cond ((> dst-col col)	; dst-col > col: right shift
		 (end-of-line 1)
		 (delete-backward-char 2)
		 (beginning-of-line 1)
		 (insert "  ")
		 (sit-for 0)
		 (setq col (1+ (1+ col))))
		((< dst-col col)	; dst-col < col: left shift
		 (beginning-of-line 1)
		 (delete-char 2)
		 (end-of-line 1)
		 (insert "  ")
		 (sit-for 0)
		 (setq col (1- (1- col))))))
	;;
	;; let the ring float down
	;;
	(hanoi-topos fly-row dst-col)
	(while (< row dst-row)		; move down to the dest row
	  (hanoi-draw-ring n nil (equal row fly-row))	; blank out ring
	  (next-line 1)			; move down a line
	  (hanoi-draw-ring n t nil)	; redraw ring
	  (sit-for 0)
	  (setq row (1+ row)))
	(setcdr r dst-row)
	(setcdr to (1- (cdr to)))))))	; adjust top row

;;;
;;; hanoi-draw-ring -	draw the ring at dot, leave dot unchanged
;;;
;;; Input:
;;;	n	-	ring #. used to select drawing character
;;;	f1	-	flag: t -> draw, nil -> erase
;;;	f2	-	flag: t -> erasing ring from fly-row -> dont redraw ?|
;;;
(defun hanoi-draw-ring (n f1 f2)
  (save-excursion
    (let ((i 0)
	  (repl (if f1
		    (car (nthcdr n '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
		  ? )))
      (delete-char (+ 1 (* 2 n)))
      (while (< i n)
	(insert repl)
	(setq i (1+ i)))
      (insert (if f1 repl (if f2 ? ?|)))
      (setq i 0)
      (while (< i n)
	(insert repl)
	(setq i (1+ i))))))

