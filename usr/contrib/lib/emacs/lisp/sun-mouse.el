;; Mouse handling for Sun windows
;; Copyright (C) 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Jeff Peck, Sun Microsystems, Jan 1987.
;;; Original idea by Stan Jefferson

(provide 'sun-mouse)

;;;
;;;     Modelled after the GNUEMACS keymap interface.
;;;
;;; User Functions:
;;;   make-mousemap, copy-mousemap, 
;;;   define-mouse, global-set-mouse, local-set-mouse,
;;;   use-global-mousemap, use-local-mousemap,
;;;   mouse-lookup, describe-mouse-bindings
;;;
;;; Options:
;;;   extra-click-wait, scrollbar-width
;;;

(defvar extra-click-wait 150
  "*Number of milliseconds to wait for an extra click.
Set this to zero if you don't want chords or double clicks.")

(defvar scrollbar-width 5
  "*The character width of the scrollbar.
The cursor is deemed to be in the right edge scrollbar if it is this near the
right edge, and more than two chars past the end of the indicated line.
Setting to nil limits the scrollbar to the edge or vertical dividing bar.")

;;;
;;; Mousemaps
;;;
(defun make-mousemap ()
  "Returns a new mousemap."
  (cons 'mousemap nil))

(defun copy-mousemap (mousemap)
  "Return a copy of mousemap."
  (copy-alist mousemap))

(defun define-mouse (mousemap mouse-list def)
  "Args MOUSEMAP, MOUSE-LIST, DEF.  Define MOUSE-LIST in MOUSEMAP as DEF.
MOUSE-LIST is a list of atoms specifing a mouse hit according to these rules:
  * One of these atoms specifies the active region of the definition.
	text, scrollbar, modeline, minibuffer
  * One or two or these atoms specify the button or button combination.
        left, middle, right, double
  * Any combination of these atoms specify the active shift keys.
        control, shift, meta
  * With a single unshifted button, you can add
	up
    to indicate an up-click.
The atom `double' is used with a button designator to denote a double click.
Two button chords are denoted by listing the two buttons.
See sun-mouse-handler for the treatment of the form DEF."
  (mousemap-set (mouse-list-to-mouse-code mouse-list) mousemap def))

(defun global-set-mouse (mouse-list def)
  "Give MOUSE-EVENT-LIST a local definition of DEF.
See define-mouse for a description of MOUSE-EVENT-LIST and DEF.
Note that if MOUSE-EVENT-LIST has a local definition in the current buffer,
that local definition will continue to shadow any global definition."
  (interactive "xMouse event: \nxDefinition: ")
  (define-mouse current-global-mousemap mouse-list def))

(defun local-set-mouse (mouse-list def)
  "Give MOUSE-EVENT-LIST a local definition of DEF.
See define-mouse for a description of the arguments.
The definition goes in the current buffer's local mousemap.
Normally buffers in the same major mode share a local mousemap."
  (interactive "xMouse event: \nxDefinition: ")
  (if (null current-local-mousemap)
      (setq current-local-mousemap (make-mousemap)))
  (define-mouse current-local-mousemap mouse-list def))

(defun use-global-mousemap (mousemap)
  "Selects MOUSEMAP as the global mousemap."
  (setq current-global-mousemap mousemap))

(defun use-local-mousemap (mousemap)
  "Selects MOUSEMAP as the local mousemap.
nil for MOUSEMAP means no local mousemap."
  (setq current-local-mousemap mousemap))


;;;
;;; Interface to the Mouse encoding defined in Emacstool.c
;;;
;;; Called when mouse-prefix is sent to emacs, additional
;;; information is read in as a list (button x y time-delta)
;;;
;;; First, some generally useful functions:
;;;

(defun logtest (x y)
  "True if any bits set in X are also set in Y.
Just like the Common Lisp function of the same name."
  (not (zerop (logand x y))))


;;;
;;; Hit accessors.
;;;

(defconst sm::ButtonBits 7)		; Lowest 3 bits.
(defconst sm::ShiftmaskBits 56)		; Second lowest 3 bits (56 = 63 - 7).
(defconst sm::DoubleBits 64)		; Bit 7.
(defconst sm::UpBits 128)		; Bit 8.

;;; All the useful code bits
(defmacro sm::hit-code (hit)
  (` (nth 0 (, hit))))
;;; The button, or buttons if a chord.
(defmacro sm::hit-button (hit)
  (` (logand sm::ButtonBits (nth 0 (, hit)))))
;;; The shift, control, and meta flags.
(defmacro sm::hit-shiftmask (hit)
  (` (logand sm::ShiftmaskBits (nth 0 (, hit)))))
;;; Set if a double click (but not a chord).
(defmacro sm::hit-double (hit)
  (` (logand sm::DoubleBits (nth 0 (, hit)))))
;;; Set on button release (as opposed to button press).
(defmacro sm::hit-up (hit)
  (` (logand sm::UpBits (nth 0 (, hit)))))
;;; Screen x position.
(defmacro sm::hit-x (hit) (list 'nth 1 hit))
;;; Screen y position.
(defmacro sm::hit-y (hit) (list 'nth 2 hit))
;;; Millisconds since last hit.
(defmacro sm::hit-delta (hit) (list 'nth 3 hit))

(defmacro sm::hit-up-p (hit)		; A predicate.
  (` (not (zerop (sm::hit-up (, hit))))))

;;;
;;; Loc accessors.  for sm::window-xy
;;;
(defmacro sm::loc-w (loc) (list 'nth 0 loc))
(defmacro sm::loc-x (loc) (list 'nth 1 loc))
(defmacro sm::loc-y (loc) (list 'nth 2 loc))

(defmacro eval-in-buffer (buffer &rest forms)
  "Macro to switches to BUFFER, evaluates FORMS, returns to original buffer."
  ;; When you don't need the complete window context of eval-in-window
  (` (let ((StartBuffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (, buffer))
	  (,@ forms))
    (set-buffer StartBuffer)))))

(put 'eval-in-buffer 'lisp-indent-hook 1)

;;; this is used extensively by sun-fns.el
;;;
(defmacro eval-in-window (window &rest forms)
  "Switch to WINDOW, evaluate FORMS, return to original window."
  (` (let ((OriginallySelectedWindow (selected-window)))
       (unwind-protect
	   (progn
	     (select-window (, window))
	     (,@ forms))
	 (select-window OriginallySelectedWindow)))))
(put 'eval-in-window 'lisp-indent-hook 1)

;;;
;;; handy utility, generalizes window_loop
;;;

;;; It's a macro (and does not evaluate its arguments).
(defmacro eval-in-windows (form &optional yesmini)
  "Switches to each window and evaluates FORM.  Optional argument
YESMINI says to include the minibuffer as a window.
This is a macro, and does not evaluate its arguments."
  (` (let ((OriginallySelectedWindow (selected-window)))
       (unwind-protect 
	   (while (progn
		    (, form)
		    (not (eq OriginallySelectedWindow
			     (select-window
			      (next-window nil (, yesmini)))))))
	 (select-window OriginallySelectedWindow)))))
(put 'eval-in-window 'lisp-indent-hook 0)

(defun move-to-loc (x y)
  "Move cursor to window location X, Y.
Handles wrapped and horizontally scrolled lines correctly."
  (move-to-window-line y)
  ;; window-line-end expects this to return the window column it moved to.
  (let ((cc (current-column))
	(nc (move-to-column
	     (if (zerop (window-hscroll))
		 (+ (current-column)
		    (min (- (window-width) 2)	; To stay on the line.
			 x))
	       (+ (window-hscroll) -1
		  (min (1- (window-width))	; To stay on the line.
		       x))))))
    (- nc cc)))


(defun minibuffer-window-p (window)
  "True iff this WINDOW is minibuffer."
  (= (screen-height)
     (nth 3 (window-edges window))	; The bottom edge.
     ))


(defun sun-mouse-handler (&optional hit)
  "Evaluates the function or list associated with a mouse hit.
Expecting to read a hit, which is a list: (button x y delta).  
A form bound to button by define-mouse is found by mouse-lookup. 
The variables: *mouse-window*, *mouse-x*, *mouse-y* are bound.  
If the form is a symbol (symbolp), it is funcall'ed with *mouse-window*,
*mouse-x*, and *mouse-y* as arguments; if the form is a list (listp),
the form is eval'ed; if the form is neither of these, it is an error.
Returns nil."
  (interactive)
  (if (null hit) (setq hit (sm::combined-hits)))
  (let ((loc (sm::window-xy (sm::hit-x hit) (sm::hit-y hit))))
    (let ((*mouse-window* (sm::loc-w loc))
	  (*mouse-x* (sm::loc-x loc))
	  (*mouse-y* (sm::loc-y loc))
	  (mouse-code (mouse-event-code hit loc)))
      (let ((form (eval-in-buffer (window-buffer *mouse-window*)
		    (mouse-lookup mouse-code))))
	(cond ((null form)
	       (if (not (sm::hit-up-p hit))	; undefined up hits are ok.
		   (error "Undefined mouse event: %s" 
			  (prin1-to-string 
			   (mouse-code-to-mouse-list mouse-code)))))
	      ((symbolp form)
	       (setq this-command form)
	       (funcall form *mouse-window* *mouse-x* *mouse-y*))
	      ((listp form)
	       (setq this-command (car form))
	       (eval form))
	      (t
	       (error "Mouse action must be symbol or list, but was: %s"
		      form))))))
  ;; Don't let 'sun-mouse-handler get on last-command,
  ;; since this function should be transparent.
  (if (eq this-command 'sun-mouse-handler)
      (setq this-command last-command))
  ;; (message (prin1-to-string this-command))	; to see what your buttons did
  nil)

(defun sm::combined-hits ()
  "Read and return next mouse-hit, include possible double click"
  (let ((hit1 (mouse-hit-read)))
    (if (not (sm::hit-up-p hit1))	; Up hits dont start doubles or chords.
	(let ((hit2 (mouse-second-hit extra-click-wait)))
	  (if hit2	; we cons'd it, we can smash it.
	      ; (setf (sm::hit-code hit1) (logior (sm::hit-code hit1) ...))
	      (setcar hit1 (logior (sm::hit-code hit1) 
				   (sm::hit-code hit2)
				   (if (= (sm::hit-button hit1) 
					  (sm::hit-button hit2))
				       sm::DoubleBits 0))))))
    hit1))

(defun mouse-hit-read ()
  "Read mouse-hit list from keyboard.  Like (read 'read-char),
but that uses minibuffer, and mucks up last-command."
  (let ((char-list nil) (char nil))
    (while (not (equal 13		; Carriage return.
		       (prog1 (setq char (read-char)) 
			 (setq char-list (cons char char-list))))))
    (read (mapconcat 'char-to-string (nreverse char-list) ""))
    ))

;;; Second Click Hackery....
;;; if prefix is not mouse-prefix, need a way to unread the char...
;;; or else have mouse flush input queue, or else need a peek at next char.

;;; There is no peek, but since one character can be unread, we only
;;; have to flush the queue when the command after a mouse click
;;; starts with mouse-prefix1 (see below).
;;;   Something to do later:  We could buffer the read commands and
;;; execute them ourselves after doing the mouse command (using
;;; lookup-key ??).

(defvar mouse-prefix1 24		; C-x
  "First char of mouse-prefix.  Used to detect double clicks and chords.")

(defvar mouse-prefix2 0			; C-@
  "Second char of mouse-prefix.  Used to detect double clicks and chords.")


(defun mouse-second-hit (hit-wait)
  "Returns the next mouse hit occurring within HIT-WAIT milliseconds."
  (if (sit-for-millisecs hit-wait) nil	; No input within hit-wait millisecs.
    (let ((pc1 (read-char)))
      (if (or (not (equal pc1 mouse-prefix1))
	      (sit-for-millisecs 3))	; a mouse prefix will have second char
	  (progn (setq unread-command-char pc1)	; Can get away with one unread.
		 nil)			; Next input not mouse event.
	(let ((pc2 (read-char)))
	  (if (not (equal pc2 mouse-prefix2))
	      (progn (setq unread-command-char pc1) ; put back the ^X
;;; Too bad can't do two: (setq unread-command-char (list pc1 pc2))
		(ding)			; user will have to retype that pc2.
		nil)			; This input is not a mouse event.
	    ;; Next input has mouse prefix and is within time limit.
	    (let ((new-hit (mouse-hit-read))) ; Read the new hit.
		(if (sm::hit-up-p new-hit)	; Ignore up events when timing.
		    (mouse-second-hit (- hit-wait (sm::hit-delta new-hit)))
		  new-hit		; New down hit within limit, return it.
		  ))))))))

(defun sm::window-xy (x y)
  "Find window containing screen coordinates X and Y.
Returns list (window x y) where x and y are relative to window."
  (or
   (catch 'found
     (eval-in-windows 
      (let ((we (window-edges (selected-window))))
	(let ((le (nth 0 we))
	      (te (nth 1 we))
	      (re (nth 2 we))
	      (be (nth 3 we)))
	  (if (= re (screen-width))
	      ;; include the continuation column with this window
	      (setq re (1+ re)))
	  (if (= be (screen-height))
	      ;; include partial line at bottom of screen with this window
	      ;; id est, if window is not multple of char size.
	      (setq be (1+ be)))

	  (if (and (>= x le) (< x re)
		   (>= y te) (< y be))
	      (throw 'found 
		     (list (selected-window) (- x le) (- y te))))))
      t))				; include minibuffer in eval-in-windows
   ;;If x,y from a real mouse click, we shouldn't get here.
   (list nil x y)
   ))

(defun sm::window-region (loc)
  "Parse LOC into a region symbol.
Returns one of (text scrollbar modeline minibuffer)"
  (let ((w (sm::loc-w loc))
	(x (sm::loc-x loc))
	(y (sm::loc-y loc)))
    (let ((right (1- (window-width w)))
	  (bottom (1- (window-height w))))
      (cond ((minibuffer-window-p w) 'minibuffer)
	    ((>= y bottom) 'modeline)
	    ((>= x right) 'scrollbar)
	    ;; far right column (window seperator) is always a scrollbar
	    ((and scrollbar-width
		  ;; mouse within scrollbar-width of edge.
		  (>= x (- right scrollbar-width))
		  ;; mouse a few chars past the end of line.
		  (>= x (+ 2 (window-line-end w x y))))
	     'scrollbar)
	    (t 'text)))))

(defun window-line-end (w x y)
  "Return WINDOW column (ignore X) containing end of line Y"
  (eval-in-window w (save-excursion (move-to-loc (screen-width) y))))

;;;
;;; The encoding of mouse events into a mousemap.
;;; These values must agree with coding in emacstool:
;;;
(defconst sm::keyword-alist 
  '((left . 1) (middle . 2) (right . 4)
    (shift . 8) (control . 16) (meta . 32) (double . 64) (up . 128)
    (text . 256) (scrollbar . 512) (modeline . 1024) (minibuffer . 2048)
    ))

(defun mouse-event-code (hit loc)
  "Maps MOUSE-HIT and LOC into a mouse-code."
;;;Region is a code for one of text, modeline, scrollbar, or minibuffer.
  (logior (sm::hit-code hit)
	  (mouse-region-to-code (sm::window-region loc))))

(defun mouse-region-to-code (region)
  "Returns partial mouse-code for specified REGION."
  (cdr (assq region sm::keyword-alist)))

(defun mouse-list-to-mouse-code (mouse-list)
  "Map a MOUSE-LIST to a mouse-code."
  (apply 'logior
	 (mapcar (function (lambda (x)
			     (cdr (assq x sm::keyword-alist))))
		  mouse-list)))

(defun mouse-code-to-mouse-list (mouse-code)
  "Map a MOUSE-CODE to a mouse-list."
  (apply 'nconc (mapcar
		 (function (lambda (x)
			     (if (logtest mouse-code (cdr x))
				 (list (car x)))))
		 sm::keyword-alist)))

(defun mousemap-set (code mousemap value)
  (let* ((alist (cdr mousemap))
	 (assq-result (assq code alist)))
    (if assq-result
	(setcdr assq-result value)
      (setcdr mousemap (cons (cons code value) alist)))))

(defun mousemap-get (code mousemap)
  (cdr (assq code (cdr mousemap))))

(defun mouse-lookup (mouse-code)
  "Look up MOUSE-EVENT and return the definition. nil means undefined."
  (or (mousemap-get mouse-code current-local-mousemap)
      (mousemap-get mouse-code current-global-mousemap)))

;;;
;;; I (jpeck) don't understand the utility of the next four functions
;;; ask Steven Greenbaum <froud@kestrel>
;;;
(defun mouse-mask-lookup (mask list)
  "Args MASK (a bit mask) and LIST (a list of (code . form) pairs).
Returns a list of elements of LIST whose code or'ed with MASK is non-zero."
  (let ((result nil))
    (while list
      (if (logtest mask (car (car list)))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    result))

(defun mouse-union (l l-unique)
  "Return the union of list of mouse (code . form) pairs L and L-UNIQUE,
where L-UNIQUE is considered to be union'ized already."
  (let ((result l-unique))
    (while l
      (let ((code-form-pair (car l)))
	(if (not (assq (car code-form-pair) result))
	    (setq result (cons code-form-pair result))))
      (setq l (cdr l)))
    result))

(defun mouse-union-first-prefered (l1 l2)
  "Return the union of lists of mouse (code . form) pairs L1 and L2,
based on the code's, with preference going to elements in L1."
  (mouse-union l2 (mouse-union l1 nil)))

(defun mouse-code-function-pairs-of-region (region)
  "Return a list of (code . function) pairs, where each code is
currently set in the REGION."
  (let ((mask (mouse-region-to-code region)))
    (mouse-union-first-prefered
     (mouse-mask-lookup mask (cdr current-local-mousemap))
     (mouse-mask-lookup mask (cdr current-global-mousemap))
     )))

;;;
;;; Functions for DESCRIBE-MOUSE-BINDINGS
;;; And other mouse documentation functions
;;; Still need a good procedure to print out a help sheet in readable format.
;;;

(defun one-line-doc-string (function)
  "Returns first line of documentation string for FUNCTION.
If there is no documentation string, then the string
\"No documentation\" is returned."
  (while (consp function) (setq function (car function)))
  (let ((doc (documentation function)))
    (if (null doc)
	"No documentation."
      (string-match "^.*$" doc)
      (substring doc 0 (match-end 0)))))

(defun print-mouse-format (binding)
  (princ (car binding))
  (princ ": ")
  (mapcar (function
	   (lambda (mouse-list)
	     (princ mouse-list)
	     (princ " ")))
	  (cdr binding))
  (terpri)
  (princ "  ")
  (princ (one-line-doc-string (car binding)))
  (terpri)
  )

(defun print-mouse-bindings (region)
  "Prints mouse-event bindings for REGION."
  (mapcar 'print-mouse-format (sm::event-bindings region)))

(defun sm::event-bindings (region)
  "Returns an alist of (function . (mouse-list1 ... mouse-listN)) for REGION,
where each mouse-list is bound to the function in REGION."
  (let ((mouse-bindings (mouse-code-function-pairs-of-region region))
	(result nil))
    (while mouse-bindings
      (let* ((code-function-pair (car mouse-bindings))
	     (current-entry (assoc (cdr code-function-pair) result)))
	(if current-entry
	    (setcdr current-entry
		    (cons (mouse-code-to-mouse-list (car code-function-pair))
			  (cdr current-entry)))
	  (setq result (cons (cons (cdr code-function-pair)
				   (list (mouse-code-to-mouse-list
					  (car code-function-pair))))
			     result))))
      (setq mouse-bindings (cdr mouse-bindings))
      )
    result))

(defun describe-mouse-bindings ()
  "Lists all current mouse-event bindings."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Text Region") (terpri)
    (princ "---- ------") (terpri)
    (print-mouse-bindings 'text) (terpri)
    (princ "Modeline Region") (terpri)
    (princ "-------- ------") (terpri)
    (print-mouse-bindings 'modeline) (terpri)
    (princ "Scrollbar Region") (terpri)
    (princ "--------- ------") (terpri)
    (print-mouse-bindings 'scrollbar)))

(defun describe-mouse-briefly (mouse-list)
  "Print a short description of the function bound to MOUSE-LIST."
  (interactive "xDescibe mouse list briefly: ")
  (let ((function (mouse-lookup (mouse-list-to-mouse-code mouse-list))))
    (if function
	(message "%s runs the command %s" mouse-list function)
      (message "%s is undefined" mouse-list))))

(defun mouse-help-menu (function-and-binding)
  (cons (prin1-to-string (car function-and-binding))
	(menu-create	; Two sub-menu items of form ("String" . nil)
	 (list (list (one-line-doc-string (car function-and-binding)))
	       (list (prin1-to-string (cdr function-and-binding)))))))

(defun mouse-help-region (w x y &optional region)
  "Displays a menu of mouse functions callable in this region."
  (let* ((region (or region (sm::window-region (list w x y))))
	 (mlist (mapcar (function mouse-help-menu)
			(sm::event-bindings region)))
	 (menu (menu-create (cons (list (symbol-name region)) mlist)))
	 (item (sun-menu-evaluate w 0 y menu))
	 )))

;;;
;;; Menu interface functions
;;;
;;; use defmenu, because this interface is subject to change
;;; really need a menu-p, but we use vectorp and the context...
;;;
(defun menu-create (items)
  "Functional form for defmenu, given a list of ITEMS returns a menu.
Each ITEM is a (STRING . VALUE) pair."
  (apply 'vector items)
  )

(defmacro defmenu (menu &rest itemlist)
  "Defines MENU to be a menu, the ITEMS are (STRING . VALUE) pairs.
See sun-menu-evaluate for interpretation of ITEMS."
  (list 'defconst menu (funcall 'menu-create itemlist))
  )

(defun sun-menu-evaluate (*menu-window* *menu-x* *menu-y* menu)
  "Display a pop-up menu in WINDOW at X Y and evaluate selected item
of MENU.  MENU (or its symbol-value) should be a menu defined by defmenu.
  A menu ITEM is a (STRING . FORM) pair;
the FORM associated with the selected STRING is evaluated,
and the resulting value is returned.  Generally these FORMs are
evaluated for their side-effects rather than their values.
  If the selected form is a menu or a symbol whose value is a menu, 
then it is displayed and evaluated as a pullright menu item.
  If the the FORM of the first ITEM is nil, the STRING of the item
is used as a label for the menu, i.e. it's inverted and not selectible."

  (if (symbolp menu) (setq menu (symbol-value menu)))
  (eval (sun-menu-internal *menu-window* *menu-x* *menu-y* 4 menu)))

(defun sun-get-frame-data (code)
  "Sends the tty-sub-window escape sequence CODE to terminal,
and returns a cons of the two numbers in returned escape sequence.
That is it returns (cons <car> <cdr>) from \"\\E[n;<car>;<cdr>t\". 
CODE values: 13 = Tool-Position, 14 = Size-in-Pixels, 18 = Size-in-Chars."
  (send-string-to-terminal (concat "\033[" (int-to-string code) "t"))
  (let (char str x y)
    (while (not (equal 116 (setq char (read-char)))) ; #\t = 116
      (setq str (cons char str)))
    (setq str (mapconcat 'char-to-string (nreverse str) ""))
    (string-match ";[0-9]*" str)
    (setq y (substring str (1+ (match-beginning 0)) (match-end 0)))
    (setq str (substring str (match-end 0)))
    (string-match ";[0-9]*" str)
    (setq x (substring str (1+ (match-beginning 0)) (match-end 0)))
    (cons (string-to-int y) (string-to-int x))))

(defun sm::font-size ()
  "Returns font size in pixels: (cons Ysize Xsize)"
  (let ((pix (sun-get-frame-data 14))	; returns size in pixels
	(chr (sun-get-frame-data 18)))	; returns size in chars
    (cons (/ (car pix) (car chr)) (/ (cdr pix) (cdr chr)))))

(defvar sm::menu-kludge-x nil 
  "Cached frame-to-window X-Offset for sm::menu-kludge")
(defvar sm::menu-kludge-y nil 
  "Cached frame-to-window Y-Offset for sm::menu-kludge")

(defun sm::menu-kludge ()
  "If sunfns.c uses <Menu_Base_Kludge> this function must be here!"
  (or sm::menu-kludge-y
      (let ((fs (sm::font-size)))
	(setq sm::menu-kludge-y (+ 8 (car fs))	; a title line and borders
	      sm::menu-kludge-x 4)))	; best values depend on .defaults/Menu
  (let ((wl (sun-get-frame-data 13)))		; returns frame location
    (cons (+ (car wl) sm::menu-kludge-y)
	  (+ (cdr wl) sm::menu-kludge-x))))

;;;
;;;  Function interface to selection/region
;;;  primative functions are defined in sunfns.c
;;;
(defun sun-yank-selection ()
  "Set mark and yank the contents of the current sunwindows selection
into the current buffer at point."
  (interactive "*")
  (set-mark-command nil)
  (insert-string (sun-get-selection)))

(defun sun-select-region (beg end)
  "Set the sunwindows selection to the region in the current buffer."
  (interactive "r")
  (sun-set-selection (buffer-substring beg end)))

;;;
;;; Support for emacstool
;;; This closes the window instead of stopping emacs.
;;;
(defun suspend-emacstool (&optional stuffstring)
  "If running under as a detached process emacstool,
you don't want to suspend  (there is no way to resume), 
just close the window, and wait for reopening."
  (interactive)
  (if (and (boundp 'suspend-hook) suspend-hook)
      (funcall suspend-hook))
  (if stuffstring (send-string-to-terminal stuffstring))
  (send-string-to-terminal "\033[2t")	; To close EmacsTool window.
  (if (and (boundp 'suspend-resume-hook) suspend-resume-hook)
      (funcall suspend-resume-hook)))
;;;
;;; initialize mouse maps
;;;

(make-variable-buffer-local 'current-local-mousemap)
(setq-default current-local-mousemap nil)
(defvar current-global-mousemap (make-mousemap))
