;;Additions to shell mode for use with kermit, etc.
;;Feb 1988, Jeff Norden - jeff@colgate.csnet
;; Copyright (C) 1988 Free Software Foundation, Inc.

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

(require 'shell)

;; I'm not sure, but I think somebody asked about running kermit under shell
;; mode a while ago.  Anyway, here is some code that I find useful.  The result
;; is that I can log onto machines with primitive operating systems (VMS and
;; ATT system V :-), and still have the features of shell-mode available for
;; command history, etc.  It's also handy to be able to run a file transfer in
;; an emacs window.  The transfer is in the "background", but you can also
;; monitor or stop it easily.

;; The ^\ key is bound to a function for sending escape sequences to kermit,
;; and ^C^Q can be used to send any control characters needed thru to the
;; system you connect to.  A more serious problem is that some brain-dead
;; systems will not recognize a ^J as an end-of-line character.  So LFD is
;; bound to a new function which acts just like CR usually does in shell-mode,
;; but a ^M is sent as an end-of-line.  Funcions are also provied to swap the
;; bindings of CR and LFD.  I've also included a filter which will clean out
;; any ^M's or ^@'s that get typed at you, but I don't really recommend it.
;; There doesn't seem to be an acceptably fast way to do this via emacs-lisp.
;; Invoking kermit by the command " kermit | tr -d '\015' " seems to work
;; better (on my system anyway).

;; Here's how I've been using this setup.  We have several machines connected
;; thru a fairly stupid terminal switch.  If I want to connect to unix system,
;; then I use the LFD key to talk to the switch, and ignore any ^M's in the
;; buffer, and do a " stty -echo nl " after I log in.  Then the only real
;; differnce from being in local shell-mode is that it is you need to to type
;; ^C^Q^C to send an interrupt, and ^C^Q^Z for a stop signal, etc.  (since ^C^C
;; just generates a local stop signal, which kermit ignores).
;; To connect to a VMS system, I use a shell script to invoke kermit thru the
;; tr filter, do "M-X kermit-send-cr", and then tell VMS that I'm on a half-duplex
;; terminal.

;; Some caveats:
;; 1) Kermit under shell mode is a real pain if you don't have pty's.  I
;; recently discovered this on our 3b2/400.  When kermit can't find a tty, it
;; assumes it is supposed to be in remote mode.  So the simple command "kermit"
;; won't work in shell mode on such a system.  You can get around this by using
;; the -c (connect) command line option, which means you also have to specify a
;; line and baud on the command line, as in "kermit -l /dev/tty53 -b 9600 -c".
;; However, this will cause kermit to exit when the connection is closed.  So
;; in order to do a file transfer, you have to think ahead and and add -r
;; (receive) to the command line.  This means that you can't use the server
;; feature.  The only fix I can see is to muck around with the source code for
;; kermit, although this problably wouldn't be too hard.  What is needed is an
;; option to force kermit to be local, to use stdin and stdout for interactive
;; speech, and to forget about cbreak mode.

;; 2) The "clean-filter" can be a troublesome item.  The main problem arises if
;; you are running a program under shell-mode which is doing periodic output,
;; and you then try to switch to another buffer.  I came across this while
;; running kermit file transfers - kermit prints a dot each time a packet is
;; received. Since emacs is interrupted each time a dot is printed, it becomes
;; impossible to edit the other buffer.  If you hit a key while the filter code
;; is running, that character will wind up in the *shell* buffer instead of the
;; current one!  So you need to be careful to turn the filter off before
;; leaving the buffer if a program is still running.  In fact, you can't even
;; use "M-x clean-shell-off" to do this, because you won't be able to type
;; "clean-shell-off" in the minibuffer!!  So you need to have this command
;; bound to a keystroke.

;; Please let me know if any bugs turn up.
;; Feb 1988, Jeff Norden - jeff@colgate.csnet

(defvar kermit-esc-char "\C-\\" "*Kermit's escape char")

(defun kermit-esc ()
  "For sending escape sequences to a kermit running in shell mode."
  (interactive)
  (process-send-string 
   (get-buffer-process (current-buffer))
   (concat kermit-esc-char (char-to-string (read-char)))))

(defun kermit-send-char ()
  "Send an arbitrary character to a program in shell mode."
  (interactive)
  (process-send-string 
   (get-buffer-process (current-buffer))
   (char-to-string (read-char))))

(define-key shell-mode-map "\C-\\" 'kermit-esc)
(define-key shell-mode-map "\C-c\C-q" 'kermit-send-char)
;; extra bindings for folks suffering form ^S/^Q braindamage:
(define-key shell-mode-map "\C-c\\" 'kermit-esc)

(defun shell-send-input-cr ()
  "Like \\[shell-send-input] but end the line with carriage-return."
  (interactive)
  (end-of-line)
    (if (eobp)
	(progn
	  (move-marker last-input-start
	       (process-mark (get-buffer-process (current-buffer))))
	  (insert ?\n)
	  (move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward shell-prompt-pattern nil t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (move-marker last-input-end (point))))
    (condition-case ()
	(save-excursion
	  (goto-char last-input-start)
	  (shell-set-directory))
      (error (funcall shell-set-directory-error-hook)))
  (let ((process (get-buffer-process (current-buffer))))
    (process-send-region process last-input-start (- last-input-end 1))
    (process-send-string process "\r")
    (set-marker (process-mark process) (point))))

;; This is backwards of what makes sense, but ...
(define-key shell-mode-map "\n" 'shell-send-input-cr)

(defun kermit-default-cr ()
  "Make RETURN end the line with carriage-return and LFD end it with a newline.
This is useful for talking to other systems on which carriage-return
is the normal way to end a line."
  (interactive)
  (define-key shell-mode-map "\r" 'shell-send-input-cr)
  (define-key shell-mode-map "\n" 'shell-send-input))

(defun kermit-default-nl ()
  "Make RETURN end the line with a newline char.  This is the default state.
In this state, use LFD to send a line and end it with a carriage-return."
  (interactive)
  (define-key shell-mode-map "\n" 'shell-send-input-cr)
  (define-key shell-mode-map "\r" 'shell-send-input))

;; This filter works, but I don't especially recommend it.
(defun kermit-clean-filter (process string)
  "A process filter which deletes all ^M's and ^@'s from the output."
  (set-buffer (process-buffer process))
  (let 
      ((firstpos (string-match "[^\C-@\r]+" string))
       (buffermark (process-mark process))
       (oldpt (point))
       (newstring '"")
       goback)
    (while firstpos
      (setq newstring 
	    (concat newstring (substring string firstpos (match-end 0))))
      (setq firstpos (string-match "[^\C-@\r]+" string (match-end 0))))
    (goto-char (marker-position buffermark))
    (setq goback (< oldpt (point)))
    (insert newstring)
    (set-marker buffermark (point))
    (if goback (goto-char oldpt))))

(defun kermit-clean-on ()
  "Delete all null characters and ^M's from the kermit output.
Note that another (perhaps better) way to do this is to use the
command `kermit | tr -d '\\015''."
  (interactive)
  (set-process-filter (get-buffer-process (current-buffer))
		      'kermit-clean-filter))

(defun kermit-clean-off ()
  "Cancel a previous kermit-clean-shell-on command"
  (interactive)
  (set-process-filter (get-buffer-process (current-buffer)) nil))


