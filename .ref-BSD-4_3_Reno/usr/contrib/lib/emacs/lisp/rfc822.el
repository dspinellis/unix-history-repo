;; Hairy rfc822 parser for mail and news and suchlike
;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.
;; Author Richard Mlynarik.

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

(provide 'rfc822)

;; uses address-start free, throws to address
(defun rfc822-bad-address (reason)
  (save-restriction
    (insert "_^_")
    (narrow-to-region address-start
		      (if (re-search-forward "[,;]" nil t)
			  (max (point-min) (1- (point)))
			(point-max)))
    ;; make the error string be suitable for inclusion in (...)
    (let ((losers '("\\" "(" ")" "\n")))
      (while losers
	(goto-char (point-min))
	(while (search-forward (car losers) nil t)
	  (backward-char 1)
	  (insert ?\\)
	  (forward-char 1))
	(setq losers (cdr losers))))
    (goto-char (point-min)) (insert "(Unparsable address -- "
				    reason
				    ":\n\t  \"")
    (goto-char (point-max)) (insert "\")"))
  (rfc822-nuke-whitespace)
  (throw 'address (buffer-substring address-start (point))))

(defun rfc822-nuke-whitespace (&optional leave-space)
  (let (ch)
    (while (cond ((eobp)
		  nil)
		 ((= (setq ch (following-char)) ?\()
		  (forward-char 1)
		  (while (if (eobp)
			     (rfc822-bad-address "Unbalanced comment (...)")
			   (/= (setq ch (following-char)) ?\)))
		    (cond ((looking-at "[^()\\]+")
			   (replace-match ""))
			  ((= ch ?\()
			   (rfc822-nuke-whitespace))
			  ((< (point) (1- (point-max)))
			   (delete-char 2))
			  (t
			   (rfc822-bad-address "orphaned backslash"))))
		  ;; delete remaining "()"
		  (forward-char -1)
		  (delete-char 2)
		  t)
		 ((memq ch '(?\ ?\t ?\n))
		  (delete-region (point)
				 (progn (skip-chars-forward " \t\n") (point)))
		  t)
		 (t
		  nil)))
    (or (not leave-space)
	(eobp)
	(bobp)
	(= (preceding-char) ?\ )
	(insert ?\ ))))

(defun rfc822-looking-at (regex &optional leave-space)
  (if (cond ((stringp regex)
	     (if (looking-at regex)
		 (progn (goto-char (match-end 0))
			t)))
	    (t
	     (if (and (not (eobp))
		      (= (following-char) regex))
		 (progn (forward-char 1)
			t))))
      (let ((tem (match-data)))
	(rfc822-nuke-whitespace leave-space)
	(store-match-data tem)
	t)))

(defun rfc822-snarf-word ()
  ;; word is atom | quoted-string
  (cond ((= (following-char) ?\")
	 ;; quoted-string
	 (or (rfc822-looking-at "\"\\([^\"\\\n]\\|\\\\.\\|\\\\\n\\)*\"")
	     (rfc822-bad-address "Unterminated quoted string")))
	((rfc822-looking-at "[^][\000-\037\177-\377 ()<>@,;:\\\".]+")
	 ;; atom
	 )
	(t
	 (rfc822-bad-address "Rubbish in address"))))

(defun rfc822-snarf-words ()
  (rfc822-snarf-word)
  (while (rfc822-looking-at ?.)
    (rfc822-snarf-word)))

(defun rfc822-snarf-subdomain ()
  ;; sub-domain is domain-ref | domain-literal
  (cond ((= (following-char) ?\[)
	 ;; domain-ref
	 (or (rfc822-looking-at "\\[\\([^][\\\n]\\|\\\\.\\|\\\\\n\\)*\\]")
	     (rfc822-bad-address "Unterminated domain literal [...]")))
	((rfc822-looking-at "[^][\000-\037\177-\377 ()<>@,;:\\\".]+")
	 ;; domain-literal = atom
	 )
	(t
	 (rfc822-bad-address "Rubbish in host/domain specification"))))

(defun rfc822-snarf-domain ()
  (rfc822-snarf-subdomain)
  (while (rfc822-looking-at ?.)
    (rfc822-snarf-subdomain)))

(defun rfc822-snarf-frob-list (name separator terminator snarfer
				    &optional return)
  (let ((first t)
	(list ())
	tem)
    (while (cond ((eobp)
		  (rfc822-bad-address
		    (format "End of addresses in middle of %s" name)))
		 ((rfc822-looking-at terminator)
		  nil)
		 ((rfc822-looking-at separator)
		  ;; multiple separators are allowed and do nothing.
		  (while (rfc822-looking-at separator))
		  t)
		 (first
		  t)
		 (t
		  (rfc822-bad-address
		    (format "Gubbish in middle of %s" name))))
      (setq tem (funcall snarfer)
	    first nil)
      (and return tem
	   (setq list (if (listp tem)
			  (nconc (reverse tem) list)
			  (cons tem list)))))
    (nreverse list)))

;; return either an address (a string) or a list of addresses
(defun rfc822-addresses-1 (&optional allow-groups)
  ;; Looking for an rfc822 `address'
  ;; Either a group (1*word ":" [#mailbox] ";")
  ;; or a mailbox (addr-spec | 1*word route-addr)
  ;;  addr-spec is (local-part "@" domain)
  ;;  route-addr is ("<" [1#("@" domain) ":"] addr-spec ">")
  ;;  local-part is (word *("." word))
  ;;  word is (atom | quoted-string)
  ;;  quoted-string is ("\([^\"\\n]\|\\.\|\\\n\)")
  ;;  atom is [^\000-\037\177 ()<>@,;:\".[]]+
  ;;  domain is sub-domain *("." sub-domain)
  ;;  sub-domain is domain-ref | domain-literal
  ;;  domain-literal is  "[" *(dtext | quoted-pair) "]"
  ;;  dtext is "[^][\\n"
  ;;  domain-ref is atom
  (let ((address-start (point))
	(n 0))
    (catch 'address
      ;; optimize common cases:
      ;;  foo
      ;;  foo.bar@bar.zap
      ;; followed by "\\'\\|,\\|([^()\\]*)\\'"
      ;; other common cases are:
      ;;  foo bar <foo.bar@baz.zap>
      ;;  "foo bar" <foo.bar@baz.zap>
      ;;  those aren't hacked yet.
      (if (and (rfc822-looking-at "[^][\000-\037\177-\377 ()<>@,;:\\\"]+\\(\\|@[^][\000-\037\177-\377 ()<>@,;:\\\"]+\\)" t)
	       (progn (or (eobp)
			  (rfc822-looking-at ?,))))
	  (progn
	    ;; rfc822-looking-at may have inserted a space
	    (or (bobp) (/= (preceding-char) ?\ ) (delete-char -1))
	    ;; relying on the fact that rfc822-looking-at <char>
	    ;;  doesn't mung match-data
	    (throw 'address (buffer-substring address-start (match-end 0)))))
      (goto-char address-start)
      (while t
	(cond ((and (= n 1) (rfc822-looking-at ?@))
	       ;; local-part@domain
	       (rfc822-snarf-domain)
	       (throw 'address
		 (buffer-substring address-start (point))))
	      ((rfc822-looking-at ?:)
	       (cond ((not allow-groups)
		      (rfc822-bad-address "A group name may not appear here"))
		     ((= n 0)
		      (rfc822-bad-address "No name for :...; group")))
	       ;; group
	       (throw 'address
		 ;; return a list of addresses
		 (rfc822-snarf-frob-list ":...; group" ?\, ?\;
					 'rfc822-addresses-1 t)))
	      ((rfc822-looking-at ?<)
	       (let ((start (point))
		     (strip t))
		 (cond ((rfc822-looking-at ?>)
			;; empty path
			())
		       ((and (not (eobp)) (= (following-char) ?\@))
			;; <@foo.bar,@baz:quux@abcd.efg>
			(rfc822-snarf-frob-list "<...> address" ?\, ?\:
			  (function (lambda ()
				      (if (rfc822-looking-at ?\@)
					  (rfc822-snarf-domain)
					(rfc822-bad-address
					  "Gubbish in route-addr")))))
			(rfc822-snarf-words)
			(or (rfc822-looking-at ?@)
			    (rfc822-bad-address "Malformed <..@..> address"))
			(rfc822-snarf-domain)
			(setq strip nil))
		       ((progn (rfc822-snarf-words) (rfc822-looking-at ?@))
			; allow <foo> (losing unix seems to do this)
			(rfc822-snarf-domain)))
		 (let ((end (point)))
		   (if (rfc822-looking-at ?\>)
		       (throw 'address
			 (buffer-substring (if strip start (1- start))
					   (if strip end (1+ end))))
		     (rfc822-bad-address "Unterminated <...> address")))))
	      ((looking-at "[^][\000-\037\177-\377 ()<>@,;:\\.]")
	       ;; this allows "." to be part of the words preceding
	       ;; an addr-spec, since many broken mailers output
	       ;; "Hern K. Herklemeyer III
	       ;;   <yank@megadeath.dod.gods-own-country>"
	       (or (= n 0)
		   (= (preceding-char) ?\ )
		   (insert ?\ ))
	       (rfc822-snarf-words)
	       (setq n (1+ n)))
	      ((= n 0)
	       (throw 'address nil))
	      ((= n 1) ; allow "foo" (losing unix seems to do this)
	       (throw 'address
		 (buffer-substring address-start (point))))
	      ((or (eobp) (looking-at ","))
	       (rfc822-bad-address "Missing comma or route-spec"))
	      (t
	       (rfc822-bad-address "Strange character or missing comma")))))))

			   
(defun rfc822-addresses (header-text)
  (if (string-match "\\`[ \t]*\\([^][\000-\037\177-\377 ()<>@,;:\\\".]+\\)[ \t]*\\'"
                    header-text)
      ;; Make very simple case moderately fast.
      (list (substring header-text (match-beginning 1) (match-end 1)))
    (let ((buf (generate-new-buffer " rfc822")))
      (unwind-protect
	(save-excursion
	  (set-buffer buf)
	  (make-local-variable 'case-fold-search)
	  (setq case-fold-search nil)	;For speed(?)
	  (insert header-text)
	  ;; unfold continuation lines
	  (goto-char (point-min))

	  (while (re-search-forward "\\([^\\]\\(\\\\\\\\\\)*\\)\n[ \t]" nil t)
	    (replace-match "\\1 " t))

	  (goto-char (point-min))
	  (rfc822-nuke-whitespace)
	  (let ((list ())
		tem
		address-start); this is for rfc822-bad-address
	    (while (not (eobp))
	      (setq address-start (point))
	      (setq tem
		    (catch 'address ; this is for rfc822-bad-address
		      (cond ((rfc822-looking-at ?\,)
			     nil)
			    ((looking-at "[][\000-\037\177-\377@;:\\.]")
			     (forward-char)
			     (rfc822-bad-address
			       (format "Strange character \\%c found"
				       (preceding-char))))
			    (t
			     (rfc822-addresses-1 t)))))
	      (cond ((null tem))
		    ((stringp tem)
		     (setq list (cons tem list)))
		    (t
		     (setq list (nconc (nreverse tem) list)))))
	    (nreverse list)))
      (and buf (kill-buffer buf))))))

