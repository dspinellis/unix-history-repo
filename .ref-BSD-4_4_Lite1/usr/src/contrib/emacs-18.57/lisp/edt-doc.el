;; From mike@yetti.UUCP Fri Aug 29 12:49:28 1986
;; Path: mit-prep!mit-hermes!mit-eddie!genrad!panda!husc6!seismo!mnetor!yetti!mike
;; From: mike@yetti.UUCP (Mike Clarkson )
;; Newsgroups: net.sources
;; Subject: Gnu Emacs EDT Emulation - Introduction - 1/3
;; Date: 27 Aug 86 23:30:33 GMT
;; Reply-To: mike@yetti.UUCP (Mike Clarkson )
;; Organization: York University Computer Science
;; 
;; Here's my EDT emulation for GNU Emacs that is based on the EDT emulation
;; for Gosling's Emacs sent out on the net a couple of years ago by Lynn Olson
;; at Tektronics.  This emulation was widely distributed as the file edt.ml
;; in the maclib directory of most Emacs distributions.
;;      
;; My emulation consists of two files: edt.el and edtdoc.el.  The edtdoc.el file
;; is the documentation, that you can add to the beginning of edt.el if you
;; want.  I have split them because I have been loading the edt.el file a lot
;; during debugging.
;;      
;; I will gladly take all criticisms and complaints to heart, and will fix
;; what bugs I can find.   As this is my first elisp hack, you may have to
;; root out a few nasties hidden in the code.  Please let me know if you
;; find any (sorry,
;; no rewards :-).  I would also be interested if there are better,
;; cleaner, faster ways of doing some of the things that I have done.
;;      
;; You must understand some design considerations that I had in mind.
;; The intention was not really to "emulate" EDT, but rather to take advantage
;; of the years of EDT experience that had accumulated in my right hand,
;; while at the same time taking advantage of EMACS.
;;      
;; Some major differences are:
;;      
;; HELP            is describe-key;
;; GOLD/HELP       is describe-function;
;; FIND            is isearch-forward/backward;
;; GOLD/HELP       is occur-menu, which finds all occurrences of a search string;
;; ENTER           is other-window;
;; SUBS            is subprocess-command.  Note that you will have to change this
;;                 yourself to shell if you are running Un*x;
;; PAGE            is next-paragraph, because that's more useful than page.
;; SPECINS         is copy-to-killring;
;; GOLD/GOLD       is mark-section-wisely, which is my command to mark the
;;                 section in a manner consistent with the major-mode.  It
;;                 uses mark-defun for emacs-lisp, lisp, mark-c-function for C,
;;                 and mark-paragraph for other modes.
;;      
;;      
;; Some subtle differences are:
;;      
;; APPEND          is append-to-buffer.  One doesn't append to the kill ring much
;;                 and SPECINS is now copy-to-killring;
;; REPLACE         is replace-regexp;
;; FILL            is fill-region-wisely, which uses indent-region for C, lisp
;;                 emacs-lisp, and fill-region for others. It asks if you really
;;                 want to fill-region in TeX-mode, because I find this to be
;;                 very dangerous.
;; CHNGCASE        is case-flip for the character under the cursor only.
;;                 I felt that case-flip region is unlikely, as usually you
;;                 upcase-region or downcase region.  Also, unlike EDT it
;;                 is independent of the direction you are going, as that
;;                 drives me nuts.
;;      
;; I use Emacs definition of what a word is.  This is considerably different from
;; what EDT thinks a word is.  This is not good for dyed-in-the-wool EDT fans,
;; but is probably preferable for experienced Emacs users.  My assumption is that
;; the former are a dying breed now that GNU Emacs has made it to VMS, but let me
;; know how you feel.  Also, when you undelete a word it leave the point at the
;; end of the undeleted text, rather than the beginning.  I might change this
;; as I'm not sure if I like this or not. I'm also not sure if I want it to
;; set the mark each time you delete a character or word.
;;      
;; Backspace does not invoke beginning-of-line, because ^H is the help prefix,
;; and I felt it should be left as such.  You can change this if you like.
;;      
;; The ADVANCE and BACKUP keys do not work as terminators for forward or
;; backward searches. In Emacs, all search strings are terminated by return.
;; The searches will however go forward or backward depending on your current
;; direction.  Also, when you change directions, the mode line will not be
;; updated immediately, but only when you next execute an emacs function.
;; Personally, I consider this to be a bug, not a feature.
;;      
;; This should also work with VT-2xx's, though I haven't tested it extensively
;; on those terminals.  It assumes that the CSI-map of vt_200.el has been defined.
;;      
;; There are also a whole bunch of GOLD letter, and GOLD character bindings:
;; look at edtdoc.el for them, or better still, look at the edt.el lisp code,
;; because after all, in the true Lisp tradition, the source code is *assumed*
;; to be self-documenting :-)
;;      
;; Mike Clarkson,            ...!allegra \             BITNET:  mike@YUYETTI or
;; CRESS, York University,   ...!decvax   \                 SYMALG@YUSOL
;; 4700 Keele Street,        ...!ihnp4     > !utzoo!yetti!mike
;; North York, Ontario,      ...!linus    /
;; CANADA M3J 1P3.           ...!watmath /      Phone: +1 (416) 736-2100 x 7767
;;      
;; Note that I am not on ARPA, and must gateway any ARPA mail through BITNET or
;; UUCP.  If you have a UUCP or BITNET address please use it for communication
;; so that I can reach you directly.  If you have both, the BITNET address
;; is preferred.
;; -- 
;; Mike Clarkson,		  ...!allegra \		BITNET:	mike@YUYETTI or
;; CRESS, York University,	  ...!decvax   \		SYMALG@YUSOL
;; 4700 Keele Street,	  ...!ihnp4     > !utzoo!yetti!mike
;; North York, Ontario,	  ...!linus    /		     
;; CANADA M3J 1P3.		  ...!watmath /	Phone: +1 (416) 737-2100 x 7767
