;;;;;;;;;;;;;;;;;;;;;;;;;;;; franz.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Franz-dependent PEARL functions, declarations, and initializations
;      that don't use PEARL functions.
;  Functions to make Franz accept UCI Lisp functions are in ucisubset.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Version numbers, major and minor.
(defvar pearlmajorversion 3)
(defvar pearlminorversion 9)
;3.1: Use of lets and other speedups and new slot encoding.
;3.2: Slot encoding applied to speeded-up match.
;3.3: New faster hashing.
;3.4: Type tags added to symbols, instances, definitions and databases.
;3.5: New print functions.
;3.6: Made hooks additive and fixed global variables in failed matches.
;3.7: Minor bug fixes in scopy and hooks.
;3.8: Unification added; minor bug fixes in setv and create.
;3.9: Bug fixes in blocks and freezing; selectq becomes selectq*.

; db:
(declare (*fexpr builddb))
(defvar *pearldb*)
(defvar *pearlinactivedb*)
(defvar db)
(defvar *db1size*)
(defvar *db2size*)

(defvar *availablesizes* '((-1. . 1.) (0. . 1.) (1. . 1.) (2. . 3.)
			   (3. . 7.) (4. . 13.) (5. . 29.) (6. . 61.)
			   (7. . 127.) (8. . 127.) (9. . 127.)
			   (10. . 127.) (11. . 127.)
			   (12. . 127.) (13. . 127.)))
;((
; For UCI Lisp or Franz    (7. . 127.) (8. . 251.) (9. . 509.)
;   with vectors (soon?).  (10. . 1021.) (11. . 2039.)
;			   (12. . 4093.) (13. . 8191.)))
; (setq buildpplst nil)

(defvar *maindb*)
(defvar *db*)
(defvar *activedbnames* nil)

; vars:
(declare (*fexpr varvalue setv *var* *global* global unbind))
(declare (*fexpr block endblock endanyblocks setblock))

; hook:
(defvar *runallslothooks* t)
(defvar *runallbasehooks* t)

(defvar *runputpathhooks* t)
(defvar *runclearpathhooks* t)
(defvar *runaddsetpathhooks* t)
(defvar *rundelsetpathhooks* t)
(defvar *runaddpredpathhooks* t)
(defvar *rundelpredpathhooks* t)
(defvar *rungetpathhooks* t)
(defvar *rungetpredpathhooks* t)
(defvar *rungethookpathhooks* t)
(defvar *runapplypathhooks* t)

(defvar *runmatchhooks* t)
(defvar *runsmergehooks* t)
(defvar *runindividualhooks* t)
(defvar *runexpandedhooks* t)
(defvar *runpatternhooks* t)
(defvar *runnextitemhooks* t)
(defvar *runfetchhooks* t)
(defvar *runinsertdbhooks* t)
(defvar *runremovedbhooks* t)
(defvar *runindbhooks* t)
(defvar *runnextequalhooks* t)
(defvar *runstrequalhooks* t)

; symord and create and scopy (and all):
(defvar *pearlunbound*)
(defvar *equivclass*)
(defvar *invisible*)
(defvar *warn* t)

(defvar *pearlsymbol*)
(defvar *pearldef*)
(defvar *pearlinst*)

(declare (*fexpr pearlbreak symbol ordinal create cr insidecreate))
(defvar nilstruct)
(defvar d:nilstruct)
(defvar i:nilstruct)
(defvar s:nilsym)
(defvar *lastcreated*)
(defvar *toplevelp*)
(defvar *currenttopcreated*)
(defvar *currenttopalists*)
(defvar *currenttopcopy*)
(defvar *currentcreatetype*)
(defvar *ordinalnames* nil)
(defvar *globallist* nil)
; So that unique numbers start at 0.
(defvar *lastsymbolnum* -1)
(defvar *unhashablevalues*)
(defvar *any*conscell*)
(defvar *blockstack* nil)
(defvar *zero-ordinal-value* 0)
(defvar *currentpearlstructure* nil)
(defvar *currentstructure* nil)
(defvar *scopieditems*)

; path:
(defvar *pathtop*)
(defvar *pathlocal*)

; print:
(declare (*fexpr foreach quiet))
(defvar *fullprint* nil)
(defvar *abbrevprint* nil)
(defvar *uniqueprint* nil)
(defvar *uniqueprintlist* nil)
(defvar *streamprintlength* 2)
(defvar *quiet* nil)
(defvar prinlevel)
(setq prinlevel 7)
(defvar printvar)
(defvar pearltraceprintfn)
(defvar pearlshowstackprintfn)
(defvar pearlbreakprintfn)
(defvar pearlfixprintfn)
(defvar msgprintfn)
(defvar pearltracebreakprintfn)
(defvar pearlprintfn)
(defvar dskprintfn)
(defvar trace-printer)
(setq trace-printer 'pearltraceprintfn)
(defvar showstack-printer)
(setq showstack-printer 'pearlshowstackprintfn)
(defvar top-level-print)
(setq top-level-print 'pearltracebreakprintfn)

; if t, then enters and exits to tracing are quiet,
;   but info is still kept so (tracedump) will work
(defvar \$tracemute)

; hash:
(defvar *stream*)
(defvar *stream:*)
(defvar *function-stream:*)
(defvar *slotvalues* (makhunk 64))
(defvar *hashingmarks* (makhunk 64))
; (and via lowlevel.l):
(defvar *multiproducts* '(16. 256. 4096. 65536. 1048576. 16777216.
			      268435456. 42944967296.))

; match:
(defvar *matchunboundsresult* nil)
(defvar *globalsavestack* nil)
(defvar *equivsavestack* nil)
(defvar *unifyunbounds* nil)
(defvar xvar)
(defvar yvar)

; history:
(defvar *historynumber* -1.)
(defvar *historysize* 64.)
(defvar *usealiases* t)
(defvar *history* (makhunk *historysize*))
(defvar *histval* (makhunk *historysize*))
(defvar *printhistorynumber* nil)
(defvar *readlinechanged*)

; PEARL-top-level:
(defvar *firststartup* t)
(defvar *pearlprompt* '|pearl> |)
(declare (*fexpr savepearl))

; Franz: PEARL-top-level:
(defvar pearl-title (concat " plus PEARL "
			    pearlmajorversion "."
			    pearlminorversion))
(defvar franz-not-virgin)
(defvar pearl-top-level-init)
(defvar top-level)
(defvar franz-minor-version-number)
(defvar franz-top-level)
(defvar +)
(defvar ++)
(defvar +++)
(defvar *)
(defvar **)
(defvar ***)
(defvar ER%tpl)
(defvar ER%brk)
(defvar ER%err)
(defvar evalhook)
(defvar \$gcprint)
(defvar funcallhook)
(defvar tpl-errlist)
(defvar user-top-level)
(defvar top-level-eof)

; PEARL-break-err-handler or trace or fixit debugger:
(defvar break-level-count)
(defvar debug-level-count)
(defvar errlist)

; (funl (x...) body) expands to (function (lambda (x...) body)).
(defmacro funl (&rest rest)
  `(function (lambda .,rest)))

; Various Lisps store functions different ways.  Check for
;    lambda-ness (expr-ness).
(de islambda (fcn)
  (and (neq 'binary (type fcn))
       (setq fcn (getd fcn)))
  (or (and (eq 'binary (type fcn))
	   (eq 'lambda (getdisc fcn)))
      (and (dtpr fcn)
	   (eq 'lambda (car fcn)))))

; Tests for an actual literal atom rather than nil!!
(defmacro reallitatom (potatom)
  `(let ((pot ,potatom))
	(and (symbolp pot)
	     pot)))
 
; To avoid problems with UCI Lisp's unbound, we use a special value
;   for PEARL (pattern-matching) variables to indicate unboundness.
(dm punbound (none)
  ''*pearlunbound*)

(defmacro pboundp (a)
  `(neq ,a (punbound)))

(defmacro punboundatomp (yyy)
  `(eq ,yyy (punbound)))

;(aliasdef 'To 'From 'Property) means define To to be the same as From
;   (under Property in UCILisp).  HOWEVER, in Franz it means copy the
;   function definition of To to From and ignore Property.
(defmacro aliasdef (to from property)
  `(putd ,to (getd ,from)))

; vi: set lisp:
