;; Psychological help for frustrated users.
;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

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


(defun cadr (x) (car (cdr x)))
(defun caddr (x) (car (cdr (cdr x))))
(defun cddr (x) (cdr (cdr x)))

(defun member (x y)
  "Like memq, but uses  equal  for comparison"
  (while (and y (not (equal x (car y))))
    (setq y (cdr y)))
  y)

(defun random-range (top)
  "Return a random nonnegative integer less than TOP."
  (let ((tem (% (random) top)))
    (if (< tem 0) (- tem) tem)))

(defun // (x) x)

(defmacro $ (what)
  "quoted arg form of doctor-$"
  (list 'doctor-$ (list 'quote what)))

(defun doctor-$ (what)
  "Return the car of a list, rotating the list each time"
  (let* ((vv (symbol-value what))
	(first (car vv))
	(ww (append (cdr vv) (list first))))
    (set what ww)
    first))

(defvar doctor-mode-map nil)
(if doctor-mode-map
    nil
  (setq doctor-mode-map (make-sparse-keymap))
  (define-key doctor-mode-map "\n" 'doctor-read-print)
  (define-key doctor-mode-map "\r" 'doctor-ret-or-read))

(defun doctor-mode ()
  "Major mode for running the Doctor (Eliza) program.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the Doctor's answer."
  (interactive)
  (text-mode)
  (make-doctor-variables)
  (use-local-map doctor-mode-map)
  (setq major-mode 'doctor-mode)
  (setq mode-name "Doctor")
  (turn-on-auto-fill)
  (doctor-type '(i am the psychotherapist \.
		 ($ please) ($ describe) your ($ problems) \.
		 each time you are finished talking, type \R\E\T twice \.))
  (insert "\n"))

(defun make-doctor-variables ()
  (make-local-variable 'monosyllables)
  (setq monosyllables
	"
     Your attitude at the end of the session was wholly unacceptable.
     Please try to come back next time with a willingness to speak more
     freely. If you continue to refuse to talk openly, there is little
     I can do to help!
")
  (make-local-variable 'typos)
  (setq typos
	(mapcar (function (lambda (x)
			    (put (car x) 'doctor-correction  (cadr x))
			    (put (cadr x) 'doctor-expansion (caddr x))
			    (car x)))
		'((theyll they\'ll (they will))
		  (theyre they\'re (they are))
		  (hes he\'s (he is))
		  (he7s he\'s (he is))
		  (im i\'m (you are))
		  (i7m i\'m (you are))
		  (isa is\ a (is a))
		  (thier their (their))
		  (dont don\'t (do not))
		  (don7t don\'t (do not))
		  (you7re you\'re (i am))
		  (you7ve you\'ve (i have))
		  (you7ll you\'ll (i will)))))
  (make-local-variable 'found)
  (setq found nil)
  (make-local-variable 'owner)
  (setq owner nil)
  (make-local-variable 'history)
  (setq history nil)
  (make-local-variable '*debug*)
  (setq *debug* nil)
  (make-local-variable 'inter)
  (setq inter
	'((well\,)
	  (hmmm \.\.\.\ so\,)
	  (so)
	  (\.\.\.and)
	  (then)))
  (make-local-variable 'continue)
  (setq continue
	'((continue)
	  (proceed)
	  (go on)
	  (keep going) ))
  (make-local-variable 'relation)
  (setq relation
	'((your relationship with)
	  (something you remember about)
	  (your feelings toward)
	  (some experiences you have had with)
	  (how you feel about)))
  (make-local-variable 'fears)
  (setq fears '( (($ whysay) you are ($ afraidof) (// feared) \?)
		 (you seem terrified by (// feared) \.)
		 (when did you first feel ($ afraidof) (// feared) \?) ))
  (make-local-variable 'sure)
  (setq sure '((sure)(positive)(certain)(absolutely sure)))
  (make-local-variable 'afraidof)
  (setq afraidof '( (afraid of) (frightened by) (scared of) ))
  (make-local-variable 'areyou)
  (setq areyou '( (are you)(have you been)(have you been) ))
  (make-local-variable 'isrelated)
  (setq isrelated '( (has something to do with)(is related to)
		     (could be the reason for) (is caused by)(is because of)))
  (make-local-variable 'arerelated)
  (setq arerelated '((have something to do with)(are related to)
		     (could have caused)(could be the reason for) (are caused by)
		     (are because of)))
  (make-local-variable 'moods)
  (setq moods '( (($ areyou)(// found) often \?)
		 (what causes you to be (// found) \?)
		 (($ whysay) you are (// found) \?) ))
  (make-local-variable 'maybe)
  (setq maybe
	'((maybe)
	  (perhaps)
	  (possibly)))
  (make-local-variable 'whatwhen)
  (setq whatwhen
	'((what happened when)
	  (what would happen if)))
  (make-local-variable 'hello)
  (setq hello
	'((how do you do \?) (hello \.) (howdy!) (hello \.) (hi \.) (hi there \.)))
  (make-local-variable 'drnk)
  (setq drnk
	'((do you drink a lot of (// found) \?)
	  (do you get drunk often \?)
	  (($ describe) your drinking habits \.) )) 
  (make-local-variable 'drugs)
  (setq drugs '( (do you use (// found) often \?)(($ areyou)
						 addicted to (// found) \?)(do you realize that drugs can
						 be very harmful \?)(($ maybe) you should try to quit using (// found)
						 \.)))
  (make-local-variable 'whywant)
  (setq whywant '( (($ whysay) (// subj) might ($ want) (// obj) \?)
		   (how does it feel to want \?)
		   (why should (// subj) get (// obj) \?)
		   (when did (// subj) first ($ want) (// obj) \?)
		   (($ areyou) obsessed with (// obj) \?)
		   (why should i give (// obj) to (// subj) \?)
		   (have you ever gotten (// obj) \?) ))
  (make-local-variable 'canyou)
  (setq canyou '((of course i can \.)
		 (why should i \?)
		 (what makes you think i would even want to \?)
		 (i am the doctor\, i can do anything i damn please \.)
		 (not really\, it\'s not up to me \.)
		 (depends\, how important is it \?)
		 (i could\, but i don\'t think it would be a wise thing to do \.)
		 (can you \?)
		 (maybe i can\, maybe i can\'t \.\.\.)
		 (i don\'t think i should do that \.)))
  (make-local-variable 'want)
  (setq want '( (want) (desire) (wish) (want) (hope) ))
  (make-local-variable 'shortlst)
  (setq shortlst
	'((can you elaborate on that \?)
	  (($ please) continue \.)
	  (go on\, don\'t be afraid \.)
	  (i need a little more detail please \.)
	  (you\'re being a bit brief\, ($ please) go into detail \.)
	  (can you be more explicit \?)
	  (and \?)
	  (($ please) go into more detail \?)
	  (you aren\'t being very talkative today\!)
	  (is that all there is to it \?)
	  (why must you respond so briefly \?)))

  (make-local-variable 'famlst)
  (setq famlst
	'((tell me ($ something) about (// owner) family \.)
	  (you seem to dwell on (// owner) family \.)
	  (($ areyou) hung up on (// owner) family \?)))
  (make-local-variable 'huhlst)
  (setq huhlst
	'((($ whysay)(// sent) \?)
	  (is it because of ($ things) that you say (// sent) \?) ))
  (make-local-variable 'longhuhlst)
  (setq longhuhlst
	'((($ whysay) that \?)
	  (i don\'t understand \.)
	  (($ thlst))
	  (($ areyou) ($ afraidof) that \?)))
  (make-local-variable 'feelings)
  (setq feelings-about
	'((feelings about)
	  (aprehensions toward)
	  (thoughts on)
	  (emotions toward)))
  (make-local-variable 'random)
  (setq random-adjective
	'((vivid)
	  (emotionally stimulating)
	  (exciting)
	  (boring)
	  (interesting)
	  (recent)
	  (random)   ;How can we omit this?
	  (unusual)
	  (shocking)
	  (embarrassing)))
  (make-local-variable 'whysay)
  (setq whysay
	'((why do you say)
	  (what makes you believe)
	  (are you sure that)
	  (do you really think)
	  (what makes you think) ))
  (make-local-variable 'isee)
  (setq isee
	'((i see \.\.\.)
	  (yes\,)
	  (i understand \.)
	  (oh \.) ))
  (make-local-variable 'please)
  (setq please 
	'((please\,)
	  (i would appreciate it if you would)
	  (perhaps you could)
	  (please\,)
	  (would you please)
	  (why don\'t you)
	  (could you)))
  (make-local-variable 'bye)
  (setq bye
	'((my secretary will send you a bill \.)
	  (bye bye \.)
	  (see ya \.)
	  (ok\, talk to you some other time \.)
	  (talk to you later \.)
	  (ok\, have fun \.)
	  (ciao \.)))
  (make-local-variable 'something)
  (setq something
	'((something)
	  (more)
	  (how you feel)))
  (make-local-variable 'things)
  (setq things 
	'(;(your interests in computers)   ;; let's make this less computer oriented
	  ;(the machines you use)
	  (your plans)
	  ;(your use of computers)
	  (your life)
	  ;(other machines you use)
	  (the people you hang around with)
	  ;(computers you like)
	  (problems at school)
	  (any hobbies you have)
	  ;(other computers you use)
	  (your sex life)
	  (hangups you have)
	  (your inhibitions)
	  (some problems in your childhood)
	  ;(knowledge of computers)
	  (some problems at home)))
  (make-local-variable 'describe)
  (setq describe
	'((describe)
	  (tell me about)
	  (talk about)
	  (discuss)
	  (tell me more about)
	  (elaborate on)))
  (make-local-variable 'ibelieve)
  (setq ibelieve 
	'((i believe) (i think) (i have a feeling) (it seems to me that)
	  (it looks like)))
  (make-local-variable 'problems)
  (setq problems '( (problems)
		    (inhibitions)
		    (hangups)
		    (difficulties)
		    (anxieties)
		    (frustrations) ))
  (make-local-variable 'bother)
  (setq bother
	'((does it bother you that)
	  (are you annoyed that)
	  (did you ever regret)
	  (are you sorry)
	  (are you satisfied with the fact that)))
  (make-local-variable 'machlst)
  (setq machlst 
	'((you have your mind on (// found) \, it seems \.)
	  (you think too much about  (// found) \.)
	  (you should try taking your mind off of (// found)\.)
	  (are you a computer hacker \?)))
  (make-local-variable 'qlist)
  (setq qlist
	'((what do you think \?)
	  (i\'ll ask the questions\, if you don\'t mind!)
	  (i could ask the same thing myself \.)
	  (($ please) allow me to do the questioning \.)
	  (i have asked myself that question many times \.)
	  (($ please) try to answer that question yourself \.)))
  (make-local-variable 'elist)
  (setq elist
	'((($ please) try to calm yourself \.)
	  (you seem very excited \. relax \. ($ please) ($ describe) ($ things)
	       \.)
	  (you\'re being very emotional \. calm down \.)))
  (make-local-variable 'foullst)
  (setq foullst
	'((($ please) watch your tongue!)
	  (($ please) avoid such unwholesome thoughts \.)
	  (($ please) get your mind out of the gutter \.)
	  (such lewdness is not appreciated \.)))
  (make-local-variable 'deathlst)
  (setq deathlst
	'((this is not a healthy way of thinking \.)
	  (($ bother) you\, too\, may die someday \?)
	  (i am worried by your obssession with this topic!)
	  (did you watch a lot of crime and violence on television as a child \?))
	)
  (make-local-variable 'sexlst)
  (setq sexlst 
	'((($ areyou) ($ afraidof) sex \?)
	  (($ describe)($ something) about your sexual history \.)
	  (($ please)($ describe) your sex life \.\.\.)
	  (($ describe) your ($ feelings-about) your sexual partner \.)
	  (($ describe) your most ($ random-adjective) sexual experience \.)
	  (($ areyou) satisfied with (// lover) \.\.\. \?)))
  (make-local-variable 'neglst)
  (setq neglst
	'((why not \?)
	  (($ bother) i ask that \?)
	  (why not \?)
	  (why not \?)
	  (how come \?)
	  (($ bother) i ask that \?)))
  (make-local-variable 'beclst)
  (setq beclst '(
		 (is it because (// sent) that you came to me \?)
		 (($ bother)(// sent) \?)
		 (when did you first know that (// sent) \?)
		 (is the fact that (// sent) the real reason \?)
		 (does the fact that (// sent) explain anything else \?)
		 (($ areyou)($ sure)(// sent) \? ) ))
  (make-local-variable 'shortbeclst)
  (setq shortbeclst '(
		      (($ bother) i ask you that \?)
		      (that\'s not much of an answer!)
		      (($ inter) why won\'t you talk about it \?)
		      (speak up!)
		      (($ areyou) ($ afraidof) talking about it \?)
		      (don\'t be ($ afraidof) elaborating \.)
		      (($ please) go into more detail \.)))
  (make-local-variable 'thlst)
  (setq thlst '(
		(($ maybe)($ things)($ arerelated) this \.)
		(is it because of ($ things) that you are going through all this \?)
		(how do you reconcile ($ things) \? )
		(($ maybe) this ($ isrelated)($ things) \?) ))
  (make-local-variable 'remlst)
  (setq remlst '( (earlier you said ($ history) \?)
		  (you mentioned that ($ history) \?)
		  (($ whysay)($ history) \? ) ))
  (make-local-variable 'toklst)
  (setq toklst
	'((is this how you relax \?)
	  (how long have you been smoking	grass \?)
	  (($ areyou) ($ afraidof) of being drawn to using harder stuff \?)))
  (make-local-variable 'states)
  (setq states
	'((do you get (// found) often \?)
	  (do you enjoy being (// found) \?)
	  (what makes you (// found) \?)
	  (how often ($ areyou)(// found) \?)
	  (when were you last (// found) \?)))
  (make-local-variable 'replist)
  (setq replist 
	'((i . (you))
	  (my . (your))
	  (me . (you))
	  (you . (me))
	  (your . (my))
	  (mine . (yours))
	  (yours . (mine))
	  (our . (your))
	  (ours . (yours))
	  (we . (you))
	  (dunno . (do not know))
;;	  (yes . ())
	  (no\, . ())
	  (yes\, . ())
	  (ya . (i))
	  (aint . (am not))
	  (wanna . (want to))
	  (gimme . (give me))
	  (gotta . (have to))
	  (gonna . (going to))
	  (never . (not ever))
	  (doesn\'t . (does not))
	  (don\'t . (do not))
	  (aren\'t . (are not))
	  (isn\'t . (is not))
	  (won\'t . (will not))
	  (can\'t . (cannot))
	  (haven\'t . (have not))
	  (i\'m . (you are))
	  (ourselves . (yourselves))
	  (myself . (yourself))
	  (yourself . (myself))
	  (you\'re . (i am))
	  (you\'ve . (i have))
	  (i\'ve . (you have))
	  (i\'ll . (you will))
	  (you\'ll . (i shall))
	  (i\'d . (you would))
	  (you\'d . (i would))
	  (here . (there))
	  (please . ())
	  (eh\, . ())
	  (eh . ())
	  (oh\, . ())
	  (oh . ())
	  (shouldn\'t . (should not))
	  (wouldn\'t . (would not))
	  (won\'t . (will not))
	  (hasn\'t . (has not))))
  (make-local-variable 'stallmanlst)
  (setq stallmanlst '(
		      (($ describe) your ($ feelings-about) him \.)
		      (($ areyou) a friend of Stallman \?)
		      (($ bother) Stallman is ($ random-adjective) \?)
		      (($ ibelieve) you are ($ afraidof) him \.)))
  (make-local-variable 'schoollst)
  (setq schoollst '(
		    (($ describe) your (// found) \.)
		    (($ bother) your grades could ($ improve) \?)
		    (($ areyou) ($ afraidof) (// found) \?)
		    (($ maybe) this ($ isrelated) to your attitude \.)
		    (($ areyou) absent often \?)
		    (($ maybe) you should study ($ something) \.)))
  (make-local-variable 'improve)
  (setq improve '((improve) (be better) (be improved) (be higher)))
  (make-local-variable 'elizalst)
  (setq elizalst '(
		   (($ areyou) ($ sure) \?)
		   (($ ibelieve) you have ($ problems) with (// found) \.)
		   (($ whysay) (// sent) \?)))
  (make-local-variable 'sportslst)
  (setq sportslst '(
		    (tell me ($ something) about (// found) \.)
		    (($ describe) ($ relation) (// found) \.)
		    (do you find (// found) ($ random-adjective) \?)))
  (make-local-variable 'mathlst)
  (setq mathlst '(
		  (($ describe) ($ something) about math \.)
		  (($ maybe) your ($ problems) ($ arerelated) (// found) \.)
		  (i do\'nt know much (// found) \, but ($ continue)
		     anyway \.)))
  (make-local-variable 'zippylst)
  (setq zippylst '(
		   (($ areyou) Zippy \?)
		   (($ ibelieve) you have some serious ($ problems) \.)
		   (($ bother) you are a pinhead \?)))
  (make-local-variable 'chatlst)
  (setq chatlst '(
		  (($ maybe) we could chat \.)
		  (($ please) ($ describe) ($ something) about chat mode \.)
		  (($ bother) our discussion is so ($ random-adjective) \?)))
  (make-local-variable 'abuselst)
  (setq abuselst '(
		   (($ please) try to be less abusive \.)
		   (($ describe) why you call me (// found) \.)
		   (i\'ve had enough of you!)))
  (make-local-variable 'abusewords)
  (setq abusewords '(boring bozo clown clumsy cretin dumb dummy
			    fool foolish gnerd gnurd idiot jerk
			    lose loser louse lousy luse luser
			    moron nerd nurd oaf oafish reek
			    stink stupid tool toolish twit))
  (make-local-variable 'howareyoulst)
  (setq howareyoulst  '((how are you) (hows it going) (hows it going eh)
			(how\'s it going) (how\'s it going eh) (how goes it)
			(whats up) (whats new) (what\'s up) (what\'s new)
			(howre you) (how\'re you) (how\'s everything)
			(how is everything) (how do you do)
			(how\'s it hanging) (que pasa)
			(how are you doing) (what do you say)))
  (make-local-variable 'whereoutp)
  (setq whereoutp '( huh remem rthing ) )
  (make-local-variable 'subj)
  (setq subj nil)
  (make-local-variable 'verb)
  (setq verb nil)
  (make-local-variable 'obj)
  (setq obj nil)
  (make-local-variable 'feared)
  (setq feared nil)
  (make-local-variable 'observation-list)
  (setq observation-list nil)
  (make-local-variable 'repetitive-shortness)
  (setq repetitive-shortness '(0 . 0))
  (make-local-variable '**mad**)
  (setq **mad** nil)
  (make-local-variable 'rms-flag)
  (setq rms-flag nil)
  (make-local-variable 'eliza-flag)
  (setq eliza-flag nil)
  (make-local-variable 'zippy-flag)
  (setq zippy-flag nil)
  (make-local-variable 'lover)
  (setq lover '(your partner))
  (make-local-variable 'bak)
  (setq bak nil)
  (make-local-variable 'lincount)
  (setq lincount 0)
  (make-local-variable '*print-upcase*)
  (setq *print-upcase* nil)
  (make-local-variable '*print-space*)
  (setq *print-space* nil)
  (make-local-variable 'howdyflag)
  (setq howdyflag nil)
  (make-local-variable 'object)
  (setq object nil))

;; Define equivalence classes of words that get treated alike.

(defun doctor-meaning (x) (get x 'doctor-meaning))

(defmacro doctor-put-meaning (symb val)
    "Store the base meaning of a word on the property list"
    (list 'put (list 'quote symb) ''doctor-meaning val))

(doctor-put-meaning howdy 'howdy)
(doctor-put-meaning hi 'howdy)
(doctor-put-meaning greetings 'howdy)
(doctor-put-meaning hello 'howdy)
(doctor-put-meaning tops20 'mach)
(doctor-put-meaning tops-20 'mach)
(doctor-put-meaning tops 'mach)
(doctor-put-meaning pdp11 'mach)
(doctor-put-meaning computer 'mach)
(doctor-put-meaning unix 'mach)
(doctor-put-meaning machine 'mach)
(doctor-put-meaning computers 'mach)
(doctor-put-meaning machines 'mach)
(doctor-put-meaning pdp11s 'mach)
(doctor-put-meaning foo 'mach)
(doctor-put-meaning foobar 'mach)
(doctor-put-meaning multics 'mach)
(doctor-put-meaning macsyma 'mach)
(doctor-put-meaning teletype 'mach)
(doctor-put-meaning la36 'mach)
(doctor-put-meaning vt52 'mach)
(doctor-put-meaning zork 'mach)
(doctor-put-meaning trek 'mach)
(doctor-put-meaning startrek 'mach)
(doctor-put-meaning advent 'mach)
(doctor-put-meaning pdp 'mach)
(doctor-put-meaning dec 'mach)
(doctor-put-meaning commodore 'mach)
(doctor-put-meaning vic 'mach)
(doctor-put-meaning bbs 'mach)
(doctor-put-meaning modem 'mach)
(doctor-put-meaning baud 'mach)
(doctor-put-meaning macintosh 'mach)
(doctor-put-meaning vax 'mach)
(doctor-put-meaning vms 'mach)
(doctor-put-meaning ibm 'mach)
(doctor-put-meaning pc 'mach)
(doctor-put-meaning bitching 'foul)
(doctor-put-meaning shit 'foul)
(doctor-put-meaning bastard 'foul)
(doctor-put-meaning damn 'foul)
(doctor-put-meaning damned 'foul)
(doctor-put-meaning hell 'foul)
(doctor-put-meaning suck 'foul)
(doctor-put-meaning sucking 'foul)
(doctor-put-meaning sux 'foul)
(doctor-put-meaning ass 'foul)
(doctor-put-meaning whore 'foul)
(doctor-put-meaning bitch 'foul)
(doctor-put-meaning asshole 'foul)
(doctor-put-meaning shrink 'foul)
(doctor-put-meaning pot 'toke)
(doctor-put-meaning grass 'toke)
(doctor-put-meaning weed 'toke)
(doctor-put-meaning marijuana 'toke)
(doctor-put-meaning acapulco 'toke)
(doctor-put-meaning columbian 'toke)
(doctor-put-meaning tokin 'toke)
(doctor-put-meaning joint 'toke)
(doctor-put-meaning toke 'toke)
(doctor-put-meaning toking 'toke)
(doctor-put-meaning tokin\' 'toke)
(doctor-put-meaning toked 'toke)
(doctor-put-meaning roach 'toke)
(doctor-put-meaning pills 'drug)
(doctor-put-meaning dope 'drug)
(doctor-put-meaning acid 'drug)
(doctor-put-meaning lsd 'drug)
(doctor-put-meaning speed 'drug)
(doctor-put-meaning heroin 'drug)
(doctor-put-meaning hash 'drug)
(doctor-put-meaning cocaine 'drug)
(doctor-put-meaning uppers 'drug)
(doctor-put-meaning downers 'drug)
(doctor-put-meaning loves 'loves)
(doctor-put-meaning love 'love)
(doctor-put-meaning loved 'love)
(doctor-put-meaning hates 'hates)
(doctor-put-meaning dislikes 'hates)
(doctor-put-meaning hate 'hate)
(doctor-put-meaning hated 'hate)
(doctor-put-meaning dislike 'hate)
(doctor-put-meaning stoned 'state)
(doctor-put-meaning drunk 'state)
(doctor-put-meaning drunken 'state)
(doctor-put-meaning high 'state)
(doctor-put-meaning horny 'state)
(doctor-put-meaning blasted 'state)
(doctor-put-meaning happy 'state)
(doctor-put-meaning paranoid 'state)
(doctor-put-meaning wish 'desire)
(doctor-put-meaning wishes 'desire)
(doctor-put-meaning want 'desire)
(doctor-put-meaning desire 'desire)
(doctor-put-meaning like 'desire)
(doctor-put-meaning hope 'desire)
(doctor-put-meaning hopes 'desire)
(doctor-put-meaning desires 'desire)
(doctor-put-meaning wants 'desire)
(doctor-put-meaning desires 'desire)
(doctor-put-meaning likes 'desire)
(doctor-put-meaning needs 'desire)
(doctor-put-meaning need 'desire)
(doctor-put-meaning frustrated 'mood)
(doctor-put-meaning depressed 'mood)
(doctor-put-meaning annoyed 'mood)
(doctor-put-meaning upset 'mood)
(doctor-put-meaning unhappy 'mood)
(doctor-put-meaning excited 'mood)
(doctor-put-meaning worried 'mood)
(doctor-put-meaning lonely 'mood)
(doctor-put-meaning angry 'mood)
(doctor-put-meaning mad 'mood)
(doctor-put-meaning pissed 'mood)
(doctor-put-meaning jealous 'mood)
(doctor-put-meaning afraid 'fear)
(doctor-put-meaning terrified 'fear)
(doctor-put-meaning fear 'fear)
(doctor-put-meaning scared 'fear)
(doctor-put-meaning frightened 'fear)
(doctor-put-meaning virginity 'sexnoun)
(doctor-put-meaning virgins 'sexnoun)
(doctor-put-meaning virgin 'sexnoun)
(doctor-put-meaning cock 'sexnoun)
(doctor-put-meaning cocks 'sexnoun)
(doctor-put-meaning dick 'sexnoun)
(doctor-put-meaning dicks 'sexnoun)
(doctor-put-meaning cunt 'sexnoun)
(doctor-put-meaning cunts 'sexnoun)
(doctor-put-meaning prostitute 'sexnoun)
(doctor-put-meaning condom 'sexnoun)
(doctor-put-meaning sex 'sexnoun)
(doctor-put-meaning rapes 'sexnoun)
(doctor-put-meaning wife 'family)
(doctor-put-meaning family 'family)
(doctor-put-meaning brothers 'family)
(doctor-put-meaning sisters 'family)
(doctor-put-meaning parent 'family)
(doctor-put-meaning parents 'family)
(doctor-put-meaning brother 'family)
(doctor-put-meaning sister 'family)
(doctor-put-meaning father 'family)
(doctor-put-meaning mother 'family)
(doctor-put-meaning husband 'family)
(doctor-put-meaning siblings 'family)
(doctor-put-meaning grandmother 'family)
(doctor-put-meaning grandfather 'family)
(doctor-put-meaning maternal 'family)
(doctor-put-meaning paternal 'family)
(doctor-put-meaning stab 'death)
(doctor-put-meaning murder 'death)
(doctor-put-meaning murders 'death)
(doctor-put-meaning suicide 'death)
(doctor-put-meaning suicides 'death)
(doctor-put-meaning kill 'death)
(doctor-put-meaning kills 'death)
(doctor-put-meaning die 'death)
(doctor-put-meaning dies 'death)
(doctor-put-meaning died 'death)
(doctor-put-meaning dead 'death)
(doctor-put-meaning death 'death)
(doctor-put-meaning deaths 'death)
(doctor-put-meaning pain 'symptoms)
(doctor-put-meaning ache 'symptoms)
(doctor-put-meaning fever 'symptoms)
(doctor-put-meaning sore 'symptoms)
(doctor-put-meaning aching 'symptoms)
(doctor-put-meaning stomachache 'symptoms)
(doctor-put-meaning headache 'symptoms)
(doctor-put-meaning hurts 'symptoms)
(doctor-put-meaning disease 'symptoms)
(doctor-put-meaning virus 'symptoms)
(doctor-put-meaning vomit 'symptoms)
(doctor-put-meaning vomiting 'symptoms)
(doctor-put-meaning barf 'symptoms)
(doctor-put-meaning toothache 'symptoms)
(doctor-put-meaning hurt 'symptoms)
(doctor-put-meaning rum 'alcohol)
(doctor-put-meaning gin 'alcohol)
(doctor-put-meaning vodka 'alcohol)
(doctor-put-meaning alcohol 'alcohol)
(doctor-put-meaning bourbon 'alcohol)
(doctor-put-meaning beer 'alcohol)
(doctor-put-meaning wine 'alcohol)
(doctor-put-meaning whiskey 'alcohol)
(doctor-put-meaning scotch 'alcohol)
(doctor-put-meaning fuck 'sexverb)
(doctor-put-meaning fucked 'sexverb)
(doctor-put-meaning screw 'sexverb)
(doctor-put-meaning screwing 'sexverb)
(doctor-put-meaning fucking 'sexverb)
(doctor-put-meaning rape 'sexverb)4
(doctor-put-meaning raped 'sexverb)
(doctor-put-meaning kiss 'sexverb)
(doctor-put-meaning kissing 'sexverb)
(doctor-put-meaning kisses 'sexverb)
(doctor-put-meaning screws 'sexverb)
(doctor-put-meaning fucks 'sexverb)
(doctor-put-meaning because 'conj)
(doctor-put-meaning but 'conj)
(doctor-put-meaning however 'conj)
(doctor-put-meaning besides 'conj)
(doctor-put-meaning anyway 'conj)
(doctor-put-meaning that 'conj)
(doctor-put-meaning except 'conj)
(doctor-put-meaning why 'conj)
(doctor-put-meaning how 'conj)
(doctor-put-meaning until 'when)
(doctor-put-meaning when 'when)
(doctor-put-meaning whenever 'when)
(doctor-put-meaning while 'when)
(doctor-put-meaning since 'when)
(doctor-put-meaning rms 'rms)
(doctor-put-meaning stallman 'rms)
(doctor-put-meaning school 'school)
(doctor-put-meaning schools 'school)
(doctor-put-meaning skool 'school)
(doctor-put-meaning grade 'school)
(doctor-put-meaning grades 'school)
(doctor-put-meaning teacher 'school)
(doctor-put-meaning teachers 'school)
(doctor-put-meaning classes 'school)
(doctor-put-meaning professor 'school)
(doctor-put-meaning prof 'school)
(doctor-put-meaning profs 'school)
(doctor-put-meaning professors 'school)
(doctor-put-meaning mit 'school)
(doctor-put-meaning emacs 'eliza)
(doctor-put-meaning eliza 'eliza)
(doctor-put-meaning liza 'eliza)
(doctor-put-meaning elisa 'eliza)
(doctor-put-meaning weizenbaum 'eliza)
(doctor-put-meaning doktor 'eliza)
(doctor-put-meaning atheletics 'sports)
(doctor-put-meaning baseball 'sports)
(doctor-put-meaning basketball 'sports)
(doctor-put-meaning football 'sports)
(doctor-put-meaning frisbee 'sports)
(doctor-put-meaning gym 'sports)
(doctor-put-meaning gymnastics 'sports)
(doctor-put-meaning hockey 'sports)
(doctor-put-meaning lacrosse 'sports)
(doctor-put-meaning soccer 'sports)
(doctor-put-meaning softball 'sports)
(doctor-put-meaning sports 'sports)
(doctor-put-meaning swimming 'sports)
(doctor-put-meaning swim 'sports)
(doctor-put-meaning tennis 'sports)
(doctor-put-meaning volleyball 'sports)
(doctor-put-meaning math 'math)
(doctor-put-meaning mathematics 'math)
(doctor-put-meaning mathematical 'math)
(doctor-put-meaning theorem 'math)
(doctor-put-meaning axiom 'math)
(doctor-put-meaning lemma 'math)
(doctor-put-meaning algebra 'math)
(doctor-put-meaning algebraic 'math)
(doctor-put-meaning trig 'math)
(doctor-put-meaning trigonometry 'math)
(doctor-put-meaning trigonometric 'math)
(doctor-put-meaning geometry 'math)
(doctor-put-meaning geometric 'math)
(doctor-put-meaning calculus 'math)
(doctor-put-meaning arithmetic 'math)
(doctor-put-meaning zippy 'zippy)
(doctor-put-meaning zippy 'zippy)
(doctor-put-meaning pinhead 'zippy)
(doctor-put-meaning chat 'chat)

(defun doctor ()
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive)
  (switch-to-buffer "*doctor*")
  (doctor-mode))

(defun doctor-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline,
Otherwise call the Doctor to parse preceding sentence"
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (doctor-read-print)
    (newline arg)))

(defun doctor-read-print nil
  "top level loop"
  (interactive)
  (let ((sent (doctor-readin)))
    (insert "\n")
    (setq lincount (1+ lincount))
    (doctor-doc sent)
    (insert "\n")
    (setq bak sent)))

(defun doctor-readin nil
  "Read a sentence. Return it as a list of words"
  (let (sentence)
    (backward-sentence 1)
    (while (not (eobp))
      (setq sentence (append sentence (list (doctor-read-token)))))
    sentence))

(defun doctor-read-token ()
  "read one word from buffer"
  (prog1 (intern (downcase (buffer-substring (point)
					     (progn
					       (forward-word 1)
					       (point)))))
    (re-search-forward "\\Sw*")))

;; Main processing function for sentences that have been read.

(defun doctor-doc (sent)
  (cond
   ((equal sent '(foo))
    (doctor-type '(bar! ($ please)($ continue))))
   ((member sent howareyoulst)
    (doctor-type '(i\'m ok \.  ($ describe) yourself \.)))
   ((or (member sent '((good bye) (see you later) (i quit) (so long)
		       (go away) (get lost)))
	(memq (car sent)
	      '(bye halt break quit done exit goodbye 
		    bye\, stop pause goodbye\, stop pause)))
    (doctor-type ($ bye)))
   ((and (eq (car sent) 'you)
	 (memq (cadr sent) abusewords))
    (setq found (cadr sent))
    (doctor-type ($ abuselst)))
   ((eq (car sent) 'whatmeans)
    (doctor-def (cadr sent)))
   ((equal sent '(parse))
    (doctor-type (list  'subj '= subj ",  "
			'verb '= verb "\n"
			'object 'phrase '= obj ","
			'noun 'form '=  object "\n"
			'current 'keyword 'is found
			", "
			'most 'recent 'possessive
			'is owner "\n"
			'sentence 'used 'was
			"..."
			'(// bak))))
   ;;   ((eq (car sent) 'forget)
   ;;    (set (cadr sent) nil)
   ;;    (doctor-type '(($ isee)($ please)
   ;;     ($ continue)\.)))
   (t
    (if (doctor-defq sent) (doctor-define sent found))
    (if (> (length sent) 12)(doctor-shorten sent))
    (setq sent (doctor-correct-spelling (doctor-replace sent replist)))
    (cond ((and (not (memq 'me sent))(not (memq 'i sent))
		(memq 'am sent))
	   (setq sent (doctor-replace sent '((am . (are)))))))
    (cond ((equal (car sent) 'yow) (doctor-zippy))
	  ((< (length sent) 2)
	   (cond ((eq (doctor-meaning (car sent)) 'howdy)
		  (doctor-howdy))
		 (t (doctor-short))))
	  (t
	   (if (memq 'am sent)
	       (setq sent (doctor-replace sent '((me . (i))))))
	   (setq sent (doctor-fixup sent))
	   (if (and (eq (car sent) 'do) (eq (cadr sent) 'not))
	       (cond ((zerop (random-range 3))
		      (doctor-type '(are you ($ afraidof) that \?)))
		     ((zerop (random-range 2))
		      (doctor-type '(don\'t tell me what to do \. i am the
					    psychiatrist here!))
		      (doctor-rthing))
		     (t
		      (doctor-type '(($ whysay) that i shouldn\'t
				     (cddr sent)
				     \?))))
	     (doctor-go (doctor-wherego sent))))))))

;; Things done to process sentences once read.

(defun doctor-correct-spelling (sent)
  "correct the spelling and expand each word in sentence"
  (if sent
      (apply 'append (mapcar '(lambda (word)
				(if (memq word typos)
				    (get (get word 'doctor-correction) 'doctor-expansion)
				  (list word)))
			     sent))))

(defun doctor-shorten (sent)
  "Make a sentence managably short using a few hacks"
  (let (foo
	retval
	(temp '(because but however besides anyway until
		    while that except why how)))
    (while temp
	   (setq foo (memq (car temp) sent))
	   (if (and foo
		    (> (length foo) 3))
	       (setq sent foo
		     sent (doctor-fixup sent)
		     temp nil
		     retval t)
	       (setq temp (cdr temp))))
    retval))

(defun doctor-define (sent found)
  (doctor-svo sent found 1 nil)
  (and
   (doctor-nounp subj)
   (not (doctor-pronounp subj))
   subj
   (doctor-meaning object)
   (put subj 'doctor-meaning (doctor-meaning object))
   t))

(defun doctor-defq (sent)
  "Set global var  found  to first keyword found in sentence SENT"
  (setq found nil)
  (let ((temp '(means applies mean refers refer related
		      similar defined associated linked like same)))
    (while temp
	   (if (memq (car temp) sent)
	       (setq found (car temp)
		     temp nil)
	       (setq temp (cdr temp)))))
  found)

(defun doctor-def (x)
  (progn
   (doctor-type (list 'the 'word x 'means (doctor-meaning x) 'to 'me)) 
   nil))

(defun doctor-forget ()
  "Delete the last element of the history list"
  (setq history (reverse (cdr (reverse history)))))

(defun doctor-query (x)
  "Prompt for a line of input from the minibuffer until a noun or a
verb word is seen. Put dialogue in buffer."
  (let (a
	(prompt (concat (doctor-make-string x)
			" what \?  "))
	retval)
    (while (not retval)
	   (while (not a)
	     (insert ?\n
		     prompt
		     (read-string prompt)
		     ?\n)
	     (setq a (doctor-readin)))
	   (while (and a (not retval))
		  (cond ((doctor-nounp (car a))
			 (setq retval (car a)))
			((doctor-verbp (car a))
			 (setq retval (doctor-build
				       (doctor-build x " ")
				       (car a))))
			((setq a (cdr a))))))
    retval))

(defun doctor-subjsearch (sent key type)
  "Search for the subject of a sentence SENT, looking for the noun closest to
and preceding KEY by at least TYPE words. Set global variable subj to the
subject noun, and return the portion of the sentence following it"
  (let ((i (- (length sent) (length (memq key sent)) type)))
    (while (and (> i -1) (not (doctor-nounp (nth i sent))))
      (setq i (1- i)))
    (cond ((> i -1)
	   (setq subj (nth i sent))
	   (nthcdr (1+ i) sent))
	  (t
	   (setq subj 'you)
	   nil))))

(defun doctor-nounp (x)
  "Returns t if the symbol argument is a noun"
	(or (doctor-pronounp x)
	    (not (or (doctor-verbp x)
		     (equal x 'not)
		     (doctor-prepp x)
		     (doctor-modifierp x) )) ))

(defun doctor-pronounp (x)
  "Returns t if the symbol argument is a pronoun"
  (memq x '(
	i me mine myself
	we us ours ourselves ourself
	you yours yourself yourselves
	he him himself she hers herself
	it that those this these things thing
	they them themselves theirs
	anybody everybody somebody
	anyone everyone someone
	anything something everything)))

(mapcar (function (lambda (x) (put x 'doctor-sentence-type 'verb)))
	'(abort aborted aborts ask asked asks am
		applied applies apply are associate
		associated ate
		be became become becomes becoming
		been being believe belived believes
		bit bite bites bore bored bores boring bought buy buys buying
		call called calling calls came can caught catch come
		contract contracted contracts control controlled controls
		could croak croaks croaked cut cuts
		dare dared define defines dial dialed dials did die died dies
		dislike disliked
		dislikes do does drank drink drinks drinking
		drive drives driving drove dying
		eat eating eats expand expanded expands
		expect expected expects expel expels expeled expelled
		explain explained explains
		fart farts feel feels felt fight fights find finds finding
		forget forgets forgot fought found fuck fucked
		fucking fucks
		gave get gets getting give gives go goes going gone got gotten
		had harm harms has hate hated hates have having
		hear heard hears hearing help helped helping helps
		hit hits hope hoped hopes hurt hurts
		implies imply is
		join joined joins jump jumped jumps
		keep keeping keeps kept
		kill killed killing kills kiss kissed kisses kissing
		knew know knows
		laid lay lays let lets lie lied lies like liked likes
		liking listen listens
		login look looked looking looks
		lose losing lost
		love loved loves loving
		luse lusing lust lusts
		made make makes making may mean means meant might
		move moved moves moving must
		need needed needs 
		order ordered orders ought
		paid pay pays pick picked picking picks 
		placed placing prefer prefers put puts
		ran rape raped rapes
		read reading reads recall receive received receives
		refer refered referred refers
		relate related relates remember remembered remembers
		romp romped romps run running runs
		said sang sat saw say says
		screw screwed screwing screws scrod see sees seem seemed
		seems seen sell selling sells
		send sendind sends sent shall shoot shot should
		sing sings sit sits sitting sold studied study
		take takes taking talk talked talking talks tell tells telling
		think thinks
		thought told took tooled touch touched touches touching
		transfer transfered transfers transmit transmits transmitted
		type types types typing
		walk walked walking walks want wanted wants was watch
		watched watching went were will wish would work worked works
		write writes writing wrote use used uses using))

(defun doctor-verbp (x) (if (symbolp x)
			    (eq (get x 'doctor-sentence-type) 'verb)))

(defun doctor-plural (x)
  "form the plural of the word argument"
  (let ((foo (doctor-make-string x)))
    (cond ((string-equal (substring foo -1) "s")
	   (cond ((string-equal (substring foo -2 -1) "s")
		  (intern (concat foo "es")))
		 (t x)))
	   ((string-equal (substring foo -1) "y")
	    (intern (concat (substring foo 0 -1)
			    "ies")))
	   (t (intern (concat foo "s"))))))

(defun doctor-setprep (sent key)
  (let ((val)
	(foo (memq key sent)))
    (cond ((doctor-prepp (cadr foo))
	   (setq val (doctor-getnoun (cddr foo)))
	   (cond (val val)
		 (t 'something)))
	  ((doctor-articlep (cadr foo))
	   (setq val (doctor-getnoun (cddr foo)))
	   (cond (val (doctor-build (doctor-build (cadr foo) " ") val))
		 (t 'something)))
	  (t 'something))))

(defun doctor-getnoun (x)
  (cond ((null x)(setq object 'something))
	((atom x)(setq object x))
	((eq (length x) 1)
	 (setq object (cond
		       ((doctor-nounp (setq object (car x))) object)
		       (t (doctor-query object)))))
	((eq (car x) 'to)
	 (doctor-build 'to\  (doctor-getnoun (cdr x))))
	((doctor-prepp (car x))
	 (doctor-getnoun (cdr x)))
	((not (doctor-nounp (car x)))
	 (doctor-build (doctor-build (cdr (assq (car x)
						(append
						 '((a . this)
						   (some . this)
						   (one . that))
						 (list
						  (cons
						   (car x) (car x))))))
				     " ")
		       (doctor-getnoun (cdr x))))
	(t (setq object (car x))) ))

(defun doctor-modifierp (x)
  (or (doctor-adjectivep x)
      (doctor-adverbp x)
      (doctor-othermodifierp x)))

(defun doctor-adjectivep (x)
  (or (numberp x)
      (doctor-nmbrp x)
      (doctor-articlep x)
      (doctor-colorp x)
      (doctor-sizep x)
      (doctor-possessivepronounp x)))

(defun doctor-adverbp (xx)
  (string-equal (substring (doctor-make-string xx) -2) "ly"))

(defun doctor-articlep (x)
  (memq x '(the a an)))

(defun doctor-nmbrp (x)
  (memq x '(one two three four five six seven eight nine ten
		eleven twelve thirteen fourteen fifteen
		sixteen seventeen eighteen nineteen
		twenty thirty forty fifty sixty seventy eighty ninety
		hundred thousand million billion
		half quarter
		first second third fourth fifth
		sixth seventh eighth nineth tenth)))
		 
(defun doctor-colorp (x)
  (memq x '(beige black blue brown crimson
		  gray grey green
		  orange pink purple red tan tawny
		  violet white yellow)))

(defun doctor-sizep (x)
  (memq x '(big large tall fat wide thick
		small petite short thin skinny)))

(defun doctor-possessivepronounp (x)
  (memq x '(my your his her our their)))

(defun doctor-othermodifierp (x)
  (memq x '(all also always amusing any anyway associated awesome
		bad beautiful best better but certain clear
		ever every fantastic fun funny
		good great gross growdy however if ignorant
		less linked losing lusing many more much
		never nice obnoxious often poor pretty real related rich
		similar some stupid super superb
		terrible terrific too total tubular ugly very)))

(defun doctor-prepp (x)
  (memq x '(about above after around as at
		  before beneath behind beside between by
		  for from in inside into
		  like near next of on onto over
		  same through thru to toward towards
		  under underneath with without)))

(defun doctor-remember (thing)
  (cond ((null history)
	 (setq history (list thing)))
	(t (setq history (append history (list thing))))))

(defun doctor-type (x)
  (setq x (doctor-fix-2 x))
  (doctor-txtype (doctor-assm x)))

(defun doctor-fixup (sent)
  (setq sent (append
	      (cdr
	       (assq (car sent)
		     (append
		      '((me  i)
			(him  he)
			(her  she)
			(them  they)
			(okay)
			(well)
			(sigh)
			(hmm)
			(hmmm)
			(hmmmm)
			(hmmmmm)
			(gee)
			(sure)
			(great)
			(oh)
			(fine)
			(ok)
			(no))
		      (list (list (car sent)
				  (car sent))))))
	      (cdr sent)))
  (doctor-fix-2 sent))

(defun doctor-fix-2 (sent)
  (let ((foo sent))
    (while foo
      (if (and (eq (car foo) 'me)
	       (doctor-verbp (cadr foo)))
	  (rplaca foo 'i)
	(cond ((eq (car foo) 'you)
	       (cond ((memq (cadr foo) '(am be been is))
		      (rplaca (cdr foo) 'are))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))
		     ((memq (cadr foo) '(was))
		      (rplaca (cdr foo) 'were))))
	      ((equal (car foo) 'i)
	       (cond ((memq (cadr foo) '(are is be been))
		      (rplaca (cdr foo) 'am))
		     ((memq (cadr foo) '(were))
		      (rplaca (cdr foo) 'was))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))))
	      ((and (doctor-verbp (car foo))
		    (eq (cadr foo) 'i)
		    (not (doctor-verbp (car (cddr foo)))))
	       (rplaca (cdr foo) 'me))
	      ((and (eq (car foo) 'a)
		    (doctor-vowelp (string-to-char
				    (doctor-make-string (cadr foo)))))
	       (rplaca foo 'an))
	      ((and (eq (car foo) 'an)
		    (not (doctor-vowelp (string-to-char
					 (doctor-make-string (cadr foo))))))
	       (rplaca foo 'a)))
	(setq foo (cdr foo))))
    sent))

(defun doctor-vowelp (x)
  (memq x '(?a ?e ?i ?o ?u)))

(defun doctor-replace (sent rlist)
  "Replaces any element of SENT that is the car of a replacement element
pair in RLIST"
  (apply 'append
	 (mapcar
	  (function
	   (lambda (x)
	     (cdr (or (assq x rlist)   ; either find a replacement
		      (list x x)))))   ; or fake an identity mapping
	  sent)))

(defun doctor-wherego (sent)
  (cond ((null sent)($ whereoutp))
	((null (doctor-meaning (car sent)))
	 (doctor-wherego (cond ((zerop (random-range 2))
				(reverse (cdr sent)))
			       (t (cdr sent)))))
	(t
	 (setq found (car sent))
	 (doctor-meaning (car sent)))))

(defun doctor-svo (sent key type mem)
  "Find subject, verb and object in sentence SENT with focus on word KEY.
TYPE is number of words preceding KEY to start looking for subject. MEM is
t if results are to be put on doctor's memory stack. Return is in global
variables subj, verb and object"
  (let ((foo (doctor-subjsearch sent key type) sent))
    (or foo
	(setq foo sent
	      mem nil))
    (while (and (null (doctor-verbp (car foo))) (cdr foo))
      (setq foo (cdr foo)))
    (setq verb (car foo))
    (setq obj (doctor-getnoun (cdr foo)))
    (cond ((eq object 'i)(setq object 'me))
	  ((eq subj 'me)(setq subj 'i)))
    (cond (mem (doctor-remember (list subj verb obj))))))

(defun doctor-possess (sent key)
  "Set possessive in SENT for keyword KEY. Hack on previous word, setting
global variable owner to possibly correct result"
  (let* ((i (- (length sent) (length (memq key sent)) 1))
	 (prev (if (< i 0) 'your
		 (nth i sent))))
    (setq owner (if (or (doctor-possessivepronounp prev)
			(string-equal "s"
				      (substring (doctor-make-string prev)
						 -1)))
		    prev
		  'your))))

;; Output of replies.

(defun doctor-txtype (ans)
  "Output to buffer a list of symbols or strings as a sentence"
  (setq *print-upcase* t *print-space* nil)
  (mapcar 'doctor-type-symbol ans)
  (insert "\n"))

(defun doctor-type-symbol (word)
  "Output a symbol to the buffer with some fancy case and spacing hacks"
  (setq word (doctor-make-string word))
  (if (string-equal word "i") (setq word "I"))
  (if *print-upcase*
      (progn
	(setq word (capitalize word))
	(if *print-space*
	    (insert " "))))
  (cond ((or (string-match "^[.,;:?! ]" word)
	     (not *print-space*))
	 (insert word))
	(t (insert ?\  word)))
  (if (> (current-column) fill-column)
      (apply auto-fill-hook nil))
  (setq *print-upcase* (string-match "[.?!]$" word)
	*print-space* t))

(defun doctor-build (str1 str2)
  "Make a symbol out of the concatenation of the two non-list arguments"
  (cond ((null str1) str2)
	((null str2) str1)
	((and (atom str1)
	      (atom str2))
	 (intern (concat (doctor-make-string str1)
			 (doctor-make-string str2))))
	(t nil)))

(defun doctor-make-string (obj)
  (cond ((stringp obj) obj)
	((symbolp obj) (symbol-name obj))
	((numberp obj) (int-to-string obj))
	(t "")))

(defun doctor-concat (x y)
  "like append, but force atomic arguments to be lists"
  (append
   (if (and x (atom x)) (list x) x)
   (if (and y (atom y)) (list y) y)))

(defun doctor-assm (proto)
  (cond ((null proto) nil)
	((atom proto) (list proto))
	((atom (car proto))
	 (cons (car proto) (doctor-assm (cdr proto))))
	(t (doctor-concat (doctor-assm (eval (car proto))) (doctor-assm (cdr proto))))))

;; Functions that handle specific words or meanings when found.

(defun doctor-go (destination)
  "Call a doctor- function"
  (funcall (intern (concat "doctor-" (doctor-make-string destination)))))

(defun doctor-desire1 ()
  (doctor-go ($ whereoutp)))

(defun doctor-huh ()
  (cond ((< (length sent) 9) (doctor-type ($ huhlst)))
	(t (doctor-type ($ longhuhlst)))))

(defun doctor-rthing () (doctor-type ($ thlst)))

(defun doctor-remem () (cond ((null history)(doctor-huh))
			     ((doctor-type ($ remlst)))))

(defun doctor-howdy ()
  (cond ((not howdyflag)
	 (doctor-type '(($ hello) what brings you to see me \?))
	 (setq howdyflag t))
	(t
	 (doctor-type '(($ ibelieve) we\'ve introduced ourselves already \.))
	 (doctor-type '(($ please) ($ describe) ($ things) \.)))))

(defun doctor-when ()
  (cond ((< (length (memq found sent)) 3)(doctor-short))
	(t
	 (setq sent (cdr (memq found sent)))
	 (setq sent (doctor-fixup sent))
	 (doctor-type '(($ whatwhen)(// sent) \?)))))

(defun doctor-conj ()
  (cond ((< (length (memq found sent)) 4)(doctor-short))
	(t
	 (setq sent (cdr (memq found sent)))
	 (setq sent (doctor-fixup sent))
	 (cond ((eq (car sent) 'of)
		(doctor-type '(are you ($ sure) that is the real reason \?))
		(setq things (cons (cdr sent) things)))
	       (t
		(doctor-remember sent)
		(doctor-type ($ beclst)))))))

(defun doctor-short ()
  (cond ((= (car repetitive-shortness) (1- lincount))
	 (rplacd repetitive-shortness
		 (1+ (cdr repetitive-shortness))))
	(t
	 (rplacd repetitive-shortness 1)))
  (rplaca repetitive-shortness lincount)
  (cond ((> (cdr repetitive-shortness) 6)
	 (cond ((not **mad**)
		(doctor-type '(($ areyou)
			       just trying to see what kind of things
			       i have in my vocabulary \? please try to
			       carry on a reasonable conversation!))
		(setq **mad** t))
	       (t
		(doctor-type '(i give up \. you need a lesson in creative
				 writing \.\.\.))
		;;(push monosyllables observation-list)
		)))
	(t
	 (cond ((equal sent (doctor-assm '(yes)))
		(doctor-type '(($ isee) ($ inter) ($ whysay) this is so \?)))
	       ((equal sent (doctor-assm '(because)))
		(doctor-type ($ shortbeclst)))
	       ((equal sent (doctor-assm '(no)))
		(doctor-type ($ neglst)))
	       (t (doctor-type ($ shortlst)))))))
	   
(defun doctor-alcohol () (doctor-type ($ drnk)))

(defun doctor-desire ()
  (let ((foo (memq found sent)))
    (cond ((< (length foo) 2)
	   (doctor-go (doctor-build (doctor-meaning found) 1)))
	  ((memq (cadr foo) '(a an))
	   (rplacd foo (append '(to have) (cdr foo)))
	   (doctor-svo sent found 1 nil)
	   (doctor-remember (list subj 'would 'like obj))
	   (doctor-type ($ whywant)))
	  ((not (eq (cadr foo) 'to))
	   (doctor-go (doctor-build (doctor-meaning found) 1)))
	  (t
	   (doctor-svo sent found 1 nil)
	   (doctor-remember (list subj 'would 'like obj))
	   (doctor-type ($ whywant))))))

(defun doctor-drug ()
  (doctor-type ($ drugs))
  (doctor-remember (list 'you 'used found)))

(defun doctor-toke ()
  (doctor-type ($ toklst)))

(defun doctor-state ()
  (doctor-type ($ states))(doctor-remember (list 'you 'were found)))

(defun doctor-mood ()
  (doctor-type ($ moods))(doctor-remember (list 'you 'felt found)))

(defun doctor-fear ()
  (setq feared (doctor-setprep sent found))
  (doctor-type ($ fears))
  (doctor-remember (list 'you 'were 'afraid 'of feared)))

(defun doctor-hate ()
  (doctor-svo sent found 1 t)
  (cond ((memq 'not sent) (doctor-forget) (doctor-huh))
	((equal subj 'you)
	 (doctor-type '(why do you (// verb)(// obj) \?)))
	(t (doctor-type '(($ whysay)(list subj verb obj))))))

(defun doctor-symptoms ()
  (doctor-type '(($ maybe) you should consult a doctor of medicine\,
		 i am a psychiatrist \.)))

(defun doctor-hates ()
  (doctor-svo sent found 1 t)
  (doctor-hates1))

(defun doctor-hates1 ()
  (doctor-type '(($ whysay)(list subj verb obj))))

(defun doctor-loves ()
  (doctor-svo sent found 1 t)
  (doctor-qloves))

(defun doctor-qloves ()
  (doctor-type '(($ bother)(list subj verb obj) \?)))

(defun doctor-love ()
  (doctor-svo sent found 1 t)
  (cond ((memq 'not sent) (doctor-forget) (doctor-huh))
	((memq 'to sent) (doctor-hates1))
	(t
	 (cond ((equal object 'something)
		(setq object '(this person you love))))
	 (cond ((equal subj 'you)
		(setq lover obj)
		(cond ((equal lover '(this person you love))
		       (setq lover '(your partner))
		       (doctor-forget)
		       (doctor-type '(with whom are you in love \?)))
		      ((doctor-type '(($ please)
				      ($ describe)
				      ($ relation)
				      (// lover)
				      \.)))))
	       ((equal subj 'i)
		(doctor-txtype '(we were discussing you!)))
	       (t (doctor-forget)
		  (setq obj 'someone)
		  (setq verb (doctor-build verb 's))
		  (doctor-qloves))))))

(defun doctor-mach ()
  (setq found (doctor-plural found))
  (doctor-type ($ machlst)))

(defun doctor-sexnoun () (doctor-sexverb))

(defun doctor-sexverb ()
  (if (or (memq 'me sent)(memq 'myself sent)(memq 'i sent))
      (doctor-foul)
    (doctor-type ($ sexlst))))

(defun doctor-death () (doctor-type ($ deathlst)))

(defun doctor-foul ()
  (doctor-type ($ foullst)))

(defun doctor-family ()
  (doctor-possess sent found)
  (doctor-type ($ famlst)))

;; I did not add this -- rms.
(defun doctor-rms ()
  (cond (rms-flag (doctor-type ($ stallmanlst)))
	(t (setq rms-flag t) (doctor-type '(do you know Stallman \?)))))

(defun doctor-school nil (doctor-type ($ schoollst)))

(defun doctor-eliza ()
  (cond (eliza-flag (doctor-type ($ elizalst)))
	(t (setq eliza-flag t)
	   (doctor-type '((// found) \? hah !
			  ($ please) ($ continue) \.)))))
	   
(defun doctor-sports ()  (doctor-type ($ sportslst)))

(defun doctor-math () (doctor-type ($ mathlst)))

(defun doctor-zippy ()
  (cond (zippy-flag (doctor-type ($ zippylst)))
	(t (setq zippy-flag t)
	   (doctor-type '(yow! are we interactive yet \?)))))


(defun doctor-chat () (doctor-type ($ chatlst)))
