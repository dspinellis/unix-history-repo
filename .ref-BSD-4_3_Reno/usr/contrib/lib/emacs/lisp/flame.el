;;; "Flame" program.  This has a chequered past.
;;;
;;; The original was on a Motorola 286 running Vanilla V.1,
;;; about 2 years ago.  It was couched in terms of a yacc (I think)
;;; script.  I pulled the data out of it and rewrote it as a piece
;;; of PL/1 on Multics.  Now I've moved it into an emacs-lisp
;;; form.  If the original author cares to contact me, I'd
;;; be very happy to credit you!
;;;
;;; Ian G. Batten, Batten@uk.ac.bham.multics
;;;

(random t)

(defvar sentence
  '((how can you say that (statement) \?)
    (I can\'t believe how (adjective) you are\.)
    (only a (der-term) like you would say that (statement) \.)
    ((statement) \, huh\?) (so\, (statement) \?)
    ((statement) \, right\?) (I mean\, (sentence))
    (don\'t you realise that (statement) \?)
    (I firmly believe that (statement) \.)
    (let me tell you something\, you (der-term) \, (statement) \.)
    (furthermore\, you (der-term) \, (statement) \.)
    (I couldn\'t care less about your (thing) \.)
    (How can you be so (adjective) \?)
    (you make me sick\.)
    (it\'s well known that (statement) \.)
    ((statement) \.)
    (it takes a (group-adj) (der-term) like you to say that (statement) \.)
    (I don\'t want to hear about your (thing) \.)
    (you\'re always totally wrong\.)
    (I\'ve never heard anything as ridiculous as the idea that (statement) \.)
    (you must be a real (der-term) to think that (statement) \.)
    (you (adjective) (group-adj) (der-term) \!)
    (you\'re probably (group-adj) yourself\.)
    (you sound like a real (der-term) \.)
    (why\, (statement) \!)
    (I have many (group-adj) friends\.)
    (save the (thing) s\!) (no nukes\!) (ban (thing) s\!)
    (I\'ll bet you think that (thing) s are (adjective) \.)
    (you know\, (statement) \.)
    (your (quality) reminds me of a (thing) \.)
    (you have the (quality) of a (der-term) \.)
    ((der-term) \!)
    ((adjective) (group-adj) (der-term) \!)
    (you\'re a typical (group-adj) person\, totally (adjective) \.)
    (man\, (sentence))))

(defvar sentence-loop (nconc sentence sentence))


(defvar quality
  '((ignorance) (stupidity) (worthlessness)
    (prejudice) (lack of intelligence) (lousiness)
    (bad grammar) (lousy spelling)
    (lack of common decency) (ugliness) (nastiness)
    (subtlety) (dishonesty) ((adjective) (quality))))


(defvar quality-loop (nconc quality quality))

(defvar adjective
  '((ignorant) (crass) (pathetic) (sick)
    (bloated) (malignant) (perverted) (sadistic)
    (stupid) (unpleasant) (lousy) (abusive) (bad)
    (braindamaged) (selfish) (improper) (nasty)
    (disgusting) (foul) (intolerable) (primitive)
    (depressing) (dumb) (phoney)
    ((adjective) and (adjective))
    (as (adjective) as a (thing))))

(defvar adjective-loop (nconc adjective adjective))

(defvar der-term
  '(((adjective) (der-term)) (sexist) (fascist)
    (weakling) (coward) (beast) (peasant) (racist)
    (cretin) (fool) (jerk) (ignoramus) (idiot)
    (wanker) (rat) (slimebag) (DAF driver)
    (Neanderthal) (sadist) (drunk) (capitalist)
    (wimp) (dogmatist) (wally) (maniac)
    (whimpering scumbag) (pea brain) (arsehole)
    (moron) (goof) (incompetant) (lunkhead) (Nazi)
    (SysThug) ((der-term) (der-term))))

(defvar der-term-loop (nconc der-term der-term))


(defvar thing
  '(((adjective) (thing)) (computer)
    (Honeywell dps8) (whale) (operation)
    (sexist joke) (ten-incher) (dog) (MicroVAX II)
    (source license) (real-time clock)
    (mental problem) (sexual fantasy)
    (venereal disease) (Jewish grandmother)
    (cardboard cut-out) (punk haircut) (surfboard)
    (system call) (wood-burning stove)
    (graphics editor) (right wing death squad)
    (disease) (vegetable) (religion)
    (cruise missile) (bug fix) (lawyer) (copyright)
    (PAD)))

(defvar thing-loop (nconc thing thing))


(defvar group-adj
  '((gay) (old) (lesbian) (young) (black)
    (Polish) ((adjective)) (white)
    (mentally retarded) (Nicaraguan) (homosexual)
    (dead) (underpriviledged) (religious)
    ((thing) \-loving) (feminist) (foreign)
    (intellectual) (crazy) (working) (unborn)
    (Chinese) (short) ((adjective)) (poor) (rich)
    (funny-looking) (Puerto Rican) (Mexican)
    (Italian) (communist) (fascist) (Iranian)
    (Moonie)))

(defvar group-adj-loop (nconc group-adj group-adj))

(defvar statement
  '((your (thing) is great) ((thing) s are fun)
    ((person) is a (der-term))
    ((group-adj) people are (adjective))
    (every (group-adj) person is a (der-term))
    (most (group-adj) people have (thing) s)
    (all (group-adj) dudes should get (thing) s)
    ((person) is (group-adj)) (trees are (adjective))
    (if you\'ve seen one (thing) \, you\'ve seen them all)
    (you\'re (group-adj)) (you have a (thing))
    (my (thing) is pretty good)
    (the Martians are coming)
    (the (paper) is always right)
    (just because you read it in the (paper) that doesn\'t mean it\'s true)
    ((person) was (group-adj))
    ((person) \'s ghost is living in your (thing))
    (you look like a (thing))
    (the oceans are full of dirty fish)
    (people are dying every day)
    (a (group-adj) man ain\'t got nothing in the world these days)
    (women are inherently superior to men)
    (the system staff is fascist)
    (there is life after death)
    (the world is full of (der-term) s)
    (you remind me of (person)) (technology is evil)
    ((person) killed (person))
    (the Russians are tapping your phone)
    (the Earth is flat)
    (it\'s OK to run down (group-adj) people)
    (Multics is a really (adjective) operating system)
    (the CIA killed (person))
    (the sexual revolution is over)
    (Lassie was (group-adj))
    (the (group-adj) s have really got it all together)
    (I was (person) in a previous life)
    (breathing causes cancer)
    (it\'s fun to be really (adjective))
    ((quality) is pretty fun) (you\'re a (der-term))
    (the (group-adj) culture is fascinating)
    (when ya gotta go ya gotta go)
    ((person) is (adjective))
    ((person) \'s (quality) is (adjective))
    (it\'s a wonderful day)
    (everything is really a (thing))
    (there\'s a (thing) in (person) \'s brain)
    ((person) is a cool dude)
    ((person) is just a figment of your imagination)
    (the more (thing) s you have, the better)
    (life is a (thing)) (life is (quality))
    ((person) is (adjective))
    ((group-adj) people are all (adjective) (der-term) s)
    ((statement) \, and (statement))
    ((statement) \, but (statement))
    (I wish I had a (thing))
    (you should have a (thing))
    (you hope that (statement))
    ((person) is secretly (group-adj))
    (you wish you were (group-adj))
    (you wish you were a (thing))
    (I wish I were a (thing))
    (you think that (statement))
    ((statement) \, because (statement))
    ((group-adj) people don\'t get married to (group-adj) people because (reason))
    ((group-adj) people are all (adjective) because (reason))
    ((group-adj) people are (adjective) \, and (reason))
    (you must be a (adjective) (der-term) to think that (person) said (statement))
    ((group-adj) people are inherently superior to (group-adj) people)
    (God is Dead)))

(defvar statement-loop (nconc statement statement))


(defvar paper
  '((Daily Mail) (Daily Express)
    (Centre Bulletin) (Sun) (Daily Mirror)
    (Daily Telegraph) (Beano) (Multics Manual)))

(defvar paper-loop (nconc paper paper))


(defvar person
  '((Reagan) (Ken Thompson) (Dennis Ritchie)
    (JFK) (the Pope) (Gadaffi) (Napoleon)
    (Karl Marx) (Groucho) (Michael Jackson)
    (Caesar) (Nietzsche) (Heidegger)
    (Henry Kissinger) (Nixon) (Castro) (Thatcher)
    (Attilla the Hun) (Alaric the Visigoth) (Hitler)))

(defvar person-loop (nconc person person))

(defvar reason
  '((they don\'t want their children to grow up to be too lazy to steal)
    (they can\'t tell them apart from (group-adj) dudes)
    (they\'re too (adjective))
    ((person) wouldn\'t have done it)
    (they can\'t spray paint that small)
    (they don\'t have (thing) s) (they don\'t know how)
    (they can\'t afford (thing) s)))

(defvar reason-loop (nconc reason reason))

(defmacro define-element (name)
  (let ((loop-to-use (intern (concat name "-loop"))))
    (` (defun (, (intern name)) nil
	 (let ((step-forward (% (random) 10)))
	   (if (< step-forward 0) (setq step-forward (- step-forward)))
	   (prog1
	       (nth step-forward (, loop-to-use))
	     (setq (, loop-to-use) (nthcdr (1+ step-forward) (, loop-to-use)))))))))

(define-element "sentence")
(define-element "quality")
(define-element "adjective")
(define-element "der-term")
(define-element "group-adj")
(define-element "statement")
(define-element "thing")
(define-element "paper")
(define-element "person")
(define-element "reason")

(defun *flame nil
  (flame-expand '(sentence)))

(defun flame-expand (object)
  (cond ((atom object)
	 object)
	(t (mapcar 'flame-expand (funcall (car object))))))

(defun flatten (list)
  (cond ((atom list)
	 (list list))
	(t (apply 'append (mapcar 'flatten list)))))

(defun flame (arg)
  "Generate ARG (default 1) sentences of half-crazed gibberish."
  (interactive "p")
  (let ((w (selected-window)))
    (pop-to-buffer (get-buffer-create "*Flame*"))
    (goto-char (point-max))
    (insert ?\n)
    (flame2 arg)
    (select-window w)))

(defun flame2 (arg)
  (let ((start (point)))
    (flame1 arg)
    (fill-region-as-paragraph start (point) t)))

(defun flame1 (arg)
  (cond ((zerop arg) t)
	(t (insert (concat (sentence-ify (string-ify (append-suffixes-hack (flatten (*flame)))))))
	   (flame1 (1- arg)))))

(defun sentence-ify (string)
  (concat (upcase (substring string 0 1))
	  (substring string 1 (length string))
	  "  "))

(defun string-ify (list)
  (mapconcat
   '(lambda (x)
      (format "%s" x))
   list
   " "))

(defun append-suffixes-hack (list)
  (cond ((null list)
	 nil)
	((memq (nth 1 list)
	       '(\? \. \, s\! \! s \'s \-loving))
	 (cons (intern (format "%s%s" (nth 0 list) (nth 1 list)))
	       (append-suffixes-hack (nthcdr 2 list))))
	(t (cons (nth 0 list)
		 (append-suffixes-hack (nthcdr 1 list))))))

(defun psychoanalyze-flamer ()
  "Mr. Angry goes to the analyst."
  (interactive)
  (doctor)				; start the psychotherapy
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (flame2 (if (= (% (random) 2) 0) 2 1))
    (sit-for 0)
    (doctor-ret-or-read 1)))
