cg = ($say "Can't go that way.\n");
ny = ($say "That exit seems impassable today!?\n");

die = ($say "You are dead.  Better luck next time.\n")
      ($spec 3 0 0 0 0);

VERB n,s,e,w,ne,se,nw,sw;
VERB up,down;
VERB enter,out;

u=up; d=down;
north=n; south=s; east=e; west=w;

Looks = ($setg LOOKP TRUE)(($dobj): ($say "Huh?\n")($exit 1));
ccg = 	(($not @GO) :
		(cg)
	)
	($setg GO FALSE)
	;


n (PREACT) = Looks; n (ACTION) = ccg;
s (PREACT) = Looks; s (ACTION) = ccg;
e (PREACT) = Looks; e (ACTION) = ccg;
w (PREACT) = Looks; w (ACTION) = ccg;

nw(PREACT) = Looks; nw(ACTION) = ccg;
ne(PREACT) = Looks; ne(ACTION) = ccg;
sw(PREACT) = Looks; sw(ACTION) = ccg;
se(PREACT) = Looks; se(ACTION) = ccg;

u (PREACT) = Looks; u (ACTION) = ccg;
d (PREACT) = Looks; d (ACTION) = ccg;

out (PREACT) = Looks;  out(ACTION) = ccg;
enter (PREACT) = Looks; enter(ACTION) = ccg;

VERB say;
say (PREACT) =
	(($not ($dobj)):
		($say "Type just what you want to say: ")
		($setg Dobj ($read)))
	(($ge ($dobj) 0) :
		($say "You want to make ")
		(($sdisc ($dobj)))
		($say " a linguistic artifact??  Strange!\n")
		($exit 1));
say (ACTION) = 
	(($eqst ($dobj) Shazm) :
		(WzTgl)
	:{else}
		($say "Very well.  '")($say ($dobj))($say "'.\n"));
incant = say; chant = say; shout = say; yell = say;

VERB take; VERB drop;
take(PREACT) = (Avail ($dobj))
	(($eq ($prop ($dobj) WEIGH) CAPAC):
		($say "You can't take that!\n")
		($exit 1)
	)
	(($ge ($plus ($prop .ME HAS)
		     ($prop ($dobj) WEIGH))
	      ($prop .ME HOLDS)):
		($say "You can't carry that much more!\n")
		($exit 1)
	);
drop(PREACT) = 
		(($ne ($loc ($dobj)) .ME) :
		($say "You don't have it with you.\n")($exit 1));
ROUTINE ctake; ROUTINE cdrop;


DWIMD = (($not (See ($dobj) ($cont ($loc .ME)))):
		($rtrn 0)
	  )
	(($not (Reach ($dobj) ($cont ($loc .ME)))):
		($rtrn 0)
	  )
	(($and ($eq ($verb) take) ($eq ($loc ($dobj)) .ME)):
		($rtrn 0)
	  )
	(($and ($eq ($verb) drop) ($ne ($loc ($dobj)) .ME)):
		($rtrn 0)
	  )
	($rtrn 1)
;

DWIMI = (($not (See ($iobj) ($cont ($loc .ME)))):
		($rtrn 0)
	  )
	(($not (Reach ($iobj) ($cont ($loc .ME)))):
		($rtrn 0)
	  )
	(($and ($eq ($verb) take) ($eq ($loc ($iobj)) .ME)):
		($rtrn 0)
	  )
	(($and ($eq ($verb) drop) ($ne ($loc ($iobj)) .ME)):
		($rtrn 0)
	  )

	($rtrn 1)
;





take(ACTION) = (ctake);
ctake =
	($setp .ME HAS ($plus ($prop .ME HAS) ($prop ($dobj) WEIGH) ) )
	{($say "You are holding ")($num ($prop .ME HAS))($say " lbs.\n")}
	($say "Taken.\n") ($move ($dobj) .ME)
	;

drop(ACTION) = (cdrop);
cdrop = 
	($say "Dropped.\n") ($move ($dobj) ($loc .ME))
	($setp .ME HAS ($minus ($prop .ME HAS)($prop ($dobj) WEIGH)))
	;

VERB put;
put (PREACT) =
	(($ne ($loc ($dobj)) .ME) :
		($say "You don't have it with you.\n")($exit 1))
	(Avail ($iobj));
put(ACTION) =
	(($not ($and ($prop ($iobj) OPEN)
		     ($prop ($iobj) OPENS))):
		($say "You can't seem to manage that trick.\n")($exit 0))
	(($gt ($plus ($prop ($iobj) HAS)
		     ($prop ($dobj) WEIGH))
	      ($prop ($iobj) HOLDS)):
		($say "It won't fit!\n")($exit 0))
	($say "OK\n") ($move ($dobj) ($iobj))
	($setp ($iobj) HAS ($plus ($prop ($iobj) HAS) ($prop ($dobj) WEIGH)))
	;









VERB open; VERB close;
open(PREACT) = (Avail ($dobj))(($iobj):(Avail ($iobj)));
close(PREACT) = (Avail ($dobj))(($iobj):(Avail ($iobj)));
open(ACTION) =
	(($not ($prop ($dobj) OPENS)):
		($say "I don't know how to open that!\n")($exit 0))
	(($and ($prop ($dobj) LOCKS)
	       ($prop ($dobj) LOCKD)):
		($say "I can't open it, it's locked!\n")($exit 0))
	(($prop ($dobj) OPEN):
		($say "It's already open!\n")($exit 0))
	($setp ($dobj) OPEN TRUE)
	($say "Opened.\n")
	(($and  ($ne 0 ($cont ($dobj)))  ($not ($prop ($dobj) TRANS))):
		(Llook 1 ($cont ($dobj)))
	)
	;

close (ACTION) =
	(($not ($prop ($dobj) OPENS)):
		($say "I don't know how to close that!\n")($exit 0))
	(($not ($prop ($dobj) OPEN)):
		($say "It's already closed!\n")($exit 0))
	($setp ($dobj) OPEN FALSE)
	($say "Closed\n")
	;


VERB lock; VERB unlock;
lock(PREACT) = (Avail ($dobj))(($iobj):(Avail ($iobj)));
unlock(PREACT) = (Avail ($dobj))(($iobj):(Avail ($iobj)));

Lockact =
	(($prop ($dobj) LOCKS) :
		($say "Hm, you don't seem to have the right key.\n")
	{else}:
		($say "I don't know how to lock or unlock such a thing.\n")
	);

lock(ACTION) = Lockact;
unlock(ACTION) = Lockact;

Lockup =
	(($prop %1 OPEN):
		($say "You must close it first.\n")($exit 1))
	(($prop %1 LOCKD):
		($say "It's already locked!\n")($exit 1))
	(($not ($prop %1 LOCKS)):($exit 0))
	(($eq ($loc %2) .ME):
		($say %3)
		($setp %1 LOCKD TRUE)
		($exit 1))
		;
Ulock  =
	(($not($prop %1 LOCKD)):
		($say "It's already unlocked!\n")($exit 1))
	(($not ($prop %1 LOCKS)):($exit 0))
	(($eq ($loc %2) .ME):
		($say %3)
		($setp %1 LOCKD FALSE)
		($exit 1))
		;




VERB move;
move(PREACT) = (Avail ($dobj));
move(ACTION) =
($say "Nothing seems to happen.\n");
push=move;
pull=move;
lift=move;

VERB break;
break(PREACT) = (Avail ($dobj));
break(ACTION) =
($say "It seems to be unbreakable.\n");

VERB wake;
wake(PREACT) = (Avail ($dobj));
wake(ACTION) = ($say "I don't know how to wake ")(($sdisc ($dobj)))
		($say ".\n");

VERB strike; hit=strike; kill=strike;
strike(PREACT) = (Avail ($dobj))
		 (($iobj):
			(($ne ($loc ($iobj)) .ME):
				($say "You don't have it with you.\n")
				($exit 1)
			 )
		  );
strike(ACTION) = ($say "Hitting ")(($sdisc ($dobj)))($say
 " doesn't seem to do anything.");

VERB touch;  feel=touch;
touch(PREACT) = (Avail ($dobj))
		(($iobj):
		       (($ne ($loc ($iobj)) .ME):
				($say "You don't have it with you.\n")
				($exit 1)
			)
		);
touch(ACTION) = ($say "Touching ")(($sdisc ($dobj)))
		($say " doesn't seem too useful.\n");






VERB rub;
rub(PREACT) = (Avail ($dobj));
rub(ACTION) = ($say "Nothing happens when you rub ")
		(($sdisc ($dobj)))($say ".\n");

VERB cut;
cut(PREACT) = (($not ($iobj)): ($say "You need tools to do that.\n")
			       ($exit 1))
	      (Avail ($iobj));

VERB pry;
pry(PREACT) =  (Avail ($dobj))
	       (($not ($iobj)): ($say "You need tools to do that.\n")
				($exit 1))
	       (Avail ($iobj));

pry(ACTION) =  ($say "I can't seem to manage that.\n");

VERB throw;
throw(PREACT) = (($ne ($loc ($dobj)) .ME): ($say "You aren't holding it!\n")
					   ($exit 1));
throw(ACTION) = ($move ($dobj) ($loc .ME))
		($say "Thrown.\n");

VERB read;
read(PREACT) = (Avail ($dobj));
read(ACTION) = ($say "It doesn't have anything on it to read.\n");

VERB light;
light(PREACT) = (Avail ($dobj));

tress = ($say "You were warned about trespassing! The fence is electrified!\n")
 	(die);



VERB look;
VERB quit;
VERB inven;
VERB score;
VERB save;
VERB restore;

look(ACTION) = (Llook 0 ($loc .ME))
		      ;
inven(ACTION) = (($not ($cont .ME)):
			($say "You are empty-handed.\n")
			($exit 1))
		($setp .ME CONTS TRUE)
		($say "You are carrying:\n")
		(Slook 1 ($cont .ME))
			;
quit(ACTION) = ($spec 3 0 0 0 0);

score(ACTION) =
	(($eq ($loc .ME) town5):
 		($say
"A mysterious glowing network of lights appears, and you can
just make out the words:
   Your account is now at $")
		($num @SCORE)
 		($say ".
   Thank you for letting Arpa-Citizen's be your host.\n")
		($say
"The network then disappears.\n")
	{else}:
		($say
"Hmm, you'll have to check at the local bank.\n"));


save(ACTION) = ($spec 4 0 0 0 0);
restore(ACTION) = ($spec 5 0 0 0 0);

VERB Debug, BEAM, JOIN, Open, GLOW,GRAB;
WizP = (($not @Wizrd) :
		($say "Only a real Wizard can do that!\n")
		($exit 1)
	);
Debug(PREACT) = WizP;
BEAM(PREACT) = WizP;
GRAB(PREACT) = WizP;
JOIN(PREACT) = WizP;
Open(PREACT) = WizP;
GLOW(PREACT) = WizP;

Debug(ACTION) = ($spec 8 8 0 0 0);
BEAM(ACTION) = ($move .ME ($dobj))
		($setg LOOKP TRUE);
JOIN(ACTION) = ($move .ME ($loc ($dobj)))
		($setg LOOKP TRUE);
Open(ACTION) = ($setp ($dobj) OPEN TRUE)($say "Opened.\n");
GLOW(ACTION) = ($setp .ME LIGHT ($not ($prop .ME LIGHT)))
		(($prop .ME LIGHT) : ($say 
"Your body begins to radiate a strong but gentle white light!\n")
		:	($say
"The light from your body fades away.\n"));
GRAB(ACTION) = ($say "You have the ")($name ($dobj))($say ", Boss!\n")
		($move ($dobj) .ME);

