NOUN startrm;
NOUN brightroom;

NOUN .ME(startrm);

VAR Obj;		{ A global }
SEEN = 1;		{ A property (bit) number }

Looker = 
	($num @Verb)
	(($ldesc ($loc .ME))   )	{ CALL Ldesc of room }
	 ($setg Obj ($cont ($loc .ME))) { start with first obj in room }
	 (WHILE @Obj :
		(($ldesc @Obj) : 	{ If it has a description, }
			(($ldesc @Obj))	{  then say it.	}
		)
		($setg Obj ($link @Obj))	{ Continue with sibling }
	 )
	;


Slook =  (($sdesc ($loc .ME)))		{ CALL Ldesc of room }
	 ($setg Obj ($cont ($loc .ME))) { start with first obj in room }
	 (WHILE @Obj :
		(($ldesc @Obj) : 	{ If it has a description, }
			(($ldesc @Obj))	{  then say it.	}
		)
		($setg Obj ($link @Obj))	{ Continue with sibling }
	 )
	;

Rdesc =			{ Room description }
	($say "\n")
	(($prop ($loc .ME) SEEN) : 		{ Have I been here? }
			(Slook)			{ Yes, be brief. }
	:{else}		(Looker)		{ Otherwise, do the spiel, }
			($setp ($loc .ME) SEEN 1)	{ but only once. }
	)
	($say "\n> ")	{ Prompt player }
	;

VERB look;
look(ACTION) = Looker;

VERB inventory;
inven = inventory;

inven(ACTION) = (($not ($cont .ME)):	{ Null "contents" field }
			($say "You are empty-handed.\n")
		:{else}
			($say "You are carrying:\n")
			($setg Obj ($cont .ME))
			(WHILE @Obj :
				(($sdesc @Obj))
				($say "\n")
				($setg Obj ($link @Obj))
			)
		)
		;

VERB take;
VERB drop;
get=take;

take(PREACT) = (($ne ($loc .ME) ($loc ($dobj))):	{ Dobj in same room? }
		($say "I don't see ")		{ No, report the fact }
		(($sdesc ($dobj)))
		($say " here.\n")
		($exit 1));			{ and end the turn. }

drop(PREACT) = (($ne ($loc ($dobj)) .ME) :	{ Must be carrying the dobj }
		($say "You aren't carrying it!\n")
		($exit 1));

take (ACTION) = ($move ($dobj) .ME)
		($say "Taken.\n")
		;

drop(ACTION) = ($move ($dobj) ($loc .ME))
		($say "Dropped.\n")
		;

DWIMD = (($and ($eq ($verb) take)
	       ($ne ($loc ($dobj)) ($loc .ME))):
			($rtrn 0))
	(($and ($eq ($verb) drop)
	       ($ne ($loc ($dobj)) .ME)):
			($rtrn 0))
	($rtrn 1);

startrm (LDESC) =
		($say 
"You are in a small but comfortable room.  You hardly want
to leave, but there is a door leading east, if you insist.\n")
		;
startrm (SDESC) =
		($say "Comfortable room.\n");


brightroom(LDESC) =
		($say
"You are in a brightly lit room.  The walls sparkle with
scintillating lights.  There is a darker room to the west.\n");

brightroom(SDESC) = ($say "Bright room.\n");


ADJECTIVE red, blue;
NOUN red pillow(startrm);
NOUN blue pillow(startrm);
red pillow(LDESC) = ($say "There is a red pillow here.\n");
red pillow(SDESC) = ($say "A red pillow");
blue pillow(LDESC) = ($say "There is a blue pillow here.\n");
blue pillow(SDESC) = ($say "A blue pillow");


NOUN platinum(brightroom); bar=platinum;
platinum(LDESC) = ($say "There is a bar of platinum here!\n");
platinum(SDESC) = ($say "Platinum bar");
platinum(ACTION) = 
		(($eq ($verb) drop) :
			(($eq ($loc .ME) ($loc [red pillow])):
				($say 
"The bar falls onto the red pillow, breaking it!  The symbolism
impresses itself upon you, and you go back to work instead of
playing these silly games!\n")
				($spec 3 0 0 0 0)
			)
		);

VERB north, south, east, west;
n=north; s=south; e=east; w=west;

cg = ($say "You can't go that way.\n");
n(ACTION) = cg;
s(ACTION) = cg;
e(ACTION) = cg;
w(ACTION) = cg;

startrm(ACTION) =  (($eq ($verb) east) : 
				($move .ME brightroom)
				($exit 1)		{ bypass default }
			);
brightroom(ACTION) = (($eq ($verb) west) : ($move .ME startrm)($exit 1));

START = ($sdem Rdesc);

