{*** TRANSITIONS ***}
	hitms = (($eq ($verb) %1) :
			(%2 : ($move .ME %2))
			($setg GO TRUE)
			(%3: (($parm 3)))
		)
		;
	synvb = (($eq ($verb) %1) :
			($setg Verb %2));
road1(ACTION) = ($setv n s e w ne nw se sw u d)
		(hitms n gard1 0)
		(hitms s gard5 0)
		(hitms e road2 0)
		(hitms w road3 0)
		(hitms ne gard2 0)
		(hitms nw gard3 0)
		(hitms se gard6 0)
		(hitms sw gard7 0)
		;

road2(ACTION) = 
		(hitms n gard2 0)
		(hitms s gard6 0)
		(hitms e road7 0)
		(hitms w road1 0)
		(hitms ne gard2 0)
		(hitms nw gard1 0)
		(hitms se gard6 0)
		(hitms sw gard5 0)
		;

road3(ACTION) = 
		(hitms n gard3 0)
		(hitms s gard7 0)
		(hitms e road1 0)
		(hitms w road8 entr8)
		(hitms ne gard1 0)
		(hitms nw gard3 0)
		(hitms se gard5 0)
		(hitms sw gard7 0)
		;

road4(ACTION) = 
		(hitms n farm1 0)
		(hitms s road7 0)
		(hitms e frst1 0)
		(hitms w gard2 0)
		(hitms ne frst1 0)
		(hitms nw gard2 0)
		(hitms se frst1 0)
		(hitms sw gard2 0)
		;

road5(ACTION) = 
		(hitms n road7 0)
		(hitms s road6 0)
		(hitms e frst1 0)
		(hitms w gard6 0)
		(hitms ne frst1 0)
		(hitms nw gard6 0)
		(hitms se frst1 0)
		(hitms sw gard6 0)
		;

road6(ACTION) = 
		(hitms n road5 0)
		(hitms s town1 0)
		(hitms e frst1 0)
		(hitms w gard6 0)
		(hitms ne frst1 0)
		(hitms nw gard6 0)
		(hitms se frst1 0)
		(hitms sw gard6 0)
		;

road7(ACTION) = 
		(hitms n road4 0)
		(hitms s road5 0)
		(hitms e frst1 0)
		(hitms w road2 0)
		(hitms ne frst1 0)
		(hitms nw gard2 0)
		(hitms se frst1 0)
		(hitms sw gard6 0)
		;

road8(ACTION) = ($miss tress tress east8 west8 0 0 0 0 0 0);

east8 = 
	($setg GO TRUE)
	( ($eq @HWY8 0) : ($move .ME road3)
      { ELSE } :        (dump8 ($cont road8))
			($setg HWY8 ($minus @HWY8 1))
			(get8 ($cont roadx))
			(Slook 0 ($loc .ME))
			($setg LOOKP FALSE)
		);

west8 =
		($setg GO TRUE)
			(dump8 ($cont road8))
			($setg HWY8 ($plus @HWY8 1))
			(get8 ($cont roadx))
			(Slook 0 ($loc .ME))
			($setg LOOKP FALSE);

dump8 = (($link %1) : (dump8 ($link %1)))       { start at bottom }
	(($ne %1 .ME) :                 { Everything else leaves }
		($setp %1 RDLOC @HWY8)  { Retain its location on HWY 8 }
		($move %1 roadx)        { Put it in the box }
	);

get8 =  ( %1 :
		(get8 ($link %1))
		(($eq ($prop %1 RDLOC) @HWY8) :
			($move %1 road8)
		)
	);

entr8 =         (dump8 ($cont road8))
		($setg HWY8 0)
		(get8 ($cont roadx));






gard1(ACTION) = 
		(hitms n gard4 0)
		(hitms s road1 0)
		(hitms e gard2 0)
		(hitms w gard3 0)
		;

gard2(ACTION) = 
		(hitms n gard4 0)
		(hitms s road2 0)
		(hitms e gard2 0)
		(hitms w gard1 0)
		;

gard3(ACTION) = 
		(hitms n gard4 0)
		(hitms s road3 0)
		(hitms e gard1 0)
		(hitms w gard3 0)
		;

gard4(ACTION) = 
		(hitms n gard4 0)
		(hitms s gard1 0)
		(hitms e gard2 0)
		(hitms w gard3 0)
		;


gard5(ACTION) = 
		(hitms n road1 0)
		(hitms s gard8 0)
		(hitms e gard6 0)
		(hitms w gard7 0)
		;

gard6(ACTION) = 
		(hitms n road2 0)
		(hitms s gard8 0)
		(hitms e gard6 0)
		(hitms w gard5 0)
		;

gard7(ACTION) = 
		(hitms n road3 0)
		(hitms s gard8 0)
		(hitms e gard5 0)
		(hitms w gard7 0)
		;

gard8(ACTION) = 
		(hitms n gard5 0)
		(hitms s gard8 0)
		(hitms e gard6 0)
		(hitms w gard7 0)
		;




frst1(ACTION) = 
		(hitms n frst3 0)
		(hitms s frst1 0)
		(hitms e frst2 0)
		(hitms w frst2 0)
		;

frst2(ACTION) = 
		(hitms n frst1 0)
		(hitms s frst2 0)
		(hitms e frst2 0)
		(hitms w frst1 0)
		;

frst3(ACTION) = 
		(hitms n frst1 0)
		(hitms s frst4 0)
		(hitms e frst2 0)
		(hitms w frst2 0)
		;

frst4(ACTION) = 
		(hitms n frst3 0)
		(hitms s frst2 0)
		(hitms e frst1 0)
		(hitms w road7 0)
		;


farm1 (ACTION) =
		(synvb enter north)
		
		(hitms n farm2 0)
		(hitms s road4 0)
		(hitms e farm3 0)
		(hitms w farm4 0)
		(hitms ne farm3 0)
		(hitms nw farm4 0)
		;

farm2 (ACTION) =
		(synvb out s)
		(synvb enter n)
		(synvb east up)

		(hitms n farm6 0)
		(hitms s farm1 0)
		(hitms up farm7 0)
		;

farm3 (ACTION) =
		(hitms n farm5 0)
		(hitms s farm1 0)
		(hitms e frst1 0)
		(hitms nw farm5 0)
		(hitms sw farm1 0)
		;

farm4 (ACTION) =
		(hitms n farm5 0)
		(hitms s farm1 0)
		(hitms w gard4 0)
		(hitms ne farm5 0)
		(hitms se farm1 0)
		;

farm5 (ACTION) =
		(hitms e farm3 0)
		(hitms w farm4 0)
		(hitms se farm3 0)
		(hitms sw farm4 0)
		;

farm6 (ACTION) =
		(hitms out farm2 0)
		(hitms s farm2 0)
		;
farm7 (ACTION) =
		(hitms e farm2 0)
		(hitms d farm2 PanL)
		 ;


town1 (ACTION) =
		(hitms n road6 0)
		(hitms s town2 0)
		(hitms e town3 0)
		(hitms w town4 0)
		;

town2 (ACTION) =
		(hitms n town1 0)
		(hitms e town5 0)
		(hitms w town6 0)
		;

town3 (ACTION) =
		(hitms out town1 0)
		(hitms w town1 0)
		;

town4 (ACTION) =
		(hitms e town1 0)
		(hitms out town1 0)
		;

town5 (ACTION) =
		(hitms w town2 0)
		(hitms out town2 0)
		(TWN5x);

town6 (ACTION) =
		(hitms e town2 0)
		(hitms out town2 0)
		;


cel01 (ACTION) = 

		(hitms n cel02 0)
		(hitms s 0 ny)
		(hitms w cel04 0)
		(hitms u farm7 0)
		;
cel02 (ACTION) =
		(hitms n cel05 0)
		(hitms s cel01 0)
		(hitms e 0 ny)
		;

cel03 (ACTION) =
		(hitms s cel05 0)
		(hitms w cel13 0)
		(hitms ne cel09 0)
		(hitms d cel09 0)
		;

cel04 (ACTION) =
		(synvb down north)
		(hitms n cel10 0)
		(hitms e cel01 0)
		(hitms w cel08 0)
		;


MV56=($setg Cel6x 5);
MV76=($setg Cel6x 7);
MV67=( ($and ($ne @Cel6x 7)
	     ($prop dragon AWAKE)): ($say
"The ice dragon blocks your attempt to cross!\n\n")
		:         ($move .ME cel07) );

MV65=( ($and ($ne @Cel6x 5)
	     ($prop dragon AWAKE)): ($say
"The ice dragon blocks your attempt to leave!\n")
		:         ($move .ME cel05) );


cel05 (ACTION) =
		(hitms n cel03 0)
		(hitms s cel02 0)
		(hitms ne cel06 MV56)
		;

cel06 (ACTION) =
		(hitms e 0 MV67)
		(hitms w 0 MV65)
		(($eq ($loc .ME) cel06) {still here}:
			($setg LOOKP TRUE)
		)
		  ;

cel07 (ACTION) =
		(synvb out west)
		(hitms w cel06 MV76)
		;

cel08 (ACTION) =
		(hitms e cel04 0)
		(hitms w 0 ny)
		;

cel09 (ACTION) =
		(hitms n 0 ny)
		;

cel10 (ACTION) =
		(hitms s cel04 0)
		(hitms d cel11 0)
		;

cel11 (ACTION) =
		(hitms e cel12 0)
		(hitms n 0 ny)
		;

cel12 (ACTION) =
		(hitms n cel11 0)
		(hitms e 0 ny)
		;

cel13 (ACTION) = (($eq ($verb) down):
			(($prop cel13 HOLED):
				($say "You plunge into the icy waters!\n")
				($move .ME cel14)
				($exit 1)
			)
		  )
		  (($eq ($verb) drop):
		    (cdrop)
		    (($not ($prop cel13 HOLED)):
			(($ge ($prop ($dobj) WEIGH)
			      ($prop [red crystal] WEIGH)
			 ):
				($say 
"You have broken the ice!  You watch helplessly as ")
				(($sdesc ($dobj)))
				($say "
sinks swiftly into the dark and frigid depths.\n")
				($setp cel13 HOLED TRUE)
				($move hole cel13)
				($move ($dobj) .ALL)
				($exit 1)
			{else}:			{give him a hint}
				($say
"The icy floor chips a little, but remains intact.\n")
			)
		   )
		   ($exit 1)
		)
		(hitms e cel03 0)
		(hitms w 0 ny)
		;

cel14 (ACTION) =
		(($eq ($verb) up):
			(($prop cel13 HOLED):
			    ($say "You pop out through a hole in the ice!\n")
			    ($move .ME cel13)
			    ($exit 1)
		:{else}
			    ($say  "You are blocked by an icy roof above!\n")
			 )
		 )
		(hitms w cel15 0)
		
		(($eq ($loc .ME) cel14):	{ Poor sap didn't move.}
				($say
"Your breath expires.  The last thing you feel is the odd sensation
of water filling your lungs.  ") (die)
		);
