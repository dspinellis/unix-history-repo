ADJECTIVE red,green,blue,clear;
ADJECTIVE glass;

NOUN red message(road2);        { It's red so there can be others elsewhere }
red message(WEIGH) = CAPAC;
red message(LDESC) = ($say "There is a message scratched in the dirt.\n");
red message(SDESC) = ($say "a message in the dirt");
red message(ACTION) = (($eq ($verb) read):
			($say "'in' is a preposition.  'enter' is a verb.\n")
			($exit 1)
		   );

NOUN keys (town4);
keys(WEIGH) = 2;
keys(LDESC) = ($say "There's a set of keys here.\n");
keys(SDESC) = ($say "a set of keys");

NOUN toolbox (farm5);
toolbox(WEIGH) = CAPAC;
toolbox(HOLDS) = 50;
toolbox(OPENS) = TRUE;
toolbox(LOCKS) = TRUE;
toolbox(LOCKD) = TRUE;
toolbox(LIGHT) = TRUE;
toolbox(LDESC) = (($eq ($loc .ME) toolbox):
			($say
"You are in a huge wooden structure, towering up at least 80 feet
above your head, with wooden walls formed of immense 10-foot by 50-foot
boards.  Everything here, even the grains of dust which cover the wooden
floor, seems immense.  ")
			(($prop toolbox OPEN):
				($say
		      "Above your head, the top of the structure is open,
and sunlight streams in.\n")
		:               ($say
		     "The structure is closed at the top, but
enough light comes in from between the boards to enable you to see.\n")
			)
	: {else} ($say
					       "  Against
the house is a heavy ")
		(($prop toolbox OPEN):
			($say "toolbox with the lid open.\n")
	{               (($cont toolbox) :
				($say "In the toolbox, you see:\n")
				($setp toolbox CONTS TRUE)
				(Slook 1 ($cont toolbox))
				($setp toolbox CONTS FALSE)
			)}
		:{else} (($prop toolbox LOCKD) :
				($say "toolbox with a rusty lock, painted
with an insignia composed of blue and green squares, joined at
their corners.\n") :
				($say "toolbox, the lid of which is ajar.\n")
			)
		)
	);

toolbox(SDESC) =
		(($eq ($loc .ME) toolbox):
			($say   "Inside huge box.\n")
		:{else} ($say "a toolbox"));

toolbox(ACTION) =
	(($eq ($loc .ME) toolbox) :
		(($or ($eq ($verb) drop)
		      ($eq ($verb) throw)):
			($say "You just lost ")(($sdesc ($dobj)))
			($say " in a crack between the wooden floorboards!\n")
			($move ($dobj) .ALL)
		)
		($miss cg cg cg cg cg cg cg cg cg cg)

	{else} :
		(($eq ($verb) lock):
		    (Lockup toolbox keys "The toolbox seals with a CLICK!\n"))
		(($eq ($verb) unlock):
		    (Ulock toolbox keys
"One of the keys fits!  The toolbox can be opened now.\n")
		)
	);

NOUN signpost(roadx);
signpost(RDLOC)=10;
signpost(WEIGH)=CAPAC;
signpost(LDESC)=($say "There is a signpost by the side of the road.\n");
signpost(SDESC)=($say "a signpost");
signpost(ACTION) = (($eq ($verb) read):
			($say
"Pointing east, it says: 'Unuchevala: 10 miles'\n")
			($exit 1)
		    );
NOUN globe(toolbox);
globe(WEIGH) = 20;
globe(LDESC) = (($prop globe LIGHT):
			($say "A globe of stone glows brightly here.\n")
		{else}:
		  ($say "There is a strange globe of polished stone here.\n")
		);
globe(SDESC) = (($prop globe LIGHT): ($say "a glowing globe of stone")
				   :  ($say "a stone globe"));
globe(ACTION) = (($eq ($verb) rub):
		    (($prop globe LIGHT):
			($say "The light from the stone globe fades away.\n")
			($setp globe LIGHT FALSE)
		   {else}:
			($say "The stone globe glows brightly!\n")
			($setp globe LIGHT TRUE)
		    )
		    ($setg LOOKP TRUE)
		    ($exit 1)
		)
		;


NOUN crowbar (town3);
crowbar(WEIGH) = 100;
crowbar(LDESC) = ($say "There is a long crowbar here.\n");
crowbar(SDESC) = ($say "a crowbar");

crydie =
			($say
"What POWER!  The magic crystal releases a flood of energy in
a split-second!  Unfortunately, you were a little close...\n")
			(die);



     { Dragon Stuff }
	AWAKE=MISC1;

NOUN dragon (cel06);


white=clear;
NOUN green crystal(cel07);
NOUN red crystal(cel08);
NOUN blue crystal(cel10);
NOUN clear crystal(cel09);

{NOUN diamond  { = clear crystal;
{NOUN sapphire{=  blue crystal;
{NOUN ruby (cel08){= red crystal;
{NOUN emerald (cel07){= green crystal;}


green crystal(WEIGH)=15;
green crystal(POINT)=25;
green crystal(LDESC) = ($say "There is a beautiful green crystal here!\n");
green crystal(SDESC) = ($say "a green crystal");

green crystal(ACTION) =
	(($eq ($verb) take):
	       (($eq ($loc .ME) cel07):
			(($not ($prop dragon AWAKE)):
				($say "You hear a stirring in the Ice Cavern.\n")
				($setg Tempr ($plus @Tempr 2))
				($setp dragon AWAKE TRUE)
				($setp cel06 VISIT FALSE)

			:{else} ($setg Tempr 0)
			)
		)
	)
	(($eq ($verb) drop):
	       (($eq ($loc .ME) cel07):
			($setg Tempr ($minus @Tempr 2))
			(($le @Tempr 0) :
				($setp dragon AWAKE FALSE)
				($say
"You hear a deep  Y A W N  from the Cavern.\n"))
			($setp cel06 VISIT FALSE)
		)
	)
	(($eq ($verb) touch):
		(($eq ($dobj) [red crystal]) :
			($say "The red and green crystals flare briefly!\n")
			(Grow)
			($move .ME cel01)
			($setg LOOKP TRUE)
			($exit 1)
		)
		(($eq ($dobj) [blue crystal]) :
			($say "The blue and green crystals flare briefly!\n")
			($move .ME toolbox)
			(Shrink)
			($setg LOOKP TRUE)
			($exit 1)
		)
	)
	(($eq ($verb) break):
		(($eq ($dobj) [green crystal]):
			(crydie)
		)
	);
ROUTINE CRout;
WORKD = MISC1;			{ Has Clear crystal shed its light? }
red crystal(WEIGH)=15;
red crystal(POINT)=25;
red crystal(LDESC) = ($say "There is a beautiful red crystal here!\n");
red crystal(SDESC) = ($say "a red crystal");
red crystal(ACTION) =
	(($eq ($verb) touch):
		(($eq ($dobj) [clear crystal]) :
		    (($not ($prop [clear crystal] WORKD)):
			($say "The red and clear crystals flare briefly!\n")
			($setp [clear crystal] WORKD TRUE)
			($setp [clear crystal] LIGHT TRUE)
			($say
"The clear crystal blazes forth with a magical incandescence brilliant
enough to penetrate even the deepest darkness!!\n\n")
			($sfus CRout 4)
			($setg LOOKP TRUE)
			($exit 1)
	     {else} :
			($say
"The clear crystal seems curiously inert now.\n")
			($exit 1)
		    )
		)
		(($eq ($dobj) [green crystal]) :
			($say "The red and green crystals flare briefly!\n")
			(Grow)
			($move .ME cel01)
			($setg LOOKP TRUE)
			($exit 1)
		)
	)
	(($eq ($verb) break):
		(($eq ($dobj) [red crystal]):
			(crydie)
		)
	);


blue crystal(WEIGH)=15;
blue crystal(POINT)=25;
blue crystal(LDESC) = ($say "There is a beautiful blue crystal here!\n");
blue crystal(SDESC) = ($say "a blue crystal");
blue crystal(ACTION) =
	(($eq ($verb) touch):
		(($eq ($dobj) [red crystal]) :
			($say "The red and blue crystals flare briefly!\n")
			($exit 1)
		)
		(($eq ($dobj) [green crystal]) :
			($say "The blue and green crystals flare briefly!\n")
			($move .ME toolbox)
			(Shrink)
			($setg LOOKP TRUE)
			($exit 1)
		)
	)
	(($eq ($verb) break):
		(($eq ($dobj) [blue crystal]):
			(crydie)
		)
	);


CRout = ($say "The glowing magical crystal seems to have gone dark.\n> ")
	($setp [clear crystal] LIGHT FALSE)
	;
clear crystal(WEIGH)=15;
clear crystal(POINT)=25;
clear crystal(LDESC) = ($say "There is a beautiful clear crystal here")
		       (($prop [clear crystal] LIGHT): ($say " (GLOWING!)"))
		       ($say "!\n");
clear crystal(SDESC) = ($say "a clear crystal")
		       (($prop [clear crystal] LIGHT): ($say " (GLOWING!)"))
		       ;
clear crystal(ACTION) =
	(($eq ($verb) touch):
		(($eq ($dobj) [red crystal]) :
		    (($not ($prop [clear crystal] WORKD)):
			($say "The red and clear crystals flare briefly!\n")
			($setp [clear crystal] WORKD TRUE)
			($setp [clear crystal] LIGHT TRUE)
			($say
"The clear crystal blazes forth with a magical incandescence brilliant
enough to penetrate even the deepest darkness!!\n\n")
			($setg LOOKP TRUE)
			($sfus CRout 4)
			($exit 1)
	     {else} :
			($say
"The clear crystal seems curiously inert now.\n")
			($exit 1)
		    )
		)
	)
	(($eq ($verb) break):
		(($eq ($dobj) [red crystal]):
			(crydie)
		)
	);



ROUTINE DRdem;          { Dragon Daemon.  Increases temper in .my presence }
DRdem=  (($not ($prop dragon AWAKE)): ($exit 0))
	(($eq ($loc .ME) ($loc dragon)):
		(($eq ($loc [green crystal]) .ME):      { even worse! }
			($setg Tempr ($plus @Tempr 1)))
		($setg Tempr ($plus @Tempr 1))  )
{       ($say "His temper is now at ")($num @Tempr)($say "\n")  }
	(($ge @Tempr 7):
		($say
"Jeez, I didn't know ice dragons could reach their boiling point!
In a final flare of rage, he opens his mouth and breathes his frigid
breath in a blast in your direction.  It's a bit much for you.\n")
		(die)
	 )
	(($ge @Tempr 6) :
		($say "Gee, he looks like he's really at the edge!\n")
		($exit 0))
	(($ge @Tempr 5) :
		($say "This is one upset dragon!  Be careful!\n")
		($exit 0))
	(($ge @Tempr 3) :
		($say "He's getting angrier...\n")($exit 0))
		;

dragon(WEIGH)=CAPAC;
dragon(LDESC)=
		(($prop dragon AWAKE):
			($say
"There is a fierce ice dragon glaring balefully in your direction.\n")
		:{else}
			($say
"There is a large white dragon sleeping peacefully in the middle
of the cavern floor.\n")
		)
		(DRdem);
dragon(SDESC)=
		(($prop dragon AWAKE): ($say "a fierce dragon")
				     : ($say "a somnolent dragon")
		)
		(@LOOKP: ($say "\n") (DRdem))
		;

dragon(ACTION)=                 { Man, the things you can try here... }
	(($prop dragon AWAKE):          { This is the harder stuff }
		(($eq ($verb) strike):
			($say "This just seems to get him angrier!\n")
			($setg Tempr ($plus @Tempr 1))
			($exit 1))
		(($eq ($verb) throw):
			(($eq ($iobj) dragon):
				(($gt ($prop ($dobj) WEIGH) 75):
					($say
			"This just bruises him!  Now he's getting mad!\n")
					($setg Tempr ($plus @Tempr 1))
					($exit 1) :
				{else}
					($say
			"That object is just too light to hurt him.\n")
					($exit 1)
				)
			:       ($say "This amuses the dragon no end!\n")
				($exit 1)
			))              { end of Throw  case}
	:{else}
					{ he's snoozing}
		(($eq ($verb) wake):
			($say "You manage to waken him.  He's not happy.\n")
			($setg Tempr 1)
			($setp dragon AWAKE TRUE)
			($exit 1))
		(($eq ($verb) strike):
			($say "Now you woke him up!  He's upset, too!\n")
			($setg Tempr 2)
			($setp dragon AWAKE TRUE)
			($exit 1))
		(($eq ($verb) throw):
			(($eq ($iobj) dragon):
				(($gt ($prop ($dobj) WEIGH) 75):
					($say
	"It's just heavy enough to waken him.  The bruise doesn't help
his temper any either.\n")
					($setp dragon AWAKE TRUE)
					($setg Tempr 3)
					($exit 1) :
				{else}
					($say
			"That object is just too light to wake him.\n")
					($exit 1)
				)
			:       ($say "Don't hurt yourself trying!\n")
				($exit 1)
			))              { end of Throw  case}
		);




NOUN bed (farm7);
bed(WEIGH) = CAPAC;
bed(SDESC) = ($say "an old, rickety bed");

bed(ACTION) =
	(($eq ($verb) move):
		(($prop bed OPEN) :
			($say "Stop messing with the bed, it's fragile!\n")
			($exit 1)
		)
		($say
"Moving the bed seems to have loosened one of the wall panels
on the west wall.\n")
		($setp bed OPEN TRUE)
		($exit 1)
	);

NOUN panel (farm7);
panel(OPENS) = TRUE;
panel(WEIGH) = CAPAC;
panel(ACTION) =
	(($or ($eq ($verb) open)
	      ($eq ($verb) pry) ):
		(($and ($eq ($iobj) crowbar)
		   ($eq ($loc crowbar) .ME)):
			(($not ($prop bed OPEN)):
				($say "There aren't any loose enough.\n")
				($exit 1))
			($say
"The loose panel comes away, revealing a secret stairway down, down...\n")
			($setp panel OPEN TRUE)
	  {else}:
			($say "You can't seem to get the panel open.\n")
		)
		($exit 1)

	);

NOUN knife (farm6);
knife(LDESC)=
	(($prop farm6 VISIT):
		($say "There is a carving knife here.\n")
	:{else} ($say "However,
on the table is a large carving knife (suitable for blind mice).\n")
	);
knife(SDESC)=($say "a carving knife");



NOUN bottle(town6);
bottle (LDESC) = ($say "There is a bottle here.\n");
bottle (SDESC) = ($say "a bottle");
bottle (TRANS) = TRUE;
bottle (ACTION) =
		(($eq ($verb) open):
			($say "Hm, the bottle is somehow sealed shut.\n")
			($exit 1))
		(($eq ($verb) break):
			($say "Wow, is that heavy glass!  It won't break!\n")
			($exit 1))
		(($eq ($verb) strike):
			(($eq ($iobj) bottle):
			   ($say "Weird thing to do with it.\n")
		     :     ($say "Nice try, but it isn't even scratched.\n")
			)
			($exit 1));

NOUN ship(bottle);
ship (LDESC) = ($say "There is a model ship here.\n");
ship (SDESC) = ($say "a model ship");
ship (POINT) = 50;

ROUTINE TWN5y;
NOUN well(town5);
well (WEIGH) = CAPAC;
well (ACTION) =
	(($eq ($iobj) well):
		(($eq ($verb) put):
			(cdrop)
			(TWN5y)
			($exit 1)
		)
	);

NOUN insignia;
insignia (WEIGH) = CAPAC;
insignia (ACTION) = ($say "There's nothing useful to do with the insignia.\n")
		    ($exit 1);
square=insignia;

NOUN glass box(cel08);
glass box(HOLDS)=1;
glass box(TRANS)=TRUE;
glass box(OPENS)=TRUE;
glass box(OPEN)=FALSE;
glass box(SHRNK)=TRUE;          { First seen as a tiny box }
glass box(LDESC) =
		(($eq ($prop .ME SHRNK) ($prop [glass box] SHRNK)):
			($say "There is a big glass case here.\n")
			($setp [glass box] WEIGH CAPAC)
			($setp [glass box] HOLDS 50)

		{else}:
			(($prop .ME SHRNK):
				($say
"Before you looms a huge glass wall, inscribed with the words,
	'program error!'\n")
			{else} :
				($say
"There is a tiny glass box with a snap lid here.\n")
				($setp [glass box] WEIGH 5)
				($setp [glass box] HOLDS 1)

			)
		);
glass box(SDESC) =
		(($eq ($prop .ME SHRNK) ($prop [glass box] SHRNK)):
			($say "a glass case")
			($setp [glass box] WEIGH CAPAC)
			($setp [glass box] HOLDS 50)

		{else}:
			(($prop .ME SHRNK):
				($say
"a huge glass wall, inscribed with the words,
	'program error'")
			{else} :
				($say "a tiny glass box")
				($setp [glass box] WEIGH 5)
				($setp [glass box] HOLDS 1)

			)
		);
case=glass box;

NOUN sand(glass box);
sand(SHRNK)=TRUE;
sand(WEIGH)=1;
sand(LDESC)=(($and ($prop sand SHRNK)
		   ($not ($prop .ME SHRNK))):
			($setp sand POINT 0)
			($say "There is a grain of sand here.\n")

		{else}: ($say "There is a beautiful porcelain statue here!\n")
			($setp sand POINT 50)
		);
sand(SDESC)=(($and ($prop sand SHRNK)
		   ($not ($prop .ME SHRNK))):
			($setp sand POINT 0)
			($say "a grain of sand") :
			($say "a porcelain statue")
			($setp sand POINT 50)
		);
statue=sand;
sand(ACTION)=(($eq ($verb) take):
		      (($and ($prop sand SHRNK)
			     ($not ($prop .ME SHRNK))):
	    ($say "You fumble the grain of sand and lose it on the ground.\n")
					($move sand .ALL)
					($exit 1)
			)
		);

NOUN hole;


NOUN rock(road5);
rock(LDESC)=($say "There is a plain-looking rock here.\n");
rock(SDESC)=($say "an ordinary rock");
rock(ACTION)=
		(($and ($eq ($verb) take)
		       ($eq ($dobj) rock)):
				($say "Mmph!  Heavy!\n"));
rock(WEIGH)=100;
{
{NOUN saw(toolbox);
{saw(WEIGH) = 20;
{saw(LDESC) = ($say "There is a saw here.\n");
{saw(SDESC) = ($say "a saw");
{saw(ACTION) = (($eq ($verb) cut):
{			(($eq ($dobj) hole):
{				(($eq ($loc .ME) cel13):
{					(($prop cel13 HOLED):
{						($say "There's already one here!\n")
{						($exit 1)
{				:{else}
{						($say
{"You cut a hole in the thick ice; icy water sloshes about two feet down.\n")
{						($setp cel13 HOLED TRUE)
{						($move hole cel13)
{						($exit 1)
{					)
{				)
{			   { cutting a hole somewhere else }
{			   ($say "There's nowhere to cut a hole.\n")
{			   ($exit 1)
{			)
{			($say "The saw isn't used that way.\n")
{			($exit 1)
{		);	/* I Know A better way to make a Hole */}
