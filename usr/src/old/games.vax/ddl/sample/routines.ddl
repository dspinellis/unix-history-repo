ROUTINE VisP;
VisP =                          { (Visp List Propno) => True IFF an object }
(($eq %1 0) : ($rtrn FALSE))   {       is visible on List that has Propno }
(($prop %1 %2) :
	($rtrn TRUE))                   { Found one! }
(($or ($prop %1 OPEN)
      ($prop %1 TRANS)):                { Look inside...}
	( (VisP ($cont %1) %2): ($rtrn TRUE) ))
($rtrn (VisP ($link %1) %2))
;

ROUTINE Reach;                  { (Reach obj loc) => True IFF the obj   }
Reach =                         {       IS the loc or can be reached    }
(($eq %2 0) : ($rtrn FALSE))    {       by the loc }
(($eq %1 %2): ($rtrn TRUE))
			{ Still explore inside }
(($prop %2 OPEN):
	((Reach %1 ($cont %2)): ($rtrn TRUE))
)
($rtrn  (Reach %1 ($link %2)))

;

ROUTINE See;                  { (See obj loc) => True IFF the obj   }
See   =                         {       IS the loc or can be seen       }
(@DARKG: ($rtrn FALSE))		{ Can't see in a dark room! }
(($eq %2 0) : ($rtrn FALSE))
(($eq %1 %2): ($rtrn TRUE))
(($or  ($prop %2 TRANS)                 { Still explore inside }
       ($prop %2 OPEN)):
	((See   %1 ($cont %2)): ($rtrn TRUE))
)
($rtrn  (See   %1 ($link %2)))
;

Avail = 
	(($eq ($loc %1) .ME) : ($rtrn TRUE))	{ shd be closure, really }
	(($not %1):($say "The what?\n")($exit 1))
	(($not (See %1 ($cont ($loc .ME)))):
		($say "I can't see that item here.\n")
		($exit 1)
	  )
	(($not (Reach %1 ($cont ($loc .ME)))):
		($say "I can't get at that item.\n")
		($exit 1)
	  )
	(($and ($prop .ME SHRNK)
	       ($not ($prop %1 SHRNK))  ):
		       (($ne ($loc %1) .ME):
				($say
"Right now, ")(($sdesc %1))($say " is too big for you to deal with.\n")
				($exit 1)
			)
	  )
	($rtrn TRUE)
;

ROUTINE LitP;
LitP =                          { (LitP Room) => True IFF Room is lit }
(($prop %1 LIGHT) : ($rtrn TRUE))
(($or (VisP ($cont %1) LIGHT)
      (VisP ($cont %1) FLAME)): ($rtrn TRUE))

(($or (VisP ($cont .ME) LIGHT)                  { Check .ME 'cause invisibl }
      (VisP ($cont .ME) FLAME)): ($rtrn TRUE))
($rtrn FALSE)
;

ROUTINE Blank;
Blank =                         { Blank n => Type n blanks }
(($gt %1 0):
	($say "  ")
	(Blank ($minus %1 1)));

ROUTINE Llook; ROUTINE Slook;
Llook =                         { (Llook Level Object) describes Object }
(($eq %2 0) : ($exit 0))
(($eq %1 0) :                   { Level 0 == This is a room.  Check lighting }
	((LitP %2):
		($setg DARKG FALSE)
		(($ldesc %2))           { Talk about the room }
		(($not @DARKG):
			(Llook 1 ($cont %2))    { Talk about its contents }
		)
	{else}:
		($say "It's mighty dark in here!\n")
		($setg DARKG TRUE)
	)

{else} :                        { Level > 0 == This is a list of objs }
	( ($ldesc %2)   :       { Talk (only) about the visible }
		(Blank %1)                      { Indent }
		(($ldesc %2))           { Blurb the object }

		( ($cont %2):      { something inside it...}
			(($or ($prop %2 OPEN)($prop %2 TRANS)):
				(Blank %1)
				($say "It contains:\n")
				($setp %2 CONTS TRUE)
				(Slook ($plus %1 1) ($cont %2))
					{ Short descriptions for contents }
			)
		)
	 )
	(Llook %1 ($link %2))
);


Slook =                         { (Llook Level Object) describes Object }
(($eq %2 0) : ($exit 0))
(($eq %1 0) :                   { Level 0 == This is a room.  Check lighting }
	((LitP %2):
		($setg DARKG FALSE)
		(($sdesc %2))           { Talk about the room }
		(($not @DARKG):
			($setp %2 CONTS FALSE)
			(Slook 1 ($cont %2))    { Talk about its contents }
		)
	{else}:
		($say "It's mighty dark in here!\n")
		($setg DARKG TRUE)
	)

{else} :                        { Level > 0 == This is a list of objs }
	(($sdesc %2) :          { Talk (only) about the visible }
		(($not ($prop ($loc %2) CONTS)):
			(Blank ($minus %1 1))
			($say "You can see:\n")
			($setp ($loc %2) CONTS TRUE)
		)
		(Blank %1)                      { Indent }
		(($sdesc %2))           { Blurb the object }
		(($and ($ne ($cont %2) 0)       { something inside it...}
		       ($or ($prop %2 OPEN)     {...and you can see it }
			    ($prop %2 TRANS)
			)
		  ):

			($setp %2 CONTS TRUE)
			($say ", containing:\n")
			(Slook ($plus %1 1) ($cont %2))
					{ Short descriptions for contents }
		   :    ($say "\n")

		)
	)
	( Slook %1 ($link %2))

);




LOOK =



	($setg WASDK @DARKG)
	(@LOOKP :
		(($prop ($loc .ME) VISIT):
			(Slook 0 ($loc .ME))
		 {else} :
			(Llook 0 ($loc .ME))
			($setp ($loc .ME) VISIT TRUE)
		)
		(@DARKG : ($setp ($loc .ME) VISIT FALSE))
	)
	($setg LOOKP FALSE)
	($itun)
	($say "\n> ")
;

ROUTINE GrowX; ROUTINE ShrnX;
GrowX = (($prop %1 SHRNK):
		($setp %1 SHRNK FALSE)
		(($cont %1):
			(GrowX ($cont %1)))
		(($link %1):
			(GrowX ($link %1)))
	)
	;

Grow = (($prop .ME SHRNK):
		($setp .ME SHRNK FALSE)
		(($cont .ME):
			(GrowX ($cont .ME)))
	)
;

ShrnX = (($not ($prop %1 SHRNK)):
		($setp %1 SHRNK TRUE)
		(($cont %1):
			(ShrnX ($cont %1)))
		(($link %1):
			(ShrnX ($link %1)))
{else}  :
		($say "You hear a tiny POP as ")(($sdesc %1))
		($say " vanishes completely!\n")
		(($link %1):
			(ShrnX ($link %1)))
		($move %1 .ALL)
	)

;

Shrink = (($not ($prop .ME SHRNK)):
		($setp .ME SHRNK TRUE)
		(($cont .ME):
			(ShrnX ($cont .ME)))
	)
;

WzTgl =			{ Toggle the Wizard flag }
	($setg Wizrd ($not @Wizrd))
	(@Wizrd:
		($say
"You hear a low rumble of thunder, shaking the very ground on
which you stand.  Suddenly, there is a blazing flash of light!!
You are unharmed, but feel great power flowing in your body.\n")
	{else}:
		($say
"Your wizardly powers unceremoniously fade away.\n")
	);
