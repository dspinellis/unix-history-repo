.ALL(LDESC) = ($say "You shouldn't be here at all.  This is THE COSMIC ALL.\n")
		($exit 1);
.ALL(SDESC) = ($say "THE COSMIC ALL");

road1(SDESC) = rdsc;
road1(LDESC) = ($say "You're on a dirt road going east-west.  Tall stalks of
corn grow on either side of the road.\n");

road2(SDESC) = rdsc;            road2(LDESC) = rdsc;
road3(SDESC) = rdsc;            road3(LDESC) = rdsc;
road4(SDESC) = rdsc;            road4(LDESC) = rdsc;
road5(SDESC) = rdsc;            road5(LDESC) = rdsc;
road6(SDESC) = rdsc;            road6(LDESC) = rdsc;

road7(LDESC) = ($say "A road from the west forks north and south here.
To the east is a dense-looking forest; cornfields are seen on the
west.\n");

road7(SDESC) = ($say "You're at the fork in the road.\n");

road8(SDESC) = rdsc;
road8(LDESC)=($say
"The road stretches a long way east-west.  A short fence divides the
road from the cornfields beyond.  Please don't tresspass.\n");


ROUTINE dump8; ROUTINE get8;    ROUTINE entr8;  ROUTINE east8; ROUTINE west8;



gard1(SDESC) = gdsc;            gard1(LDESC) = gdsc;
gard2(SDESC) = gdsc;            gard2(LDESC) = gdsc;
gard3(SDESC) = gdsc;            gard3(LDESC) = gdsc;
gard4(SDESC) = gdsc;            gard4(LDESC) = gdsc;
gard5(SDESC) = gdsc;            gard5(LDESC) = gdsc;
gard6(SDESC) = gdsc;            gard6(LDESC) = gdsc;
gard7(SDESC) = gdsc;            gard7(LDESC) = gdsc;
gard8(SDESC) = gdsc;            gard8(LDESC) = gdsc;

frst1(SDESC) = fdsc;            frst1(LDESC) = fdsc;
frst2(SDESC) = fdsc;            frst2(LDESC) = fdsc;
frst3(SDESC) = fdsc;            frst3(LDESC) = fdsc;
frst4(SDESC) = fdsc;            frst4(LDESC) = fdsc;

farm1(SDESC) = ($say "Entrance to farm.\n");
farm1 (LDESC) = ($say
"You are at the entrance on the south side of a rundown
farmhouse.  Someone has painted 'DIE, HEATHENS' on the
front door, which barely stays on its hinges.  Marks of
smoke and flame are visible.  A road runs to the south,
and you may walk around the farmhouse by going east or west.\n");

farm2(LDESC) = ($say
"You are in the entry hall of a long-deserted old farmhouse.
The floor is covered with the dust of years of disuse.  The
front door to the south is open, and there is another room
to the north.  A stairway on the east side of the room goes up.\n");
farm2(SDESC) = ($say "Entry hall\n");


farm3(LDESC) = ($say
"You are on the east side of the farmhouse.  To the east, a forest
stretches away into the distance.  The entrance to the farmhouse
is to the south.\n");
farm3(SDESC) = ($say "East side of the farmhouse\n");

farm4(LDESC) = ($say
"You are on the west side of the farmhouse.  The cornfields
stretch away forever to the west.  The entrance to the house
is to the south.\n");
farm4(SDESC) = ($say "West side of the farmhouse\n");

farm5(LDESC) = ($say
"You are on the north side of the farmhouse.  A high fence
separates you from the cornfields to the north.");
farm5(SDESC) = ($say "North side of the farmhouse\n");


farm6(LDESC) = ($say
"You are in an old, empty farmhouse kitchen.  All the
cupboards and cabinets are bare (unless you can eat the
thick layer of dust which covers everything).")
	       (($prop farm6 VISIT):($say "\n"));
farm6(SDESC) = ($say "Farmhouse kitchen\n");

farm7(LDESC) = ($say
"You are in the upstairs bedroom of the farmhouse.  All the
windows are boarded up, and nobody has slept here for years.
To the east is an exit leading to the entry hall below.\n")

	(($prop bed OPEN ) :
		($say
"In the middle of the room is an old, rickety bed.   The west
wall ")
		(($prop panel OPEN) :
			($say
     "has a panel missing, revealing a secret stairway down.\n")
	{else}:         ($say
     "seems to have a loose panel.\n")
		)
 {else} :
		($say
"Against the west wall is an old, rickety bed (which may be
why nobody has slept here for years).\n")
	);
farm7(SDESC) = ($say "Upstairs bedroom\n");

PanL =                  { Try to go through panel }
	(($prop panel OPEN):
		($move .ME cel01)
		($say "You descend into the gloom...\n")
	{else}: (($eq ($loc .ME) farm7):(cg))
	);


town1(SDESC) = ($say "One horse town.\n");
town1(LDESC) =  ($say
"You are at the northern end of a one-horse town.  Although the buildings
are in good shape, all seem deserted.  You can't even find one horse.
To the east is an old smithy.  On the west side of the street is an
abandoned hotel.  The street runs north and south.\n");

town2(SDESC) = ($say "One horse business district.\n");
town2(LDESC) = ($say
"You are at the southern end of a one-horse town.  This is the
business district.  On the east side of the street is an old
building with a tastefully lettered sign, 'Arpa-Citizen's Savings'.
To the west is the legendary 'Packet Inn'.  The street runs north-south.\n");

town3(SDESC) = ($say "Blacksmith's Shop.\n");
town3(LDESC) = ($say
"You're in what is left of a once-thriving blacksmith's shop.
Long ago abandoned, nothing much of interest remains.\n");

town4(SDESC) = ($say "Flophouse.\n");
town4(LDESC) = ($say
"You're in the Flophouse hotel.  Never worth the visit even in
its heyday, a quick search reveals nothing but a lot of dusty rooms.\n");

town5(SDESC) = ($say "Arpa-Citizen's Savings.\n");
town5(LDESC) = ($say
"You are in the ancient and venerable institution of Arpa-Citizen's
Savings (there's a branch in your town).  The lobby is tastefully
decorated in marble.  The only feature is a strange sweet-smelling
well in the center of the room.  A mysterious plaque on the side ")
		($say
"reads:
	'Drop your bits here!'
");
TWN5x =
	(($eq ($verb) drop): (TWN5y));
TWN5y =         
		(cdrop)			{ Do the right thing }
		($say
"A strange glowing network of lights appears, and you can read:\n")
		(($prop ($dobj) POINT):
			($setg SCORE ($plus @SCORE ($prop ($dobj) POINT)))
			($move ($dobj) .ALL)
			($say "   Your account is now at $")($num @SCORE)
($say ".\n   Thank you for letting Arpa-Citizen's be your host.\n")
		{else}:
			($say "   Your deposit is worthless")
			(($pct 25):
				($say ", but thanks for the trinket!\n")
				($move ($dobj) .ALL)
			{else}:
				($say "!  Keep it!\n")
			)
		)
		($say "The Arpa network mercifully disappears.\n")
		($exit 1)			{ Short circuit the verb }
		;

town6(SDESC)  = ($say "The Packet Inn.\n");
town6(LDESC)  =
	($say
"You are at that famous old saloon, the Packet Inn.  Thousands
of the local adventurers drank 'till they were totally zorked in
this colorful old haunt.  A little wandering convinces you that
nothing much is to be seen.\n");

{*** DUNGEON PROPER ***}
cel01(LDESC) = ($say
"You are in a secret cellar below the farmhouse.  In the
corner of one wall is a strange insignia composed of red
and green squares, joined at the corners.  Passages
lead to north, west and south.  An old stairway leads up and out.\n")
($move insignia cel01);
cel01(SDESC) = ($say "Secret cellar.\n")
	       ($move insignia cel01);

cel02(LDESC) = ($say
"You are in a cool cave, once used for the storage of rare
wines.  A cool breeze from the caves to the north keeps this cave
at an even temperature the year round.  Exits can be seen
to the south and east.\n");
cel02(SDESC) = ($say "Cool cave.\n");

{ cel03 was moved to after cel08 }


cel04(LDESC) = ($say
"This is a small temple to Elbereth, Queen of the Stars.  The walls
glow with a warm iridescence of their own.  They are covered
with murals depicting the kindling of the stars at the Beginning of
Days.  In one corner, someone has added a strange insignia composed
of red and white squares, joined at the corners.  Passages lead
east and west, and a stairway to the north leads down into darkness.\n")
($move insignia cel04);
cel04(SDESC) = ($say "Temple to Elbereth.\n")
	       ($move insignia cel04);
cel04(LIGHT)=TRUE;


cel05(LDESC) = ($say
"You are in an icy cave.  All the walls are covered with thick
layers of ice.  The cave continues north and south, and there
are side tunnels leading to the northeast and northwest.\n");
cel05(SDESC) = ($say "Ice Cave.\n");

cel06(LDESC) = ($say
"You have reached a large cavern of ice.  You cannot see
the roof, but you are sure it is covered with icicles.
There are exits east and west.\n");
cel06(SDESC) = ($say "Great Ice Cavern.\n");


cel07(LDESC) = ($say
"You are in a tiny side room where the ice dragon keeps his
single prize treasure.  You may leave through a western doorway.\n");
cel07(SDESC) = ($say "Dragon's Treasure Room.\n");

cel08(LDESC) = ($say
"You are in an old abandoned room, once the resident of a frustrated
artist.  A huge mural depicting a barren wasteland covers the west
wall.  A dim glow comes from the corridor to the east.\n");
cel08(SDESC) = ($say "Artist's Retreat.\n");

cel03(LDESC) = ($say
"You are standing on the north bank of a frozen underground
river.  The source of the river seems to have been to the
west, and you can walk that way on the ice.  However, to
the northeast, the river forms a frozen waterfall you can
slide down.\n");
cel03(SDESC) = ($say "Top of frozen fall.\n");

cel09(LDESC) = ($say
"You are at the southern end of a dry river gorge.  The source
of the river was once a waterfall to the south, but for some reason
it seems frozen solid.  The riverbed continues north.\n");
cel09(SDESC) = ($say "Southern gorge.\n");

cel10(LDESC) = ($say
"You are in a dark cave.  The low-hanging stalactites and tall
stalagmites cast eerily human-like shadows on the craggy walls.
A roughly-hewn stairway to the south is lit by a dim illumination
from above, while a slimy pit in the middle of the room leads
down into even inkier blackness.  The system of caverns continues
to the west.\n");
cel10(SDESC) = ($say "Shadowy cave.\n");
CELdL =
(($and   (See [clear crystal] ($cont ($loc .ME)) )
	      ($prop [clear crystal] LIGHT)):

			($say
"You are in an impossibly dark cave.  Only the light from your
magic crystal makes it possible to see, even dimly, here.  You
can make out a tunnel leading north, and another leading east.
No other exits are visible.\n")
		     :  ($say "It's mighty dark in here!")
			((See globe ($cont ($loc .ME))):
				(($prop globe LIGHT):
					($say
"  Even your glowing globe is barely visible!")
				)
			)
			($say "\n")
			($setg DARKG TRUE)
		);
cel11(LDESC) = CELdL;
CELdS =
	       ($say "DARK cave.\n")
	       (($and   (See [clear crystal] ($cont ($loc .ME)) )
			($prop [clear crystal] LIGHT)):
			($exit 0):($setg DARKG TRUE));
cel11(SDESC) = CELdS;

cel12(LDESC) = CELdL;
cel12(SDESC) = CELdS;

cel13(LDESC) = ($say
"You are on a large underground lake of ice, thick enough
to walk on.  To the west you can dimly make out a cavern
entrance; to the east is an icy river you can walk on.\n")
		(($prop cel13 HOLED):
			($say
"In the middle of the lake is a hole; icy water sloshes about
two feet below the ice.\n")
		);
cel13(SDESC) = ($say "Icy lake.\n");

cel14(LDESC) = (($ge ($prop .ME HAS) ($prop globe WEIGH)):
			($say
"Whatever it is you're carrying drags you down into the icy depths!\n")
			(die)
		)
		($say
"You are swimming beneath the ice.  There seems to be light to the
west, but you can't hold your breath for long...\n")
		;
cel14(SDESC) = ($say "Back in the icy lake.\n");

cel15(LDESC) = ($say
"You are on a sandy beach in a small cave.  A small pool of water
comes up to the beach, and a quick survey of the place shows that it
is completely covered by the stony roof.  You can walk up to the
back of the cave, but the pool seems to be the only other exit.\n");
cel15(SDESC) = ($say "Sandy beach.");

cel16(LDESC) = ($say 
"You are at the back of a small cave.  A sandy shore slopes down
to a blue pool of water.  To the west is a small crawlway.\n");
cel16(SDESC) = ($say "Back of cave.\n");
