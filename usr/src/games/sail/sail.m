.. @(#)sail.m	1.1 83/03/17
.TH SAIL PUBLIC 
.UC 4
.SH NAME
sail \- multi-user wooden ships and iron men
.SH SYNOPSIS
.B sail
[
.B \-x
] [
.B num
]
.br
.fi
.SH DESCRIPTION
.I Sail
is a computer version of Avalon Hill's game of fighting sail
originally developed by S. Craig Taylor.
.SH NOTES
.I Sail
is really two programs in one.  Each player keeps track of his
own ship plus a
.I DRIVER
program is execl'd (by the first player) to keep track of the
computer's ships.
.PP
The player is given the first available ship in a scenario and the
computer takes the rest.  Obviously the more ships in your game, the
longer the 
.I DRIVER
will take to move them. If additional players join the game, they
will be given ships and the
.I DRIVER
will have less work to do.  
.SH HISTORICAL INFO
Old Square Rigger's were very maneuverable ships capable of intricate
sailing. Their one main disadvantage was being unable to sail very
close to the wind. The design of wooden ship allowed only for the
guns to bear to the left and right sides.  A few guns of small
aspect (usually 6 or 9 pounders) could point forward, but their
effect would be small compared to a 68 gun broadside of 24 pounders.
The guns bear approximately like so:
.nf

       \\
        b----------------
    ---0
        \\
         \\
          \\     up to a range of ten (for round shot)
           \\
            \\
             \\

.fi
.bp
An interesting phenomenon occurred when a broadside could fire
down the length of an enemy ship.  The shot tended to bounce along
the deck and did several times more damage. This phenomenon was called
a rake. It happened that a stern rake (firing from the stern to the
bow) occasioned more damage than a bow rake, so that was the most
desirable. 
.nf

                       \\
                        b----------------
                    ---0
                        \\
                         \\      0a   ---  Stern rake!
                          \\
                           \\
                            \\
                             \\
                              \\

.fi
Most ships were equipped with Carronades which were very large, close
range cannons.  The carronades have a range of two in this game and can
considerably add to your fire-power when they come to bear.
If the distance to the target ship is greater than 6, the guns
can only fire at the rigging.
A ship's guns could fire a variety of ammunition.  For example:
.SH ROUND
Range of 10.  Good for hull or rigging hits.
.SH DOUBLE
Range of 1.  Extra good for hull or rigging hits.
Double takes two turns to load.
.SH CHAIN
Range of 3.  Excellent for tearing down rigging.
Cannot damage hull or guns, though.
.SH GRAPE
Range of 1.  Devastating against enemy crews.
.PP
When a ship has been battered into a hulk (zero hull), it has no 
choice but to surrender to the firing ship.  This ceremony is called
\'striking your colours.\'  A struck ship has a chance of exploding or
sinking after a while.  When a ship surrenders, its point value is
given to the aggressor. When a ship is captured, twice the point
value is awarded the victor.
.PP
Normally, ships sailed into battle with greatly shortened sail to
avoid excessive damage to the precious rigging.  However, in this game
the player can increase to full sails and move much faster if he wishes.
But, all rigging hits incurred with full sails set are doubled.
The direction rose displayed on the sample screen gives the maximum
speeds possible for a specific ship at all attitudes to the wind.
The full sail speeds are in parenthesis.
.PP
Repairs can be made at the slow rate of two (hull, gun, or rigging)
hits restored per three turns.
.PP
Ships of class 3 or greater drift when there is wind at the rate of
one \'square\' per turn. Ships of the Line drift one \'square\'
every other turn.
.SH INSTRUCTIONS
.I Sail
follows the Avalon Hill advanced rules very closely using the
optional rules for 'exploding ships', 'full sails', and some others.
A few unique commands have been added which seemed to be helpful on
the reduced screen. 'i' is such a command.
.PP
Boarding had to go through a major revision.  To prevent immediate
capture of an unprepared crew (fouling is often not reported until
after boarding has commenced) the boarded ship automatically fights
defensively (at a small disadvantage) if no DBP's have been prepared.
.PP
The Order of Play has been eliminated for the player, but the
.I DRIVER
still abides by it.
.PP
The commands for the player were designed to be as intelligent as
possible to save typing.  Some of the nuances I developed should be
explained.
.nf

    ~    Your prompt

The others I will illustrate with examples.

    move(3, 2): r1l             /* 3 movements max, of which two
                                   may be 45' turns. */

    move(3,'2): 1r1             /* 3 movements max of which two may
                                   be 45' turns, but the ship must
                                   move ahead before turning (there
                                   is a loss of headway after
                                   drifting) */

    move(0,'0): r               /* You can always make one turn
                                   even when you can't move straight
                                   ahead. */

.fi
If you are grappled, fouled, or out of crew, you cannot move of course.
.bp
.SH COMMANDS
.nf

    'f'  Fire broadsides if they bear
    'l'  Reload
    'm'  Move (see above & below)
    'i'  Ask lookout for closest ship
    'I'  Ask lookout for closest enemy ship
    's'  Send a message around the fleet
    'b'  Attempt to board an enemy ship
    'L'  Unload broadsides (to change ammo)
    'B'  Recall boarding parties
    'c'  Change set of sail
    'r'  Repair
    'u'  Attempt to unfoul
    'g'  Grapple/ungrapple
   '^L'  Redraw screen
    'q'  Quit

.fi
.bg
.SH SCENARIOS
.br
.SH Ranger vs. Drake:
.nf
Wind from the N, blowing a fresh breeze.

(a) Ranger            19 gun Sloop (crack crew) (7 pts)
(b) Drake             17 gun Sloop (crack crew) (6 pts)
.SH The Battle of Flamborough Head:
.nf
Wind from the S, blowing a fresh breeze.

.fi
This is John Paul Jones' first famous battle.  Aboard the Bonhomme
Richard, he was able to overcome the Serapis's greater firepower
by quickly boarding her.
.nf

(a) Bonhomme Rich     42 gun Corvette (crack crew) (11 pts)
(b) Serapis           44 gun Frigate (crack crew) (12 pts)
.SH Arbuthnot and Des Touches:
.nf
Wind from the N, blowing a gale.

(b) America           64 gun Ship of the Line (crack crew) (20 pts)
(b) Befford           74 gun Ship of the Line (crack crew) (26 pts)
(b) Adamant           50 gun Ship of the Line (crack crew) (17 pts)
(b) London            98 gun 3 Decker SOL (crack crew) (28 pts)
(b) Royal Oak         74 gun Ship of the Line (crack crew) (26 pts)
(f) Neptune           74 gun Ship of the Line (average crew) (24 pts)
(f) Duc Bougogne      80 gun 3 Decker SOL (average crew) (27 pts)
(f) Conquerant        74 gun Ship of the Line (average crew) (24 pts)
(f) Provence          64 gun Ship of the Line (average crew) (18 pts)
(f) Romulus           44 gun Ship of the Line (average crew) (10 pts)
.SH Suffren and Hughes:
.nf

Wind from the S, blowing a fresh breeze.

(b) Monmouth          74 gun Ship of the Line (average crew) (24 pts)
(b) Hero              74 gun Ship of the Line (crack crew) (26 pts)
(b) Isis              50 gun Ship of the Line (crack crew) (17 pts)
(b) Superb            74 gun Ship of the Line (crack crew) (27 pts)
(b) Burford           74 gun Ship of the Line (average crew) (24 pts)
(f) Flamband          50 gun Ship of the Line (average crew) (14 pts)
(f) Annibal           74 gun Ship of the Line (average crew) (24 pts)
(f) Severe            64 gun Ship of the Line (average crew) (18 pts)
(f) Brilliant         80 gun Ship of the Line (crack crew) (31 pts)
(f) Sphinx            80 gun Ship of the Line (average crew) (27 pts)
.SH Nymphe vs. Cleopatre:
.nf
Wind from the S, blowing a fresh breeze.

(b) Nymphe            36 gun Frigate (crack crew) (11 pts)
(f) Cleopatre         36 gun Frigate (average crew) (10 pts)
.SH Mars vs. Hercule:
Wind from the S, blowing a fresh breeze.
.nf
(b) Mars              74 gun Ship of the Line (crack crew) (26 pts)
(f) Hercule           74 gun Ship of the Line (average crew) (23 pts)
.SH Ambuscade vs. Baionnaise:
.nf
Wind from the N, blowing a fresh breeze.

(b) Ambuscade         32 gun Frigate (average crew) (9 pts)
(f) Baionnaise        24 gun Corvette (average crew) (9 pts)
.SH Constellation vs. Insurgent:
.nf
Wind from the S, blowing a gale.

(a) Constellation     38 gun Corvette (elite crew) (17 pts)
(f) Insurgent         36 gun Corvette (average crew) (11 pts)
.SH Constellation vs. Vengeance:
.nf
Wind from the S, blowing a fresh breeze.

(a) Constellation     38 gun Corvette (elite crew) (17 pts)
(f) Vengeance         40 gun Frigate (average crew) (15 pts)
.SH The Battle of Lissa:
.nf
Wind from the S, blowing a fresh breeze.

(b) Amphion           32 gun Frigate (elite crew) (13 pts)
(b) Active            38 gun Frigate (elite crew) (18 pts)
(b) Volage            22 gun Frigate (elite crew) (11 pts)
(b) Cerberus          32 gun Frigate (elite crew) (13 pts)
(f) Favorite          40 gun Frigate (average crew) (15 pts)
(f) Flore             40 gun Frigate (average crew) (15 pts)
(f) Danae             40 gun Frigate (crack crew) (17 pts)
(f) Bellona           32 gun Frigate (green crew) (9 pts)
(f) Corona            40 gun Frigate (green crew) (12 pts)
(f) Carolina          32 gun Frigate (green crew) (7 pts)
.SH Constitution vs. Guerriere:
.nf
Wind from the SW, blowing a gale.

(a) Constitution      44 gun Corvette (elite crew) (24 pts)
(b) Guerriere         38 gun Frigate (crack crew) (15 pts)
.SH United States vs. Macedonian:
.nf
Wind from the S, blowing a fresh breeze.

(a) United States     44 gun Frigate (elite crew) (24 pts)
(b) Macedonian        38 gun Frigate (crack crew) (16 pts)
.SH Constitution vs. Java:
.nf
Wind from the S, blowing a fresh breeze.

(a) Constitution      44 gun Corvette (elite crew) (24 pts)
(b) Java              38 gun Corvette (crack crew) (19 pts)
.SH Chesapeake vs. Shannon:
.nf
Wind from the S, blowing a fresh breeze.

(a) Chesapeake        38 gun Frigate (average crew) (14 pts)
(b) Shannon           38 gun Frigate (elite crew) (17 pts)
.SH The Battle of Lake Erie:
.nf
Wind from the S, blowing a light breeze.

(a) Lawrence          20 gun Sloop (crack crew) (9 pts)
(a) Niagara           20 gun Sloop (elite crew) (12 pts)
(b) Lady Prevost      13 gun Brig (crack crew) (5 pts)
(b) Detroit           19 gun Sloop (crack crew) (7 pts)
(b) Q. Charlotte      17 gun Sloop (crack crew) (6 pts)
.SH Wasp vs. Reindeer:
.nf
Wind from the S, blowing a light breeze.

(a) Wasp              20 gun Sloop (elite crew) (12 pts)
(b) Reindeer          18 gun Sloop (elite crew) (9 pts)
.SH Constitution vs. Cyane and Levant:
.br
Wind from the S, blowing a moderate breeze.

(a) Constitution      44 gun Corvette (elite crew) (24 pts)
(b) Cyane             24 gun Sloop (crack crew) (11 pts)
(b) Levant            20 gun Sloop (crack crew) (10 pts)
.br
.SH Pellew vs. Droits de L'Homme:
.nf
Wind from the N, blowing a gale.

(b) Indefatigable     44 gun Frigate (elite crew) (14 pts)
(b) Amazon            36 gun Frigate (crack crew) (14 pts)
(f) Droits L'Hom      74 gun Ship of the Line (average crew) (24 pts)
.SH Algeciras:
.nf
Wind from the SW, blowing a moderate breeze.

(b) Caesar            80 gun Ship of the Line (crack crew) (31 pts)
(b) Pompee            74 gun Ship of the Line (crack crew) (27 pts)
(b) Spencer           74 gun Ship of the Line (crack crew) (26 pts)
(b) Hannibal          98 gun 3 Decker SOL (crack crew) (28 pts)
(s) Real-Carlos       112 gun 3 Decker SOL (green crew) (27 pts)
(s) San Fernando      96 gun 3 Decker SOL (green crew) (24 pts)
(s) Argonauta         80 gun Ship of the Line (green crew) (23 pts)
(s) San Augustine     74 gun Ship of the Line (green crew) (20 pts)
(f) Indomptable       80 gun Ship of the Line (average crew) (27 pts)
(f) Desaix            74 gun Ship of the Line (average crew) (24 pts)
.SH Lake Champlain:
.nf
Wind from the N, blowing a fresh breeze.

(a) Saratoga          26 gun Sloop (crack crew) (12 pts)
(a) Eagle             20 gun Sloop (crack crew) (11 pts)
(a) Ticonderoga       17 gun Sloop (crack crew) (9 pts)
(a) Preble            7 gun Brig (crack crew) (4 pts)
(b) Confiance         37 gun Frigate (crack crew) (14 pts)
(b) Linnet            16 gun Sloop (elite crew) (10 pts)
(b) Chubb             11 gun Brig (crack crew) (5 pts)
.SH Last Voyage of the USS President:
.nf
Wind from the N, blowing a fresh breeze.

(a) President         44 gun Frigate (elite crew) (24 pts)
(b) Endymion          40 gun Frigate (crack crew) (17 pts)
(b) Pomone            44 gun Frigate (crack crew) (20 pts)
(b) Tenedos           38 gun Frigate (crack crew) (15 pts)
.SH Hornblower and the Natividad:
.nf
Wind from the E, blowing a gale.

.fi
A scenario for you Horny fans.  Remember, he sank the Natividad
against heavy odds and winds.  Hint: don't try to board the Natividad,
her crew is much bigger, albeit green.
.nf

(b) Lydia             36 gun Frigate (elite crew) (13 pts)
(s) Natividad         50 gun Ship of the Line (green crew) (14 pts)
.SH Curse of the Flying Dutchman:
.nf
Wind from the S, blowing a fresh breeze.

Just for fun, take the Piece of cake.

(s) Piece of Cake     24 gun Corvette (average crew) (9 pts)
(f) Flying Dutchy     120 gun 3 Decker SOL (elite crew) (43 pts)
.SH The South Pacific:
.nf
Wind from the S, blowing a strong breeze.

(a) USS Scurvy        136 gun 3 Decker SOL (mutinous crew) (27 pts)
(b) HMS Tahiti        120 gun 3 Decker SOL (elite crew) (43 pts)
(s) Australian        32 gun Frigate (average crew) (9 pts)
(f) Bikini Atoll      7 gun Brig (crack crew) (4 pts)
.SH Hornblower and the battle of Rosas bay:
.nf
Wind from the E, blowing a fresh breeze.

The only battle Hornblower ever lost.  He was able to dismast one
ship and stern rake the anothers though.  See if you can do as well.
.nf

(b) Sutherland        74 gun Ship of the Line (crack crew) (26 pts)
(f) Turenne           80 gun 3 Decker SOL (average crew) (27 pts)
(f) Nightmare         74 gun Ship of the Line (average crew) (24 pts)
(f) Paris             112 gun 3 Decker SOL (green crew) (27 pts)
(f) Napolean          74 gun Ship of the Line (green crew) (20 pts)
.SH Cape Horn:
.nf
Wind from the NE, blowing a strong breeze.

(a) Concord           80 gun Ship of the Line (average crew) (27 pts)
(a) Berkeley          98 gun 3 Decker SOL (crack crew) (28 pts)
(b) Thames            120 gun 3 Decker SOL (elite crew) (43 pts)
(s) Madrid            112 gun 3 Decker SOL (green crew) (27 pts)
(f) Musket            80 gun 3 Decker SOL (average crew) (27 pts)
.SH New Orleans:
.nf
Wind from the SE, blowing a fresh breeze.

Watch that little Cypress go!

(a) Alligator         120 gun 3 Decker SOL (elite crew) (43 pts)
(b) Firefly           74 gun Ship of the Line (crack crew) (27 pts)
(b) Cypress           44 gun Frigate (elite crew) (14 pts)
.SH Botany Bay:
.nf
Wind from the N, blowing a fresh breeze.

(b) Shark             64 gun Ship of the Line (average crew) (18 pts)
(f) Coral Snake       44 gun Corvette (elite crew) (24 pts)
(f) Sea Lion          44 gun Frigate (elite crew) (24 pts)
.SH Voyage to the Bottom of the Sea:
.nf
Wind from the NW, blowing a fresh breeze.

This one is dedicated to David Hedison.

(a) Seaview           120 gun 3 Decker SOL (elite crew) (43 pts)
(a) Flying Sub        40 gun Frigate (crack crew) (17 pts)
(b) Mermaid           136 gun 3 Decker SOL (mutinous crew) (27 pts)
(s) Giant Squid       112 gun 3 Decker SOL (green crew) (27 pts)
.SH Frigate Action:
.nf
Wind from the E, blowing a fresh breeze.

(a) Killdeer          40 gun Frigate (average crew) (15 pts)
(b) Sandpiper         40 gun Frigate (average crew) (15 pts)
(s) Curlew            38 gun Frigate (crack crew) (16 pts)
.SH The Battle of Midway:
.nf
Wind from the E, blowing a moderate breeze.

(a) Enterprise        80 gun Ship of the Line (crack crew) (31 pts)
(a) Yorktown          80 gun Ship of the Line (average crew) (27 pts)
(a) Hornet            74 gun Ship of the Line (average crew) (24 pts)
(f) Akagi             112 gun 3 Decker SOL (green crew) (27 pts)
(f) Kaga              96 gun 3 Decker SOL (green crew) (24 pts)
(f) Soryu             80 gun Ship of the Line (green crew) (23 pts)
.SH EXAMPLE OF MOVE:
.nf

    / Max distance (including turns)
   /  Max number of 45 degree turns (one at a time only)
  /  /
Move(3, 2): r1l    /* move right, ahead, left
                    *
                    *         0 START
                    *        b
                    *--------------------------
                    *
                    *        b0 RIGHT
                    *--------------------------
                    *
                    *       b0  ONE
                    *--------------------------
                    *        0
                    *       b   LEFT
                    *--------------------------

.fi
.SH SAMPLE GAME:
.nf

% sail
Choose a scenario:


NUMBER  SHIPS   IN PLAY TITLE
0):     2       no      Ranger vs. Drake
1):     2       no      The Battle of Flamborough Head
2):     10      no      Arbuthnot and Des Touches
3):     10      no      Suffren and Hughes
4):     2       no      Nymphe vs. Cleopatre
5):     2       no      Mars vs. Hercule
6):     2       no      Ambuscade vs. Baionnaise
7):     2       no      Constellation vs. Insurgent
8):     2       no      Constellation vs. Vengeance
9):     10      no      The Battle of Lissa
10):    2       no      Constitution vs. Guerriere
11):    2       no      United States vs. Macedonian
12):    2       no      Constitution vs. Java
13):    2       no      Chesapeake vs. Shannon
14):    5       no      The Battle of Lake Erie
15):    2       no      Wasp vs. Reindeer
16):    3       no      Constitution vs. Cyane and Levant
17):    3       no      Pellew vs. Droits de L'Homme
18):    10      no      Algeciras
19):    7       no      Lake Champlain
20):    4       no      Last Voyage of the USS President
21):    2       no      Hornblower and the Natividad
22):    2       no      Curse of the Flying Dutchman
23):    4       no      The South Pacific
24):    5       no      Hornblower and the battle of Rosas bay
25):    5       no      Cape Horn
26):    3       no      New Orleans
27):    3       no      Botany Bay
28):    4       no      Voyage to the Bottom of the Sea
29):    3       no      Frigate Action
30):    6       no      The Battle of Midway

Scenario number? 21
Your ship is the Lydia, a 36 gun Frigate (elite crew).
Your name, Captain? Dave #1 

Initial broadside left (grape, chain, round, double): d

Initial broadside right (grape, chain, round, double): r

Class 3 (36 guns) Frigate 'Lydia' (b0)          Points: 0  Fouls: 0  Grapples: 0
--------------------------------------------------------------------------------
|                                                                              |
|                                                                              |
|                                                ~0  -- a sinking ship         |
|                        0                                                     |
|                       b                   #1  -- an exploding ship           |
|                       ^                                                      |
|                       bow of Lydia                    |----------------------|
|                                                       |      wind speed   -5+|
|                                                       |    and direction     |
|                                                       | (blowing from right) |
|         !  -- a struck ship           S0              |----------------------|
|          1                               \\                                   |
|                                           stern of Natividad                 |
|                                       Natividad has full sails set.          |
|                                                                              |
------------------------------------Turn 0--------------------------------------
Aye aye, Sir                    load: port and starboard - Load  D! R!    0 1(1)
~                                                          Hull   9      \|/
                                crew: 3 sections --------- Crew  4  4  2 -^-1(1)
                                guns: port and starboard - Guns  4  4    /|\
                        carronades: port and starboard --- Carr  2  2     | 3(5)
                                rigging 4 masts ---------- Rigg 5 5 5 5   2(4)  
.fi
.SH "Ken Arnold Code"
curses library (pu!)
.SH Author
Dave Riggle
.SH "Bug-author"
Ed Wang (pronounced Wong)
.SH Refitting
Craig Leres
.SH Consultants
.nf
Chris Guthrie
Captain Happy
Nancy Reagan
.fi
.SH "SEE ALSO"
midway(PUBLIC)
.SH BUGS
