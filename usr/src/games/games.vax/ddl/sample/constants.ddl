
{  *** Intrinsic Properties *** }

VISIT = 1;		{ I've been here / seen this }
OPENS = 2;		{ This can be opened }
LOCKS = 3;		{ This can be locked }
OPEN  = 4;		{ This is opened }
LOCKD = 5;		{ This is locked }
TRANS = 6;		{ This is transparent }
LIGHT = 7;		{ This gives off light }
FLAME = 8;		{ This is on fire }
CONTS = 9;		{ Printed contents for the object (Look Kludge) }
SHRNK = 10;		{ Has it been shrunk? }
CRYST = 11;		{ Is it one of our magic thingies? }

MISC1 = 16;		{ Miscellaneous Property (Semantics per object) }
HOLED = MISC1;

RDLOC = 17;		{ Location on Highway 8 }
WEIGH = 18;
HOLDS = 19;
HAS   = 20;
POINT = 21;

{ Constants }
TRUE = 1; FALSE = 0;
CAPAC = 250;		{ More than I can carry }

Shazm = "ShaZam";	{ The Wizard's incantation }
{ Globals }
VAR DARKG ;
VAR HWY8  ;
VAR LOOKP ;
VAR SCORE ;
VAR Tempr ;		{ Dragon's Upset? }
VAR Cel6x ;		{ Where he came from to enter Cel06 }
VAR WASDK ;
VAR Wizrd ;		{ Wizardly Ability }
VAR GO;			{ Did he move OK? }
