
INCLUDE "constants";

PREP with;
PREP to;
PREP into,  in;
PREP at;

INCLUDE "locnames";

NOUN .ME (road1);
.ME(OPEN) = TRUE;
.ME(HOLDS) = CAPAC;

INCLUDE "routines";

INCLUDE "verbs";

START = ($sdem LOOK)
	($setg LOOKP TRUE)
	($setv n s e w ne nw se sw u d)
	;

INCLUDE "objects";
INCLUDE "locales";
INCLUDE "transitions";
