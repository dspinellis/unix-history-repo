
INCLUDE "constants.ddl";

PREP with;
PREP to;
PREP into,  in;
PREP at;

INCLUDE "locnames.ddl";

NOUN .ME (road1);
.ME(OPEN) = TRUE;
.ME(HOLDS) = CAPAC;

INCLUDE "routines.ddl";

INCLUDE "verbs.ddl";

START = ($sdem LOOK)
	($setg LOOKP TRUE)
	($setv n s e w ne nw se sw u d)
	;

INCLUDE "objects.ddl";
INCLUDE "locales.ddl";
INCLUDE "transit.ddl";
