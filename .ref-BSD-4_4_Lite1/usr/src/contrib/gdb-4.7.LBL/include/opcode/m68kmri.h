#define MOTOROLA_SYNTAX

/* Allow OP as an alias for OP.l (or OP.w or OP.b, depend on OP). */
#define ALLOW_DEFAULT_SIZES

/* Customers want bhi and friends to be variable sized - stolen from jhi */
#define BRANCH_IS_VARIABLE_SIZED

#include "m68k.h"
