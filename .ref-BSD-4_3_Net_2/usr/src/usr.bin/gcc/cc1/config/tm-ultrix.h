#include "tm-vax.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dultrix -Dbsd4_2 -Dvax -Dunix"

/* By default, allow $ to be part of an identifier.  */
#define DOLLARS_IN_IDENTIFIERS 1
