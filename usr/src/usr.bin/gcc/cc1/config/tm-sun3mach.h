#include "tm-sun3.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -Dsun -Dsun3 -Dunix -DMACH -DCMU -DMTXINU -DBIT_MSF -DBYTE_MSF"

/* LINK_SPEC is needed only for Sunos 4.  */

#undef LINK_SPEC
