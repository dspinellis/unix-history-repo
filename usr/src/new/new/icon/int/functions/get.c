#include "../h/rt.h"

/*
 * get(x) - get an element from end of list x.
 * Synonym for pop(x).
 */

extern Xpop();

struct b_iproc Bget = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xpop),
   1,
   -1,
   0,
   0,
   {3, "get"}
   };
