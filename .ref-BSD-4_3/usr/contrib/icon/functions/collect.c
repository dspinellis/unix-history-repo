#include "../h/rt.h"
#ifdef AZ
/*
 * collect(i) - explicit call to garbage collector.
 */

Xcollect(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   defshort(&arg1, 0);
   gcollect(INTVAL(arg1));
   arg0 = nulldesc;
   }

Procblock(collect,1)
#else AZ
char junk;	/* prevent empty object module */
#endif AZ
