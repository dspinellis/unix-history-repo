#include "../h/rt.h"

/*
 * integer(x) - convert x to integer.
 */

Xinteger(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   long l;

   switch (cvint(&arg1, &l)) {
      case T_INTEGER:
#ifdef LONGS
      case T_LONGINT:
#endif LONGS
         mkint(l, &arg0);
         break;
      default:
         fail();
      }
   }

Procblock(integer,1)
