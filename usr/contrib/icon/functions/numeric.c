#include "../h/rt.h"

/*
 * numeric(x) - convert x to numeric type.
 */
Xnumeric(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   union numeric n1;

   switch (cvnum(&arg1, &n1)) {
#ifdef LONGS
      case T_LONGINT:
#else LONGS
      case T_INTEGER:
#endif LONGS
         mkint(n1.integer, &arg0);
         break;

      case T_REAL:
         mkreal(n1.real, &arg0);
         break;

      default:
         fail();
      }
   }

Procblock(numeric,1)
