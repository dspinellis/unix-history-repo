#include "../h/rt.h"

/*
 * cvreal - convert to real
 */

cvreal(d, r)
register struct descrip *d;
double *r;
   {
   union numeric result;

   switch (cvnum(d, &result)) {
      case T_LONGINT:
         *r = result.i;
         return (T_REAL);

      case T_REAL:
         *r = result.r;
         return (T_REAL);

      default:
         return (NULL);
      }
   }
