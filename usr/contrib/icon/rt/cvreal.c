#include "../h/rt.h"

/*
 * cvreal - convert to real and put the result into *r.
 */

cvreal(d, r)
register struct descrip *d;
double *r;
   {
   union numeric result;

   /*
    * Use cvnum to classify the value.  Cast integers into reals and
    *  fail if the value is non-numeric.
    */
   switch (cvnum(d, &result)) {
      case T_LONGINT:
         *r = result.integer;
         return (T_REAL);

      case T_REAL:
         *r = result.real;
         return (T_REAL);

      default:
         return (NULL);
      }
   }
