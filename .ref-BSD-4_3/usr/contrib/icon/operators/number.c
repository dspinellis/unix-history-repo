#include "../h/rt.h"

/*
 * +x - convert x to numeric type.
 *  Operational definition: generate runerr if x is not numeric.
 */

number(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   union numeric n;

   SetBound;
   switch (cvnum(&arg1, &n)) {
      case T_LONGINT:
         mkint(n.integer, &arg0);
         break;

      case T_REAL:
         mkreal(n.real, &arg0);
         break;

      default:
         runerr(102, &arg1);
      }
   ClearBound;
   }

Opblock(number,1,"+")
