#include "../h/rt.h"

/*
 * abs(x) - absolute value of x.
 */
Xabs(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   union numeric result;

   switch (cvnum(&arg1, &result)) {
      /*
       * If x is convertible to a numeric, turn arg0 into
       *  a descriptor for the appropriate type and value.  If the
       *  conversion fails, produce an error.  This code assumes that
       *  x = -x is always valid, but this assumption does not always
       *  hold.
       */
      case T_LONGINT:
         if (result.integer < 0L)
            result.integer = -result.integer;
         mkint(result.integer, &arg0);
         break;

      case T_REAL:
         if (result.real < 0.0)
            result.real = -result.real;
         mkreal(result.real, &arg0);
         break;

      default:
         runerr(102, &arg1);
      }
   }

Procblock(abs,1)
