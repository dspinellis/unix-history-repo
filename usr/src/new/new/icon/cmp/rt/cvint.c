#include "../h/rt.h"

/*
 * cvint - convert to integer
 */

cvint(d, i)
register struct descrip *d;
long *i;
   {
   DclSave
   union numeric result;

   switch (cvnum(d, &result)) {
      case T_LONGINT:
         *i = result.i;
#ifndef BIT32
         if (*i < (long)(int)MINSHORT || *i > (long)(int)MAXSHORT)
            return (T_LONGINT);
#endif
         return (T_INTEGER);

      case T_REAL:
         if (result.r > MAXLONG || result.r < MINLONG)
            return (NULL);
         *i = (long)result.r;
#ifndef BIT32
         if (*i < MINSHORT || *i > MAXSHORT)
            return (T_LONGINT);
#endif
         return (T_INTEGER);

      default:
         return (NULL);
      }
   }
