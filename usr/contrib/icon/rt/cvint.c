#include "../h/rt.h"

/*
 * cvint - convert the value represented by d into an integer and write
 *  the value into the location referenced by i.  cvint returns one of
 *  T_INTEGER, T_LONGINT, and NULL depending on the outcome of the conversion.
 */

cvint(d, i)
register struct descrip *d;
long *i;
   {
   DclSave
   union numeric result;

   /*
    * Use cvnum to attempt the conversion into "result".
    */
   switch (cvnum(d, &result)) {
      case T_LONGINT:
         /*
          * The value converted into an integer.  Assign the value to *i.
          *  On systems with longs, distinguish between integer and long
          *  integer results via the return value.
          */
         *i = result.integer;
#ifdef LONGS
         if (*i < (long)(int)MINSHORT || *i > (long)(int)MAXSHORT)
            return (T_LONGINT);
#endif LONGS
         return (T_INTEGER);

      case T_REAL:
         /*
          * The value converted into a real number.  If it's not in the
          *  range of an integer, return a 0, otherwise convert the
          *  real value into an integer.  As before, distinguish between
          *  integers and long integers if necessary.
          */
         if (result.real > MAXLONG || result.real < MINLONG)
            return (NULL);
         *i = (long)result.real;
#ifdef LONGS
         if (*i < MINSHORT || *i > MAXSHORT)
            return (T_LONGINT);
#endif LONGS
         return (T_INTEGER);

      default:
         return (NULL);
      }
   }
