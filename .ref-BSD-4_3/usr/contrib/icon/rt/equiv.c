#include "../h/rt.h"

/*
 * equiv - test equivalence of two objects.
 */

equiv(arg1, arg2)
struct descrip *arg1, *arg2;
   {
   register int result, i;
   register char *s1, *s2;

   result = 0;

      /*
       * If the descriptors are identical, the objects are equivalent.
       */
   if (arg1->type == arg2->type && BLKLOC(*arg1) == BLKLOC(*arg2))
      result = 1;
   else if (QUAL(*arg1) && QUAL(*arg2)) {

      /*
       * Both objects are strings, or one is &null and the other a string,
       * If either is &null the other isn't or we wouldn't have
       *  gotten this far.
       *  If both are strings of equal length, compare their characters.
       */

      if (NULLDESC(*arg1) || NULLDESC(*arg2))
         result = 0;
      else if ((i = STRLEN(*arg1)) == STRLEN(*arg2)) {
         s1 = STRLOC(*arg1);
         s2 = STRLOC(*arg2);
         result = 1;
         while (i--)
           if (*s1++ != *s2++) {
              result = 0;
              break;
              }
         }
      }
   else if (arg1->type == arg2->type)
      switch (TYPE(*arg1)) {
         /*
          * For integers and reals, just compare the values.
          */
         case T_INTEGER:
            result = (INTVAL(*arg1) == INTVAL(*arg2));
            break;
#ifdef LONGS
         case T_LONGINT:
            result =
               (BLKLOC(*arg1)->longint.intval == BLKLOC(*arg2)->longint.intval);
            break;
#endif LONGS
         case T_REAL:
            result =
               (BLKLOC(*arg1)->realblk.realval == BLKLOC(*arg2)->realblk.realval);
            break;
         case T_CSET:
            /*
             * Compare the bit arrays of the csets.
             */
            result = 1;
            for (i = 0; i < CSETSIZE; i++)
               if (BLKLOC(*arg1)->cset.bits[i] != BLKLOC(*arg2)->cset.bits[i]) {
                  result = 0;
                  break;
                  }
         }
   else
      /*
       * arg1 and arg2 are of different types, so they can't be
       *  equivalent.
       */
      result = 0;

   return (result);
   }
