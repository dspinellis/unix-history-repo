#include "../h/rt.h"

/*
 * anycmp - compare any two objects.  The result of the comparison is
 *  an integer such that:
 *    d1 = d2 -> 0
 *    d1 > d2 -> >0  (1 if same type)
 *    d1 < d2 -> <0  (-1 if same type)
 */

anycmp(d1,d2)
struct descrip *d1, *d2;
   {
   register int o1, o2;
   register long lresult;
   register double fresult;

   /*
    * Get a collating number for d1 and d2.
    */
   o1 = order(d1);
   o2 = order(d2);

   /*
    * If d1 and d2 aren't of the same type, return the difference of
    *  their collating numbers.
    */
   if (o1 != o2)
      return (o1 - o2);

   if (o1 == D_NULL)
      /*
       * o1 0, (D_NULL), return 0 because all null values are the same.
       */
      return (0);
   if (o1 == 3)
      /*
       * d1 and d2 are strings, use lexcmp to compare them.
       */
      return (lexcmp(d1,d2));

   switch (TYPE(*d1)) {
      /*
       * For numbers, return -1, 0, 1, depending on whether d1 <, =, > d2.
       */
      case T_INTEGER:
         lresult = INTVAL(*d1) - INTVAL(*d2);
         if (lresult == 0)
            return (0);
         return ((lresult > 0) ? 1 : -1);
#ifdef LONGS
      case T_LONGINT:
         lresult = BLKLOC(*d1)->longint.intval - BLKLOC(*d2)->longint.intval;
         if (lresult == 0)
            return (0);
         return ((lresult > 0) ? 1 : -1);
#endif LONGS
      case T_REAL:
         fresult = BLKLOC(*d1)->realblk.realval - BLKLOC(*d2)->realblk.realval;
         if (fresult == 0)
            return (0);
         return ((fresult > 0) ? 1 : -1);

      case T_CSET:
      case T_FILE:
      case T_PROC:
      case T_LIST:
      case T_TABLE:
#ifdef SETS
      case T_SET:
#endif SETS
      case T_RECORD:
      case T_ESTACK:
         /*
          * Csets, files, procedures, lists, tables, records, co-expressions
          *  and sets have no specified collating sequence so any two of 
          *  the same type are considered to be equal.
          */
         return (0);

      default:
         syserr("anycmp: unknown datatype.");
      }
   }

/*
 * order(x) - return collating number for object x.
 */

order(d)
struct descrip *d;
   {
   if (QUAL(*d))
      if (STRLOC(*d) == 0)
         return(0);               /* &null */
      else
         return (3);              /* some string */
   switch (TYPE(*d)) {
      case T_INTEGER:
#ifdef LONGS
      case T_LONGINT:
#endif LONGS
         return (1);
      case T_REAL:
         return (2);
      case T_CSET:
         return (4);
      case T_ESTACK:
         return (5);
      case T_FILE:
         return (6);
      case T_PROC:
         return (7);
      case T_LIST:
         return (8);
      case T_TABLE:
         return (9);
#ifdef SETS
      case T_SET:
         return (10);
#endif SETS
      case T_RECORD:
         return (11);
      default:
         syserr("order: unknown datatype.");
      }
   }
