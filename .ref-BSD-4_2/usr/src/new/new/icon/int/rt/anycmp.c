#include "../h/rt.h"

/*
 * anycmp(d1,d2) - compare any two objects.
 */

anycmp(d1,d2)
struct descrip *d1, *d2;
   {
   register int o1, o2;
   register long lresult;
   register double fresult;

   o1 = order(d1);
   o2 = order(d2);

   if (o1 != o2)
      return (o1 - o2);

   if (o1 == D_NULL)
      return (0);
   if (o1 == 3)         /* have two strings */
      return (lexcmp(d1,d2));

   switch (TYPE(*d1)) {
      case T_INTEGER:
         lresult = INTVAL(*d1) - INTVAL(*d2);
         if (lresult == 0)
            return (0);
         return ((lresult > 0) ? 1 : -1);
#ifndef VAX
      case T_LONGINT:
         lresult = BLKLOC(*d1)->longint.intval - BLKLOC(*d2)->longint.intval;
         if (lresult == 0)
            return (0);
         return ((lresult > 0) ? 1 : -1);
#endif
      case T_REAL:
         fresult = BLKLOC(*d1)->real.realval - BLKLOC(*d2)->real.realval;
         if (fresult == 0)
            return (0);
         return ((fresult > 0) ? 1 : -1);

      case T_CSET:
      case T_FILE:
      case T_PROC:
      case T_LIST:
      case T_TABLE:
      case T_RECORD:
      case T_ESTACK:
         return (0);    /* unspecified sort order */

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
#ifndef VAX
      case T_LONGINT:
#endif
         return (1);

      case T_REAL:
         return (2);

      case T_CSET:
         return (4);

      case T_FILE:
         return (5);

      case T_PROC:
         return (6);

      case T_LIST:
         return (7);

      case T_TABLE:
         return (8);

      case T_ESTACK:
         return (10);

      case T_RECORD:
         return (11);

      default:
         syserr("order: unknown datatype.");
      }
   }
