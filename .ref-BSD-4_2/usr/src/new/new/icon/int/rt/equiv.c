#include "../h/rt.h"

/*
 * equiv - tests equivalence of two objects.
 * Used by === and ~=== operators, and by table referencing.
 */

equiv(arg1, arg2)
struct descrip *arg1, *arg2;
   {
   register int result, csw;

   if (QUAL(*arg1) && QUAL(*arg2)) {
      if (NULLDESC(*arg1))
         result = NULLDESC(*arg2);
      else
         result = (lexcmp(arg1, arg2) == 0);
      }
   else if (arg1->type == arg2->type) {
      switch (TYPE(*arg1)) {
         case T_INTEGER:
            result = (INTVAL(*arg1) == INTVAL(*arg2));
            break;
         case T_REAL:
            result = (BLKLOC(*arg1)->realval == BLKLOC(*arg2)->realval);
            break;
         case T_CSET:
            result = 1;
            for (csw = 0; csw < CSETSIZE; csw++) {
               if (BLKLOC(*arg1)->bits[csw] != BLKLOC(*arg2)->bits[csw]) {
                  result = 0;
                  break;
                  }
               }
            break;
         case T_PROC:
         case T_FILE:
         case T_LIST:
         case T_TABLE:
         case T_RECORD:
         case T_ESTACK:
            result = (BLKLOC(*arg1) == BLKLOC(*arg2));
            break;
         default:
            syserr("equiv: comparision of unimplemented types.");
            break;
         }
      }
   else
      result = 0;

   return (result);
   }
