#include "../h/rt.h"
#include "../h/record.h"

/*
 * x.y - access field y of record x.
 */

field(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int fnum;
   register struct b_record *rp;
   register struct descrip *dp;
#ifdef INT
   extern int *ftab, *records;
#endif INT
#ifdef CMP
   extern int *ftab[];
#endif CMP

   SetBound;

   deref(&arg1);
   if (QUAL(arg1) || TYPE(arg1) != T_RECORD)
      runerr(107, &arg1);
   if (QUAL(arg2) || TYPE(arg2) != T_INTEGER)
      syserr("field: second operand not field number");

   rp = BLKLOC(arg1);
#ifdef INT
   fnum = ftab[INTVAL(arg2) * *records + rp->recptr->proc.recnum - 1];
#endif INT
#ifdef CMP
   fnum = ftab[INTVAL(arg2)][rp->recptr->proc.recnum - 1];
#endif CMP
   if (fnum < 0)
      runerr(207, &arg1);

   dp = &rp->fields[fnum];
   arg0.type = D_VAR + ((int *)dp - (int *)rp);
   BLKLOC(arg0) = dp;
   ClearBound;
   }
