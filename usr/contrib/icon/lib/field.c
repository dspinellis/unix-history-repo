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
   extern int *ftab, *records;

   SetBound;

   DeRef(arg1)
   /*
    * x must be a record and y must be a field number.
    */
   if (QUAL(arg1) || TYPE(arg1) != T_RECORD)
      runerr(107, &arg1);
   if (QUAL(arg2) || TYPE(arg2) != T_INTEGER)
      syserr("field: second operand not field number");

   /*
    * Map the field number into a field number for the record x.
    */
   rp = (struct b_record *) BLKLOC(arg1);
   fnum = ftab[INTVAL(arg2) * *records + rp->recptr->recnum - 1];
   /*
    * If fnum < 0, x doesn't contain the specified field.
    */
   if (fnum < 0)
      runerr(207, &arg1);

   /*
    * Return a pointer to the descriptor for the appropriate field.
    */
   dp = &rp->fields[fnum];
   arg0.type = D_VAR + ((int *)dp - (int *)rp);
   BLKLOC(arg0) = (union block *) dp;
   ClearBound;
   }
