#include "../h/rt.h"
#include "../h/record.h"

/*
 * mkrec - create a record.
 */

mkrec(nargs)
int nargs;
   {
   register int i;
   register struct b_proc *bp;
   register struct b_record *rp;
   extern struct b_record *alcrecd();

   SetBound;

   if (QUAL(ARG(0)) || TYPE(ARG(0)) != T_PROC)
      syserr("mkrec: procedure block not on stack");

   hneed(sizeof(struct b_record) +
         BLKLOC(ARG(0))->nfields*sizeof(struct descrip)); /* check heap */

   bp = BLKLOC(ARG(0));
   rp = alcrecd(bp->nfields, bp);

   for (i = bp->nfields; i > nargs; i--)
      rp->fields[i-1] = nulldesc;
   for ( ; i > 0; i--) {
      rp->fields[i-1] = ARG(i);
      deref(&rp->fields[i-1]);
      }

   ARGTYPE(0) = D_RECORD;
   ARGVAL(0) = rp;
   ClearBound;
   }
