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
   /*
    * Be sure that call is from a procedure.
    */
   if (QUAL(ARG(0)) || TYPE(ARG(0)) != T_PROC)
      syserr("mkrec: procedure block not on stack");

   /*
    * Ensure space for the record to be created.
    */
   hneed(sizeof(struct b_record) +
         BLKLOC(ARG(0))->proc.nfields*sizeof(struct descrip));

   /*
    * Get a pointer to the record constructor procedure and allocate
    *  a record with the appropriate number of fields.
    */
   bp = (struct b_proc *) BLKLOC(ARG(0));
   rp = alcrecd(bp->nfields, bp);

   /*
    * Set all fields in the new record to null value.
    */
   for (i = bp->nfields; i > nargs; i--)
      rp->fields[i-1] = nulldesc;

   /*
    * Assign each argument value to a record element and dereference it.
    */
   for ( ; i > 0; i--) {
      rp->fields[i-1] = ARG(i);
      DeRef(rp->fields[i-1])
      }

   ARGTYPE(0) = D_RECORD;
   ARGVAL(0) = (int) rp;
   ClearBound;
   }
