#include "../h/rt.h"
#ifdef SETS

/*
 * insert(S,x) - insert element x into set S if not already there
 *  (always succeeds and returns S).
 */

Xinsert(nargs,arg2,arg1,arg0)
int nargs;
struct descrip arg0, arg1, arg2;
   {
   register struct descrip *pd;
   register int  hn;
   int res;
   extern struct b_selem *alcselem();
   extern struct descrip *memb();

   DeRef(arg1)
   DeRef(arg2)
   arg0 = arg1;

   if (QUAL(arg1) || TYPE(arg1) != T_SET)
      runerr(119,&arg1);

      /*
      * We may need at most one new element.
      */
   hneed(sizeof(struct b_selem));
   hn = hash(&arg2);
   /*
    * If arg2 is a member of set arg1 then res will have the
    *  value 1 and pd will have a pointer to the descriptor
    *  that points to that member.
    *  If arg2 is not a member of the set then res will have
    *  the value 0 and pd will point to the descriptor
    *  which should point to the member - thus we know where
    *  to link in the new element without having to do any
    *  repetitive looking.
    */
   pd = memb(BLKLOC(arg1),&arg2,hn,&res);
   if (res == 0)
      /*
      * The element is not in the set - insert it.
      */
      addmem(BLKLOC(arg1),alcselem(&arg2,hn),pd);
   }

Procblock(insert,2)

#else SETS
char junk;			/* prevent null object file  */
#endif SETS
