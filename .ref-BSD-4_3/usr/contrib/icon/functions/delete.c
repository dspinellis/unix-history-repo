#include "../h/rt.h"
#ifdef SETS
/*
 * delete(S,x) - delete element x from set S if it is there
 *  (always succeeds and returns S).
 */

Xdelete(nargs,arg2,arg1,arg0)
int nargs;
struct descrip arg0, arg1, arg2;
   {
   register struct descrip *pd;
   int res ;
   extern struct descrip *memb();

   DeRef(arg1)
   DeRef(arg2)
   arg0 = arg1;

   if (QUAL(arg1) || TYPE(arg1) != T_SET)
       runerr(119,&arg1);
      /*
      * The technique and philosophy here are the same
      *  as used in insert.c - see comment there.
      */
   pd = memb(BLKLOC(arg1),&arg2,hash(&arg2),&res);
   if (res == 1) {
      /*
      * The element is there so delete it.
      */
      *pd = BLKLOC(*pd)->selem.sblink;
      (BLKLOC(arg1)->set.setsize)--;
      }
   }

Procblock(delete,2)

#else SETS
char junk;			/* prevent null object file  */
#endif SETS
