#include "../h/rt.h"
#ifdef SETS

/*
 * member(S,x) - returns x if x is a member of set S otherwise fails.
 */

Xmember(nargs,arg2,arg1,arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   int res;
   extern struct descrip *memb();

   DeRef(arg1)
   DeRef(arg2)
   if (QUAL(arg1) || TYPE(arg1) != T_SET)
      runerr(119,&arg1);	/* S is not a set  */
   /* If arg2 is a member of set arg1 then "res" will have the
    * value 1 otherwise it will have the value 0.
    */
   memb(BLKLOC(arg1),&arg2,hash(&arg2),&res);
   if (res == 1) {		/* It is a member. */
      arg0 = arg2;		/* Return the member if it is in arg1. */
      return;
      }
   fail();
   }

Procblock(member,2)

#else SETS
char junk;			/* prevent null object file */
#endif SETS
