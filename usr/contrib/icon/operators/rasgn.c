#include "../h/rt.h"

/*
 * x <- y - assign y to x.
 * Reverses assignment if resumed.
 */

rasgn(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;
   /*
    * x must be a variable.
    */
   if (QUAL(arg1) || !VAR(arg1))
      runerr(111, &arg1);
   /*
    * The return value is the variable x, so make a copy of it before
    *  it is dereferenced.
    */
   arg0 = arg1;
   DeRef(arg1)
   DeRef(arg2)
   /*
    * Assign y to x and suspend.
    */
   doasgn(&arg0, &arg2);
   suspend();
   /*
    * <- has been resumed, reverse the assignment by assigning the old value
    *  of x (present as arg1) back into x and fail.
    */
   doasgn(&arg0, &arg1);
   fail();
   }

Opblock(rasgn,2,"<-")
