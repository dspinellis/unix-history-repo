#include "../h/rt.h"

/*
 * x := y - assign y to x.
 */

asgn(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;
   /*
    * Make sure that x is a variable.
    */
   if (QUAL(arg1) || !VAR(arg1))
      runerr(111, &arg1);
   /*
    * Return value is the variable being assigned to.
    */
   arg0 = arg1;
   DeRef(arg2)
   /*
    * doasgn does all the work.
    */
   doasgn(&arg1, &arg2);
   ClearBound;
   }

Opblock(asgn,2,":=")
