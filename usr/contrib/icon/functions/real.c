#include "../h/rt.h"

/*
 * real(x) - convert x to real.
 */

Xreal(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   double r;

   /*
    * If x is already a real, just return it.  Otherwise convert it and
    *  return it, failing if the conversion is unsuccessful.
    */
   DeRef(arg1)
   if (!QUAL(arg1) && TYPE(arg1) == T_REAL)
      arg0 = arg1;
   else if (cvreal(&arg1, &r) == T_REAL)
      mkreal(r, &arg0);
   else
      fail();
   }

Procblock(real,1)
