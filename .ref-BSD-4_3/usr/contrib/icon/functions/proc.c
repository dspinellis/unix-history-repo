#include "../h/rt.h"
#ifdef XPX
/*
 * proc(x,args) - convert x to a procedure if possible; use args to
 *  resolve ambiguous string names.
 */
Xproc(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   
   /*
    * If x is already a proc, just return it in arg0.
    */
   arg0 = arg1;
   DeRef(arg0)
   if (!QUAL(arg0) && TYPE(arg0) == T_PROC)
      return;
   /*
    * args defaults to 1.
    */
   defshort(&arg2, 1);
   /*
    * Attempt to convert arg0 to a procedure descriptor using args to
    *  discriminate between procedures with the same names.  Fail if
    *  the conversion isn't successful.
    */
   if (strprc(&arg0,INTVAL(arg2)))
      return;
   else
      fail();
   }

Procblock(proc,2)

#else XPX
char junk;			/* prevent null object file */
#endif XPX
