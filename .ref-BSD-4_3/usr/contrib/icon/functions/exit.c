#include "../h/rt.h"

/*
 * exit(status) - exit process with specified status, defaults to 0.
 */

Xexit(nargs, arg1)
int nargs;
struct descrip arg1;
   {
   defshort(&arg1, 0);
   c_exit(INTVAL(arg1));
   }

Procblock(exit,1)
