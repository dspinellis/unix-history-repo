#include "../h/rt.h"

/*
 * exit(status) - exit process with status.
 */

Xexit(nargs, arg1)
int nargs;
struct descrip arg1;
   {
   defshort(&arg1, 0);
   c_exit(arg1.value.integer);
   }

struct b_iproc Bexit = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xexit),
   1,
   -1,
   0,
   0,
   {4, "exit"}
   };
