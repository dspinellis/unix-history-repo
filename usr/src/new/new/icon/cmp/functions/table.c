#include "../h/rt.h"

/*
 * table(def) - create a table of default value def.
 */

Xtable(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   extern struct b_table *alctable();

   hneed(sizeof(struct b_table));

   deref(&arg1);
   arg0.type = D_TABLE;
   BLKLOC(arg0) = alctable(&arg1);
   }

struct b_iproc Btable = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xtable),
   1,
   -1,
   0,
   0,
   {5, "table"}
   };
