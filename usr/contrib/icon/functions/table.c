#include "../h/rt.h"

/*
 * table(x) - create a table with default value x.
 */
Xtable(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   extern struct b_table *alctable();

   hneed(sizeof(struct b_table));
   DeRef(arg1)
   arg0.type = D_TABLE;
   BLKLOC(arg0) = (union block *) alctable(&arg1);
   }

Procblock(table,1)
