#include "../h/rt.h"

/*
 * pos(i) - test if &pos is at position i in &subject.
 */
Xpos(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;
   long l;

   /*
    * i must be an integer.
    */
   if (cvint(&arg1, &l) == NULL)
      runerr(101, &arg1);

   /*
    * Fail if &pos isn't equivalent to i, return i otherwise.
    */
   if ((i = cvpos(l, STRLEN(k_subject))) != k_pos)
      fail();
   arg0.type = D_INTEGER;
   INTVAL(arg0) = i;
   }

Procblock(pos,1)
