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

   if (cvint(&arg1, &l) == NULL)
      runerr(101, &arg1);

   if ((i = cvpos(l, STRLEN(k_subject))) != k_pos)
      fail();
   arg0.type = D_INTEGER;
   BLKLOC(arg0) = i;
   }

struct b_iproc Bpos = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xpos),
   1,
   -1,
   0,
   0,
   {3, "pos"}
   };
