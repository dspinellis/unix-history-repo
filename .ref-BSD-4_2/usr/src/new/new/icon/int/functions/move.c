#include "../h/rt.h"

/*
 * move(i) - move &pos by i, return substring of &subject spanned.
 * Generator (reversible).
 */

Xmove(nargs, oldsubj, arg1, arg0)
int nargs;
struct descrip oldsubj, arg1, arg0;
   {
   register int i, j;
   long l;
   int oldpos;

   switch (cvint(&arg1, &l)) {
      case T_INTEGER:  j = (int)l; break;
#ifndef BIT32
      case T_LONGINT:  fail();
#endif
      default:         runerr(101, &arg1);
      }

   oldsubj = k_subject;		/* save old &subject and &pos */
   oldpos = i = k_pos;

   if (i + j <= 0 || i + j > STRLEN(k_subject) + 1)
      fail();

   k_pos += j;			/* set new &pos */

   if (j < 0) {			/* make sure j >= 0 */
      i += j;
      j = -j;
      }

   STRLEN(arg0) = j;
   STRLOC(arg0) = STRLOC(k_subject) + i - 1;
   suspend();

   k_subject = oldsubj;
   k_pos = oldpos;
   fail();
   }

struct b_iproc Bmove = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xmove),
   2,
   -1,
   0,
   0,
   {4, "move"}
   };
