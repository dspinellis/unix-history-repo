#include "../h/rt.h"

/*
 * move(i) - move &pos by i, return substring of &subject spanned.
 *  Reverses effects if resumed.
 */
Xmove(nargs, oldsubj, arg1, arg0)
int nargs;
struct descrip oldsubj, arg1, arg0;
   {
   register int i, j;
   long l;
   int oldpos;

   /*
    * i must be a (non-long) integer.
    */
   switch (cvint(&arg1, &l)) {
      case T_INTEGER:  j = (int)l; break;
#ifdef LONGS
      case T_LONGINT:  fail();
#endif LONGS
      default:         runerr(101, &arg1);
      }

   /*
    * Save old &subject and &pos.  Local variable i holds &pos
    *  before the move.
    */
   oldsubj = k_subject;
   oldpos = i = k_pos;

   /*
    * If attempted move is past either end of the string, fail.
    */
   if (i + j <= 0 || i + j > STRLEN(k_subject) + 1)
      fail();

   /*
    * Set new &pos.
    */
   k_pos += j;

   /*
    * Make sure j >= 0.
    */
   if (j < 0) {
      i += j;
      j = -j;
      }

   /*
    * Suspend substring of &subject that was moved over.
    */
   STRLEN(arg0) = j;
   STRLOC(arg0) = STRLOC(k_subject) + i - 1;
   suspend();

   /*
    * If move is resumed, restore the old subject and position
    *  and fail.
    */
   k_subject = oldsubj;
   k_pos = oldpos;
   fail();
   }

Procblock(move,2)
