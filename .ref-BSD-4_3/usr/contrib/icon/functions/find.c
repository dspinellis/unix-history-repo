#include "../h/rt.h"

/*
 * find(s1,s2,i,j) - find string s1 in s2[i:j] and return position in
 *  s2 of beginning of s1.
 * Generates successive positions.
 */

Xfind(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int l;
   register char *s1, *s2;
   int i, j, t;
   long l1, l2;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];

   /*
    * s1 must be a string.  s2 defaults to &subject; i defaults to &pos
    *  if s defaulted, 1 otherwise;  j defaults to 0.
    */
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if (defstr(&arg2, sbuf2, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   /*
    * Convert i and j to absolute positions in s2 and order them so i <= j.
    */
   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   /*
    * Loop through s2[i:j] trying to find s1 at each point, stopping
    *  when the remaining portion s2[i:j] is too short to contain s1.
    */
   while (i <= j - STRLEN(arg1)) {
      s1 = STRLOC(arg1);
      s2 = STRLOC(arg2) + i - 1;
      l = STRLEN(arg1);
      /*
       * Compare strings on bytewise basis; if end is reached before
       *  inequality is found, suspend the position of the string.
       */
      do {
         if (l-- <= 0) {
            arg0.type = D_INTEGER;
            INTVAL(arg0) = i;
            suspend();
            break;
            }
         } while (*s1++ == *s2++);
      i++;
      }

   fail();
   }

Procblock(find,4)
