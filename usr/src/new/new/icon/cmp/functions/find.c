#include "../h/rt.h"

/*
 * find(s1,s2,i,j) - find string s1 in s2[i:j].
 * Returns position in s2 of beginning of s1.
 * Generator.
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

   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if (defstr(&arg2, sbuf2, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   while (i <= j - STRLEN(arg1)) {
      s1 = STRLOC(arg1);
      s2 = STRLOC(arg2) + i - 1;
      l = STRLEN(arg1);
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

struct b_iproc Bfind = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xfind),
   4,
   -1,
   0,
   0,
   {4, "find"}
   };
