#include "../h/rt.h"

/*
 * match(s1,s2,i,j) - test if s1 is prefix of s2[i:j].
 */
Xmatch(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int i;
   register char *s1, *s2;
   int j, t;
   long l1, l2;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];

   /*
    * s1 must be a string.  s2 defaults to &subject;  i defaults to &pos
    *  if s defaulted, 1 otherwise; j defaults to 0.
    */
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if (defstr(&arg2, sbuf2, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   /*
    * Convert i and j to absolute positions and then make i the smaller
    *  of the two positions and make j the length of the substring.
    */
   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i > j) {
      t = i;
      i = j;
      j = t - j;
      }
   else
      j = j - i;

   /*
    * Can't match unless s1 is as long as s2[i:j].
    */
   if (j < STRLEN(arg1))
      fail();

   /*
    * Compare s1 with s2[i:j] for *s1 characters; fail if an inequality
    *  if found.
    */
   s1 = STRLOC(arg1);
   s2 = STRLOC(arg2) + i - 1;
   for (j = STRLEN(arg1); j > 0; j--)
      if (*s1++ != *s2++)
         fail();

   /*
    * Return position of end of matched string in s2.
    */
   arg0.type = D_INTEGER;
   INTVAL(arg0) = i + STRLEN(arg1);
   }

Procblock(match,4)
