#include "../h/rt.h"

/*
 * many(c,s,i,j) - find longest prefix of s[i:j] of characters in c.
 */

Xmany(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int i, j;
   int t, *cs, csbuf[CSETSIZE];
   long l1, l2;
   char sbuf[MAXSTRING];

   /*
    * c must be a cset.  s defaults to &subject;  i defaults to &pos if s
    *  defaulted, 1 otherwise;  j defaults to 0.
    */
   if (cvcset(&arg1, &cs, csbuf) == NULL)
      runerr(104, &arg1);
   if (defstr(&arg2, sbuf, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   /*
    * Convert i and j to absolute positions and order them.  If i == j,
    *  then the specified substring of s is the empty string and many
    *  fails.
    */
   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i == j)
      fail();
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   /*
    * Fail if first character of s[i:j] isn't in c.
    */
   if (!tstb(STRLOC(arg2)[i-1], cs))
      fail();

   /*
    * Move i along s[i:j] until a character that is not in c is found or
    *  the end of the string is reached.
    */
   i++;
   while (i < j && tstb(STRLOC(arg2)[i-1], cs))
      i++;

   /*
    * Return the position of the first character not in c.
    */
   arg0.type = D_INTEGER;
   INTVAL(arg0) = i;
   }

Procblock(many,4)
