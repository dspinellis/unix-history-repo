#include "../h/rt.h"

/*
 * repl(s,n) - concatenate n copies of string s.
 */
Xrepl(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int cnt;
   register char *sloc;
   long l1;
   char sbuf[MAXSTRING];
   extern char *alcstr();

   /*
    * Make sure that s is a string.
    */
   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);
   /*
    * Make sure that n is a (non-long) integer.
    */
   switch (cvint(&arg2, &l1)) {
      case T_INTEGER:
         if ((cnt = (int)l1) >= 0)
            break;
#ifdef LONGS
      case T_LONGINT:
#endif LONGS
            runerr(205, &arg2);
      default:
            runerr(101, &arg2);
      }

   /*
    * Be sure that resulting string won't be too long. (This is bogus
    *  on the VAX as MAXSHORT is 32k.)
    */
   if ((l1 * STRLEN(arg1)) > MAXSHORT)
      runerr(302, NULL);
   /*
    * Return a empty string if n == 0.
    */
   if (cnt == 0) {
      arg0.type = D_NULL;
      INTVAL(arg0) = 1;
      }
   else {
      /*
       * Ensure enough space for the replicated string and allocate a copy
       *  of s.  Then allocate and copy s n-1 times.
       */
      sneed(cnt * STRLEN(arg1));
      sloc = alcstr(STRLOC(arg1), STRLEN(arg1));
      cnt--;
      while (cnt--)
         alcstr(STRLOC(arg1), STRLEN(arg1));
      /*
       * Make arg0 a descriptor for the replicated string.
       */
      STRLEN(arg0) = (int)l1 * STRLEN(arg1);
      STRLOC(arg0) = sloc;
      }
   }

Procblock(repl,2)
