#include <ctype.h>
#include "../h/rt.h"
#ifdef XPX
#include "../h/pnames.h"
/*
 * strprc - Convert the qualified string named by *d into a procedure
 *  descriptor if possible.  n is the number of arguments that the desired
 *  procedure has.  n is only used when the name of the procedure is
 *  non-alphabetic (hence, an operator).
 *  A return value of 1 indicates successful conversion.
 *  0 indicates that the string could not be converted.
 */
strprc(d,n)
struct descrip *d;
int n;
   {
      extern struct descrip *gnames, *globals, *eglobals;
      struct descrip *np, *gp;
      struct pstrnm *p;
      char *s;
      int ns, l;
      
      /*
       * Look in global name list first.
       */
      np = gnames; gp = globals;
      while (gp < eglobals) {
         if (!lexcmp(np++,d))
            if (BLKLOC(*gp)->proc.type == T_PROC) {
               STRLEN(*d) = D_PROC; /* really type field */
               BLKLOC(*d) = BLKLOC(*gp);
               return 1;
               }
         gp++;
         }
      /*
       * The name is not a global, see if it is a builtin or an operator.
       */
      s = STRLOC(*d);
      l = STRLEN(*d);
      for (p = pntab; p->pstrep; p++)
         /*
          * Compare the desired name with each standard procedure/operator
          *  name.
          */
         if (!slcmp(s,l,p->pstrep)) {
            if (isalpha(*s)) {
               /*
                * The names are the same and s starts with an alphabetic,
                *  so it's the one being looked for; return it.
                */
               STRLEN(*d) = D_PROC;
               BLKLOC(*d) = (union block *) p->pblock;
               return 1;
               }
            if ((ns = p->pblock->nstatic) < 0)
               ns = -ns;
            else
               ns = p->pblock->nparam;
            if (n == ns) {
               STRLEN(*d) = D_PROC; /* really type field */
               BLKLOC(*d) = (union block *) p->pblock;
               return 1;
               }
            }
      return 0;
   }

/*
 * slcmp - lexically compare l1 bytes of s1 with null-terminated s2.
 */

slcmp(s1, l1, s2)
int l1;
char *s1,*s2;
   {
   register int minlen;
   int l2;

   l2 = strlen(s2);

   minlen = (l1 <= l2) ? l1 : l2;

   while (minlen--)
      if (*s1++ != *s2++)
         return ((*--s1 & 0377) - (*--s2 & 0377));

   return (l1 - l2);
   }
#else XPX
char junk;	/* prevent null object module */
#endif XPX
