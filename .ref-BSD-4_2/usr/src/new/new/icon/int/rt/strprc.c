#include <ctype.h>
#include "../h/rt.h"
#ifdef EXT
#include "../h/pnames.h"
/*
 * strprc - take string referenced by descrip d, and make descrip d
 *           reference procedure named by the string (if any)
 */
struct b_proc *strprc(d,n)
struct descrip *d;
int n;
   {
      extern struct descrip *gnames, *globals, *eglobals;
      struct descrip *np, *gp;
      struct pstrnm *p;
      char *s;
      int ns, l;
      
      /* Look in global name table first */
      np = gnames; gp = globals;
      while (gp < eglobals) {
         if (!lexcmp(np++,d))
	    if (BLKLOC(*gp)->type == T_PROC) {
	       STRLEN(*d) = D_PROC; /* really type field */
	       BLKLOC(*d) = BLKLOC(*gp);
	       return 1;
	       }
	 gp++;
	 }
      /* If the name is not a global, see if it is a builtin or an op */
      s = STRLOC(*d);
      l = STRLEN(*d);
      for (p = pntab; p->pstrep; p++)
         if (!slcmp(s,l,p->pstrep)) {
	    if (isalpha(*s) || (p->pblock->nparam == n)) {
	       STRLEN(*d) = D_PROC; /* really type field */
	       BLKLOC(*d) = p->pblock;
	       return 1;
	       }
	    else
	       if ((ns = p->pblock->nstatic) < 0)
	          if (-ns == n) {
	             STRLEN(*d) = D_PROC; /* really type field */
	             BLKLOC(*d) = p->pblock;
	             return 1;
	             }
	    }
      return 0;
   }
/*
 * slcmp - lexically compare l1 bytes of s1 with null terminated s2
 *  (A lexcmp clone)
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
#endif EXT

