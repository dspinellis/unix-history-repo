#include "../h/rt.h"

/*
 * hash - compute hash value on arbitrary object for table accessing.
 */

hash(d)
struct descrip *d;
   {
   register int i, j;
   register char *s;

   if (QUAL(*d)) {			/* string */
      i = 0;
      s = STRLOC(*d);
      for (j = STRLEN(*d); j > 0; j--)
	 i += *s++ & 0377;
      }
   else {
      switch (TYPE(*d)) {
      	 case T_INTEGER:		/* integer */
      	    i = INTVAL(*d);
	    break;

#ifndef BIT32
      	 case T_LONGINT:		/* long integer */
      	    i = BLKLOC(*d)->longint.intval;
	    break;

#endif
      	 case T_REAL:			/* real number */
      	    i = BLKLOC(*d)->real.realval;
	    break;

      	 case T_CSET:			/* cset */
      	    i = 0;
      	    for (j = 0; j < CSETSIZE; j++)
      	       i ^= BLKLOC(*d)->cset.bits[j];
	    break;

	 default:			/* anything else */
	    i = TYPE(*d);
	    break;
	 }
      }
   return ((unsigned)i % NBUCKETS);
   }

