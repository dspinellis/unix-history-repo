#include "../h/rt.h"

/*
 * mksubs - form a substring at position i, length j.
 * Makes a trapped variable if var is a variable; otherwise
 * creates a string qualifier.  Does the work for substr,
 * section, and subsc and bang (on strings only).
 */

struct descrip mksubs(var, val, i, j, result)
register struct descrip *var, *val, *result;
int i, j;
   {
   extern struct b_tvsubs *alcsubs();

   if (QUAL(*var) || !VAR(*var)) {	/* if not variable, return string */
      STRLEN(*result) = j;
      STRLOC(*result) = STRLOC(*val) + i - 1;
      return;
      }

   if (TVAR(*var)) {
      switch (TYPE(*var)) {
	 case T_TVSUBS:			/* if already substring, modify it */
	    i += BLKLOC(*var)->tvsubs.sspos - 1;
            var = &BLKLOC(*var)->tvsubs.ssvar;
            break;

	 default:			/* for other tv's, no special case */
	    break;
	 }
      }

   result->type = D_TVSUBS;		/* form a substring tv */
   BLKLOC(*result) = alcsubs(j, i, var);
   return;
   }
