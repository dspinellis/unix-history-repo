#include "../h/rt.h"

/*
 * mksubs - form a substring.  var is a descriptor for the string from
 *  which the substring is to be formed.  var may be a variable.  val
 *  is a dereferenced version of val.  The descriptor for the resulting
 *  substring is placed in *result.  The substring starts at position
 *  i and extends for j characters.
 */

struct descrip mksubs(var, val, i, j, result)
register struct descrip *var, *val, *result;
int i, j;
   {
   extern struct b_tvsubs *alcsubs();

   if (QUAL(*var) || !VAR(*var)) {
      /*
       * var isn't a variable, just form a descriptor that points into
       *  the string named by val.
       */
      STRLEN(*result) = j;
      STRLOC(*result) = STRLOC(*val) + i - 1;
      return;
      }

   if (TVAR(*var)) {
      /*
       * var is a trapped variable.  If it is a substring trapped variable,
       *  adjust the position and make var the substrung string.  For other
       *  trapped variables, don't do anything.
       */
      switch (TYPE(*var)) {
         case T_TVSUBS:
            i += BLKLOC(*var)->tvsubs.sspos - 1;
            var = &BLKLOC(*var)->tvsubs.ssvar;
            break;

         default:
            break;
         }
      }

   /*
    * Make a substring trapped variable by passing the buck to alcsubs.
    */
   result->type = D_TVSUBS;
   BLKLOC(*result) = (union block *) alcsubs(j, i, var);
   return;
   }
