/*
#	GETENV(3.icon)
#
#	Get contents of environment variables
#
#	Stephen B. Wampler
#
#	Last modified 8/19/84
#
*/

#include "../h/rt.h"

/*
 * getenv(s) - return contents of environment variable s
 */

Xgetenv(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register char *p;
   register int len;
   char sbuf[MAXSTRING];
   extern char *getenv();
   extern char *alcstr();

   DeRef(arg1)

   if (!QUAL(arg1))			/* check legality of argument */
      runerr(103, &arg1);
   if (STRLEN(arg1) <= 0 || STRLEN(arg1) >= MAXSTRING)
      runerr(401, &arg1);
   qtos(&arg1, sbuf);			/* convert argument to C-style string */

   if ((p = getenv(sbuf)) != NULL) {	/* get environment variable */
      len = strlen(p);
      sneed(len);
      STRLEN(arg0) = len;
      STRLOC(arg0) = alcstr(p, len);
      }
   else					/* fail if variable not in environment */
      fail();
   }

Procblock(getenv,-1)
