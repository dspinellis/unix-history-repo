#include "../h/rt.h"

/*
 * string(x) - convert x to string.
 */

Xstring(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   char sbuf[MAXSTRING];
   extern char *alcstr();

   arg0 = arg1;
   switch (cvstr(&arg0, sbuf)) {
      /*
       * If x isn't a string, allocate it and return it; if it is a
       *  string, just return it; fail if it isn't a string and can't
       *  be converted.
       */
      case 1:
         sneed(STRLEN(arg0));
         STRLOC(arg0) = alcstr(STRLOC(arg0), STRLEN(arg0));
      case 2:
         return;
      default:
         fail();
      }
   }

Procblock(string,1)
