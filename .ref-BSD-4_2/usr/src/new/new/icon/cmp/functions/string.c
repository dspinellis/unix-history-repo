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
      case 1:
	 sneed(STRLEN(arg0));
         STRLOC(arg0) = alcstr(STRLOC(arg0), STRLEN(arg0));
      case 2:
	 return;
      default:
	 fail();
      }
   }

struct b_iproc Bstring = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xstring),
   1,
   -1,
   0,
   0,
   {6, "string"}
   };
