#include "../h/rt.h"

/*
 * system(s) - execute string s as a system command.
 */

Xsystem(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   char sbuf[MAXSTRING];

   DeRef(arg1)

   /*
    * s must be string and smaller than MAXSTRING characters long.
    */
   if (!QUAL(arg1) || STRLEN(arg1) < 0)
      runerr(103, &arg1);
   if (STRLEN(arg1) >= MAXSTRING)
      runerr(210, &arg1);
   qtos(&arg1, sbuf);

   /*
    * Pass the C string made by qtos to the UNIX system() function and
    *  return the exit code of the command as the result of system().
    */
   mkint((long)((system(sbuf) >> 8) & 0377), &arg0);
   }

Procblock(system,1)
