#include "../h/rt.h"

/*
 * stop(a,b,...) - stop and write arguments (starting on error output).
 */

Xstop(nargs)
int nargs;
   {
   register int n;
   char sbuf[MAXSTRING];
   struct descrip arg;
   FILE *f;

   f = stderr;

   for (n = 1; n <= nargs; n++) {
      arg = ARG(n);
      deref(&arg);

      if (!QUAL(arg) && TYPE(arg) == T_FILE) {
         if (n > 1)
            putc('\n', f);
         if ((BLKLOC(arg)->file.status & FS_WRITE) == 0)
   	    runerr(213, &arg);
         f = BLKLOC(arg)->file.fd;
         }
      else {
	 if (n == 1 && (k_output.status & FS_WRITE) == 0)
	    runerr(213, NULL);
	 defany(&arg, &nullstr);
	 if (cvstr(&arg, sbuf) == NULL)
	    runerr(109, &arg);
         putstr(f, STRLOC(arg), STRLEN(arg));
         }
      }

   putc('\n', f);
   c_exit(1);
   }

struct b_iproc Bstop = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xstop),
   -1,
   -1,
   0,
   0,
   {4, "stop"}
   };
