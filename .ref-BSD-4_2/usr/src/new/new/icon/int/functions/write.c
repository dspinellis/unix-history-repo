#include "../h/rt.h"

/*
 * write(a,b,...) - write arguments.
 */

Xwrite(nargs)
int nargs;
   {
   register int n;
   char sbuf[MAXSTRING];
   struct descrip arg;
   FILE *f;

   f = stdout;
   arg = nullstr;

   for (n = 1; n <= nargs; n++) {
      arg = ARG(n);
      deref(&arg);

      if (!QUAL(arg) && TYPE(arg) == T_FILE) {
         if (n > 1) {
            putc('\n', f);
	    /* Added fflush for buffering--whm Fri Feb 25 01:59:20 1983 */
	    fflush(f);
	    }
         if ((BLKLOC(arg)->file.status & FS_WRITE) == 0)
      	    runerr(213, &arg);
         f = BLKLOC(arg)->file.fd;
         arg = nullstr;
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
   /* Added fflush for buffering--whm */
   fflush(f);
   if (STRLOC(arg) >= sbuf && STRLOC(arg) < sbuf + MAXSTRING) {
      sneed(STRLEN(arg));
      STRLOC(arg) = alcstr(STRLOC(arg), STRLEN(arg));
      }
   ARG(0) = arg;
   }

struct b_iproc Bwrite = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xwrite),
   -1,
   -1,
   0,
   0,
   {5, "write"}
   };
