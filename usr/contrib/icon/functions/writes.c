#include "../h/rt.h"

/*
 * writes(a,b,...) - write arguments without newline terminator.
 */

Xwrites(nargs)
int nargs;
   {
   register int n;
   char sbuf[MAXSTRING];
   struct descrip arg;
   FILE *f;
   extern char *alcstr();

   f = stdout;
   arg = nullstr;

   /*
    * Loop through the arguments, dereferencing each in turn.
    */
   for (n = 1; n <= nargs; n++) {
      arg = ARG(n);
      DeRef(arg)

      if (!QUAL(arg) && TYPE(arg) == T_FILE) {/* Current argument is a file */
         /*
          * Switch the current file to the file named by the current argument
          *  providing it is a file.  arg is made to be a empty string to
          *  avoid a special case.
          */
         if ((BLKLOC(arg)->file.status & FS_WRITE) == 0)
            runerr(213, &arg);
         f = BLKLOC(arg)->file.fd;
         arg = nullstr;
         }
      else {	/* Current argument is a string */
         /*
          * On first argument, check to be sure that &output is open
          *  for output.
          */
         if (n == 1 && (k_output.status & FS_WRITE) == 0)
            runerr(213, NULL);
         /*
          * Convert the argument to a string, defaulting to a empty string.
          */
         defany(&arg, &nullstr);
         if (cvstr(&arg, sbuf) == NULL)
            runerr(109, &arg);
         /*
          * Output the string and flush the file.
          */
         putstr(f, STRLOC(arg), STRLEN(arg));
         fflush(f);
         }
      }
   /*
    * If the beginning of the last string output lies in sbuf,
    *  allocate it as a real string.  Note that some of the string
    *  conversions don't always leave the converted string at the
    *  start of the conversion buffer, hence the range check.
    */
   if (STRLOC(arg) >= sbuf && STRLOC(arg) < sbuf + MAXSTRING) {
      sneed(STRLEN(arg));
      STRLOC(arg) = alcstr(STRLOC(arg), STRLEN(arg));
      }
   /*
    * Return the string corresponding to the last argument.
    */
   ARG(0) = arg;
   }

Procblock(writes,-1)
