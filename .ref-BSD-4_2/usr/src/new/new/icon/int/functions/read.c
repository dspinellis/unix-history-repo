#include "../h/rt.h"

/*
 * read(f) - read line on file f.
 */

Xread(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int slen;
   int status;
   char sbuf[MAXREADSTRING];
   FILE *f;
   extern char *alcstr();

   deffile(&arg1, &input);
   f = BLKLOC(arg1)->file.fd;
   status = BLKLOC(arg1)->file.status;
   if ((status & FS_READ) == 0)
      runerr(212, &arg1);

   if ((slen = getstr(sbuf,MAXREADSTRING,f)) < 0)
      fail();
   sneed(slen);
   STRLEN(arg0) = slen;
   STRLOC(arg0) = alcstr(sbuf,slen);
   }

struct b_iproc Bread = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xread),
   1,
   -1,
   0,
   0,
   {4, "read"}
   };
