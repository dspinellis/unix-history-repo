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

   /*
    * Default f to &input.
    */
   deffile(&arg1, &input);
   /*
    * Get a pointer to the file and be sure that it's open for reading.
    */
   f = BLKLOC(arg1)->file.fd;
   status = BLKLOC(arg1)->file.status;
   if ((status & FS_READ) == 0)
      runerr(212, &arg1);

   /*
    * Use getstr to read a line from the file, failing if getstr
    *  encounters end of file.
    */
   if ((slen = getstr(sbuf,MAXREADSTRING,f)) < 0)
      fail();
   /*
    * Allocate the string read and make arg0 a descriptor for it.
    */
   sneed(slen);
   STRLEN(arg0) = slen;
   STRLOC(arg0) = alcstr(sbuf,slen);
   }

Procblock(read,1)
