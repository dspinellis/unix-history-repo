#include "../h/rt.h"

/*
 * reads(f,i) - read i characters on file f.
 */
Xreads(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int cnt;
   int status;
   FILE *f;

   /*
    * f defaults to &input and i defaults to 1 (character).
    */
   deffile(&arg1, &input);
   defshort(&arg2, 1);

   /*
    * Get a pointer to the file and be sure that it's open for reading.
    */
   f = BLKLOC(arg1)->file.fd;
   status = BLKLOC(arg1)->file.status;
   if ((status & FS_READ) == 0)
      runerr(212, &arg1);

   /*
    * Be sure that a positive number of bytes is to be read.
    */
   if ((cnt = INTVAL(arg2)) <= 0)
      runerr(205, &arg2);

   /*
    * Ensure that enough space for the string exists and read it directly
    *  into the string space.  (By reading directly into the string space,
    *  no arbitrary restrictions are placed on the size of the string that
    *  can be read.)  Make arg0 a descriptor for the string and return it.
    */
   sneed(cnt);
   if (sfree + cnt > estrings)
      runerr(302, NULL);
   STRLOC(arg0) = sfree;
   if ((cnt = fread(STRLOC(arg0), sizeof(char), cnt, f)) <= 0)
      fail();
   STRLEN(arg0) = cnt;
   sfree += cnt;
   }

Procblock(reads,2)
