#include "../h/rt.h"

/*
 * reads(f,i) - read i chars on file f.
 */

Xreads(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int i, cnt;
   int status;
   long l1;
   FILE *f;

   deffile(&arg1, &input);
   defshort(&arg2, 1);

   f = BLKLOC(arg1)->file.fd;
   status = BLKLOC(arg1)->file.status;
   if ((status & FS_READ) == 0)
      runerr(212, &arg1);

   if ((cnt = arg2.value.integer) <= 0)
      runerr(205, &arg2);

   sneed(cnt);
   if (sfree + cnt > estrings)
      runerr(302, NULL);
   STRLOC(arg0) = sfree;
   if ((cnt = fread(STRLOC(arg0), sizeof(char), cnt, f)) <= 0)
      fail();
   STRLEN(arg0) = cnt;
   sfree += cnt;
   }

struct b_iproc Breads = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xreads),
   2,
   -1,
   0,
   0,
   {5, "reads"}
   };
