#include "../h/rt.h"

/*
 * close(f) - close file f.
 */

Xclose(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;

   deref(&arg1);
   if (QUAL(arg1) || TYPE(arg1) != T_FILE)
      runerr(105, &arg1);

   for (i = 0; i < numbufs; i++) {
      if (bufused[i] == BLKLOC(arg1)->file.fd) {
         bufused[i] = NULL;
         break;
         }
      }
   if (BLKLOC(arg1)->file.status & FS_PIPE)
      pclose(BLKLOC(arg1)->file.fd);
   else
      fclose(BLKLOC(arg1)->file.fd);
   BLKLOC(arg1)->file.status = 0;
   arg0 = arg1;
   }

struct b_iproc Bclose = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xclose),
   1,
   -1,
   0,
   0,
   {5, "close"}
   };
