#include "../h/rt.h"

/*
 * close(f) - close file f.
 */

Xclose(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;

   /*
    * f must be a file.
    */
   DeRef(arg1)
   if (QUAL(arg1) || TYPE(arg1) != T_FILE)
      runerr(105, &arg1);

   /*
    * If f has a buffer associated with it, "free" it.
    */
   for (i = 0; i < numbufs; i++) {
      if (bufused[i] == BLKLOC(arg1)->file.fd) {
         bufused[i] = NULL;
         break;
         }
      }

   /*
    * Close f, using fclose or pclose as appropriate.
    */
   if (BLKLOC(arg1)->file.status & FS_PIPE)
      pclose(BLKLOC(arg1)->file.fd);
   else
      fclose(BLKLOC(arg1)->file.fd);
   BLKLOC(arg1)->file.status = 0;
   
   /*
    * Return the closed file.
    */
   arg0 = arg1;
   }

Procblock(close,1)
