/*
#	SEEK(3.icon)
#
#	Seek to position in stream
#
#	Stephen B. Wampler
#
#	Last modified 8/19/84
#
*/

#include "../h/rt.h"

/*
 * seek(file,offset,start) - seek to offset byte from start in file.
 */

Xseek(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   long l1, l2;
   int status;
   FILE *fd;
   long ftell();

   DeRef(arg1)
   if (arg1.type != D_FILE)
      runerr(106);

   defint(&arg2, &l1, 0);
   defshort(&arg3, 0);

   fd = BLKLOC(arg1)->file.fd;

   if ((BLKLOC(arg1)->file.status == 0) ||
       (fseek(fd, l1, arg3.value.integr) == -1))
      fail();
   mkint(ftell(fd), &arg0);
   }

Procblock(seek,3)
