#include "../h/rt.h"
#include "../h/gc.h"
int stkbase;
int stktop;

/*
 * ddump - dump a descriptor.  Used only for debugging.
 */

ddump(desc)
struct descrip *desc;
   {
   printf("%08x: ",desc);
   if (isptr(desc))
      if (VAR(*desc))
         printf("%15s","interior ptr.");
      else
         printf("%15s",blkname[(desc->type)&TYPEMASK]);
   else
      if (QUAL(*desc))
         printf("%15s","str. qualifier");
      else
         printf("%15s","integer");
   printf(" %08x %08x\n",desc->type,desc->value.integr);
   }


/*
 * mdump - dump the heap.  Used only for debugging.
 */

mdump()
   {
   register char *blk;
   register int type, size, fdesc;
   register struct descrip *ndesc;

   printf("\nDump of heap.  base:%08x free:%08x max:%08x\n",
           hpbase,hpfree,maxheap);
   printf("  loc     type              size  contents\n");

   for (blk = hpbase; blk < hpfree; blk += getsize(blk)) {
      type = blktype(blk);
      size = getsize(blk);
      printf(" %08x   %15s   %4d\n",blk,blkname[type],size);
      if ((fdesc = firstd[type]) > 0)
         for (ndesc = (struct descrip *) (blk + fdesc);
               ndesc < (struct descrip *) (blk + size);ndesc++)  {
            printf("                                 ");
            ddump(ndesc);
            }
      printf("\n");
      }
   printf("end of heap.\n");
   }
