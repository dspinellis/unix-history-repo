#include "../h/rt.h"
#include "../h/record.h"

/*
 * copy(x) - make a copy of object x.
 */

Xcopy(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i, j;
   struct descrip d, *dp, *d1, *d2;
   union block *bp, *ep, **tp;
   extern struct b_table *alctable();
   extern struct b_telem *alctelem();
   extern union block *allocate();

   deref(&arg1);

   if (NULLDESC(arg1) || QUAL(arg1))
      arg0 = arg1;
   else {
      switch (TYPE(arg1)) {
         case T_INTEGER:
#ifndef BIT32
         case T_LONGINT:
#endif
         case T_REAL:
         case T_FILE:
         case T_CSET:
         case T_PROC:
         case T_ESTACK:
            arg0 = arg1;
            break;

         case T_LIST:
	    cplist(&arg1, &arg0, 1, BLKLOC(arg1)->list.cursize + 1);
            break;

         case T_TABLE:
	    hneed((sizeof(struct b_table)) +
	          (sizeof(struct b_telem)) * BLKLOC(arg1)->table.cursize);
            bp = alctable(0);
            bp->table = BLKLOC(arg1)->table;
            for (i = 0; i < NBUCKETS; i++) {
               tp = &(BLKLOC(bp->table.buckets[i]));
               for (ep = *tp; ep != NULL; ep = *tp) {
                  *tp = alctelem();
                  (*tp)->telem = ep->telem;
                  tp = &(BLKLOC((*tp)->telem.blink));
                  }
               }
            arg0.type = D_TABLE;
            BLKLOC(arg0) = bp;
            break;

         case T_RECORD:
	    i = BLKLOC(arg1)->record.size;
	    hneed(i);
	    bp = allocate(i);
            bp->record = BLKLOC(arg1)->record;
            i = bp->record.recptr->nfields;
            d1 = bp->record.fields;
	    d2 = BLKLOC(arg1)->record.fields;
	    while (i--)
               *d1++ = *d2++;
            arg0.type = D_RECORD;
            BLKLOC(arg0) = bp;
            break;

         default:
            syserr("copy: illegal datatype.");
         }
      }
   }

struct b_iproc Bcopy = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xcopy),
   1,
   -1,
   0,
   0,
   {4, "copy"}
   };
