#include "../h/rt.h"
#include "../h/record.h"

/*
 * copy(x) - make a copy of object x.
 */

Xcopy(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;
   struct descrip  *d1, *d2;
   union block *bp, *ep, **tp;
   extern struct b_table *alctable();
   extern struct b_telem *alctelem();
   extern struct b_set *alcset();
   extern struct b_selem *alcselem();
   extern union block *allocate();

   DeRef(arg1)

   if (NULLDESC(arg1) || QUAL(arg1))
      /*
       * x is a string or &null, just copy its descriptor
       *  into arg0.
       */
      arg0 = arg1;
   else {
      switch (TYPE(arg1)) {
         case T_INTEGER:
#ifdef LONGS
         case T_LONGINT:
#endif LONGS
         case T_REAL:
         case T_FILE:
         case T_CSET:
         case T_PROC:
         case T_ESTACK:
            /*
             * Copy integers, long integers, reals, files, csets, procedures,
             *  and co-expressions by copying the descriptor.  Note that for
             *  integers, this results in the assignment of a value, for the
             *  other types, a pointer is directed to a data block.
             */
            arg0 = arg1;
            break;

         case T_LIST:
            /*
             * Pass the buck to cplist to copy a list.
             */
            cplist(&arg1, &arg0, 1, BLKLOC(arg1)->list.cursize + 1);
            break;

         case T_TABLE:
            /*
             * Allocate space for table and elements and copy old table
             *  block into new.
             */
            hneed((sizeof(struct b_table)) +
                  (sizeof(struct b_telem)) * BLKLOC(arg1)->table.cursize);
            bp = (union block *) alctable(&nulldesc);
            bp->table = BLKLOC(arg1)->table;
            /*
             * Work down the chain of table element blocks in each bucket
             *  and create identical chains in new table.
             */
            for (i = 0; i < NBUCKETS; i++) {
               tp = &(BLKLOC(bp->table.buckets[i]));
               for (ep = *tp; ep != NULL; ep = *tp) {
                  *tp = (union block *) alctelem();
                  (*tp)->telem = ep->telem;
                  tp = &(BLKLOC((*tp)->telem.blink));
                  }
               }
            /*
             * Return the copied table.
             */
            arg0.type = D_TABLE;
            BLKLOC(arg0) = bp;
            break;

#ifdef SETS
         case T_SET:
            /*
             * Allocate space for set and elements and copy old set
             *  block into new.
             */
            hneed((sizeof(struct b_set)) +
                  (sizeof(struct b_selem)) * BLKLOC(arg1)->set.setsize);
            bp = (union block *) alcset(&nulldesc);
            bp->set = BLKLOC(arg1)->set;
            /*
             * Work down the chain of set elements in each bucket
             *  and create identical chains in new set.
             */
            for (i = 0; i < NBUCKETS; i++) {
               tp = &(BLKLOC(bp->set.sbucks[i]));
               for (ep = *tp; ep != NULL; ep = *tp) {
                  *tp = (union block *) alcselem(&nulldesc,0);
                  (*tp)->selem = ep->selem;
                  tp = &(BLKLOC((*tp)->selem.sblink));
                  }
               }
            /*
             * Return the copied set.
             */
            arg0.type = D_SET;
            BLKLOC(arg0) = bp;
            break;
#endif SETS

         case T_RECORD:
            /*
             * Allocate space for the new record and copy the old
             *  one into it.
             */
            i = BLKLOC(arg1)->record.size;
            hneed(i);
            bp = allocate(i);
            bp->record = BLKLOC(arg1)->record;
            /*
             * The above assignment doesn't copy the fields, they are
             *  copied individually via a loop.
             */
            i = bp->record.recptr->nfields;
            d1 = bp->record.fields;
            d2 = BLKLOC(arg1)->record.fields;
            while (i--)
               *d1++ = *d2++;
            /*
             * Return the copied record
             */
            arg0.type = D_RECORD;
            BLKLOC(arg0) = bp;
            break;

         default:
            syserr("copy: illegal datatype.");
         }
      }
   }

Procblock(copy,1)
