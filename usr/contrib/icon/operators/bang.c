#include "../h/rt.h"
#include "../h/record.h"

/*
 * !x - generate successive values from object x.
 */

bang(nargs, arg1v, arg1, arg0)
int nargs;
struct descrip arg1v, arg1, arg0;
   {
   register int i, j, slen;
   register union block *bp, *ep;
   register struct descrip *dp;
   int typ1;
   char sbuf[MAXSTRING];
   FILE *fd;
   extern char *alcstr();

   SetBound;
   arg1v = arg1;

   if ((typ1 = cvstr(&arg1, sbuf)) != NULL) {
      /*
       * A string is being banged.
       */
      i = 1;
      while (i <= STRLEN(arg1)) {
         /*
          * Loop through the string using i as an index.
          */
         if (typ1 == 1) {
            /*
             * x was converted to a string, thus, the resulting string
             *  can't be modified and a trapped variable isn't needed.
             *  Make a one-character string out of the next character
             *  in x and suspend it.
             */
            sneed(1);
            STRLEN(arg0) = 1;
            STRLOC(arg0) = alcstr(STRLOC(arg1)+i-1, 1);
            suspend();
            }
         else {
            /*
             * x a string and thus a trapped variable must be made
             *  for the one character string being suspended.
             */
            hneed(sizeof(struct b_tvsubs));
            mksubs(&arg1v, &arg1, i, 1, &arg0);
            suspend();
            arg1 = arg1v;
            DeRef(arg1)
            if (!QUAL(arg1))
               runerr(103, &arg1);
            }
         i++;
         }
      }
   else {
      /*
       * x isn't a string.
       */
      DeRef(arg1)
      switch (TYPE(arg1)) {
         case T_LIST:
            /*
             * x is a list.  Chain through each list element block and for
             *  each one, suspend with a variable pointing to each
             *  element contained in the block.
             */
            bp = BLKLOC(arg1);
            for (arg1 = bp->list.listhead; arg1.type == D_LELEM;
               arg1 = BLKLOC(arg1)->lelem.listnext) {
               bp = BLKLOC(arg1);
               for (i = 0; i < bp->lelem.nused; i++) {
                  j = bp->lelem.first + i;
                  if (j >= bp->lelem.nelem)
                     j -= bp->lelem.nelem;
                  dp = &bp->lelem.lslots[j];
                  arg0.type = D_VAR + ((int *)dp - (int *)bp);
                  VARLOC(arg0) = dp;
                  suspend();
                  bp = BLKLOC(arg1);   /* bp is untended, must reset */
                  }
               }
            break;


         case T_FILE:
            /*
             * x is a file.  Read the next line into the string space
             *  and suspend the newly allocated string.
             */
            fd = BLKLOC(arg1)->file.fd;
            if ((BLKLOC(arg1)->file.status & FS_READ) == 0)
               runerr(212, &arg1);
            while ((slen = getstr(sbuf,MAXSTRING,fd)) >= 0) {
               sneed(slen);
               STRLEN(arg0) = slen;
               STRLOC(arg0) = alcstr(sbuf,slen);
               suspend();
               }
            break;

         case T_TABLE:
            /*
             * x is a table.  Chain down the element list in each bucket
             *  and suspend a variable pointing to each element in turn.
             */
            for (i = 0; i < NBUCKETS; i++) {
               bp = BLKLOC(arg1);
               for (arg1v = bp->table.buckets[i]; arg1v.type == D_TELEM;
                    arg1v = BLKLOC(arg1v)->telem.blink) {
                  ep = BLKLOC(arg1v);
                  dp = &ep->telem.tval;
                  arg0.type = D_VAR + ((int *)dp - (int *)bp);
                  VARLOC(arg0) = dp;
                  suspend();
                  bp = BLKLOC(arg1);   /* bp is untended, must reset */
                  }
               }
            break;

#ifdef SETS
         case T_SET:
           /*
            *  This is similar to the method for tables except that a
            *  value is returned instead of a variable.
            */
               for(i = 0; i < NBUCKETS; i++) {
                  bp = BLKLOC(arg1);
                  for(arg1v = bp->set.sbucks[i]; arg1v.type == D_SELEM;
                     arg1v = BLKLOC(arg1v)->selem.sblink) {
                     arg0 = BLKLOC(arg1v)->selem.setmem;
                     suspend();
                    bp = BLKLOC(arg1);	/* bp untended, must be reset */
                    }
                }
                break;
#endif SETS

         case T_RECORD:
            /*
             * x is a record.  Loop through the fields and suspend
             *  a variable pointing to each one.
             */
            bp = BLKLOC(arg1);
            j = bp->record.recptr->nfields;
            for (i = 0; i < j; i++) {
               dp = &bp->record.fields[i];
               arg0.type = D_VAR + ((int *)dp - (int *)bp);
               VARLOC(arg0) = dp;
               suspend();
               bp = BLKLOC(arg1);   /* bp is untended, must reset */
               }
            break;

         default: /* This object can not be compromised. */
            runerr(116, &arg1);
         }
      }

   /*
    * Eventually fail.
    */
   fail();
   }

Opblockx(bang,2,"!",1)
