#include "../h/rt.h"
#include "../h/record.h"

/*
 * x[y] - access yth character or element of x.
 */

subsc(nargs, arg1v, arg2, arg1, arg0)
int nargs;
struct descrip arg1v, arg2, arg1, arg0;
   {
   register int i, j;
   register union block *bp;
   int typ1;
   long l1;
   struct descrip *dp;
   char sbuf[MAXSTRING];
   extern char *alcstr();
   extern struct b_tvtbl *alctvtbl();

   SetBound;
   /*
    * Make a copy of x.
    */
   arg1v = arg1;

   if ((typ1 = cvstr(&arg1, sbuf)) != NULL) {
      /*
       * x is a string, make sure that y is an integer.
       */
      if (cvint(&arg2, &l1) == NULL)
         runerr(101, &arg2);
      /*
       * Convert y to a position in x and fail if the position is out
       *  of bounds.
       */
      i = cvpos(l1, STRLEN(arg1));
      if (i > STRLEN(arg1))
         fail();
      if (typ1 == 1) {
         /*
          * x was converted to a string, so it can't be assigned back into.
          *  Just return a string containing the selected character.
          */
         sneed(1);
         STRLEN(arg0) = 1;
         STRLOC(arg0) = alcstr(STRLOC(arg1)+i-1, 1);
         }
      else {
         /*
          * x is a string, make a substring trapped variable for the one
          *  character substring selected and return it.
          */
         hneed(sizeof(struct b_tvsubs));
         mksubs(&arg1v, &arg1, i, 1, &arg0);
         }
      ClearBound;
      return;
      }

   /*
    * x isn't a string or convertible to one, see if it's an aggregate.
    */
   DeRef(arg1)
   switch (TYPE(arg1)) {
      case T_LIST:
         /*
          * x is a list.  Make sure that y is an integer and that the
          *  subscript is in range.
          */
         if (cvint(&arg2, &l1) == NULL)
            runerr(101, &arg2);
         i = cvpos(l1, BLKLOC(arg1)->list.cursize);
         if (i > BLKLOC(arg1)->list.cursize)
            fail();
         /*
          * Locate the list block containing the desired element.
          */
         bp = BLKLOC(BLKLOC(arg1)->list.listhead);        
         j = 1;
         while (i >= j + bp->lelem.nused) {
            j += bp->lelem.nused;
            if (TYPE(bp->lelem.listnext) != T_LELEM)
               syserr("list reference out of bounds in subsc");
            bp = BLKLOC(bp->lelem.listnext);
            }
         /*
          * Locate the desired element in the block that contains it and
          *  return a pointer to it.
          */
         i += bp->lelem.first - j;
         if (i >= bp->lelem.nelem)
            i -= bp->lelem.nelem;
         dp = &bp->lelem.lslots[i];
         arg0.type = D_VAR + ((int *)dp - (int *)bp);
         VARLOC(arg0) = dp;
         ClearBound;
         return;

      case T_TABLE:
         /*
          * x is a table.  Dereference y and locate the appropriate bucket
          *  based on the hash value.
          */
         DeRef(arg2)
         hneed(sizeof(struct b_tvtbl));
         i = hash(&arg2);		/* get hash number of subscript  */
         bp = BLKLOC(BLKLOC(arg1)->table.buckets[i % NBUCKETS]);
         /*
          * Work down the chain of elements for the bucket and if an
          *  element with the desired subscript value is found, return
          *  a pointer to it.
          * Elements are ordered in the chain by hashnumber value
          * from smallest to largest.
          */
         while (bp != NULL) {
           if (bp->telem.hashnum > i)		/* past it - not there */
               break;
            if ((bp->telem.hashnum == i)  &&  (equiv(&bp->telem.tref, &arg2))) {
               dp = &bp->telem.tval;
               arg0.type = D_VAR + ((int *)dp - (int *)bp);
               VARLOC(arg0) = dp;
               ClearBound;
               return;
               }
            /* We haven't reached the right hashnumber yet or
             *  the element is not the right one.
             */
            bp = BLKLOC(bp->telem.blink);
            }
           /*
            * x[y] is not in the table, make a table element trapped variable
            *  and return it as the result.
            */
         arg0.type = D_TVTBL;
         BLKLOC(arg0) = (union block *) alctvtbl(&arg1, &arg2, i);
         ClearBound;
         return;

      case T_RECORD:
         /*
          * x is a record.  Convert y to an integer and be sure that it
          *  it is in range as a field number.
          */
         if (cvint(&arg2, &l1) == NULL)
            runerr(101, &arg2);
         bp = BLKLOC(arg1);
         i = cvpos(l1, bp->record.recptr->nfields);
         if (i > bp->record.recptr->nfields)
            fail();
         /*
          * Locate the appropriate field and return a pointer to it.
          */
         dp = &bp->record.fields[i-1];
           arg0.type = D_VAR + ((int *)dp - (int *)bp);
         VARLOC(arg0) = dp;
         ClearBound;
         return;

      default:
         /*
          * x is of a type that can't be subscripted.
          */
         runerr(114, &arg1);
      }        
   ClearBound;
   }

Opblockx(subsc,3,"[]",2)
