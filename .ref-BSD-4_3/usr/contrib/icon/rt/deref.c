#include "../h/rt.h"

/*
 * deref - dereference a descriptor.
 */
deref(d)
struct descrip *d;
   {
   register int i, j;
   register union block *bp;
   struct descrip v, tbl, tref;
   char sbuf[MAXSTRING];
   extern char *alcstr();

   if (!QUAL(*d) && VAR(*d)) {
      /*
       * *d is a variable and must be dereferenced.
       */
      if (TVAR(*d)) {
         switch (TYPE(*d)) {
            case T_TVSUBS:
               /*
                * A substring trapped variable is being dereferenced.  Point
                *  bp at the trapped variable block and v at the substrung
                *  string.
                */
               bp = TVARLOC(*d);
               v = bp->tvsubs.ssvar;
               /*
                * Convert v into a string.
                */
               switch (cvstr(&v, sbuf)) {
                  case NULL:
                     runerr(103,&v);
                  case 1:
                     /*
                      * A conversion to string was made.  Allocate the
                      *  converted string and fall through to common code
                      *  for allocated strings.  Note that bp is reassigned
                      *  in case the sneed caused a garbage collection.
                      */
                     sneed(STRLEN(v));
                     STRLOC(v) = alcstr(STRLOC(v), STRLEN(v));
                     bp = TVARLOC(*d);
                  case 2:
                     /*
                      * The substrung string is an allocated string.  Be sure
                      *  that the position is in range.
                      */
                     if (bp->tvsubs.sspos + bp->tvsubs.sslen - 1 > STRLEN(v))
                        runerr(205,NULL);
                     /*
                      * Make a descriptor for the substring by getting the
                      *  length and  pointing into the substrung string.
                      */
                     STRLEN(*d) = bp->tvsubs.sslen;
                     STRLOC(*d) = STRLOC(v) + bp->tvsubs.sspos - 1;
                  }
               break;

            case T_TVTBL:
               /*
                * A table element trapped variable is being dereferenced.
                *  Point tbl at the table header block,
                *  tref at the subscripting value,
                *  bp at the appropriate element chain.  Make d a
                *  descriptor for the default value in case the value
                *  referenced by the subscript isn't in the table.
                */
               if (BLKLOC(*d)->tvtbl.type == T_TELEM) {
               /*
                    * the tvtbl has been converted to a telem and is in the table
                * just replace d with the value of the element
                      */
                        *d = BLKLOC(*d)->telem.tval;
                        break;
                        }
               tbl = BLKLOC(*d)->tvtbl.tvtable;
               tref = BLKLOC(*d)->tvtbl.tvtref;
               i = BLKLOC(*d)->tvtbl.hashnum;
               *d = BLKLOC(tbl)->table.defvalue;
               bp = BLKLOC(BLKLOC(tbl)->table.buckets[i % NBUCKETS]);
               /*
                * Look down the element chain for the subscript value.
                *  If found, replace d with the value of the element.
                */
               while (bp != NULL) {
                  if (bp->telem.hashnum > i)
                           break;
                     if ((bp->telem.hashnum == i) &&
                       (equiv(&bp->telem.tref, &tref))) {
                                     *d = bp->telem.tval;
                                     break;
                                     }
                  bp = BLKLOC(bp->telem.blink);
                  }
               break;

            case T_TVPOS:
               /*
                * &pos is being dereferenced, make a descriptor for the
                *  appropriate integer value.
                */
               d->type = D_INTEGER;
               INTVAL(*d) = k_pos;
               break;

            case T_TVRAND:
               /*
                * &random is being dereferenced, use mkint to make a
                *  an integer descriptor from the value of k_random.
                */
               mkint(k_random, d);
               break;

            case T_TVTRACE:
               /*
                * &trace is being dereferenced, make a descriptor for the
                *  appropriate integer value.
                */
               d->type = D_INTEGER;
               INTVAL(*d) = k_trace;
               break;

            default:
               syserr("deref: illegal trapped variable");
            }
         }
      else
         /*
          * An ordinary variable is being dereferenced, just replace *d
          *  with the the descriptor *d is pointing to.
          */
         *d = *VARLOC(*d);
      }
#ifdef DEBUG
   if (!QUAL(*d) && VAR(*d))
      syserr("deref: didn't get dereferenced");
#endif DEBUG
   return (1);
   }
