#include "../h/rt.h"
#ifdef SETS
/*
 * memb - sets res flag to 1 if x is a member of set S,
 *  sets res flag to 0 if not.
 *  Returns pointer to the descriptor which points to
 *  the element (or which would point to it if it were
 *  there).
 */

struct descrip *memb(ps,x,hn,res)
int *res;				/* pointer to integer result flag */
struct b_set *ps;
struct descrip  *x;
int hn;
   {
    register struct descrip *lp;
    register struct b_selem  *pe;
    extern int equiv();

   lp = &(ps->sbucks[hn % NBUCKETS]);
   /*
    * Look for x in the hash chain.
    */
   *res = 0;
   while (BLKLOC(*lp) != NULL) {
       pe = (struct b_selem *) BLKLOC(*lp);
       if ( pe->hnum > hn)		/* too far - it isn't there */
         return (lp);
       else if (( pe->hnum == hn ) && ( equiv(&pe->setmem, x )))  {
         *res = 1;
         return (lp);
         }
   /*
    * We haven't reached the right hashnumber yet or
    *  the element isn't the right one so keep looking.
    */
      lp = &(pe->sblink);
   }
   /*
    *  At end of chain - not there.
    */
   return (lp);
   }
#else SETS
char junk;   /* prevent null object file */
#endif SETS
