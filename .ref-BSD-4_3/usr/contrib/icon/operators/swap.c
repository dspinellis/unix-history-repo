#include "../h/rt.h"

/*
 * x :=: y - swap values of x and y.
 */

swap(nargs, arg2v, arg2, arg1, arg0)
int nargs;
struct descrip arg2v, arg2, arg1, arg0;
   {
   register union block *bp1, *bp2;
   int adj1, adj2;

   SetBound;
   /*
    * x and y must be variables.
    */
   if (QUAL(arg1) || !VAR(arg1))
      runerr(111, &arg1);
   if (QUAL(arg2) || !VAR(arg2))
      runerr(111, &arg2);
   /*
    * Make copies of x and y as variables in arg0 and arg2v.
    */
   arg0 = arg1;
   arg2v = arg2;
   adj1 = adj2 = 0;
   if (arg1.type == D_TVSUBS && arg2.type == D_TVSUBS) {
      bp1 = BLKLOC(arg1);
      bp2 = BLKLOC(arg2);
      if (VARLOC(bp1->tvsubs.ssvar) == VARLOC(bp2->tvsubs.ssvar)) {
         /*
	  * x and y are both substrings of the same string, set
	  *  adj1 and adj2 for use in locating the substrings after
	  *  an assignment has been made.  If x is to the right of y,
	  *  set adj1 := *x - *y, otherwise if y is to the right of x,
	  *  set adj2 := *y - *x.  Note that the adjustment values may
	  *  be negative.
	  */
         if (bp1->tvsubs.sspos > bp2->tvsubs.sspos)
            adj1 = bp1->tvsubs.sslen - bp2->tvsubs.sslen;
         else if (bp2->tvsubs.sspos > bp1->tvsubs.sspos)
            adj2 = bp2->tvsubs.sslen - bp1->tvsubs.sslen;
   	 }
      }
   DeRef(arg1)
   DeRef(arg2)
   /*
    * Do x := y
    */
   doasgn(&arg0, &arg2);
   if (adj2 != 0)
      /*
       * y is to the right of x and the assignment x := y has shifted
       *  the position of y.  Add adj2 to the position of y to account
       *  for the replacement of x by y.
       */
      BLKLOC(arg2)->tvsubs.sspos += adj2;
   /*
    * Do y := x
    */
   doasgn(&arg2v, &arg1);
   if (adj1 != 0)
      /*
       * x is to the right of y and the assignment y := x has shifted
       *  the position of x.  Add adj2 to the position of x to account
       *  for the replacement of y by x.
       */
      BLKLOC(arg1)->tvsubs.sspos += adj1;
   ClearBound;
   }

Opblockx(swap,3,":=:",2)
