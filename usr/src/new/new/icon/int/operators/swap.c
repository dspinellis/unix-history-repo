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
   if (QUAL(arg1) || !VAR(arg1))
      runerr(111, &arg1);
   if (QUAL(arg2) || !VAR(arg2))
      runerr(111, &arg2);
   arg0 = arg1;
   arg2v = arg2;
   adj1 = adj2 = 0;
   if (arg1.type == D_TVSUBS && arg2.type == D_TVSUBS) {
      bp1 = BLKLOC(arg1);
      bp2 = BLKLOC(arg2);
      if (VARLOC(bp1->tvsubs.ssvar) == VARLOC(bp2->tvsubs.ssvar)) {
         if (bp1->tvsubs.sspos > bp2->tvsubs.sspos)
            adj1 = bp1->tvsubs.sslen - bp2->tvsubs.sslen;
         else if (bp2->tvsubs.sspos > bp1->tvsubs.sspos)
            adj2 = bp2->tvsubs.sslen - bp1->tvsubs.sslen;
   	 }
      }
   deref(&arg1);
   deref(&arg2);
   doasgn(&arg0, &arg2); 	   /* lhs := rhs */
   if (adj2 != 0)
      BLKLOC(arg2)->tvsubs.sspos += adj2;
   doasgn(&arg2v, &arg1);	   /* rhs := lhs */
   if (adj1 != 0)
      BLKLOC(arg1)->tvsubs.sspos += adj1;
   ClearBound;
   }
struct b_iproc Bswap = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(swap),
   3,
   -1,
   -2,
   0,
   {3, ":=:"}
   };
