#include "../h/rt.h"
/*
 * display(i,f) - display local variables of i most recent
 * procedure activations, plus global variables.
 * Output to file f (default &errout).
 */

Xdisplay(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int *ap, *fp;
   register struct descrip *dp;
   register struct descrip *np;
   register int n;
   long l;
   int count;
   FILE *f;
   struct b_proc *bp;
   extern int *boundary;
   extern struct descrip *globals, *eglobals;
   extern struct descrip *gnames;
   extern struct descrip *statics;

   /*
    * i defaults to &level; f defaults to &errout.
    */
   defint(&arg1, &l, k_level);
   deffile(&arg2, &errout);
   /*
    * Produce error if file can't be written on.
    */
   f = BLKLOC(arg2)->file.fd;
   if ((BLKLOC(arg2)->file.status & FS_WRITE) == 0)
      runerr(213, &arg2);

   /*
    * Produce error if i is negative; constrain i to be >= &level.
    */
   if (l < 0)
      runerr(205, &arg1);
   else if (l > k_level)
      count = k_level;
   else
      count = l;

   fp = boundary;		/* start fp at most recent procedure frame */
   while (count--) {		/* go back through 'count' frames */
#ifdef VAX
      ap = (int *) fp[2];	/* get old ap */
      fp = (int *) fp[3];	/* get old fp */
      if (fp == 0)		/* only trace back to start of current stack*/
         break;
      n = ap[1];		/* get number of arguments */
				/* calculate address of procedure descriptor*/
      dp = (struct descrip *) (ap + 2 + 2*n);
#endif VAX
#ifdef PORT
/*
 * Insert code here to calculate (in dp) the address of arg0 in
 *  procedure frame prior to one referenced by fp.
 */
#endif PORT
#ifdef PDP11
      fp = fp[0];		/* get old fp */
      if (fp == 0)		/* only trace back to start of current stack*/
         break;
      n = fp[2];		/* get number of arguments */
      dp = fp + 3 + 2*n;	/* calculate address of procedure descriptor*/
#endif PDP11
      bp = (struct b_proc *) BLKLOC(*dp);	/* get address of procedure block */

      /*
       * Print procedure name.
       */
      putstr(f, STRLOC(bp->pname), STRLEN(bp->pname));
      fprintf(f, " local identifiers:\n");

      /*
       * Print arguments.
       */
      np = bp->lnames;
      for (n = bp->nparam; n > 0; n--) {
         fprintf(f, "   ");
         putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
         outimage(f, --dp, 0);
         putc('\n', f);
         np++;
         }

      /*
       * Print local dynamics.
       */
      dp = (struct descrip *) (fp - FRAMELIMIT);
      for (n = bp->ndynam; n > 0; n--) {
         fprintf(f, "   ");
         putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
         outimage(f, --dp, 0);
         putc('\n', f);
         np++;
         }

      /*
       * Print local statics.
       */
      dp = &statics[bp->fstatic];
      for (n = bp->nstatic; n > 0; n--) {
         fprintf(f, "   ");
         putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
         outimage(f, dp++, 0);
         putc('\n', f);
         np++;
         }

      }

   /*
    * Print globals.
    */
   fprintf(f, "global identifiers:\n");
   dp = globals;
   np = gnames;
   while (dp < eglobals) {
      fprintf(f, "   ");
      putstr(f, STRLOC(*np), STRLEN(*np));
      fprintf(f, " = ");
      outimage(f, dp++, 0);
      putc('\n', f);
      np++;
      }
   fflush(f);
   arg0 = nulldesc;     	/* Return null value. */
   }

Procblock(display,2)

