#include "../h/rt.h"
#ifdef VAX
/*
 * display(i,f) - display local variables of i most recent
 * procedure activations, plus global variables.
 * Output to file f (default &errout).
 */

Xdisplay(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int *ap, *r5; /* Note that position is important, we assume
   			that ap is in r11, and r5 is in r10 */
   register struct descrip *dp;
   register struct descrip *np;
   register int n;
   long l;
   int count;
   FILE *f;
   struct b_proc *bp;
   extern int *boundary;
#ifdef INT
   extern struct descrip *globals, *eglobals;
   extern struct descrip *gnames;
   extern struct descrip *statics;
#endif INT
#ifdef CMP
   extern struct descrip globals[], eglobals[];
   extern struct descrip gnames[];
   extern struct descrip statics[];
#endif CMP

   defint(&arg1, &l, k_level);
   deffile(&arg2, &errout);
   f = BLKLOC(arg2)->file.fd;
   if ((BLKLOC(arg2)->file.status & FS_WRITE) == 0)
      runerr(213, &arg2);

   if (l < 0)
      runerr(205, &arg1);
   else if (l > k_level)
      count = k_level;
   else
      count = l;

   r5 = boundary;		/* start r5 at most recent procedure frame */
   while (count--) {
      ap = r5[2];
      r5 = r5[3];
      n = ap[1];		/* get number of arguments */
      dp = ap + 2 + 2*n;	/* calculate address of procedure descriptor*/
      bp = BLKLOC(*dp);		/* get address of procedure block */

      /* print procedure name */
      putstr(f, STRLOC(bp->pname), STRLEN(bp->pname));
      fprintf(f, " local identifiers:\n");

      /* print arguments */
      np = bp->lnames;
      for (n = bp->nparam; n > 0; n--) {
	 fprintf(f, "   ");
	 putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
	 outimage(f, --dp, 0);
	 putc('\n', f);
	 np++;
	 }

      /* print local dynamics */
      dp = r5 - 2;
      for (n = bp->ndynam; n > 0; n--) {
	 fprintf(f, "   ");
	 putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
	 outimage(f, --dp, 0);
	 putc('\n', f);
	 np++;
	 }

      /* print local statics */
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
   arg0 = nulldesc;     	/* return &null */
   }

struct b_iproc Bdisplay = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xdisplay),
   2,
   -1,
   0,
   0,
   {7, "display"}
   };
#endif VAX
#ifdef PDP11
/*
 * display(i,f) - display local variables of i most recent
 * procedure activations, plus global variables.
 * Output to file f (default &errout).
 */

Xdisplay(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register struct descrip *dp;
   register struct descrip *np;
   register int n;
   register int *r5;
   long l;
   int count;
   static struct descrip errout = {D_FILE, &k_errout};
   FILE *f;
   struct b_proc *bp;
   extern int *boundary;
#ifdef INT
   extern struct descrip *globals, *eglobals;
   extern struct descrip *gnames;
   extern struct descrip *statics;
#endif INT
#ifdef CMP
   extern struct descrip globals[], eglobals[];
   extern struct descrip gnames[];
   extern struct descrip statics[];
#endif CMP

   defint(&arg1, &l, k_level);
   deffile(&arg2, &errout);
   f = BLKLOC(arg2)->file.fd;
   if ((BLKLOC(arg2)->file.status & FS_WRITE) == 0)
      runerr(213, &arg2);

   if (l < 0)
      runerr(205, &arg1);
   else if (l > k_level)
      count = k_level;
   else
      count = l;

   r5 = *boundary;		/* start r5 at most recent procedure frame */
   while (count--) {
      n = r5[2];		/* get number of arguments */
      dp = r5 + 3 + 2*n;	/* calculate address of procedure descriptor */
      bp = BLKLOC(*dp);		/* get address of procedure block */

      /* print procedure name */
      putstr(f, STRLOC(bp->pname), STRLEN(bp->pname));
      fprintf(f, " local identifiers:\n");

      /* print arguments */
      np = bp->lnames;
      for (n = bp->nparam; n > 0; n--) {
	 fprintf(f, "   ");
	 putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
	 outimage(f, --dp, 0);
	 putc('\n', f);
	 np++;
	 }

      /* print local dynamics */
      dp = r5 - 5;
      for (n = bp->ndynam; n > 0; n--) {
	 fprintf(f, "   ");
	 putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
	 outimage(f, --dp, 0);
	 putc('\n', f);
	 np++;
	 }

      /* print local statics */
      dp = &statics[bp->fstatic];
      for (n = bp->nstatic; n > 0; n--) {
	 fprintf(f, "   ");
	 putstr(f, STRLOC(*np), STRLEN(*np));
         fprintf(f, " = ");
	 outimage(f, dp++, 0);
	 putc('\n', f);
	 np++;
	 }

      r5 = *r5;
      }

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
   arg0 = nulldesc;     	/* return &null */
   }

struct b_iproc Bdisplay = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xdisplay),
   2,
   -1,
   0,
   0,
   {7, "display"}
   };
#endif PDP11
