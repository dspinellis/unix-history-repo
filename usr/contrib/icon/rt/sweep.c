#include "../h/rt.h"
#include "../h/gc.h"
#ifdef VAX
/*
 * sweep - sweep the stack, marking all descriptors there.  Method
 *  is to start at a known point, specifically, the frame that the
 *  fp points to, and then trace back along the stack looking for
 *  descriptors and local variables, marking them when they are found.
 *  The sp starts at the first frame, and then is moved up through
 *  the stack.  Procedure, generator, and expression frames are
 *  recognized when the sp is a certain distance from the fp, gfp,
 *  and efp respectively.
 *
 * Sweeping problems can be manifested in a variety of ways due to
 *  the "if it can't be identified it's a descriptor" methodology.
 */
#undef gfp
#undef efp
#undef fp
sweep(fp)
int *fp;
   {
   register int *sp, *gfp, *efp, *ap;
   int *getap();
   int nargs;

   ap = getap(fp);			/* Get pointer to 0(ap) for frame
					    on top of stack. */
   sp = fp - FRAMELIMIT;		/* Start sp at _file word of current
					    frame, the lowest word of it. */
   nargs = 0;				/* Nargs counter is 0 initially. */
   while ((fp != 0 || nargs)) {		/* Keep going until current fp is
					    0 and no arguments are left. */
      if (sp == fp - FRAMELIMIT) {	/* The sp has reached the lower
					    boundary of a procedure frame,
					    process the frame. */
         efp = (int *) ap[-1];		/* Get saved efp out of frame, it's
					    the first word below 0(ap). */
         gfp = (int *) ap[-2];		/* Saved gfp is second word below
					    0(ap). */
         sp = ap + 2;			/* First argument descriptor is
					    second word above 0(ap), point
					    sp at it. */
         ap = (int *) fp[2];		/* Get saved ap */
         fp = (int *) fp[3];		/*  and fp from frame. */
         nargs = sp[-1];		/* Nargs is in word below first
					    argument descriptor. */
         }
      else if (sp == gfp - 3) {		/* The sp has reached the lower end
					    of a generator frame, process
					    the frame.*/
         fp = (int *) gfp[0];		/* Get old fp, available as saved
					    boundary value in the frame. */
         ap = getap(fp);		/* Find 0(ap) corresponding to just-
					    restored boundary value. */
         sp = fp - FRAMELIMIT;		/* Set up to trigger recognition of
					    procedure frame next time around*/
         }
      else if (sp == efp - 2) {		/* The sp has reached the lower end
					    of a generator frame, process
					    the frame. */
         gfp = (int *) efp[-1];		/* Restore gfp, */
         efp = (int *) efp[0];		/*  and efp from frame. */
         sp += 3;			/* Move up past expression frame
					    marker. */
         }
      else {				/* Assume the sp is pointing at a
					    descriptor. */
         mark(sp);			/* Mark it. */
         sp += 2;			/* Move past descriptor. */
         if (nargs)			/* Decrement argument count if in an*/
            nargs--;			/*  argument list. */
         }
      }
   }
/*
 * getap - return ap of frame addressed by fp
 */
int *getap(fp)
register int *fp;
   {
   register
    int mask = fp[1]>>15 & 0x1ffe;	/* Get register mask out of saved
					    psw word. */
   fp += 5;				/* Point fp at where first saved
					    register (if any) is. */
   while (mask >>= 1)			/* Move fp up for each register  */
      fp += mask & 1;			/*  saved.  When done, fp points */
   return fp;				/*  at word where ap would point */
   }
#endif VAX

#ifdef PORT
sweep()
   {
   syserr("Call to sweep() in garbage collector not implemented yet");
   }
#endif PORT

#ifdef PDP11
/*
 * sweep - sweep the stack, marking all descriptors there.
 */

sweep(b)
int *b;
   {
   register int *sp, *r5, *r4;
   int *r3, nargs;

   r5 = b;			/* start at last Icon/C boundary */
   sp = r5 - 5;
   nargs = 0;
   while ((r5 != 0 || nargs) && sp < (int *)0177776) {
      if (sp == r5 - 5) {  /* at a procedure frame */
         r3 = r5[-2];
	 r4 = r5[-1];
	 r5 = r5[0];
	 sp += 8;
	 nargs = sp[-1];
	 }
      else if (sp == r3 - 3) {	/* at a generator frame */
	 r5 = r3[0];		/*   go to next boundary */
	 sp = r5 - 5;
	 }
      else if (sp == r4 - 2) {	/* at an expression frame */
	 r3 = r4[-1];
	 r4 = r4[0];
	 sp += 3;
	 }
      else {			/* must be a descriptor! */
  	 mark(sp);
	 sp += 2;
	 if (nargs)
	    nargs--;
	 }
      }
   }
#endif PDP11
