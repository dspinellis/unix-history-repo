#include "../h/rt.h"

/*
 * ctrace - print trace of procedure calls.
 * Extra parameters are a result of double stack frame,
 * since invoke creates an Icon frame, then the C calling
 * sequence creates another frame when invoke calls ctrace.
 */
ctrace(bp, nargs, arg)
struct b_proc *bp;
int nargs;
struct descrip *arg;
   {
   register int n;

   if (k_trace > 0)
      k_trace--;
   showline(file, line);
   showlevel(k_level);
   putstr(stderr, STRLOC(bp->pname), STRLEN(bp->pname));
   putc('(', stderr);
   while (nargs--) {
      outimage(stderr, arg--, 0);
      if (nargs)
         putc(',', stderr);
      }
   putc(')', stderr);
   putc('\n', stderr);
   fflush(stderr);
   }

/*
 * rtrace - print trace of procedure returns.
 */

rtrace(bp, rval)
register struct b_proc *bp;
struct descrip *rval;
   {
   register int n;

   if (k_trace > 0)
      k_trace--;
   showline(file, line);
   showlevel(k_level);
   putstr(stderr, STRLOC(bp->pname), STRLEN(bp->pname));
   fprintf(stderr, " returned ");
   outimage(stderr, rval, 0);
   putc('\n', stderr);
   fflush(stderr);
   }

/*
 * ftrace - print trace of procedure failures.
 */

ftrace(nargs, bp)
int nargs;
register struct b_proc *bp;
   {
   register int n;

   if (k_trace > 0)
      k_trace--;
   showline(file, line);
   showlevel(k_level);
   putstr(stderr, STRLOC(bp->pname), STRLEN(bp->pname));
   fprintf(stderr, " failed");
   putc('\n', stderr);
   fflush(stderr);
   }

/*
 * strace - print trace of procedure suspends.
 */

strace(bp, rval)
register struct b_proc *bp;
struct descrip *rval;
   {
   register int n;

   if (k_trace > 0)
      k_trace--;
   showline(file, line);
   showlevel(k_level);
   putstr(stderr, STRLOC(bp->pname), STRLEN(bp->pname));
   fprintf(stderr, " suspended ");
   outimage(stderr, rval, 0);
   putc('\n', stderr);
   fflush(stderr);
   }

/*
 * atrace - print trace of procedure reactivations.
 */

atrace(bp)
register struct b_proc *bp;
   {
   register int n;

   if (k_trace > 0)
      k_trace--;
   showline(file, line);
   showlevel(k_level);
   putstr(stderr, STRLOC(bp->pname), STRLEN(bp->pname));
   fprintf(stderr, " resumed");
   putc('\n', stderr);
   fflush(stderr);
   }

static showline(f, l)
char *f;
int l;
   {
   if (l > 0)
      fprintf(stderr, "%.10s: %d\t", f, l);
   else
      fprintf(stderr, "\t\t");
   }

static showlevel(n)
register int n;
   {
   while (n-- > 0) {
      putc('|', stderr);
      putc(' ', stderr);
      }
   }
