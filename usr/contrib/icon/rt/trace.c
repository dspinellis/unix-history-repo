#include "../h/rt.h"

/*
 * ctrace - procedure *bp is being called with nargs arguments, the first
 *  of which is at arg; produce a trace message.
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
 * rtrace - procedure *bp is returning *rval; produce a trace message.
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
 * ftrace - procedure *bp is failing; produce a trace message.
 */

ftrace(bp)
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
 * strace - procedure *bp is suspending *rval; produce a trace message.
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
 * atrace - procedure *bp is being resumed; produce a trace message.
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

/*
 * showline - print file and line number information.
 */
static showline(f, l)
char *f;
int l;
   {
   if (l > 0)
      fprintf(stderr, "%.10s: %d\t", f, l);
   else
      fprintf(stderr, "\t\t");
   }

/*
 * showlevel - print "| " n times.
 */
static showlevel(n)
register int n;
   {
   while (n-- > 0) {
      putc('|', stderr);
      putc(' ', stderr);
      }
   }
