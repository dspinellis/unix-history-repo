/* advertise.c - the heart of adios */
#ifndef	lint
static char ident[] = "@(#)$Id: advertise.c,v 1.4 1993/08/25 17:18:31 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#ifdef	BSD42
#include <sys/types.h>
#include <sys/uio.h>
#endif	/* BSD42 */

/* For 4.2BSD systems, use writev() for slightly better performance.  Why?
   Well, there are a couple of reasons.  Primarily, it gives a smoother
   output...  More importantly though, it's a sexy syscall()...
 */

extern int  errno;
#ifndef	BSD44	/* in <stdio.h> */
extern int  sys_nerr;
extern char *sys_errlist[];
#endif

/*  */

/* VARARGS3 */

void advertise (what, tail, fmt, a, b, c, d, e, f)
char   *what,
       *tail,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    int	    eindex = errno;
#ifdef	BSD42
    char    buffer[BUFSIZ],
            err[BUFSIZ];
    struct iovec    iob[20];
    register struct iovec  *iov = iob;
#endif	/* BSD42 */

    (void) fflush (stdout);

#ifndef	BSD42
    if (invo_name && *invo_name)
	fprintf (stderr, "%s: ", invo_name);
    fprintf (stderr, fmt, a, b, c, d, e, f);
    if (what) {
	if (*what)
	    fprintf (stderr, " %s: ", what);
	if (eindex > 0 && eindex < sys_nerr)
	    fprintf (stderr, "%s", sys_errlist[eindex]);
	else
	    fprintf (stderr, "Error %d", eindex);
    }
    if (tail)
	fprintf (stderr, ", %s", tail);
    (void) fputc ('\n', stderr);
#else	/* BSD42 */
    (void) fflush (stderr);

    if (invo_name && *invo_name) {
	iov -> iov_len = strlen (iov -> iov_base = invo_name);
	iov++;
	iov -> iov_len = strlen (iov -> iov_base = ": ");
	iov++;
    }
    
    (void) sprintf (buffer, fmt, a, b, c, d, e, f);
    iov -> iov_len = strlen (iov -> iov_base = buffer);
    iov++;
    if (what) {
	if (*what) {
	    iov -> iov_len = strlen (iov -> iov_base = " ");
	    iov++;
	    iov -> iov_len = strlen (iov -> iov_base = what);
	    iov++;
	    iov -> iov_len = strlen (iov -> iov_base = ": ");
	    iov++;
	}
	if (eindex > 0 && eindex < sys_nerr)
	    iov -> iov_len = strlen (iov -> iov_base = sys_errlist[eindex]);
	else {
	    (void) sprintf (err, "Error %d", eindex);
	    iov -> iov_len = strlen (iov -> iov_base = err);
	}
	iov++;
    }
    if (tail && *tail) {
	iov -> iov_len = strlen (iov -> iov_base = ", ");
	iov++;
	iov -> iov_len = strlen (iov -> iov_base = tail);
	iov++;
    }
    iov -> iov_len = strlen (iov -> iov_base = "\n");
    iov++;
    (void) writev (fileno (stderr), iob, iov - iob);
#endif	/* BSD42 */
}
