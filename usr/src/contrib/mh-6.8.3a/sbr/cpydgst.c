/* cpydgst.c - copy from one fd to another in encapsulating mode */
#ifndef	lint
static char ident[] = "@(#)$Id: cpydgst.c,v 1.4 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>


/* All we want to do is to perform the substitution

	\n(-.*)\n	-->	\n- \1\n

    we could use

	sed -e 's%^-%- -%' < ifile > ofile

    to do this, but the routine below is faster than the pipe, fork, and
    exec.
 */

#define	S1	0
#define	S2	1

#define	output(c)   if (bp >= dp) {flush(); *bp++ = c;} else *bp++ = c
#define	flush()	    if ((j = bp - outbuf) && write (out, outbuf, j) != j) \
			adios (ofile, "error writing"); \
		    else \
			bp = outbuf

/*  */

void cpydgst (in, out, ifile, ofile)
register int    in,
                out;
register char  *ifile,
               *ofile;
{
    register int    i,
                    state;
    register char  *cp,
                   *ep;
    char    buffer[BUFSIZ];
    register int    j;
    register char  *bp,
                   *dp;
    char    outbuf[BUFSIZ];

    dp = (bp = outbuf) + sizeof outbuf;
    for (state = S1; (i = read (in, buffer, sizeof buffer)) > 0;)
	for (ep = (cp = buffer) + i; cp < ep; cp++) {
	    if (*cp == '\0')
		continue;
	    switch (state) {
		case S1: 
		    if (*cp == '-') {
			output ('-');
			output (' ');
		    }
		    state = S2;	/* fall */

		case S2: 
		    output (*cp);
		    if (*cp == '\n')
			state = S1;
		    break;
	    }
	}

    if (i == NOTOK)
	adios (ifile, "error reading");
    flush ();
}
