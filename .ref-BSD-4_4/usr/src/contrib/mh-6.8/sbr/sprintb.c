/* sprintb.c - sprintf a %b string */
#ifndef	lint
static char ident[] = "@(#)$Id: sprintb.c,v 1.3 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>


char   *sprintb (buffer, v, bits)
register char  *buffer,
               *bits;
register unsigned   v;
{
    register int    i,
                    j;
    register char   c,
                   *bp;

    (void) sprintf (buffer, bits && *bits == 010 ? "0%o" : "0x%x", v);
    bp = buffer + strlen (buffer);

    if (bits && *++bits) {
	j = 0;
	*bp++ = '<';
	while (i = *bits++)
	    if (v & (1 << (i - 1))) {
		if (j++)
		    *bp++ = ',';
		for (; (c = *bits) > 32; bits++)
		    *bp++ = c;
	    }
	    else
		for (; *bits > 32; bits++)
		    continue;
	*bp++ = '>';
	*bp = 0;
    }

    return buffer;
}
