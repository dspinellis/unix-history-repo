/* fdcompare.c - are two files identical? */
#ifndef	lint
static char Id[] = "@(#)$Id: fdcompare.c,v 1.4 1993/08/25 18:29:28 jromine Exp $";
#endif

#include "../h/mh.h"
#include <stdio.h>
#include <sys/types.h>	/* for off_t */

off_t lseek();


fdcompare (fd1, fd2)
register int fd1,
	     fd2;
{
    register int    i,
		    n1,
                    n2,
                    resp;
    register char  *c1,
                   *c2;
    char    b1[BUFSIZ],
            b2[BUFSIZ];

    resp = 1;
    while ((n1 = read (fd1, b1, sizeof b1)) >= 0
	    && (n2 = read (fd2, b2, sizeof b2)) >= 0
	    && n1 == n2) {
	c1 = b1;
	c2 = b2;
	for (i = n1 < sizeof b1 ? n1 : sizeof b1; i--;)
	    if (*c1++ != *c2++) {
		resp = 0;
		goto leave;
	    }
	if (n1 < sizeof b1)
	    goto leave;
    }
    resp = 0;

leave: ;
    (void) lseek (fd1, (off_t)0, 0);
    (void) lseek (fd2, (off_t)0, 0);
    return resp;
}
