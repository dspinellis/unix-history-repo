#ifndef lint
static char	*sccsid = "@(#)slave.c	1.2	(Berkeley) 3/5/86";
#endif

#include "common.h"

/*
 * SLAVE
 *
 * note slave status, which is actually unimplemented.
 */

slave(argc, argv)
int	argc;
char	*argv[];
{
	printf("%d Kinky, kinky.  I don't support such perversions.\r\n",
		OK_SLAVE);		/* Thanks Serge! */
	(void) fflush(stdout);
}
