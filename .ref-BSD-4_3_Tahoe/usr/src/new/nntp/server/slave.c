#ifndef lint
static char	*sccsid = "@(#)slave.c	1.3	(Berkeley) 6/26/87";
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
