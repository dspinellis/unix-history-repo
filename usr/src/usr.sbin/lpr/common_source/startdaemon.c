/*	startdaemon.c	4.1	83/04/29	*/
/*
 * Tell the printer daemon that there are new files in the spool directory.
 */

#include "lp.h"

startdaemon()
{
	register int rem, i, err = 0;
	char buf[BUFSIZ];

	rem = getport();
	if (rem < 0)
		return(0);
	(void) sprintf(buf, "\1%s\n", printer);
	i = strlen(buf);
	if (write(rem, buf, i) != i) {
		(void) close(rem);
		return(0);
	}
	while ((i = read(rem, buf, sizeof(buf))) > 0) {
		(void) fwrite(buf, 1, i, stdout);
		err++;
	}
	(void) close(rem);
	return(i == 0 && err == 0);
}
