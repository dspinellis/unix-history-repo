/*	startdaemon.c	4.4	83/05/27	*/
/*
 * Tell the printer daemon that there are new files in the spool directory.
 */

#include "lp.h"

startdaemon()
{
	register int rem, i, err = 0;
	char buf[BUFSIZ];

	rem = getport(host);
	if (rem < 0) {
		perr();
		return(0);
	}
	(void) sprintf(buf, "\1%s\n", printer);
	i = strlen(buf);
	if (write(rem, buf, i) != i) {
		perr();
		(void) close(rem);
		return(0);
	}
	while ((i = read(rem, buf, sizeof(buf))) > 0) {
		(void) fwrite(buf, 1, i, stdout);
		err++;
	}
	if (i < 0)
		perr();
	(void) close(rem);
	return(i == 0 && err == 0);
}

static
perr()
{
	extern int sys_nerr;
	extern char *sys_errlist[];

	printf("%s: ", name);
	fputs(errno < sys_nerr ? sys_errlist[errno] : "Unknown error" , stdout);
	putchar('\n');
}
