#ifndef lint
static char sccsid[] = "@(#)gethostname.c	8.1 (Berkeley) 4/13/94";
#endif /* not lint */

/* Solaris doesn't include the gethostname call by default. */
#include <sys/systeminfo.h>

#include <netdb.h>

int
gethostname(host, len)
	char *host;
	int len;
{
	return (sysinfo(SI_HOSTNAME, host, len));
}
