#ifndef lint
static char sccsid[] = "@(#)gethostname.c	8.3 (Berkeley) 4/26/94";
#endif /* not lint */

/* Solaris doesn't include the gethostname call by default. */
#include <sys/utsname.h>
#include <sys/systeminfo.h>

#include <netdb.h>

int
gethostname(host, len)
	char *host;
	int len;
{
	return (sysinfo(SI_HOSTNAME, host, len) == -1 ? -1 : 0);
}
