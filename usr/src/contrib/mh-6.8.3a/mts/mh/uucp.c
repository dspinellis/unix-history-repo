/* uucp.c - various parameters for rmail */

/* LINTLIBRARY */

#include "../h/strings.h"
#include <stdio.h>


#define	NOTOK	(-1)
#define	OK	0

#define	SYSFILE	"/usr/lib/uucp/L.sys"

/*  */

int    uucpsite (system)
register char  *system;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    register FILE  *fp;

    if ((fp = fopen (SYSFILE, "r")) == NULL)
	return NOTOK;

    while (fgets (buffer, sizeof buffer, fp)) {
	if (strncmp (buffer, "xxx", 3) == 0
		|| (bp = index (buffer, ' ')) == NULL)
	    continue;
	*bp = NULL;
	if (strcmp (buffer, system) == 0) {
	    (void) fclose (fp);
	    return OK;
	}
    }

    (void) fclose (fp);
    return NOTOK;
}
