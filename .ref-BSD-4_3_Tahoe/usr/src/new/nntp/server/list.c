#ifndef lint
static char	*sccsid = "@(#)list.c	1.8	(Berkeley) 7/6/87";
#endif

#include "common.h"

/*
 * LIST
 *
 * List active newsgroups.
 *
 */

list(argc, argv)
	int		argc;
	char		*argv[];
{
	char		line[MAX_STRLEN];
	char		*grparray[2];
	register char	*cp;
	register FILE	*active_fp;

	grparray[0] = line;
	grparray[1] = NULL;

	active_fp = fopen(activefile, "r");

	if (active_fp == NULL) {
		printf("%d No list of newsgroups available.\r\n", ERR_FAULT);
		(void) fflush(stdout);
#ifdef SYSLOG
		syslog(LOG_ERR, "list: fopen %s: %m", activefile);
#endif
		return;
	}

	printf("%d Newsgroups in form \"group high low y/n/m\".\r\n",
		OK_GROUPS);

	while (fgets(line, sizeof(line), active_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if (ngpermcount)
			if (ngmatch(strneql, ALLBUT,
			    ngpermlist, ngpermcount,
			    grparray, 1) == 0)
				continue;
		putline(line);
	}
	(void) fclose(active_fp);

	putline(".");
	(void) fflush(stdout);
}
