#ifndef lint
static char	*sccsid = "@(#)list.c	1.3	(Berkeley) 3/5/86";
#endif

#include "common.h"

/*
 * LIST
 *
 * List active newsgroups.
 *
 */

list(argc, argv)
int	argc;
char	*argv[];
{
	char		line[MAX_STRLEN];
	register char	*cp;
	register FILE	*active_fp;

	active_fp = fopen(ACTIVE_FILE, "r");

	if (active_fp == NULL) {
		printf("%d No list of newsgroups available.\r\n", ERR_FAULT);
		(void) fflush(stdout);
		syslog(LOG_ERR, "list: fopen %s: %m", ACTIVE_FILE);
		return;
	}

	printf("%d Newsgroups in form \"group high low y/n\".\r\n", OK_GROUPS);

	while (fgets(line, sizeof(line), active_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		printf("%s\r\n", line);
	}
	(void) fclose(active_fp);

	printf(".\r\n");
	(void) fflush(stdout);
}
