#ifndef lint
static char	*sccsid = "@(#)ihave.c	1.10	(Berkeley) 10/15/87";
#endif

#include "common.h"

#ifdef LOG
int	ih_accepted;
int	ih_rejected;
int	ih_failed;
#endif

/*
 * IHAVE <messageid>
 *
 * Accept an article for transferral if we haven't seen it before.
 */

ihave(argc, argv)
	int		argc;
	char		*argv[];
{
	char		errbuf[2 * MAX_STRLEN];
	int		retcode;
	register char	*cp;

	if (argc != 2) {
		printf("%d Usage: IHAVE <message-id>.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	cp = gethistent(argv[1]);
	if (cp != NULL) {
		printf("%d Got it.\r\n", ERR_GOTIT);
		(void) fflush(stdout);
#ifdef LOG
		ih_rejected++;
#ifdef IHAVE_DEBUG
		syslog(LOG_DEBUG, "%s ihave %s rejected", hostname, argv[1]);
#endif
#endif
		return;
	}
		
	retcode = spawn(rnews, "rnews", (char *) 0, CONT_XFER, ERR_XFERFAIL, errbuf);
	if (retcode <= 0)
		printf("%d %s\r\n", ERR_XFERFAIL, errbuf);
	else if (retcode > 0)
		printf("%d Thanks.\r\n",
			OK_XFERED);
	(void) fflush(stdout);

#ifdef LOG
	if (retcode == 1)
		ih_accepted++;
	else
		ih_failed++;
		
#ifdef IHAVE_DEBUG
	syslog(LOG_DEBUG, "%s ihave %s accepted %s",
		hostname, argv[1], retcode == 1 ? "succeeded" : "failed");
#endif
#endif

}
