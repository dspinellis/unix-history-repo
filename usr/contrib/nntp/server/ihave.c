#ifndef lint
static char	*sccsid = "@(#)ihave.c	1.3	(Berkeley) 3/2/86";
#endif

#include "common.h"

/*
 * IHAVE <messageid>
 *
 * Accept an article for transferral if we haven't seen it before.
 */

ihave(argc, argv)
int	argc;
char	*argv[];
{
	int		retcode;
	register FILE	*fp;

	if (argc != 2) {
		printf("%d IHAVE requires two arguments.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	fp = openartbyid(argv[1]);
	if (fp != NULL) {
		(void) fclose(fp);
		printf("%d Already seen that one, thanks.\r\n", ERR_GOTIT);
		(void) fflush(stdout);
#ifdef LOG
		syslog(LOG_INFO, "%s ihave %s rejected", hostname, argv[1]);
#endif
		return;
	}
		
	retcode = spawn(RNEWS, "rnews", (char *) 0, CONT_XFER, ERR_XFERFAIL);
	if (retcode <= 0)
		printf("%d Transfer failed.\r\n", ERR_XFERFAIL);
	else if (retcode > 0)
		printf("%d Article transferred successfully.  Thank you.\r\n",
			OK_XFERED);
	(void) fflush(stdout);

#ifdef LOG
	syslog(LOG_INFO, "%s ihave %s accepted %s",
		hostname, argv[1], retcode == 1 ? "succeeded" : "failed");
#endif

}
