#ifndef lint
static char	*sccsid = "@(#)help.c	1.3	(Berkeley) 3/5/86";
#endif

#include "common.h"

/*
 * HELP
 *
 * Provide a naive user with a brief help message.
 *
 */

help(argc, argv)
int	argc;
char	*argv[];
{
	printf("%d This server accepts the following commands:\r\n", INF_HELP);
	printf("ARTICLE     BODY         GROUP\r\n");
	printf("HEAD        LAST         LIST\r\n");
	printf("NEXT        POST         QUIT\r\n");
	printf("STAT        NEWGROUPS    HELP\r\n");
	printf("IHAVE       NEWNEWS      SLAVE\r\n");
	printf("Bugs to %s\r\n", BUGS_TO);
	printf(".\r\n");
	(void) fflush(stdout);
}
