#ifndef lint
static char	*sccsid = "@(#)help.c	1.7	(Berkeley) 6/26/87";
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
	printf("\r\nAdditionally, the following extention is supported:\r\n\r\n");
	printf("XHDR        Retrieve a single header line from a range of articles.\r\n");
	printf("\r\n");
	printf("Bugs to Phil Lapsley (Internet: phil@berkeley.edu; UUCP: ...!ucbvax!phil)\r\n");
	printf(".\r\n");
	(void) fflush(stdout);
}
