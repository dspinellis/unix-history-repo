#ifndef lint
static char *sccsid = "@(#)hostname.c	1.4 (Berkeley) 8/11/83"; 
#endif
/*
 * hostname -- get (or set hostname)
 */
#include <stdio.h>

char hostname[32];
extern int errno;

main(argc,argv)
	char *argv[];
{
	int	myerrno;

	argc--;
	argv++;
	if (argc) {
		if (sethostname(*argv,strlen(*argv)))
			perror("sethostname");
		myerrno = errno;
	} else {
		gethostname(hostname,sizeof(hostname));
		myerrno = errno;
		printf("%s\n",hostname);
	}
	exit(myerrno);
}
