static char *sccsid = "@(#)hostname.c	1.2 1.2 83/01/02"; 
/*
 * hostname -- get (or set hostname)
 */
#include <stdio.h>

char hostname[32];
extern int errno;

main(argc,argv)
	char *argv[];
{
	argc--;
	argv++;
	if (argc) {
		if (sethostname(*argv,strlen(*argv)))
			perror("sethostname");
	} else {
		gethostname(hostname,sizeof(hostname));
		printf("%s\n",hostname);
	}
	exit(errno);
}
