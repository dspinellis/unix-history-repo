/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)hostname.c	5.1 (Berkeley) 4/30/85";
#endif not lint

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
