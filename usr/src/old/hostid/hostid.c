/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)hostid.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <netdb.h>

main(argc, argv)
	int argc;
	char **argv;
{
	register char *id;
	struct hostent *hp;
	u_long addr, inet_addr();
	long hostid, gethostid();
	char *index();

	if (argc < 2) {
		printf("%#lx\n", gethostid());
		exit(0);
	}

	id = argv[1];
	if (hp = gethostbyname(id)) {
		bcopy(hp->h_addr, &addr, sizeof(addr));
		hostid = addr;
	} else if (index(id, '.')) {
		if ((hostid = inet_addr(id)) == -1)
			goto usage;
	} else {
		if (id[0] == '0' && (id[1] == 'x' || id[1] == 'X'))
			id += 2;
		if (sscanf(id, "%lx", &hostid) != 1) {
usage:			fputs("usage: hostid [hexnum or internet address]\n",
			    stderr);
			exit(1);
		}
	}

	if (sethostid(hostid) < 0) {
		perror("sethostid");
		exit(1);
	}

	exit(0);
}
