/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)x25accton.c	5.2 (Berkeley) %G%
 */
#include <sys/param.h>
#include <sys/socket.h>
#include <netccitt/x25.h>
/*
 * Enable or disable X.25 accounting.
 */

main(argc, argv)
	char **argv;
{
	if (x25acct(argc > 1 ? argv[1] : (char *)0) < 0) {
		perror(argv[0]);
		exit(1);
	}
	exit(0);
}
x25acct(name)
char *name;
{
	int s = socket(AF_CCITT, SOCK_STREAM, 0);

	if (s < 0)
		return (s);
	if (name)
		return (setsockopt(s, CCITTPROTO_X25, PK_ACCTFILE,
			name, strlen(name) + 1));
	else
		return (setsockopt(s, CCITTPROTO_X25, PK_ACCTFILE,
			(char *)&name, 0));

}
