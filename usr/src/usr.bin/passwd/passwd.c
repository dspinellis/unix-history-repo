/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)passwd.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <unistd.h>

#ifdef KERBEROS
int use_kerberos = 1;
#endif

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int ch;
	char *uname;

#ifdef KERBEROS
	while ((ch = getopt(argc, argv, "l")) != EOF)
		switch (ch) {
		case 'l':		/* change local password file */
			use_kerberos = 0;
			break;
#else
	while ((ch = getopt(argc, argv, "")) != EOF)
		switch (ch) {
#endif
		default:
		case '?':
			usage();
			exit(1);
		}

	argc -= optind;
	argv += optind;

	uname = getlogin();

	switch(argc) {
	case 0:
		break;
	case 1:
#ifdef	KERBEROS
		if (use_kerberos && strcmp(argv[1], uname)) {
			(void)fprintf(stderr,
			   "passwd: must kinit to change another's password\n");
			exit(1);
		}
#endif
		uname = argv[0];
		break;
	default:
		usage();
		exit(1);
	}

#ifdef	KERBEROS
	if (use_kerberos)
		exit(krb_passwd());
#endif
	exit(local_passwd(uname));
}

usage()
{
#ifdef	KERBEROS
	(void)fprintf(stderr, "usage: passwd [-l] user\n");
#else
	(void)fprintf(stderr, "usage: passwd user\n");
#endif
}
