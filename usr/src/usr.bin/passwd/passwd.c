/*
 * Copyright (c) 1988, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)passwd.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "extern.h"

void	usage __P((void));

#ifdef KERBEROS
int use_kerberos = 1;
#endif

int
main(argc, argv)
	int argc;
	char **argv;
{
	int ch;
	char *uname;

	while ((ch = getopt(argc, argv, "l")) != EOF)
		switch (ch) {
#ifdef KERBEROS
		case 'l':		/* change local password file */
			use_kerberos = 0;
			break;
#endif
		default:
		case '?':
			usage();
		}

	argc -= optind;
	argv += optind;

	if ((uname = getlogin()) == NULL)
		err(1, "getlogin");

	switch(argc) {
	case 0:
		break;
	case 1:
#ifdef	KERBEROS
		if (use_kerberos && strcmp(argv[0], uname))
			errx(1,"%s\n\t%s\n%s\n",
		"to change another user's Kerberos password, do",
		"\"kinit user; passwd; kdestroy\";",
		"to change a user's local passwd, use \"passwd -l user\"");
#endif
		uname = argv[0];
		break;
	default:
		usage();
	}

#ifdef	KERBEROS
	if (use_kerberos)
		exit(krb_passwd());
#endif
	exit(local_passwd(uname));
}

void
usage()
{

#ifdef	KERBEROS
	(void)fprintf(stderr, "usage: passwd [-l] user\n");
#else
	(void)fprintf(stderr, "usage: passwd user\n");
#endif
	exit(1);
}
