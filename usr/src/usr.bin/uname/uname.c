/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)uname.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/sysctl.h>

#include <err.h>
#include <stdio.h>
#include <stdlib.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
#define	MFLAG	0x01
#define	NFLAG	0x02
#define	RFLAG	0x04
#define	SFLAG	0x08
#define	VFLAG	0x10
	u_int flags;
	int ch, name[2];
	size_t len, tlen;
	char *p, *prefix, buf[1024];

	flags = 0;
	while ((ch = getopt(argc, argv, "amnrsv")) != EOF)
		switch(ch) {
		case 'a':
			flags |= (MFLAG | NFLAG | RFLAG | SFLAG | VFLAG);
			break;
		case 'm':
			flags |= MFLAG;
			break;
		case 'n':
			flags |= NFLAG;
			break;
		case 'r':
			flags |= RFLAG;
			break;
		case 's':
			flags |= SFLAG;
			break;
		case 'v':
			flags |= VFLAG;
			break;
		case '?':
		default:
			usage();
		}

	argc -= optind;
	argv += optind;

	if (argc)
		usage();

	if (!flags)
		flags |= SFLAG;

	prefix = "";

	if (flags & SFLAG) {
		name[0] = CTL_KERN;
		name[1] = KERN_OSTYPE;
		len = sizeof(buf);
		if (sysctl(name, 2, &buf, &len, NULL, 0) == -1)
			err(1, "sysctl");
		(void)printf("%s%.*s", prefix, len, buf);
		prefix = " ";
	}
	if (flags & NFLAG) {
		name[0] = CTL_KERN;
		name[1] = KERN_HOSTNAME;
		len = sizeof(buf);
		if (sysctl(name, 2, &buf, &len, NULL, 0) == -1)
			err(1, "sysctl");
		(void)printf("%s%.*s", prefix, len, buf);
		prefix = " ";
	}
	if (flags & RFLAG) {
		name[0] = CTL_KERN;
		name[1] = KERN_OSRELEASE;
		len = sizeof(buf);
		if (sysctl(name, 2, &buf, &len, NULL, 0) == -1)
			err(1, "sysctl");
		(void)printf("%s%.*s", prefix, len, buf);
		prefix = " ";
	}
	if (flags & VFLAG) {
		name[0] = CTL_KERN;
		name[1] = KERN_VERSION;
		len = sizeof(buf);
		if (sysctl(name, 2, &buf, &len, NULL, 0) == -1)
			err(1, "sysctl");
		for (p = buf, tlen = len; tlen--; ++p)
			if (*p == '\n' || *p == '\t')
				*p = ' ';
		(void)printf("%s%.*s", prefix, len, buf);
		prefix = " ";
	}
	if (flags & MFLAG) {
		name[0] = CTL_HW;
		name[1] = HW_MACHINE;
		len = sizeof(buf);
		if (sysctl(name, 2, &buf, &len, NULL, 0) == -1)
			err(1, "sysctl");
		(void)printf("%s%.*s", prefix, len, buf);
		prefix = " ";
	}
	(void)printf("\n");
	exit (0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: uname [-amnrsv]\n");
	exit(1);
}
