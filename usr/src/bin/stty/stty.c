/*-
 * Copyright (c) 1989, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)stty.c	5.27 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "stty.h"
#include "extern.h"

static char usage[] = "usage: stty: [-eg] [-f file] [options]";

main(argc, argv) 
	int argc;
	char **argv;
{
	register struct modes *mp;
	register struct cchar *cp;
	struct info i;
	struct key *kp;
	enum FMT fmt;
	int ch;

	fmt = NOTSET;
	i.fd = STDIN_FILENO;

	opterr = 0;
	while (strspn(argv[optind], "-aefg") == strlen(argv[optind]) &&
	    (ch = getopt(argc, argv, "aef:g")) != EOF)
		switch(ch) {
		case 'a':		/* undocumented: POSIX compatibility */
			fmt = POSIX;
			break;
		case 'e':
			fmt = BSD;
			break;
		case 'f':
			if ((i.fd = open(optarg, O_RDONLY | O_NONBLOCK)) < 0)
				err("%s: %s", optarg, strerror(errno));
			break;
		case 'g':
			fmt = GFLAG;
			break;
		case '?':
		default:
			goto args;
		}

args:	argc -= optind;
	argv += optind;

	if (ioctl(i.fd, TIOCGETD, &i.ldisc) < 0)
		err("TIOCGETD: %s", strerror(errno));
	if (tcgetattr(i.fd, &i.t) < 0)
		err("tcgetattr: %s", strerror(errno));
	if (ioctl(i.fd, TIOCGWINSZ, &i.win) < 0)
		warn("TIOCGWINSZ: %s\n", strerror(errno));

	checkredirect();			/* conversion aid */

	switch(fmt) {
	case NOTSET:
		if (*argv)
			break;
		/* FALLTHROUGH */
	case BSD:
	case POSIX:
		print(&i.t, &i.win, i.ldisc, fmt);
		break;
	case GFLAG:
		gprint(&i.t, &i.win, i.ldisc);
		break;
	}
	
	for (i.set = i.wset = 0; *argv; ++argv) {
		i.off = **argv == '-';
		if (kp = ksearch(i.off ? *argv + 1 : *argv)) {
			if (!(kp->flags & F_OFFOK) && i.off)
				err("illegal option -- %s\n%s", *argv, usage);
			if (kp->flags & F_NEEDARG && !(i.arg = *++argv))
				err("option requires an argument -- %s\n%s",
				    kp->name, usage);
			kp->f(&i);
			continue;
		}
		

#define	CHK(s)	(**argv == s[0] && !strcmp(*argv, s))
		for (mp = cmodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				i.t.c_cflag &= ~mp->unset;
				i.t.c_cflag |= mp->set;
				i.set = 1;
				goto next;
			}
		for (mp = imodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				i.t.c_iflag &= ~mp->unset;
				i.t.c_iflag |= mp->set;
				i.set = 1;
				goto next;
			}
		for (mp = lmodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				i.t.c_lflag &= ~mp->unset;
				i.t.c_lflag |= mp->set;
				i.set = 1;
				goto next;
			}
		for (mp = omodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				i.t.c_oflag &= ~mp->unset;
				i.t.c_oflag |= mp->set;
				i.set = 1;
				goto next;
			}

		for (cp = cchars1; cp->name; ++cp)
			if (CHK(cp->name))
				goto ccfound;
		for (cp = cchars2; cp->name; ++cp) {
			if (!CHK(cp->name))
				continue;
ccfound:		if (!*++argv)
				err("option requires an argument -- %s\n%s",
				    cp->name, usage);
			if (CHK("undef") || CHK("<undef>"))
				i.t.c_cc[cp->sub] = _POSIX_VDISABLE;
			else if (**argv == '^')
				i.t.c_cc[cp->sub] = 
				    ((*argv)[1] == '?') ? 0177 :
				    ((*argv)[1] == '-') ? _POSIX_VDISABLE :
				    (*argv)[1] & 037;
			else
				i.t.c_cc[cp->sub] = **argv;
			i.set = 1;
			continue;
		}

		if (isdigit(**argv)) {
			cfsetospeed(&i.t, atoi(*argv));
			cfsetispeed(&i.t, atoi(*argv));
			continue;
		}
		if (!strncmp(*argv, "gfmt1", sizeof("gfmt1") - 1)) {
			gread(&i.t, *argv + sizeof("gfmt1") - 1);
			continue;
		}

		err("illegal option -- %s\n%s", *argv, usage);
next:		continue;
	}

	if (i.set && tcsetattr(i.fd, 0, &i.t) < 0)
		err("tcsetattr: %s", strerror(errno));
	if (i.wset && ioctl(i.fd, TIOCSWINSZ, &i.win) < 0)
		warn("TIOCSWINSZ: %s", strerror(errno));
	exit(0);
}
