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
static char sccsid[] = "@(#)stty.c	5.26 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "stty.h"
#include "extern.h"

static void usage __P((void));

main(argc, argv) 
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int opterr, optind;
	extern struct cchar cchars1[], cchars2[];
	extern struct modes cmodes[], imodes[], lmodes[], omodes[];
	register struct modes *mp;
	register struct cchar *cp;
	struct winsize win;
	struct termios t;
	enum FMT fmt;
	int ch, ctl, ldisc, set, tmp, wset;

	ctl = STDIN_FILENO;
	fmt = NOTSET;
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
			if ((ctl = open(optarg, O_RDONLY | O_NONBLOCK)) < 0)
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

	if (ioctl(ctl, TIOCGETD, &ldisc) < 0)
		err("TIOCGETD: %s", strerror(errno));
	if (tcgetattr(ctl, &t) < 0)
		err("tcgetattr: %s", strerror(errno));
	if (ioctl(ctl, TIOCGWINSZ, &win) < 0)
		warn("TIOCGWINSZ: %s\n", strerror(errno));

	checkredirect();			/* conversion aid */

	switch(fmt) {
	case NOTSET:
		if (*argv)
			break;
		/* FALLTHROUGH */
	case BSD:
	case POSIX:
		print(&t, &win, ldisc, fmt);
		break;
	case GFLAG:
		gprint(&t, &win, ldisc);
		break;
	}
	
#define	CHK(s)	(**argv == s[0] && !strcmp(*argv, s))

	for (set = wset = 0; *argv; ++argv) {
		if (CHK("-nl")) {
			t.c_iflag |= ICRNL;
			t.c_oflag |= ONLCR;
			set = 1;
			continue;
		}
		if (CHK("all")) {
			print(&t, &win, ldisc, BSD);
			continue;
		}
		if (CHK("-cbreak"))
			goto reset;
		if (CHK("cbreak")) {
			t.c_iflag | BRKINT|IXON|IMAXBEL;
			t.c_oflag |= OPOST;
			t.c_lflag |= ISIG|IEXTEN;
			t.c_lflag &= ~ICANON;
			set = 1;
			continue;
		}
		if (CHK("cols")) {
			if (!*++argv)
				err("option requires an argument -- cols");
			goto columns;
		}
		if (CHK("columns")) {
			if (!*++argv)
				err("option requires an argument -- columns");
columns:		win.ws_col = atoi(*argv);
			wset = 1;
			continue;
		}
		if (CHK("cooked"))
			goto reset;
		if (CHK("dec")) {
			t.c_cc[VERASE] = (u_char)0177;
			t.c_cc[VKILL] = CTRL('u');
			t.c_cc[VINTR] = CTRL('c');
			t.c_lflag &= ~ECHOPRT;
			t.c_lflag |= ECHOE|ECHOKE|ECHOCTL;
			t.c_iflag &= ~IXANY;
			set = 1;
			continue;
		}
		if (CHK("everything")) {
			print(&t, &win, ldisc, BSD);
			continue;
		}
		if (CHK("-extproc")) {
			tmp = 0;
			ioctl(ctl, TIOCEXT, &tmp);
			continue;
		}
		if (CHK("extproc")) {
			tmp = 1;
			ioctl(ctl, TIOCEXT, &tmp);
			continue;
		}
		if (CHK("ispeed")) {
			if (!*++argv)
				err("option requires an argument -- ispeed");
			cfsetispeed(&t, atoi(*argv));
			set = 1;
			continue;
		}
		if (CHK("new"))
			goto tty;
		if (CHK("nl")) {
			t.c_iflag &= ~ICRNL;
			t.c_oflag &= ~ONLCR;
			set = 1;
			continue;
		}
		if (CHK("old"))
			goto tty;
		if (CHK("ospeed")) {
			if (!*++argv)
				err("option requires an argument -- ospeed");
			cfsetospeed(&t, atoi(*argv));
			set = 1;
			continue;
		}
		if (CHK("-raw"))
			goto reset;
		if (CHK("raw")) {
			cfmakeraw(&t);
			t.c_cflag &= ~(CSIZE|PARENB);
			t.c_cflag |= CS8;
			set = 1;
			continue;
		}
		if (CHK("rows")) {
			if (!*++argv)
				err("option requires an argument -- rows");
			win.ws_row = atoi(*argv);
			wset = 1;
			continue;
		}
		if (CHK("sane")) {
reset:			t.c_cflag = TTYDEF_CFLAG | (t.c_cflag & CLOCAL);
			t.c_iflag = TTYDEF_IFLAG;
			t.c_iflag |= ICRNL;
			/* preserve user-preference flags in lflag */
#define	LKEEP	(ECHOKE|ECHOE|ECHOK|ECHOPRT|ECHOCTL|ALTWERASE|TOSTOP|NOFLSH)
			t.c_lflag = TTYDEF_LFLAG | (t.c_lflag & LKEEP);
			t.c_oflag = TTYDEF_OFLAG;
			set = 1;
			continue;
		}
		if (CHK("size")) {
			(void)printf("%d %d\n", win.ws_row, win.ws_col);
			continue;
		}
		if (CHK("speed")) {
			(void)printf("%d\n", cfgetospeed(&t));
			continue;
		}
		if (CHK("tty")) {
tty:			tmp = TTYDISC;
			if (ioctl(0, TIOCSETD, &tmp) < 0)
				err("TIOCSETD: %s", strerror(errno));
			continue;
		}
		
		for (mp = cmodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				t.c_cflag &= ~mp->unset;
				t.c_cflag |= mp->set;
				set = 1;
				goto next;
			}
		for (mp = imodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				t.c_iflag &= ~mp->unset;
				t.c_iflag |= mp->set;
				set = 1;
				goto next;
			}
		for (mp = lmodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				t.c_lflag &= ~mp->unset;
				t.c_lflag |= mp->set;
				set = 1;
				goto next;
			}
		for (mp = omodes; mp->name; ++mp)
			if (CHK(mp->name)) {
				t.c_oflag &= ~mp->unset;
				t.c_oflag |= mp->set;
				set = 1;
				goto next;
			}
		for (cp = cchars1; cp->name; ++cp) {
			if (!CHK(cp->name))
				continue;
			goto ccfound;
		}
		for (cp = cchars2; cp->name; ++cp) {
			if (!CHK(cp->name))
				continue;
ccfound:		if (!*++argv)
				err("option requires an argument -- %s",
				    cp->name);
			if (CHK("undef") || CHK("<undef>"))
				t.c_cc[cp->sub] = _POSIX_VDISABLE;
			else if (**argv == '^')
				t.c_cc[cp->sub] = 
				    ((*argv)[1] == '?') ? 0177 :
				    ((*argv)[1] == '-') ? _POSIX_VDISABLE :
				    (*argv)[1] & 037;
			else
				t.c_cc[cp->sub] = **argv;
			set = 1;
			goto next;
		}

		if (isdigit(**argv)) {
			cfsetospeed(&t, atoi(*argv));
			cfsetispeed(&t, atoi(*argv));
			goto next;
		}
		if (!strncmp(*argv, "gfmt1", sizeof("gfmt1") - 1)) {
			gread(&t, *argv + sizeof("gfmt1") - 1);
			goto next;
		}

		err("illegal option -- %s", *argv);
next:		continue;
	}

	if (set && tcsetattr(ctl, 0, &t) < 0)
		err("tcsetattr: %s", strerror(errno));
	if (wset && ioctl(ctl, TIOCSWINSZ, &win) < 0)
		warn("TIOCSWINSZ: %s", strerror(errno));
	exit(0);
}

static void
usage()
{
	(void)fprintf(stderr, "usage: stty: [-eg] [-f file] [options]\n");
	exit(1);
}
