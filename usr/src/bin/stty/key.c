/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)key.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "stty.h"
#include "extern.h"

__BEGIN_DECLS
void	f_all __P((struct info *));
void	f_cbreak __P((struct info *));
void	f_columns __P((struct info *));
void	f_dec __P((struct info *));
void	f_everything __P((struct info *));
void	f_extproc __P((struct info *));
void	f_ispeed __P((struct info *));
void	f_nl __P((struct info *));
void	f_ospeed __P((struct info *));
void	f_raw __P((struct info *));
void	f_rows __P((struct info *));
void	f_sane __P((struct info *));
void	f_size __P((struct info *));
void	f_speed __P((struct info *));
void	f_tty __P((struct info *));
__END_DECLS

static struct key {
	char *name;				/* name */
	void (*f) __P((struct info *));		/* function */
#define	F_NEEDARG	0x01			/* needs an argument */
#define	F_OFFOK		0x02			/* can turn off */
	int flags;
} keys[] = {
	{ "all",	f_all,		0 },
	{ "cbreak",	f_cbreak,	F_OFFOK },
	{ "cols",	f_columns,	F_NEEDARG },
	{ "columns",	f_columns,	F_NEEDARG },
	{ "cooked", 	f_sane,		0 },
	{ "dec",	f_dec,		0 },
	{ "everything",	f_everything,	0 },
	{ "extproc",	f_extproc,	F_OFFOK },
	{ "ispeed",	f_ispeed,	F_NEEDARG },
	{ "new",	f_tty,		0 },
	{ "nl",		f_nl,		F_OFFOK },
	{ "old",	f_tty,		0 },
	{ "ospeed",	f_ospeed,	F_NEEDARG },
	{ "raw",	f_raw,		F_OFFOK },
	{ "rows",	f_rows,		F_NEEDARG },
	{ "sane",	f_sane,		0 },
	{ "size",	f_size,		0 },
	{ "speed",	f_speed,	0 },
	{ "tty",	f_tty,		0 },
};

int
ksearch(argvp, ip)
	char ***argvp;
	struct info *ip;
{
	register struct key *kp;
	register char *name;
	struct key tmp;
	static int c_key __P((const void *, const void *));

	name = **argvp;
	if (*name == '-') {
		ip->off = 1;
		++name;
	} else
		ip->off = 0;

	tmp.name = name;
	if (!(kp = (struct key *)bsearch(&tmp, keys,
	    sizeof(keys)/sizeof(struct key), sizeof(struct key), c_key)))
		return (0);
	if (!(kp->flags & F_OFFOK) && ip->off) {
		errx(1, "illegal option -- %s", name);
		usage();
	}
	if (kp->flags & F_NEEDARG && !(ip->arg = *++*argvp)) {
		errx(1, "option requires an argument -- %s", name);
		usage();
	}
	kp->f(ip);
	return (1);
}

static int
c_key(a, b)
        const void *a, *b;
{
        return (strcmp(((struct key *)a)->name, ((struct key *)b)->name));
}

void
f_all(ip)
	struct info *ip;
{
	print(&ip->t, &ip->win, ip->ldisc, BSD);
}

void
f_cbreak(ip)
	struct info *ip;
{
	if (ip->off)
		f_sane(ip);
	else {
		ip->t.c_iflag |= BRKINT|IXON|IMAXBEL;
		ip->t.c_oflag |= OPOST;
		ip->t.c_lflag |= ISIG|IEXTEN;
		ip->t.c_lflag &= ~ICANON;
		ip->set = 1;
	}
}

void
f_columns(ip)
	struct info *ip;
{
	ip->win.ws_col = atoi(ip->arg);
	ip->wset = 1;
}

void
f_dec(ip)
	struct info *ip;
{
	ip->t.c_cc[VERASE] = (u_char)0177;
	ip->t.c_cc[VKILL] = CTRL('u');
	ip->t.c_cc[VINTR] = CTRL('c');
	ip->t.c_lflag &= ~ECHOPRT;
	ip->t.c_lflag |= ECHOE|ECHOKE|ECHOCTL;
	ip->t.c_iflag &= ~IXANY;
	ip->set = 1;
}

void
f_everything(ip)
	struct info *ip;
{
	print(&ip->t, &ip->win, ip->ldisc, BSD);
}

void
f_extproc(ip)
	struct info *ip;
{
	int tmp;

	if (ip->set) {
		tmp = 1;
		(void)ioctl(ip->fd, TIOCEXT, &tmp);
	} else {
		tmp = 0;
		(void)ioctl(ip->fd, TIOCEXT, &tmp);
	}
}

void
f_ispeed(ip)
	struct info *ip;
{
	cfsetispeed(&ip->t, atoi(ip->arg));
	ip->set = 1;
}

void
f_nl(ip)
	struct info *ip;
{
	if (ip->off) {
		ip->t.c_iflag |= ICRNL;
		ip->t.c_oflag |= ONLCR;
	} else {
		ip->t.c_iflag &= ~ICRNL;
		ip->t.c_oflag &= ~ONLCR;
	}
	ip->set = 1;
}

void
f_ospeed(ip)
	struct info *ip;
{
	cfsetospeed(&ip->t, atoi(ip->arg));
	ip->set = 1;
}

void
f_raw(ip)
	struct info *ip;
{
	if (ip->off)
		f_sane(ip);
	else {
		cfmakeraw(&ip->t);
		ip->t.c_cflag &= ~(CSIZE|PARENB);
		ip->t.c_cflag |= CS8;
		ip->set = 1;
	}
}

void
f_rows(ip)
	struct info *ip;
{
	ip->win.ws_row = atoi(ip->arg);
	ip->wset = 1;
}

void
f_sane(ip)
	struct info *ip;
{
	ip->t.c_cflag = TTYDEF_CFLAG | (ip->t.c_cflag & CLOCAL);
	ip->t.c_iflag = TTYDEF_IFLAG;
	ip->t.c_iflag |= ICRNL;
	/* preserve user-preference flags in lflag */
#define	LKEEP	(ECHOKE|ECHOE|ECHOK|ECHOPRT|ECHOCTL|ALTWERASE|TOSTOP|NOFLSH)
	ip->t.c_lflag = TTYDEF_LFLAG | (ip->t.c_lflag & LKEEP);
	ip->t.c_oflag = TTYDEF_OFLAG;
	ip->set = 1;
}

void
f_size(ip)
	struct info *ip;
{
	(void)printf("%d %d\n", ip->win.ws_row, ip->win.ws_col);
}

void
f_speed(ip)
	struct info *ip;
{
	(void)printf("%d\n", cfgetospeed(&ip->t));
}

void
f_tty(ip)
	struct info *ip;
{
	int tmp;

	tmp = TTYDISC;
	if (ioctl(0, TIOCSETD, &tmp) < 0)
		err(1, "TIOCSETD");
}
