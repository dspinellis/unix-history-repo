/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwinit.c	3.40 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>
#include <fcntl.h>
#include "char.h"

struct ww_tty wwoldtty;
struct ww_tty wwwintty;
struct ww_tty wwnewtty = {
	{ 0, 0, -1, -1, 0 },
	{ -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1 },
	0, 0, 0
};
int _wwdtablesize;
char _wwtermcap[1024];
char _wwkeys[512];
static char *kp = _wwkeys;
int wwncol, wwnrow;
wwinit()
{
	static char done = 0;
	char *kp;
	int s;

	if (done)
		return 0;
	done++;
	_wwdtablesize = getdtablesize();
	if (wwgettty(0, &wwoldtty) < 0)
		return -1;
	wwwintty = wwoldtty;
#ifdef OLD_TTY
	wwwintty.ww_sgttyb.sg_flags &= ~XTABS;
	wwnewtty.ww_sgttyb = wwoldtty.ww_sgttyb;
	wwnewtty.ww_sgttyb.sg_erase = -1;
	wwnewtty.ww_sgttyb.sg_kill = -1;
	wwnewtty.ww_sgttyb.sg_flags |= CBREAK;
	wwnewtty.ww_sgttyb.sg_flags &= ~(ECHO|CRMOD);
	wwnewtty.ww_tchars.t_intrc = -1;
	wwnewtty.ww_tchars.t_quitc = -1;
	wwnewtty.ww_tchars.t_startc = -1;
	wwnewtty.ww_tchars.t_stopc = -1;
	wwnewtty.ww_tchars.t_eofc = -1;
	wwnewtty.ww_tchars.t_brkc = -1;
	wwnewtty.ww_ltchars.t_suspc = -1;
	wwnewtty.ww_ltchars.t_dsuspc = -1;
	wwnewtty.ww_ltchars.t_rprntc = -1;
	wwnewtty.ww_ltchars.t_flushc = -1;
	wwnewtty.ww_ltchars.t_werasc = -1;
	wwnewtty.ww_ltchars.t_lnextc = -1;
	wwnewtty.ww_lmode = wwoldtty.ww_lmode | LLITOUT;
	wwnewtty.ww_ldisc = wwoldtty.ww_ldisc;
#else
#ifndef OXTABS
#define OXTABS XTABS
#endif
#ifndef _POSIX_VDISABLE
#define _POSIX_VDISABLE -1
#endif
	wwwintty.ww_termios.c_oflag &= ~OXTABS;
	wwnewtty.ww_termios = wwoldtty.ww_termios;
	wwnewtty.ww_termios.c_iflag &=
		~(ISTRIP | INLCR | IGNCR | ICRNL | IXON | IXOFF | IMAXBEL);
	wwnewtty.ww_termios.c_iflag |= INPCK;
	wwnewtty.ww_termios.c_oflag = 0;
	wwnewtty.ww_termios.c_cflag &= ~(CSIZE | PARENB);
	wwnewtty.ww_termios.c_cflag |= CS8;
	wwnewtty.ww_termios.c_lflag = 0;
	for (i = 0; i < NCCS; i++)
		wwnewtty.ww_termios.c_cc[i] = _POSIX_VDISABLE;
#endif
	wwnewtty.ww_fflags = wwoldtty.ww_fflags | FASYNC;
	if (wwsettty(0, &wwnewtty) < 0)
		return -1;
	if (Winit(2, 1) != 0)
		return -1;
	WSetRealCursor = 1;
	Wscreensize(&wwnrow, &wwncol);

	if (tgetent(_wwtermcap, getenv("TERM")) != 1)
		return -1;
	kp = wwwintermcap;
	if (wwavailmodes & WWM_REV)
		wwaddcap1(WWT_REV, &kp);
	if (wwavailmodes & WWM_BLK)
		wwaddcap1(WWT_BLK, &kp);
	if (wwavailmodes & WWM_UL)
		wwaddcap1(WWT_UL, &kp);
	if (wwavailmodes & WWM_GRP)
		wwaddcap1(WWT_GRP, &kp);
	if (wwavailmodes & WWM_DIM)
		wwaddcap1(WWT_DIM, &kp);
	if (wwavailmodes & WWM_USR)
		wwaddcap1(WWT_USR, &kp);
	if (tt.tt_insline && tt.tt_delline || tt.tt_setscroll)
		wwaddcap1(WWT_ALDL, &kp);
	if (tt.tt_inschar)
		wwaddcap1(WWT_IMEI, &kp);
	if (tt.tt_insspace)
		wwaddcap1(WWT_IC, &kp);
	if (tt.tt_delchar)
		wwaddcap1(WWT_DC, &kp);
	wwaddcap("kb", &kp);
	wwaddcap("ku", &kp);
	wwaddcap("kd", &kp);
	wwaddcap("kl", &kp);
	wwaddcap("kr", &kp);
	wwaddcap("kh", &kp);
	if ((j = tgetnum("kn")) >= 0) {
		char cap[32];

		sprintf(kp, "kn#%d:", kn);
		for (; *kp; kp++)
			;
		for (i = 1; i <= j; i++) {
			sprintf(cap, "k%d", i);
			wwaddcap(cap, &kp);
			cap[0] = 'l';
			wwaddcap(cap, &kp);
		}
	}
	/*
	 * It's ok to do this here even if setenv() is destructive
	 * since tt_init() has already made its own copy of it and
	 * wwterm now points to the copy.
	 */
	(void) setenv("TERM", WWT_TERM, 1);

	(void) sigsetmask(s);
	/* catch typeahead before ASYNC was set */
	(void) kill(getpid(), SIGIO);
	xxstart();
	return 0;
}

wwaddcap(cap, kp)
	register char *cap;
	register char **kp;
{
	static char tbuf[512];
	static char *tp = tbuf;
	register char *str, *p;
	char *tgetstr();

	if ((str = tgetstr(cap, &tp)) != 0) {
		while (*(*kp)++ = *cap++)
			;
		(*kp)[-1] = '=';
		while (*str) {
			for (p = unctrl(*str++); *(*kp)++ = *p++;)
				;
			(*kp)--;
		}
		*(*kp)++ = ':';
		**kp = 0;
	}
}

wwaddcap1(cap, kp)
	register char *cap;
	register char **kp;
{
	while (*(*kp)++ = *cap++)
		;
	(*kp)--;
}
