#ifndef lint
static	char *sccsid = "@(#)wwinit.c	2.1 83/07/30";
#endif

#include "ww.h"

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
	int kn;

	if (done)
		return 0;
	done++;
	_wwdtablesize = getdtablesize();
	if (wwgettty(0, &wwoldtty) < 0)
		return -1;
	wwwintty = wwoldtty;
	wwwintty.ww_sgttyb.sg_flags &= ~XTABS;
	wwnewtty.ww_sgttyb.sg_flags = wwoldtty.ww_sgttyb.sg_flags;
	wwnewtty.ww_sgttyb.sg_ispeed = wwoldtty.ww_sgttyb.sg_ispeed;
	wwnewtty.ww_sgttyb.sg_ospeed = wwoldtty.ww_sgttyb.sg_ospeed;
	wwnewtty.ww_lmode = wwoldtty.ww_lmode;
	wwnewtty.ww_pgrp = wwoldtty.ww_pgrp;
	wwnewtty.ww_ldisc = wwoldtty.ww_ldisc;
	wwnewtty.ww_sgttyb.sg_flags |= CBREAK;
	wwnewtty.ww_sgttyb.sg_flags &= ~(ECHO|CRMOD);
	wwnewtty.ww_lmode |= LLITOUT;
	if (wwsettty(0, &wwnewtty) < 0)
		return -1;
	if (Winit(2, 1) != 0)
		return -1;
	WSetRealCursor = 1;
	Wscreensize(&wwnrow, &wwncol);

	if (tgetent(_wwtermcap, getenv("TERM")) != 1)
		return -1;
	addcap("kb");
	addcap("ku");
	addcap("kd");
	addcap("kl");
	addcap("kr");
	addcap("kh");
	if ((kn = tgetnum("kn")) >= 0) {
		char cap[5];
		int i;

		sprintf(kp, "kn#%d:", kn);
		for (; *kp; kp++)
			;
		for (i = 1; i <= kn; i++) {
			sprintf(cap, "k%d", i);
			addcap(cap);
			cap[0] = 'l';
			addcap(cap);
		}
	}
	return 0;
}

addcap(cap)
register char *cap;
{
	static char tbuf[512];
	static char *tp = tbuf;
	register char *str, *p;
	char *tgetstr();

	if ((str = tgetstr(cap, &tp)) != 0) {
		while (*kp++ = *cap++)
			;
		kp[-1] = '=';
		while (*str) {
			for (p = unctrl(*str++); *kp++ = *p++;)
				;
			kp--;
		}
		*kp++ = ':';
		*kp = 0;
	}
}
