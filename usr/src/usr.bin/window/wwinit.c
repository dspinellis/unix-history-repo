#ifndef lint
static	char *sccsid = "@(#)wwinit.c	3.10 83/09/15";
#endif

#include "ww.h"
#include "tt.h"

static char *kp = wwkeys;
extern char _sobuf[];

wwinit()
{
	register i, j;
	int kn;

	setbuf(stdout, _sobuf);
	wwdtablesize = getdtablesize();
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
		goto bad;

	if ((wwterm = getenv("TERM")) == 0) {
		wwerrno = WWE_BADTERM;
		goto bad;
	}
	if (tgetent(wwtermcap, wwterm) != 1) {
		wwerrno = WWE_BADTERM;
		goto bad;
	}
	wwbaud = wwbaudmap[wwoldtty.ww_sgttyb.sg_ospeed];

	if (ttinit() < 0)
		goto bad;
	wwnrow = tt.tt_nrow;
	wwncol = tt.tt_ncol;
	wwavailmodes = tt.tt_availmodes;
	wwwrap = tt.tt_wrap;
	(*tt.tt_init)();

	if (wwavailmodes & WWM_REV)
		wwcursormodes = WWM_REV | wwavailmodes & WWM_BLK;
	else if (wwavailmodes & WWM_UL)
		wwcursormodes = WWM_UL;

	if ((wwsmap = wwalloc(0, 0, wwnrow, wwncol, sizeof (char))) == 0)
		goto bad;
	for (i = 0; i < wwnrow; i++)
		for (j = 0; j < wwncol; j++)
			wwsmap[i][j] = WWX_NOBODY;

	wwos = (union ww_char **)
		wwalloc(0, 0, wwnrow, wwncol, sizeof (union ww_char));
	if (wwos == 0)
		goto bad;
	for (i = 0; i < wwnrow; i++)
		for (j = 0; j < wwncol; j++)
			wwos[i][j].c_w = ' ';
	wwns = (union ww_char **)
		wwalloc(0, 0, wwnrow, wwncol, sizeof (union ww_char));
	if (wwns == 0)
		goto bad;
	for (i = 0; i < wwnrow; i++)
		for (j = 0; j < wwncol; j++)
			wwns[i][j].c_w = ' ';

	wwtouched = malloc((unsigned) wwnrow);
	if (wwtouched == 0) {
		wwerrno = WWE_NOMEM;
		goto bad;
	}
	for (i = 0; i < wwnrow; i++)
		wwtouched[i] = 0;

	wwindex[WWX_NOBODY] = &wwnobody;
	wwnobody.ww_order = NWW;

	addcap("kb");
	addcap("ku");
	addcap("kd");
	addcap("kl");
	addcap("kr");
	addcap("kh");
	if ((kn = tgetnum("kn")) >= 0) {
		char cap[5];
		int i;

		(void) sprintf(kp, "kn#%d:", kn);
		for (; *kp; kp++)
			;
		for (i = 1; i <= kn; i++) {
			(void) sprintf(cap, "k%d", i);
			addcap(cap);
			cap[0] = 'l';
			addcap(cap);
		}
	}
	return 0;
bad:
	/*
	 * Don't bother to free storage.  We're supposed
	 * to exit when wwinit fails anyway.
	 */
	(void) wwsettty(0, &wwoldtty);
	return -1;
}

static
addcap(cap)
register char *cap;
{
	char tbuf[512];
	char *tp = tbuf;
	register char *str, *p;

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
