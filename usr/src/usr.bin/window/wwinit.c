#ifndef lint
static	char *sccsid = "@(#)wwinit.c	1.2 83/07/17";
#endif

#include "ww.h"

struct ww_tty wwoldtty;
struct ww_tty wwnewtty = {
	{ 0, 0, -1, -1, 0 },
	{ -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1 },
	0, 0, 0
};

wwinit()
{
	static char done = 0;

	if (done)
		return 0;
	done++;
	if (wwgettty(0, &wwoldtty) < 0)
		return -1;
	wwnewtty.ww_sgttyb.sg_flags = wwoldtty.ww_sgttyb.sg_flags;
	wwnewtty.ww_sgttyb.sg_ispeed = wwoldtty.ww_sgttyb.sg_ispeed;
	wwnewtty.ww_sgttyb.sg_ospeed = wwoldtty.ww_sgttyb.sg_ospeed;
	wwnewtty.ww_lmode = wwoldtty.ww_lmode;
	wwnewtty.ww_pgrp = wwoldtty.ww_pgrp;
	wwnewtty.ww_ldisc = wwoldtty.ww_ldisc;
	wwnewtty.ww_sgttyb.sg_flags |= CBREAK;
	wwnewtty.ww_sgttyb.sg_flags &= ~ECHO;
	wwnewtty.ww_lmode |= LLITOUT;
	wwnewtty.ww_tchars.t_quitc = wwoldtty.ww_tchars.t_quitc;
	if (wwsettty(0, &wwnewtty) < 0)
		return -1;
	if (Winit(&wwnewtty.ww_sgttyb, 1) != 0)
		return -1;
	WSetRealCursor = 1;
	return 0;
}
