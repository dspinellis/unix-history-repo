#ifndef lint
static	char *sccsid = "@(#)wwinit.c	3.16 84/03/23";
#endif

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>
#include <fcntl.h>

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
	register char **p, **q;
	char **env, **termcap;
	extern char **environ;

#ifndef O_4_1A
	if (done)
		return 0;
	done++;
	_wwdtablesize = getdtablesize();
	if (wwgettty(0, &wwoldtty) < 0)
		return -1;
	wwwintty = wwoldtty;
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
	wwnewtty.ww_fflags = wwoldtty.ww_fflags | FASYNC;

	if (wwsettty(0, &wwnewtty) < 0)
		return -1;
	if (Winit(2, 1) != 0)
		return -1;
	WSetRealCursor = 1;
	Wscreensize(&wwnrow, &wwncol);

	if (tgetent(_wwtermcap, getenv("TERM")) != 1)
		return -1;
	kp = wwkeys;
	addcap("kb", &kp);
	addcap("ku", &kp);
	addcap("kd", &kp);
	addcap("kl", &kp);
	addcap("kr", &kp);
	addcap("kh", &kp);
	if ((j = tgetnum("kn")) >= 0) {
		char cap[32];
		int i;

		sprintf(kp, "kn#%d:", kn);
		for (; *kp; kp++)
			;
		for (i = 1; i <= j; i++) {
			sprintf(cap, "k%d", i);
			addcap(cap, &kp);
			cap[0] = 'l';
			addcap(cap, &kp);
		}
	}
	for (i = 0, p = environ; *p++; i++)
		;
	if ((env = (char **)malloc((unsigned)(i + 3) * sizeof (char *))) == 0)
		goto bad;
	for (p = environ, q = env; *p; p++, q++) {
		if (strncmp(*p, "TERM=", 5) == 0)
			*q = WWT_TERM;
		else if (strncmp(*p, "TERMCAP=", 8) == 0)
			termcap = q;
		else
			*q = *p;
	}
	*(termcap ? termcap : q++) = wwwintermcap;
	*q = 0;
	environ = env;

	(void) sigrelse(SIGIO);
	return 0;
}

addcap(cap, kp)
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
