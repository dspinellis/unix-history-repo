static	char *sccsid = "@(#)reset.c	4.3 (Berkeley) 12/3/80";
/*
 * reset - restore tty to sensible state after crapping out in raw mode.
 */
#include <sgtty.h>

#define	CTRL(x)	('x'&037)

main()
{
	struct sgttyb buf;
	struct tchars tbuf;
	struct ltchars ltbuf;

	gtty(2, &buf);
	ioctl(2, TIOCGETC, &tbuf);
	ioctl(2, TIOCGLTC, &ltbuf);
	buf.sg_flags &= ~(RAW|CBREAK|VTDELAY|ALLDELAY);
	buf.sg_flags |= XTABS|ECHO|CRMOD|ANYP;
	reset(&buf.sg_erase, CTRL(h));
	reset(&buf.sg_kill, '@');
	reset(&tbuf.t_intrc, 0177);
	reset(&tbuf.t_quitc, CTRL(\\\\));
	reset(&tbuf.t_startc, CTRL(q));
	reset(&tbuf.t_stopc, CTRL(s));
	reset(&tbuf.t_eofc, CTRL(d));
	reset(&ltbuf.t_suspc, CTRL(z));
	reset(&ltbuf.t_dsuspc, CTRL(y));
	reset(&ltbuf.t_rprntc, CTRL(r));
	reset(&ltbuf.t_flushc, CTRL(o));
	reset(&ltbuf.t_lnextc, CTRL(v));
	reset(&ltbuf.t_werasc, CTRL(w));
	/* brkc is left alone */
	ioctl(2, TIOCSETN, &buf);
	ioctl(2, TIOCSETC, &tbuf);
	ioctl(2, TIOCSLTC, &ltbuf);
	execlp("tset", "tset", "-Q", "-I", 0);	/* fix term dependent stuff */
	exit(1);
}

reset(cp, def)
	char *cp;
	int def;
{

	if (*cp == 0 || (*cp&0377)==0377)
		*cp = def;
}
