/*	cons.c	7.9	91/05/16	*/

/*
 * Tahoe console processor driver
 *
 * Minor device 0 is the CP itself.
 *	  No real read/writes can be done to him.
 * Minor 1 is the console terminal.
 * Minor 2 is the remote line trminal.
 */
#include "sys/param.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/ioctl.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/tty.h"
#include "sys/callout.h"
#include "sys/systm.h"
#include "sys/kernel.h"
#include "sys/syslog.h"

#include "cp.h"
#include "../include/cpu.h"
#include "../include/mtpr.h"

int	cnrestart();
int	timeout();

struct	tty CPtty;
struct	tty cons;
struct	tty RLtty;
struct	tty *cntty[3] = { &CPtty, &cons, &RLtty };

struct	tty *constty = 0;	/* virtual console */

struct	consoftc {
	char	cs_flags;
#define	CSF_ACTIVE	0x1	/* timeout active */
#define	CSF_POLLING	0x2	/* polling for input */
	char	cs_lastc;	/* last char sent */
	int	cs_timo;	/* timeouts since interrupt */
	u_long	cs_wedgecnt;	/* times restarted */
} consoftc[3];

struct speedtab cnspeedtab[] = {
	9600,	13,
	4800,	12,
	2400,	11,
	1800,	10,
	1200,	9,
	600,	8,
	300,	7,
	200,	6,
	150,	5,
	134,	4,
	110,	3,
	75,	2,
	50,	1,
	0,	13,
	-1,	-1,
};

/*
 * We check the console periodically to make sure
 * that it hasn't wedged.  Unfortunately, if an XOFF
 * is typed on the console, that can't be distinguished
 * from more catastrophic failure.
 */
#define	CN_TIMERVAL	(hz)		/* frequency at which to check cons */
#define	CN_TIMO		(2*60)		/* intervals to allow for output char */

struct	cpdcb_o consout[3] = { 
	{ CPTAKE|CPDONE }, { CPTAKE|CPDONE }, { CPTAKE|CPDONE }
};
struct	cpdcb_i consin[3] = {
	{ CPTAKE|CPDONE }, { CPTAKE|CPDONE }, { CPTAKE|CPDONE }
};
struct	cphdr *cnlast;

int	cnstart();
int	ttrstrt();
char	partab[];

/*
 * Wait for CP to accept last CP command sent
 * before setting up next command.
 */
#define	waitforlast(timo) { \
	if (cnlast) { \
		(timo) = 10000; \
		do \
			uncache((char *)&cnlast->cp_unit); \
		while ((cnlast->cp_unit&CPTAKE) == 0 && --(timo)); \
	} \
}

/*ARGSUSED*/
cnopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	int unit = minor(dev);
	int cnparams();

	if (unit > CPREMOT) 
		return (ENXIO);
	tp = cntty[unit];
	if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	cnpostread(unit);		/* post request for input */
	tp->t_oproc = cnstart;
	tp->t_param = cnparams;
	tp->t_dev = dev;
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_iflag = TTYDEF_IFLAG|ICRNL;
		tp->t_oflag = TTYDEF_OFLAG|OPOST|ONLCR;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_cflag = CS8|CREAD;
		tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		tp->t_state = TS_ISOPEN|TS_CARR_ON;
		cnparams(tp, &tp->t_termios);
		ttsetwater(tp);
	}
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

cnpostread(unit)
	int unit;
{
	register struct cpdcb_i *cin;
	register int timo;

	waitforlast(timo);
	cin = &consin[unit];
	cin->cp_hdr.cp_unit = unit;
	cin->cp_hdr.cp_comm = CPREAD;
	cin->cp_hdr.cp_count = 1;	/* Get ready for input */
	mtpr(CPMDCB, vtoph((struct proc *)0, (unsigned)cin));
	cnlast = &cin->cp_hdr;
}

cnclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp = cntty[minor(dev)];

	(*linesw[tp->t_line].l_close)(tp, flag);
	ttyclose(tp);
}

/*ARGSUSED*/
cnread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	struct tty *tp = cntty[minor(dev)];

	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

/*ARGSUSED*/
cnwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = cntty[minor(dev)];

	if (tp == &cons && constty &&
	    (constty->t_state & (TS_CARR_ON | TS_ISOPEN)) ==
	    (TS_CARR_ON | TS_ISOPEN))
		tp = constty;
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*
 * Got a console receive interrupt -
 * the console processor wants to give us a character.
 * Catch the character, and see who it goes to.
 */
cnrint(dev)
	dev_t dev;
{
	register int unit, timo;
	register struct tty *tp;
	int c;

	unit = minor(dev);
	if (!intenable || consoftc[unit].cs_flags&CSF_POLLING)
		return;
	/* make sure we dont take it from cache */
	uncache(&consin[unit].cpi_buf[0]);
	c = consin[unit].cpi_buf[0];
	waitforlast(timo);
	/* This resets status bits */
	consin[unit].cp_hdr.cp_unit = unit;
	/* Ready for new character */
	mtpr(CPMDCB, vtoph((struct proc *)0, (unsigned)&consin[unit]));
	cnlast = &consin[unit].cp_hdr;

	tp = cntty[unit];
	if ((tp->t_cflag&CSIZE) != CS8)
		c &= 0177;
#ifdef KADB
	if (unit == CPCONS && kdbrintr(c, tp))
		return;
#endif
	(*linesw[tp->t_line].l_rint)(c & 0377, tp);
}

cnioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp = cntty[minor(dev)];
	register error;
 
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (error >= 0)
		return error;
	if ((error = ttioctl(tp, cmd, addr, flag)) < 0)
		error = ENOTTY;
	return (error);
}

extern	int consintr;
/*
 * Got a console transmission interrupt -
 * the console processor wants another character.
 */
cnxint(dev)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;

	if (!intenable || !consintr)
		return;
	unit = minor(dev);
#ifdef CPPERF
	scope_in(unit == CPCONS ? 1 : 2);
#endif
	tp = cntty[unit];
	tp->t_state &= ~TS_BUSY;
	consoftc[unit].cs_timo = 0;
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		cnstart(tp);
}

cnstart(tp)
	register struct tty *tp;
{
	register c, s;

#ifdef	CPPERF
	scope_in(minor(tp->t_dev) == CPCONS ? 3 : 4);
#endif
	s = spl8();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	c = getc(&tp->t_outq) & 0xff;
	if (tp->t_cflag&PARENB && ((tp->t_cflag&CSIZE)==CS7)) {
		c &= 0177;
		c |= (tp->t_cflag&PARODD ? ~partab[c] : partab[c]) & 0200;
	}
	cnputchar(c, tp);
	tp->t_state |= TS_BUSY;
out:
	splx(s);
}

cnputc(c)
	char c;
{

	if (c == '\n')
		cnputchar('\r', (struct tty *)0);
	cnputchar(c, (struct tty *)0);
}

/*
 * Print a character on console.
 */
cnputchar(c, tp)
	char c;
	register struct tty *tp;
{
	register timo;
	register struct cpdcb_o *current;
	register struct consoftc *cs;
	int unit;

	/* tp == 0 only in system error messages */
	if (tp == 0) {
		tp = &cons;
		tp->t_dev = CPCONS;		/* may not be open */
		c |= partab[c&0177]&0200;
	}
	unit = minor(tp->t_dev);
	current = &consout[unit];
	timo = 30000;
	/*
	 * Try waiting for the console tty to finish previous command
	 * on this unit, otherwise give up after a reasonable time.
	 */
	do
		uncache(&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit&CPDONE) == 0 && --timo);

	current->cp_hdr.cp_comm = CPWRITE;
	current->cp_hdr.cp_count = 1;
	current->cp_buf[0] = c;
	/*
	 * Try waiting for the console tty
	 * to accept previous command.
	 */
	waitforlast(timo);

	/* Reset done bit */
	current->cp_hdr.cp_unit = (char)unit;
#ifdef	CPPERF
	if (intenable != 0)
		scope_in(5);
#endif
	cs = &consoftc[unit];
	cs->cs_lastc = c;
	cs->cs_timo = CN_TIMO;
	if ((cs->cs_flags&CSF_ACTIVE) == 0 && clk_enable) {
		cs->cs_flags |= CSF_ACTIVE;
		timeout(cnrestart, (caddr_t)tp, CN_TIMERVAL);
	}
	mtpr(CPMDCB, vtoph((struct proc *)0, (unsigned)current));
	cnlast = &current->cp_hdr;
}

#if defined(KADB) || defined(GENERIC)
cngetc()
{
	register int c, s;

	s = spl8();		/* block cnrint while we poll */
	c = cngetchar(&cons);
	if (c == '\r')
		c = '\n';
	splx(s);
	return (c);
}

cngetchar(tp)
	register struct tty *tp;
{
	register timo, unit;
	register struct cpdcb_i *current;
	char c;

	unit = minor(tp->t_dev);
	current = &consin[unit];
	waitforlast(timo);
	current->cp_hdr.cp_unit = unit;		/* Resets done bit */
	current->cp_hdr.cp_comm = CPREAD;
	current->cp_hdr.cp_count = 1;
	mtpr(CPMDCB, vtoph((struct proc *)0, (unsigned)current));
	while ((current->cp_hdr.cp_unit&CPDONE) == 0) 
		uncache(&current->cp_hdr.cp_unit);
	uncache(&current->cpi_buf[0]);
	c = current->cpi_buf[0] & 0x7f;
	cnlast = &current->cp_hdr;
	return (c);
}
#endif

/*
 * Restart (if necessary) transfer to CP line.
 * This way, lost transmit interrupts don't wedge output.
 */
cnrestart(tp)
	struct tty *tp;
{
	register struct consoftc *cs;

	cs = &consoftc[minor(tp->t_dev)];
	cs->cs_flags &= ~CSF_ACTIVE;
	if (cs->cs_timo) {
		if (--cs->cs_timo == 0) {
			cs->cs_wedgecnt++;
			cnreset(tp);
			cnputchar(cs->cs_lastc, tp);
		} else {
			cs->cs_flags |= CSF_ACTIVE;
			timeout(cnrestart, (caddr_t)tp, CN_TIMERVAL);
		}
	}
}

/*
 * Reset console.
 */
cnreset(tp)
	register struct tty *tp;
{
	register timo;
	register struct cpdcb_o *current;
	register unit;
	static int failed;

	unit = minor(tp->t_dev);
	current = &consout[unit];
	current->cp_hdr.cp_comm = CPRESET;
	current->cp_hdr.cp_count = 0;
	current->cp_hdr.cp_unit = unit; 
	mtpr(CPMDCB, vtoph((struct proc *)0, (unsigned)current));
	cnlast = &current->cp_hdr;
	timo = 10000;
	do
		uncache(&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit&CPTAKE) == 0 && --timo);
	if (current->cp_hdr.cp_unit & CPTAKE) {
		cnparams(tp, &tp->t_termios);
		failed = 0;
	} else {
		if (failed++ == 0)
			log(LOG_ERR, "Console wedged, reset failed.\n");
		ttyflush(tp, FWRITE);
	}
}

/*
 * Set line parameters
 */
cnparams(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register timo = 30000;
	int unit = minor(tp->t_dev);
	register struct cpdcb_o *current = &consout[unit];
	register cflag = t->c_cflag;
	int speedcode, csize;

	if (((speedcode == ttspeedtab(t->c_ospeed, cnspeedtab)) < 0) ||
	   (t->c_ispeed && t->c_ispeed != t->c_ospeed) ||
	   ((csize = (cflag&CSIZE)) != CS7 && csize != CS8))
		return (EINVAL);
	/*XXX*/return (0);
	/*
	 * Try waiting for the console tty to finish any output,
	 * otherwise give up after a reasonable time.
	 */
	do
		uncache(&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit&CPDONE) == 0 && --timo);
	current->cp_hdr.cp_comm = CPSTTY;
	current->cp_hdr.cp_count = 4;
	current->cp_buf[0] = speedcode;
#ifdef notyet
	/* parity */
	current->cp_buf[1] = (cflag&PARENB) ? ((cflag&PARODD) ? 2 : 1) : 0;	
	/* stop bits */
	current->cp_buf[2] = (cflag&CSTOPB) ? 2 : 0;
	/* data bits */
	current->cp_buf[3] = (csize==CS8) ? 8 : 7;
#else
	current->cp_buf[1] = 0;	/* no parity */
	current->cp_buf[2] = 0;	/* stop bits */
	current->cp_buf[3] = 8;	/* data bits */
#endif

	/* Reset done bit */
	current->cp_hdr.cp_unit = unit; 

	waitforlast(timo);
	mtpr(CPMDCB, vtoph((struct proc *)0, (unsigned)current));
	cnlast = &current->cp_hdr;
	cnpostread(unit);
	return (0);
}

#ifdef KADB
/*
 * Turn input polling on/off (used by debugger).
 */
cnpoll(onoff)
	int onoff;
{

	if (!onoff) {
		consoftc[CPCONS].cs_flags &= ~CSF_POLLING;
		cnpostread(CPCONS);		/* restart input */
	} else
		consoftc[CPCONS].cs_flags |= CSF_POLLING;
}
#endif
