/*	cons.c	1.4	86/11/25	*/

/*
 * Tahoe console processor driver
 *
 * Minor device 0 is the CP itself.
 *	  No real read/writes can be done to him.
 * Minor 1 is the console terminal.
 * Minor 2 is the remote line trminal.
 */
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "ioctl.h"
#include "user.h"
#include "proc.h"
#include "tty.h"
#include "uio.h"
#include "callout.h"
#include "systm.h"

#include "../tahoe/cp.h"
#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"

int	cnrestart();
int	timeout();

struct	tty cons;
struct	tty CPtty;
struct	tty RLtty;
struct	tty *constty[3] = { &CPtty, &cons, &RLtty };
struct	consoftc {
	char	cs_lastc;	/* last char sent */
	int	cs_flags;
#define	CSF_ACTIVE	0x1	/* timeout active */
#define	CSF_RETRY	0x2	/* try again at a later time */
#define	CSF_POLLING	0x4	/* polling for input */
} consoftc[3];
struct	cpdcb_o consout[3] = { 
	/* 	unit,		cmd,count, buf */
	{ (char)(CPTAKE | CPDONE),0,   0 },
	{ (char)(CPTAKE | CPDONE),0,   0 },
	{ (char)(CPTAKE | CPDONE),0,   0 }
};
struct	cpdcb_i consin[3] = {
	/* 	unit,		cmd,count, buf */
	{ (char)(CPTAKE | CPDONE),0,   0 },
	{ (char)(CPTAKE | CPDONE),0,   0 },
	{ (char)(CPTAKE | CPDONE),0,   0 }
};
struct	cphdr *lasthdr;

int	cnstart();
int	ttrstrt();
char	partab[];

/*ARGSUSED*/
cnopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	int unit = minor(dev);

	if (unit > CPREMOT) 
		return (EEXIST);
	tp = constty[unit];
	if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	cnpostread(unit);		/* post request for input */
	tp->t_oproc = cnstart;
	tp->t_dev = dev;
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_state = TS_ISOPEN|TS_CARR_ON;
		tp->t_flags = EVENP|ECHO|XTABS|CRMOD;
	}
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

cnpostread(unit)
	int unit;
{
	register struct cpdcb_i *cin;

	if (lasthdr != (struct cphdr *)0) {
		register int timo;

		timo = 10000;
		uncache(&lasthdr->cp_unit);
		while ((lasthdr->cp_unit&CPTAKE) == 0 && --timo)
			uncache(&lasthdr->cp_unit);
	}
	cin = &consin[unit];
	cin->cp_hdr.cp_unit = unit;
	cin->cp_hdr.cp_comm = CPREAD;
	cin->cp_hdr.cp_count = 1;	/* Get ready for input */
	mtpr(CPMDCB, cin);
	lasthdr = (struct cphdr *)cin;
}

cnclose(dev)
	dev_t dev;
{
	register struct tty *tp = constty[minor(dev)];

	(*linesw[tp->t_line].l_close)(tp);
	ttyclose(tp);
}

/*ARGSUSED*/
cnread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	struct tty *tp = constty[minor(dev)];

	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

/*ARGSUSED*/
cnwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	struct tty *tp = constty[minor(dev)];

	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * Got a console receive interrupt -
 * the console processor wants to give us a character.
 * Catch the character, and see who it goes to.
 */
cnrint(dev)
	dev_t dev;
{
	register int c, timo;
	register struct tty *tp;
	int unit;

	unit = minor(dev);
	if (!intenable || consoftc[unit].cs_flags&CSF_POLLING)
		return;
	/* make sure we dont take it from cache */
	uncache(&consin[unit].cpi_buf[0]);
	c = consin[unit].cpi_buf[0];
	/*
	 * Wait about 5 milli for last CPMDCB to be read by CP,
	 * otherwise give up
	 */
	timo = 10000;
	uncache(&lasthdr->cp_unit);
	while ((lasthdr->cp_unit&CPTAKE) == 0  && --timo)
		uncache(&lasthdr->cp_unit);
	uncache(&lasthdr->cp_unit);
	if (lasthdr->cp_unit&CPTAKE) {
		consin[unit].cp_hdr.cp_unit = unit;
			/* This resets status bits */
		mtpr(CPMDCB, &consin[unit]); /* Ready for new character */
		lasthdr = (struct cphdr *)&consin[unit];
		tp = constty[unit];
#ifdef KDB
		if (unit == CPCONS && kdbrintr(c, tp))
			return;
#endif
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
}

cnioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp = constty[minor(dev)];
	register error;
 
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (error >= 0)
		return error;
	if ((error = ttioctl(tp, cmd, addr, flag)) < 0)
		error = ENOTTY;
	else if (cmd == TIOCSETP || cmd == TIOCSETN)
		cnparams(tp);
	return (error);
}

int	consintr = 1;
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
	tp = constty[unit];
	tp->t_state &= ~TS_BUSY;
	consoftc[unit].cs_lastc = (char)0;
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
	if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
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
	if (tp->t_flags&(RAW|LITOUT))
		cnputchar(c, tp);
	else if (c <= 0177)
		cnputchar((c | (partab[c]&0200))&0xff, tp);
	else {
		timeout(ttrstrt, (caddr_t)tp, (c&0177));
		tp->t_state |= TS_TIMEOUT;
		goto out;
	}
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
	register char c;
	register struct tty *tp;
{
	register timo, unit;
	register struct cpdcb_o *current;

	/* tp == 0 only in system error messages */
	if (tp == 0) {
		current = &consout[CPCONS];
		unit = CPCONS;
		if (lasthdr == 0)	/* not done anythig yet */
			lasthdr = (struct cphdr *)current;
		c |= partab[c&0177]&0200;
	} else {
		current = &consout[minor(tp->t_dev)];
		unit = minor(tp->t_dev);
	}
	timo = 30000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 * make sure we dont test this bit in cache!
	 */
	uncache(&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit&CPDONE) == 0 && --timo)
		uncache(&current->cp_hdr.cp_unit);
	current->cp_hdr.cp_comm = CPWRITE;
	current->cp_hdr.cp_count = 1;
	current->cp_buf[0] = c & 0xff;
	timo = 10000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 */
	uncache(&lasthdr->cp_unit);
	while ((lasthdr->cp_unit&CPTAKE) == 0 && --timo)
		uncache(&lasthdr->cp_unit);
	/* Reset done bit */
	current->cp_hdr.cp_unit = (char)unit;
	lasthdr = (struct cphdr *)current;
#ifdef	CPPERF
	if (intenable != 0)
		scope_in(5);
#endif
	consoftc[unit].cs_lastc = c;
	if ((consoftc[unit].cs_flags&CSF_ACTIVE) == 0 && clk_enable) {
		consoftc[unit].cs_flags |= CSF_ACTIVE;
		timeout(cnrestart, (caddr_t)tp, 10);
	}
	consoftc[unit].cs_flags |= CSF_RETRY;	/* wait some more */
	mtpr(CPMDCB, current);
}

#if defined(KDB) || defined(GENERIC)
cngetc()
{
	register int c, s;

	s = spl8();		/* block cnrint while we poll */
	c = cngetchar((struct tty *)0);
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

	/* tp == 0 only in system error messages */
	if (tp == 0) {
		current = &consin[CPCONS];
		unit = CPCONS;
		if (lasthdr == 0)	/* not done anything yet */
			lasthdr = (struct cphdr *)current;
	} else {
		current = &consin[minor(tp->t_dev)];
		unit = minor(tp->t_dev);
	}
	timo = 10000;
	uncache((char *)&lasthdr->cp_unit);
	while ((lasthdr->cp_unit&CPTAKE) == 0 && --timo)
		uncache(&lasthdr->cp_unit);
	current->cp_hdr.cp_unit = unit;		/* Resets done bit */
	current->cp_hdr.cp_comm = CPREAD;
	current->cp_hdr.cp_count = 1;
	mtpr(CPMDCB, current);
	while ((current->cp_hdr.cp_unit & CPDONE) == 0) 
		uncache(&current->cp_hdr.cp_unit);
	uncache(&current->cpi_buf[0]);
	c = current->cpi_buf[0] & 0x7f;
	lasthdr = (struct cphdr *)current;
	return (c);
}
#endif

/*
 * Restart (if necessary) transfer to CP line.
 * This way, lost 'transmit' interrupts don't break the chain.
 */
cnrestart(tp)
	struct tty *tp;
{
	register struct consoftc *cs;

	cs = &consoftc[tp == 0 ? CPCONS : minor(tp->t_dev)];
	if (cs->cs_flags&CSF_RETRY) {
		cs->cs_flags &= ~CSF_RETRY;
		timeout(cnrestart, (caddr_t)tp, 10);
		return;
	}
	cs->cs_flags &= ~CSF_ACTIVE;
	if (cs->cs_lastc != (char)0)
		cnputchar(cs->cs_lastc, tp);
}

/*
 * Set line parameters
 */
cnparams(tp)
	register struct tty *tp;
{
	register timo;
	register struct cpdcb_o *current;
	register struct cpdcb_i *cin;

	current = &consout[minor(tp->t_dev)];
	timo = 30000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 * make sure we dont test this bit in cache!
	 */
	uncache(&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit&CPDONE) == 0 && --timo)
		uncache(&current->cp_hdr.cp_unit);
	current->cp_hdr.cp_comm = CPSTTY;
	current->cp_hdr.cp_count = 4;
	current->cp_buf[0] = tp->t_ispeed;
	/* the rest are defaults */
	current->cp_buf[1] = 0;	/* no parity */
	current->cp_buf[2] = 0;	/* stop bits */
	current->cp_buf[3] = 8;	/* data bits */
	timo = 10000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 */
	uncache(&lasthdr->cp_unit);
	while ((lasthdr->cp_unit&CPTAKE) == 0 && --timo)
		uncache(&lasthdr->cp_unit);
	/* Reset done bit */
	current->cp_hdr.cp_unit = (char)minor(tp->t_dev); 
	lasthdr = (struct cphdr *)current;
	mtpr(CPMDCB, current);

	timo = 10000;
	uncache(&lasthdr->cp_unit);
	while ((lasthdr->cp_unit&CPTAKE) == 0 && --timo)
		uncache(&lasthdr->cp_unit);
	cin = &consin[minor(tp->t_dev)];
	cin->cp_hdr.cp_unit = minor(tp->t_dev);
	cin->cp_hdr.cp_comm = CPREAD;
	cin->cp_hdr.cp_count = 1;	/* Get ready for input */
	mtpr(CPMDCB, cin);
	lasthdr = (struct cphdr *)cin;
}

#ifdef KDB
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
