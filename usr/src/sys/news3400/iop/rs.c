/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: rs.c,v 4.300 91/06/09 06:43:03 root Rel41 $ SONY
 *
 *	@(#)rs.c	7.4 (Berkeley) %G%
 */

/*	rs.c	6.1	83/07/29	*/

#include "rs.h"
#if NRS > 0
/*
 * RS driver
 *
 */
#if NBK > 0
#include "bk.h"
#endif
#include <sys/param.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/kernel.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/buf.h>
#include <sys/malloc.h>

#ifdef CPU_SINGLE
#include <news3400/hbdev/hbvar.h>
#else
#include "../iop/iopvar.h"
#endif

#include <news3400/iop/rsreg.h>
#include <news3400/sio/sccparam.h>

#define	RS_RXE	RXE
#define	RS_TXE	TXE
#define	RS_ON	(RXE|TXE|RTS|DTR)
#define	RS_OFF	TXE
#define	RS_RTS	RTS
#define	RS_DTR	DTR
#define	RS_CTS	CTS
#define	RS_DCD	DCD
#define	RS_DSR	DSR
#define	RS_RI	RI
#define	RS_BRK	XBREAK

#ifdef AUTO_ENABLE
#define	RS_AUTO_ENABLE	AUTO_ENABLE
#endif

#ifdef CPU_SINGLE
#define	iop_device	hb_device
#define	ii_unit		hi_unit
#define	ii_flags	hi_flags
#define	ii_alive	hi_alive
#endif

/*
 * Definition of the driver for the auto-configuration program.
 */
int rsprobe(), rsattach(), rsrint(), rsxint(), rssint();
struct iop_device *rsinfo[NRS];

#ifdef CPU_SINGLE
struct hb_driver rsdriver = { rsprobe, 0, rsattach, 0, 0, "rs", rsinfo };
#else
struct iop_driver rsdriver = { rsprobe, 0, rsattach, 0, "rs", rsinfo };
#endif

/*
 * Local variables for the driver
 */

struct	tty rs_tty[NRS*4];
char	rssoftCAR[NRS];

int	rs_flags[NRS*4];
int	rs_param[NRS*4];
char	rs_active[NRS*4];
char	rs_stopped[NRS*4];

int	rs_rate[NRS*4];
int	rs_average[NRS*4];
char	rs_timeout[NRS*4];
char	rs_watch;

#ifndef lint
int	nrs = NRS*4;			/* used by iostat */
#endif

extern	int tty00_is_console;
extern void rsstart();
extern void ttrstrt();
extern void rsctrl();

#define	RS_CARR(unit) (rssoftCAR[(unit) >> 2] & (1 << ((unit) & 03)))
#define	RS_FLAG(unit, flag) (rs_flags[unit] & (flag))

#define	RF_FLOWCTL	0x0010		/* use H/W flow control */
#define	RF_EXTCLK	0x0100		/* allow external clock */
#define	RF_NODELAY	0x1000		/* disable interrupt delay */

/*
 * Routine for configuration
 */
/*ARGSUSED*/
rsprobe(ii)
	struct iop_device *ii;
{

	return (rs_probe(ii));
}

/*
 * Routine called to attach a rs.
 */
rsattach(ii)
	register struct iop_device *ii;
{
	int i;

	rssoftCAR[ii->ii_unit] = ii->ii_flags;
	for (i = 0; i < 4; i++)
		rs_flags[ii->ii_unit * 4 + i] =
		    (ii->ii_flags >> i) & (RF_FLOWCTL|RF_EXTCLK|RF_NODELAY);
	if (rs_watch == 0) {
		rs_watchdog();
		rs_watch = 1;
	}
}

rs_watchdog()
{
	register int unit, s;

	for (unit = 0; unit < NRS*4; unit++) {
		if (rs_active[unit] == 0)
			continue;
		s = spltty();
		rs_average[unit] = (rs_average[unit] * 7 + rs_rate[unit]) >> 3;
		rs_rate[unit] = 0;
		(void) splx(s);
	}
	timeout(rs_watchdog, (caddr_t)0, hz / 10);
}

/*
 * Open a RS line. Turn on this rs if this is the first use of it.
 */
/*ARGSUSED*/
rsopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register int unit;
	register struct tty *tp;
	register struct iop_device *ii;
	int s;

	unit = minor(dev);
	if (unit >= NRS*4 || (ii = rsinfo[unit >> 2]) == 0 || ii->ii_alive == 0)
		return (ENXIO);
	if (rs_active[unit] == 0) {
		if (rs_init(unit) < 0)
			return (ENXIO);
		rs_enable(unit);
		rs_active[unit] = 1;
	}
	tp = &rs_tty[unit];
	if (tp->t_state&TS_XCLUDE && curproc->p_ucred->cr_uid != 0)
		return (EBUSY);
	tp->t_addr = (caddr_t)0;
	tp->t_oproc = rsstart;
#ifdef notyet /* KU:XXX */
	tp->t_ctrlproc = rsctrl;
#endif
	/*
	 * If this is first open, initialze tty state to default.
	 */
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		}
		rsparam(tp, &tp->t_termios);
		ttsetwater(tp);
	}
	/*
	 * Wait receiver and status interrupt
	 */
	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	rsmctl(dev, RS_ON, DMSET);
	if (rs_param[unit] & DCD || RS_CARR(unit))
		tp->t_state |= TS_CARR_ON;
	s = spltty();		/* spl5 -> spltty, 90/02/28 sak */
	while ((tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
#ifdef notyet /* KU:XXX */
	if (RS_FLAG(unit, RF_FLOWCTL)) {
		tp->t_state |= TS_HFLWCTL;
		rsmctl(dev, RS_AUTO_ENABLE, DMBIS);
	} else {
		tp->t_state &= ~TS_HFLWCTL;
		rsmctl(dev, RS_AUTO_ENABLE, DMBIC);
	}
#endif
	(void) splx(s);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*
 * Close a RS line.
 */
/*ARGSUSED*/
rsclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	register unit;

	unit = minor(dev);
	tp = &rs_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	(void) rsmctl(unit, RS_BRK, DMBIC);
	if (tp->t_cflag & HUPCL || (tp->t_state & TS_ISOPEN) == 0)
		(void) rsmctl(unit, RS_OFF, DMSET);
	ttyclose(tp);

	if (RS_FLAG(unit, RF_FLOWCTL))
		(void)rsmctl(unit, RS_RTS, DMBIC);
}

rsread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct tty *tp;

	tp = &rs_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

rswrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct tty *tp;

	tp = &rs_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

rsenable(unit)
	int unit;
{

	rs_timeout[unit] = 0;
	rs_enable(unit);
}

/*
 * RS receiver interrupt.
 */
_rsrint(unit, buf, n)
	register int unit;
	register char *buf;
	register int n;
{
	register struct iop_device *ii;
	register struct tty *tp;
	register int (*rint)();

#ifdef notyet /* KU:XXX */
	intrcnt[INTR_RS0 + unit]++;
#endif
	ii = rsinfo[unit >> 2];
	if (ii == 0 || ii->ii_alive == 0)
		return;
	tp = &rs_tty[unit];
	if ((tp->t_state & TS_ISOPEN) == 0) {
		wakeup((caddr_t)&tp->t_rawq);
		goto enable;
	}
	/*
	 * Loop fetching characters from the silo for this
	 * rs until there are no more in the silo.
	 */
	rint = linesw[tp->t_line].l_rint;
	while (--n >= 0) {
#if NBK > 0
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
#endif /* NBK > 0 */
			(*rint)(*buf++, tp);
	}
enable:
	rs_rate[unit]++;
	if (rs_average[unit] >= 10 && RS_FLAG(unit, RF_NODELAY) == 0) {
		if (rs_timeout[unit] == 0) {
			rs_timeout[unit] = 1;
			timeout(rsenable, (caddr_t)unit, hz / 100);
		}
	} else
		rs_enable(unit);
}

/*ARGSUSED*/
rsioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register int unit = minor(dev);
	int error;
 
	tp = &rs_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		(void) rsmctl(dev, RS_BRK, DMBIS);
		break;

	case TIOCCBRK:
		(void) rsmctl(dev, RS_BRK, DMBIC);
		break;

	case TIOCSDTR:
		(void) rsmctl(dev, RS_DTR|RS_RTS, DMBIS);
		break;

	case TIOCCDTR:
		if (curproc->p_ucred->cr_uid &&
		    curproc->p_session->s_ttyp != tp)
			return (EACCES);
		(void) rsmctl(dev, RS_DTR|RS_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) rsmctl(dev, dmtors(*(int *)data), DMSET);
		break;

	case TIOCMBIS:
		(void) rsmctl(dev, dmtors(*(int *)data), DMBIS);
		break;

	case TIOCMBIC:
		(void) rsmctl(dev, dmtors(*(int *)data), DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = rstodm(rsmctl(dev, 0, DMGET));
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dmtors(bits)
	register int bits;
{
	register int b;

	b = 0;
	if (bits & DML_LE)  b |= RS_TXE|RS_RXE;
	if (bits & DML_DTR) b |= RS_DTR;
	if (bits & DML_RTS) b |= RS_RTS;
	if (bits & DML_CTS) b |= RS_CTS;
	if (bits & DML_CAR) b |= RS_DCD;
	if (bits & DML_RNG) b |= RS_RI;
	if (bits & DML_DSR) b |= RS_DSR;
#ifdef AUTO_ENABLE
	if (bits & DML_USR) b |= RS_AUTO_ENABLE;
#endif /* AUTO_ENABLE */
	return(b);
}

rstodm(bits)
	register int bits;
{
	register int b;

	b = 0;
	if (bits & (RS_TXE|RS_RXE)) b |= DML_LE;
	if (bits & RS_DTR) b |= DML_DTR;
	if (bits & RS_RTS) b |= DML_RTS;
	if (bits & RS_CTS) b |= DML_CTS;
	if (bits & RS_DCD) b |= DML_CAR;
	if (bits & RS_RI)  b |= DML_RNG;
	if (bits & RS_DSR) b |= DML_DSR;
#ifdef AUTO_ENABLE
	if (bits & RS_AUTO_ENABLE) b |= DML_USR;
#endif
	return(b);
}
 
/*
 * compat table
 */
struct speedtab rsspeedtab[] = {
	0,	0,
	50,	1,
	75,	2,
	110,	3,
	134,	4,
	150,	5,
	200,	6,
	300,	7,
	600,	8,
	1200,	9,
	1800,	10,
	2400,	11,
	4800,	12,
	9600,	13,
	19200,	14,
	38400,	15,
	-1,	-1
};

/*
 * Set parameters from open or stty into the RS hardware
 * registers.
 */
rsparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register int param;
	register int cflag = t->c_cflag;
	int unit = minor(tp->t_dev);
	int s;
	int ospeed = ttspeedtab(t->c_ospeed, rsspeedtab);

	/* check requested parameters */
	if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed) ||
	    (cflag & CSIZE) == CS5 || (cflag & CSIZE) == CS6)
		return (EINVAL);
	/* and copy to tty */
	tp->t_ispeed = t->c_ispeed;
	tp->t_ospeed = t->c_ospeed;
	tp->t_cflag = cflag;

	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = spltty();
	if (tp->t_ospeed == 0) {
		tp->t_cflag |= HUPCL;
		(void) rsmctl(unit, RS_OFF, DMSET);
		(void) splx(s);
		return (0);
	}

	param = rs_get_param(unit) &
		~(CHAR_SIZE|PARITY|EVEN|STOPBIT|BAUD_RATE|NOCHECK);
	if ((cflag & CREAD) == 0)
		param &= ~RXE;
	if (cflag & CS6)
		param |= C6BIT;
	if (cflag & CS7)
		param |= C7BIT;
	if (cflag & PARENB)
		param |= PARITY;
	if ((cflag & PARODD) == 0)
		param |= EVEN;
	if ((tp->t_iflag & INPCK) == 0)
		param |= NOCHECK;
	if (cflag & CSTOPB)
		param |= STOP2;
	else
		param |= STOP1;

	rs_param[unit] = param | ospeed;

	if (RS_FLAG(unit, RF_EXTCLK))
		rs_param[unit] |= EXTCLK_ENABLE;
	else
		rs_param[unit] &= ~EXTCLK_ENABLE;
	rs_set_param(unit, rs_param[unit]);
	(void) splx(s);

	return (0);
}

/*
 * RS transmitter interrupt.
 * Restart the idle line.
 */
_rsxint(unit, count)
	int unit;
	int count;
{
	register struct tty *tp;
	register int s;

#ifdef notyet /* KU:XXX */
	intrcnt[INTR_RS0 + unit]++;
#endif
	rs_stopped[unit] = 0;
	tp = &rs_tty[unit];
	tp->t_state &= ~TS_BUSY;
	s = spltty();
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	else
		ndflush(&tp->t_outq, count);
	(void) splx(s);
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		rsstart(tp);
}

/*
 * Start (restart) transmission on the given RS line.
 */
void
rsstart(tp)
	register struct tty *tp;
{
	register int unit, nch;
	int s;

	unit = minor(tp->t_dev);

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spltty();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	/*
	 * If ther are still characters in the IOP,
	 * just reenable transmit.
	 */
	if (rs_stopped[unit]) {
		rs_start(unit);
		rs_stopped[unit] = 0;
		goto out;
	}
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & (RAW|LITOUT))
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		/*
		 * If first thing on queue is a delay process it.
		 */
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch) {
		tp->t_state |= TS_BUSY;
		rs_output(unit, nch);
	}
out:
	(void) splx(s);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
/*ARGSUSED*/
rsstop(tp, flag)
	register struct tty *tp;
{
	register int unit, s;

	unit = minor(tp->t_dev);
	s = spltty();
	if (tp->t_state & TS_BUSY) {
		rs_stop(unit, 0);
		rs_stopped[unit] = 1;
		if ((tp->t_state & TS_TTSTOP) == 0) {
			tp->t_state |= TS_FLUSH;
			rs_stop(unit, 1);
		}
	}
	(void) splx(s);
}

/*
 * RS modem control
 */
rsmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register int unit, mbits;
	int s;

#ifdef AUTO_ENABLE
	bits &= (RS_RXE|RS_TXE|RS_RTS|RS_DTR|RS_BRK|RS_AUTO_ENABLE);
#else
	bits &= (RS_RXE|RS_TXE|RS_RTS|RS_DTR|RS_BRK);
#endif

	unit = minor(dev);
	s = spltty();		/* spl5 -> spltty, 90/02/28 sak */

	mbits = rs_get_param(unit);
	switch (how) {

	case DMSET:
		mbits = mbits & ~(RS_RXE|RS_TXE|RS_RTS|RS_DTR|RS_BRK) | bits;
		break;

	case DMBIS:
		mbits |= bits;
		break;

	case DMBIC:
		mbits &= ~bits;
		break;

	case DMGET:
		(void) splx(s);
		return(mbits);
	}
	rs_param[unit] = mbits;
	rs_set_param(unit, rs_param[unit]);

	(void) splx(s);
	return(mbits);
}

/*
 * Reset state of driver if IOP reset was necessary.
 * Reset the parameter and status, and
 * restart transmitters.
 */
rsreset()
{
	register int unit;
	register struct tty *tp;
	register struct iop_device *ii;

	for (unit = 0; unit < NRS * 4; unit++) {
		ii = rsinfo[unit >> 2];
		if (ii == 0 || ii->ii_alive == 0)
			continue;
		printf(" rs%d", unit);
		tp = &rs_tty[unit];
		if (tp->t_state & (TS_ISOPEN|TS_WOPEN)) {
			rs_reset(unit);
			rsparam(tp, &tp->t_termios);
			(void) rsmctl(unit, RS_ON, DMSET);
			tp->t_state &= ~TS_BUSY;
			rsstart(tp);
		}
	}
}

/*
 * RS status interrupt
 */
_rssint(unit, stat)
	int unit;
	int stat;
{
	register struct tty *tp;

#ifdef notyet /* KU:XXX */
	intrcnt[INTR_RS0 + unit]++;
#endif
	tp = &rs_tty[unit];
	if (stat & RS_DCD) {
		rs_param[unit] |= RS_DCD;
		(void)(*linesw[tp->t_line].l_modem)(tp, 1);
	} else if (RS_CARR(unit) == 0 &&
	    (*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
		rs_param[unit] &= ~(RS_DCD | RS_DTR);
		rs_set_param(unit, rs_param[unit]);
	}
	if (stat & OVERRUN_ERROR) {
		printf("rs%d: fifo overflow\n", unit);
		rs_param[unit] &= ~OVERRUN_ERROR;
		rs_set_param(unit, rs_param[unit]);
	}
	if (stat & RBREAK) {
		rs_param[unit] &= ~RBREAK;
		if (tp->t_state & TS_ISOPEN)
			(*linesw[tp->t_line].l_rint)
			    (tp->t_flags & RAW ? '\0' : tp->t_cc[VINTR], tp);
	}
}

/*
 * RS control interrupt
 */
rscint(rs)
	int rs;
{

	printf("rscint: %d\n", rs);
}

/*
 * RS H/W control
 */
void
rsctrl(tp, cmd, arg)
	struct tty *tp;
	int cmd;
	int arg;
{
#ifdef notyet /* KU:XXX */
	int unit = minor(tp->t_dev);

	switch (cmd) {

	case TC_HBLOCK:
		if (RS_FLAG(unit, RF_FLOWCTL))
			rsflowctl(unit, arg);
		break;

	default:
		break;
	}

	return (0);
#endif
}

rsflowctl(unit, block)
	int unit;
	int block;
{
	int s;

	s = spltty();
	if (block)
		rs_param[unit] &= ~RS_RTS;
	else
		rs_param[unit] |= RS_RTS;
	rs_set_param(unit, rs_param[unit]);
	(void) splx(s);
}

/*
 * Machine dependent functions
 *
 *	rs_probe()
 *	rs_init()
 *	rsrint()
 *	rsxint()
 *	rssint()
 *	rs_enable()
 *	rs_output()
 *	rs_start()
 *	rs_stop()
 *	rs_reset()
 *	rs_get_param()
 *	rs_set_param()
 */
#ifdef CPU_SINGLE
#include <news3400/hbdev/hbvar.h>
#include <news3400/hbdev/rsreg.h>
#include <news3400/sio/scc.h>

int	rslastcount[NRS*4];
int	scc_unit[] = { 0, 1, -1, -1, 2, 3, 4, 5, 6, 7, 8, 9 };
int	rs_unit[] = { 0, 1, 4, 5, 6, 7, 8, 9, 10, 11 };

rs_probe(hi)
	struct hb_device *hi;
{
	register int i, cmax;

	for (i = (hi->hi_unit << 2), cmax = 4; cmax > 0; cmax--, i++) {
		if (i == 2 || i == 3)
			continue;
		if (scc_probe(scc_unit[i]))
			continue;
		return (0);
	}
	return (1);
}

rs_init(unit)
	int unit;
{

	if (scc_open(scc_unit[unit])) {
		printf("rs_init: chan %d open failed.\n", scc_unit[unit]);
		return (-1);
	}
	return (0);
}

rs_enable(unit)
	int unit;
{

	scc_enable(scc_unit[unit]);
}

rsrint(scc, buf, cnt)
	int scc;
	char *buf;
	int cnt;
{

	_rsrint(rs_unit[scc], buf, cnt);
}

rsxint(scc)
	int scc;
{
	int unit = rs_unit[scc];

	_rsxint(unit, rslastcount[unit]);
}

rssint(scc, stat)
	int scc;
	int stat;
{

	_rssint(rs_unit[scc], stat);
}

rs_start(unit)
	int unit;
{

	scc_start(scc_unit[unit]);
}

rs_output(unit, n)
	int unit;
	int n;
{

	rslastcount[unit] =
	    scc_write(scc_unit[unit], rs_tty[unit].t_outq.c_cf, n);
}

rs_stop(unit, flush)
	int unit;
	int flush;
{

	if (flush)
		scc_flush(scc_unit[unit]);
}

rs_reset(unit)
	int unit;
{

	scc_reset(scc_unit[unit]);
}

rs_get_param(unit)
	int unit;
{

	return (scc_get_param(scc_unit[unit]));
}

rs_set_param(unit, param)
	int unit;
	int param;
{

	scc_set_param(scc_unit[unit], param);
}
#endif /* CPU_SINGLE */

#ifdef IPC_MRX
#include "../ipc/newsipc.h"
#include "../mrx/h/scc.h"
#include "../mrx/h/cio.h"

int	port_rsrecv[NRS*4];
int	port_rsxmit[NRS*4];
int	port_rsstat[NRS*4];
int	port_rsctrl[NRS*4];
int	port_recv_iop[NRS*4];
int	port_xmit_iop[NRS*4];
int	port_ctrl_iop[NRS*4];
int	port_stat_iop[NRS*4];

/*
 *	minor No: 0 - 12 ----> SCC unit No : 0 - 9
 */
int	scc_unit[] = { 1, 0, -1, -1, 3, 2, 5, 4, 7, 6, 9, 8 };

rs_probe(ii)
	struct iop_device *ii;
{
	register int base = ii->ii_unit << 2;
	register int i, j;
	char buf[16];

#define	PT_CREATE(buf, name, unit, func, arg)	\
	port_create(make_name(buf, name, unit), func, arg)
#define	OB_QUERY(buf, name, unit) \
	object_query(make_name(buf, name, unit))
	for (i = base; i < base+4; i++) {
		if ((j = scc_unit[i]) < 0)
			continue;
		port_recv_iop[i] = OB_QUERY(buf, "scc_inputX", j);
		if (port_recv_iop[i] <= 0)
			return (0);
		port_xmit_iop[i] = OB_QUERY(buf, "scc_outputX", j);
		port_ctrl_iop[i] = OB_QUERY(buf, "scc_ctrlX", j);
		port_stat_iop[i] = OB_QUERY(buf, "scc_statX", j);

		port_rsrecv[i] = PT_CREATE(buf, "@rsrecvX", j, rsrint, i);
		port_rsxmit[i] = PT_CREATE(buf, "@rsxmitX", j, rsxint, i);
		port_rsctrl[i] = PT_CREATE(buf, "@rsctrlX", j, NULL, 0);
		port_rsstat[i] = PT_CREATE(buf, "@rsstatX", j, rssint, i);
	}
	return (1);
}

rs_init(unit)
	int unit;
{
	int len;

	msg_send(port_stat_iop[unit], port_rsstat[unit], NULL, 0, 0);
	return (0);
}

rs_enable(unit)
	int unit;
{
	int len;

	len = MAX_CIO;
	msg_send(port_recv_iop[unit], port_rsrecv[unit], &len, sizeof(len), 0);
}

rsrint(unit)
	register int	unit;
{
	char *addr;
	int from, len;

	msg_recv(port_rsrecv[unit], &from, &addr, &len, 0);
#ifdef mips
	clean_dcache(addr, len + 8);
#endif
	_rsrint(unit, addr, len);
}

rsxint(unit)
	register int unit;
{
	int from, *len;

	msg_recv(port_rsxmit[unit], &from, &len, NULL, 0);
	_rsxint(unit, *len);
}

rssint(unit)
	register int unit;
{
	int from, *reply;

	msg_recv(port_rsstat[unit], &from, &reply, NULL, 0);
	_rssint(unit, *reply);
	msg_send(from, port_rsstat[unit], NULL, 0, 0);
}

rs_start(unit)
	int unit;
{
	int func;

	func = CIO_START;
	msg_send(port_ctrl_iop[unit], 0, &func, sizeof(func), 0);
}

rs_output(unit, n)
	int unit;
	int n;
{

	msg_send(port_xmit_iop[unit], port_rsxmit[unit],
	    rs_tty[unit].t_outq.c_cf, min(n, MAX_CIO), 0);
}

rs_stop(unit, flush)
	int unit;
	int flush;
{
	int func;

	func = flush ? CIO_FLUSH : CIO_STOP;
	msg_send(port_ctrl_iop[unit], 0, &func, sizeof(func), 0);
}

rs_reset(unit)
	int unit;
{
	int func;

	func = CIO_RESET;
	msg_send(port_ctrl_iop[unit], 0, &func, sizeof(func), 0);
}

rs_get_param(unit)
	register int unit;
{
	register int port;
	struct scc_ctrl_req req;
	int param, *reply;

	port = port_rsctrl[unit];
	req.scc_func = CIO_GETPARAMS;

	/* message length 8 means 2 * sizeof(int) : func and status */
	msg_send(port_ctrl_iop[unit], port, &req, 8, 0);
	msg_recv(port, NULL, &reply, NULL, 0);

	param = *reply;
	msg_free(port);

	return (param);
}

rs_set_param(unit, param)
	register int unit;
	int param;
{
	struct scc_ctrl_req req;

	req.scc_func = CIO_SETPARAMS;
	req.scc_arg = param;

	/* message length 8 means 2 * sizeof(int) : func and param */
	msg_send(port_ctrl_iop[unit], 0, &req, 8, 0);
}
#endif /* IPC_MRX */
#endif /* NRS > 0 */
