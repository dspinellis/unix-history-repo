/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: dcm.c 1.17 89/10/01$
 *
 *	@(#)dcm.c	7.1 (Berkeley) %G%
 */

/*
 *  Console support is not finished.
 */

#include "dcm.h"
#if NDCM > 0
/*
 *  98642/MUX
 */
#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "tty.h"
#include "user.h"
#include "conf.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"
#include "time.h"

#include "device.h"
#include "dcmreg.h"
#include "machine/cpu.h"
#include "machine/isr.h"

int	dcmprobe();
struct	driver dcmdriver = {
	dcmprobe, "dcm",
};

#define NDCMLINE (NDCM*4)

int	dcmstart(), dcmparam(), dcmintr();
int	dcmsoftCAR[NDCM];
int     dcmintschm[NDCM];
int	dcm_active;
int	ndcm = NDCM;
int	dcmconsole = -1;
struct	dcmdevice *dcm_addr[NDCM];
struct	tty dcm_tty[NDCMLINE];
int	dcm_cnt = NDCMLINE;
struct	isr dcmisr[NDCM];
int     dcmintrval = 5; /* rate in secs that interschem is examined */
long    dcmbang[NDCM];
int	dcmchrrd[NDCM];	/* chars read during each sample time */
int     dcmintrocc[NDCM];	/* # of interrupts for each sample time */


struct speedtab dcmspeedtab[] = {
	0,	BR_0,
	50,	BR_50,
	75,	BR_75,
	110,	BR_110,
	134,	BR_134,
	150,	BR_150,
	300,	BR_300,
	600,	BR_600,
	1200,	BR_1200,
	1800,	BR_1800,
	2400,	BR_2400,
	4800,	BR_4800,
	9600,	BR_9600,
	19200,	BR_19200,
	38400,	BR_38400,
	-1,	-1
};

#ifdef DEBUG
int	dcmdebug = 0x00;
#define DDB_SIOERR	0x01
#define DDB_PARAM	0x02
#define DDB_INPUT	0x04
#define DDB_OUTPUT	0x08
#define DDB_INTR	0x10
#define DDB_IOCTL       0x20
#define DDB_INTSCHM     0x40
#define DDB_MOD         0x80
#define DDB_OPENCLOSE	0x100

long unsigned int dcmrsize[33];	/* read sizes, 32 for over 31, 0 for 0 */
#endif

extern	struct tty *constty;

#define UNIT(x)		minor(x)
#define	BOARD(x)	((x) >> 2)
#define PORT(x)		((x) & 3)
#define MKUNIT(b,p)	(((b) << 2) | (p))

dcmprobe(hd)
	register struct hp_device *hd;
{
	register struct dcmdevice *dcm;
	register int i;
	register int timo = 0;
	int s, brd;

	dcm = (struct dcmdevice *)hd->hp_addr;
	if ((dcm->dcm_rsid & 0x1f) != DCMID)
		return (0);
	brd = hd->hp_unit;
	s = spltty();
	dcm->dcm_rsid = DCMRS;
	DELAY(50000);	/* 5000 is not long enough */
	dcm->dcm_rsid = 0; 
	dcm->dcm_ic = IC_IE;
	dcm->dcm_cr = CR_SELFT;
	while ((dcm->dcm_ic & IC_IR) == 0) {
		if (++timo == 20000) {
			printf("dcm%d: timeout on selftest interrupt\n", brd);
			printf("dcm%d:rsid %x, ic %x, cr %x, iir %x\n",
			       brd, dcm->dcm_rsid, dcm->dcm_ic,
			       dcm->dcm_cr, dcm->dcm_iir);
			return(0);
		}
	}
	DELAY(50000)	/* XXX why is this needed ???? */
	while ((dcm->dcm_iir & IIR_SELFT) == 0) {
		if (++timo == 400000) {
			printf("dcm%d: timeout on selftest\n", brd);
			printf("dcm%d:rsid %x, ic %x, cr %x, iir %x\n",
			       brd, dcm->dcm_rsid, dcm->dcm_ic,
			       dcm->dcm_cr, dcm->dcm_iir);
			return(0);
		}
	}
	DELAY(50000)	/* XXX why is this needed ???? */
	if (dcm->dcm_stcon != ST_OK) {
		printf("dcm%d: self test failed: %x\n", brd, dcm->dcm_stcon);
		return(0);
	}
	dcm->dcm_ic = IC_ID;
	splx(s);

	hd->hp_ipl = DCMIPL(dcm->dcm_ic);
	dcmisr[brd].isr_ipl = hd->hp_ipl;
	dcmisr[brd].isr_arg = brd;
	dcmisr[brd].isr_intr = dcmintr;
	dcm_addr[brd] = dcm;
	dcm_active |= 1 << brd;
	dcmsoftCAR[brd] = hd->hp_flags;
	dcmintschm[brd] = 1;	/* start with interrupt/char */
	for (i = 0; i < 256; i++)
		dcm->dcm_bmap[i].data_data = 0x0f;
	dcmintrocc[brd] = 0;
	dcmchrrd[brd] = 0;
	isrlink(&dcmisr[brd]);
	dcm->dcm_mdmmsk = MI_CD;	/* Enable carrier detect interrupts */
	dcm->dcm_ic = IC_IE;	/* turn interrupts on */
	/*
	 * Need to reset baud rate, etc. of next print so reset dcmconsole.
	 * Also make sure console is always "hardwired"
	 */
	if (brd == BOARD(dcmconsole)) {
		dcmsoftCAR[brd] |= (1 << PORT(dcmconsole));
		dcmconsole = -1;
	}
	return (1);
}

dcmopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit, brd;

	unit = UNIT(dev);
	brd = BOARD(unit);
	dcmbang[brd] = time.tv_sec;	/* for interrupt scheme */
	if (unit >= dcm_cnt || (dcm_active & (1 << brd)) == 0)
		return (ENXIO);
	tp = &dcm_tty[unit];
	tp->t_oproc = dcmstart;
	tp->t_param = dcaparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_iflag = TTYDEF_IFLAG;
		tp->t_oflag = TTYDEF_OFLAG;
		tp->t_cflag = TTYDEF_CFLAG;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		dcmparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	if (PORT(unit) == 0)	/* enable port 0 */
		(void) dcmmctl(dev, MO_ON, DMSET);
	if (dcmsoftCAR[brd] & (1 << PORT(unit)))
		tp->t_state |= TS_CARR_ON;
	else if (PORT(unit))		/* Only port 0 has modem control */
		tp->t_state |= TS_CARR_ON;
	else if (dcmmctl(dev, MO_OFF, DMGET) & MI_CD)
		tp->t_state |= TS_CARR_ON;
	(void) spltty();
	while (!(flag&O_NONBLOCK) && !(tp->t_cflag&CLOCAL) &&
	       (tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	(void) spl0();

#ifdef DEBUG
	if (dcmdebug & DDB_OPENCLOSE)
		printf("dcmopen: u %x st %x fl %x\n",
			unit, tp->t_state, tp->t_flags);
#endif
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}
 
/*ARGSUSED*/
dcmclose(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	int unit;
 
	unit = UNIT(dev);
	tp = &dcm_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	if (tp->t_cflag&HUPCL || tp->t_state&TS_WOPEN || 
	    (tp->t_state&TS_ISOPEN) == 0)
		(void) dcmmctl(dev, MO_OFF, DMSET);
#ifdef DEBUG
	if (dcmdebug & DDB_OPENCLOSE)
		printf("dcmclose: u %x st %x fl %x\n",
			unit, tp->t_state, tp->t_flags);
#endif
	ttyclose(tp);
	return(0);
}
 
dcmread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;
 
	tp = &dcm_tty[UNIT(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
dcmwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	int unit = UNIT(dev);
	register struct tty *tp;
 
	tp = &dcm_tty[unit];
	if (unit == dcmconsole && constty &&
	    (constty->t_state&(TS_CARR_ON|TS_ISOPEN))==(TS_CARR_ON|TS_ISOPEN))
		tp = constty;
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}
 
dcmintr(brd)
	register int brd;
{
	register struct dcmdevice *dcm;
	int i, code, pcnd[4], mcnd, delta;

	dcm = dcm_addr[brd];
	SEM_LOCK(dcm);
	if ((dcm->dcm_ic & IC_IR) == 0) {
		SEM_UNLOCK(dcm);
		return(0);
	}
	for (i = 0; i < 4; i++) {
		pcnd[i] = dcm->dcm_icrtab[i].dcm_data;
		dcm->dcm_icrtab[i].dcm_data = 0;
	}
	mcnd = dcm->dcm_mdmin;
	code = dcm->dcm_iir & IIR_MASK;
	dcm->dcm_iir = 0;
	SEM_UNLOCK(dcm);

#ifdef DEBUG
	if (dcmdebug & DDB_INTR)
		printf("dcmintr: iir %x p0 %x p1 %x p2 %x p3 %x m %x\n", 
			code, pcnd[0], pcnd[1], pcnd[2], 
			pcnd[3], mcnd);
#endif
	if (code & IIR_PORT0)
		dcmpint(MKUNIT(brd, 0), pcnd[0], dcm);
	if (code & IIR_PORT1)
		dcmpint(MKUNIT(brd, 1), pcnd[1],  dcm);
	if (code & IIR_PORT2)
		dcmpint(MKUNIT(brd, 2), pcnd[2], dcm);
	if (code & IIR_PORT3)
		dcmpint(MKUNIT(brd, 3), pcnd[3], dcm);
	if (code & IIR_MODM)
		dcmmint(MKUNIT(brd, 0), mcnd, dcm);	/* always port 0 */
	if (code & IIR_TIMEO)
		dcmrint(brd, dcm);

	/*
	 * See if need to change interrupt rate.
	 * 16.7ms is the polling interrupt rate.
	 * Reference: 16.7ms is about 550 buad; 38.4k is 72 chars in 16.7ms
	 */
	if ((delta = time.tv_sec - dcmbang[brd]) >= dcmintrval) {  
		dcmbang[brd] = time.tv_sec;
		/*
		 * 66 threshold of 600 buad, use 70
		 */
		if (dcmintschm[brd] && dcmintrocc[brd] > 70 * delta)
			dcm_setintrschm(dcm, 0, brd);
		else if (!dcmintschm[brd] && dcmintrocc[brd] > dcmchrrd[brd]) {
			dcm_setintrschm(dcm, 1, brd);
			/*
			 * Must check the receive queue after switch
			 * from polling mode to interrupt/char
			 */
			dcmrint(brd, dcm);
		}
		dcmintrocc[brd] = 0;
		dcmchrrd[brd] = 0;
	} else
		dcmintrocc[brd]++;

	return(1);
}

/*
 *  Port interrupt.  Can be two things:
 *	First, it might be a special character (exception interrupt);
 *	Second, it may be a buffer empty (transmit interrupt);
 */
dcmpint(unit, code, dcm)
	int unit, code;
	register struct dcmdevice *dcm;
{
	register struct tty *tp;
	register int port = PORT(unit);

	if (code & IT_SPEC) {
		tp = &dcm_tty[unit];
		if ((tp->t_state & TS_ISOPEN) != 0)
			dcmreadbuf(unit, dcm, tp, port);
		else
			dcm->dcm_rhead[port].ptr = dcm->dcm_rtail[port].ptr & RX_MASK;
	}
	if (code & IT_TX)
		dcmxint(unit, dcm);
}

dcmrint(brd, dcm)
	int brd;
	register struct dcmdevice *dcm;
{
	register struct tty *tp;
	register int i, unit;

	unit = MKUNIT(brd, 0);
	tp = &dcm_tty[unit];
	for (i = 0; i < 4; i++, tp++, unit++) {
		/* TS_WOPEN catch race when switching to polling mode */
		if ((tp->t_state & (TS_ISOPEN|TS_WOPEN)) != 0)
			dcmreadbuf(unit, dcm, tp, i);
		else
			dcm->dcm_rhead[i].ptr = dcm->dcm_rtail[i].ptr & RX_MASK;
	}
}

dcmreadbuf(unit, dcm, tp, port)
	int unit;
	register struct dcmdevice *dcm;
	register struct tty *tp;
	register int port;
{
	register int c, stat;
	register unsigned head;
	unsigned tail;
#ifdef DEBUG
	int silocnt;
	silocnt = 0;
#endif /* DEBUG */

readrestart:
	head = dcm->dcm_rhead[port].ptr & RX_MASK;
	tail = dcm->dcm_rtail[port].ptr & RX_MASK;

	while (head != tail) {
		c = dcm->dcm_rfifos[3 - port][head / 2].data_char;
		stat = dcm->dcm_rfifos[3 - port][head / 2].data_stat;
		dcmchrrd[BOARD(unit)]++;
#ifdef DEBUG
		silocnt++;
#endif
		head = (head + 2) & RX_MASK;
		dcm->dcm_rhead[port].ptr = head;

#ifdef DEBUG
		if (dcmdebug & DDB_INPUT)
			printf("dcmreadbuf: u%d p%d c%x s%x f%x h%x t%x char %c\n",
			       BOARD(unit), PORT(unit), c&0xFF, stat&0xFF,
			       tp->t_flags, head, tail, c);
#endif
		if (stat & RD_MASK) {	/* Check for errors */
#ifdef DEBUG
			if (dcmdebug & DDB_INPUT || dcmdebug & DDB_SIOERR)
				printf("dcm%d port%d: data error: stat 0x%x data 0x%x chr %c\n",
				       BOARD(unit), PORT(unit), stat, c, c);
#endif
			if (stat & (RD_BD | RD_FE))
				c |= TTY_FE;
			else if (stat & RD_PE)
				c |= TTY_PE;
			else if (stat & RD_OVF)
				log(LOG_WARNING,
				    "dcm%d port%d: silo overflow\n",
				    BOARD(unit), PORT(unit));
			else if (stat & RD_OE)
				log(LOG_WARNING,
				    "dcm%d port%d: uart overflow\n",
				    BOARD(unit), PORT(unit));
		}
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
	/* for higher speed need to processes everything that might
	 * have arrived since we started; see if tail changed */
	if (tail != dcm->dcm_rtail[port].ptr & RX_MASK)
		goto readrestart;

#ifdef DEBUG
	if (silocnt < 33)
		dcmrsize[silocnt]++;
	else
		dcmrsize[32]++;
#endif
}

dcmxint(unit, dcm)
	int unit;
	struct dcmdevice *dcm;
{
	register struct tty *tp;

	tp = &dcm_tty[unit];
	tp->t_state &= ~TS_BUSY;
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		dcmstart(tp);
}

dcmmint(unit, mcnd, dcm)
	register int unit;
	register struct dcmdevice *dcm;
        int mcnd;
{
	register struct tty *tp;

#ifdef DEBUG
	if (dcmdebug & DDB_MOD)
		printf("dcmmint: unit %x mcnd %x\n", unit, mcnd);
#endif DEBUG
	tp = &dcm_tty[unit];
	if ((dcmsoftCAR[BOARD(unit)] & (1 << PORT(unit))) == 0) {
		if (mcnd & MI_CD)
			(void) (*linesw[tp->t_line].l_modem)(tp, 1);
		else if ((*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
			dcm->dcm_mdmout &= ~(MO_DTR | MO_RTS);
			SEM_LOCK(dcm);
			dcm->dcm_cr |= CR_MODM;
			SEM_UNLOCK(dcm);
		}
	}
}

dcmioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register int unit = UNIT(dev);
	register struct dcmdevice *dcm;
	register int port;
	int error;
 
#ifdef DEBUG
	if (dcmdebug & DDB_IOCTL)
		printf("dcmioctl: unit %d cmd %x data %x flag %x\n",
		       unit, cmd, *data, flag);
#endif
	tp = &dcm_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	port = PORT(unit);
	dcm = dcm_addr[BOARD(unit)];
	switch (cmd) {
	case TIOCSBRK:
		dcm->dcm_cmdtab[port].dcm_data = CT_BRK;
		SEM_LOCK(dcm);
		dcm->dcm_cr = (1 << port);	/* start break */
		SEM_UNLOCK(dcm);
		break;

	case TIOCCBRK:
		dcm->dcm_cmdtab[port].dcm_data = CT_BRK;
		SEM_LOCK(dcm);
		dcm->dcm_cr = (1 << port);	/* end break */
		SEM_UNLOCK(dcm);
		break;

	case TIOCSDTR:
		(void) dcmmctl(dev, MO_ON, DMBIS);
		break;

	case TIOCCDTR:
		(void) dcmmctl(dev, MO_ON, DMBIC);
		break;

	case TIOCMSET:
		(void) dcmmctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) dcmmctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) dcmmctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dcmmctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dcmparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register struct dcmdevice *dcm;
	register int mode, cflag = t->c_cflag;
	register int port;
	int unit = UNIT(tp->t_dev);
	int ospeed = ttspeedtab(t->c_ospeed, dcmspeedtab);
 
	/* check requested parameters */
        if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed))
                return(EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	dcm = dcm_addr[BOARD(unit)];
	if (ospeed == 0) {
		(void) dcmmctl(unit, MO_OFF, DMSET);	/* hang up line */
		return;
	}
	port = PORT(unit);
	dcm->dcm_data[port].dcm_baud = ospeed;
	switch (cflag&CSIZE) {
	case CS5:
		mode = LC_5BITS; break;
	case CS6:
		mode = LC_6BITS; break;
	case CS7:
		mode = LC_7BITS; break;
	case CS8:
		mode = LC_8BITS; break;
	}
	if (cflag&PARENB) {
		if (cflag&PARODD)
			mode |= LC_PODD;
		else
			mode |= LC_PEVEN;
	}
	if (cflag&CSTOPB)
		mode |= LC_2STOP;
	else
		mode |= LC_1STOP;
#ifdef DEBUG
	if (dcmdebug & DDB_PARAM)
		printf("dcmparam: unit %d cflag %x mode %x speed %x\n",
		       unit, cflag, mode, ospeed);
#endif
	/* wait for transmitter buffer to empty */
	while (dcm->dcm_thead[port].ptr != dcm->dcm_ttail[port].ptr)
		;

	dcm->dcm_data[port].dcm_conf = mode;
	dcm->dcm_cmdtab[port].dcm_data = CT_CON;
	SEM_LOCK(dcm);
	dcm->dcm_cr = (1 << port);
	SEM_UNLOCK(dcm);
}
 
dcmstart(tp)
	register struct tty *tp;
{
	register struct dcmdevice *dcm;
	int s, unit, c;
	register int tail, next, head, port;
	int restart = 0, nch = 0;
 
	unit = UNIT(tp->t_dev);
	port = PORT(unit);
	dcm = dcm_addr[BOARD(unit)];
	s = spltty();
#ifdef DEBUG
	if (dcmdebug & DDB_OUTPUT)
		printf("dcmstart: unit %d state %x flags %x outcc %d\n",
		       unit, tp->t_state, tp->t_flags, tp->t_outq.c_cc);
#endif
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
	tail = dcm->dcm_ttail[port].ptr & TX_MASK;
	next = (dcm->dcm_ttail[port].ptr + 1) & TX_MASK;
	head = dcm->dcm_thead[port].ptr & TX_MASK;
#ifdef DEBUG
      if (dcmdebug & DDB_OUTPUT)
              printf("\thead %x tail %x next %x\n",
                     head, tail, next);
#endif
	if (tail == head && tp->t_outq.c_cc)
		restart++;
	while (tp->t_outq.c_cc && next != head) {
		nch++;
		c = getc(&tp->t_outq);
		dcm->dcm_tfifos[3 - port][tail].data_char = c;
		dcm->dcm_ttail[port].ptr = next;
		tail = next;
		next = (tail + 1) & TX_MASK;
	}
	if (restart && nch) {
		tp->t_state |= TS_BUSY;
		SEM_LOCK(dcm);
#ifdef DEBUG
	if (dcmdebug & DDB_INTR)
		printf("TX on port %d head %x tail %x cc %d\n",
			port, tail, head, tp->t_outq.c_cc);
#endif
		dcm->dcm_cmdtab[port].dcm_data = CT_TX;
		dcm->dcm_cr = (1 << port);
		SEM_UNLOCK(dcm);
	}
out:
	splx(s);
}
 
/*
 * Stop output on a line.
 */
dcmstop(tp, flag)
	register struct tty *tp;
{
	int s;

	s = spltty();
	if (tp->t_state & TS_BUSY) {
		if ((tp->t_state&TS_TTSTOP)==0)
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}
 
/* Modem control */

dcmmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct dcmdevice *dcm;
	int s, hit = 0;

	/* Only port 0 has modem control lines. For right now the following */
	/* is ok, but needs to changed for the 8 port board. */
	if (PORT(UNIT(dev)) != 0)
		return(bits);

	dcm = dcm_addr[BOARD(UNIT(dev))];
	s = spltty();
	switch (how) {

	case DMSET:
		dcm->dcm_mdmout = bits;
		hit++;
		break;

	case DMBIS:
		dcm->dcm_mdmout |= bits;
		hit++;
		break;

	case DMBIC:
		dcm->dcm_mdmout &= ~bits;
		hit++;
		break;

	case DMGET:
		bits = dcm->dcm_mdmin;
		break;
	}
	if (hit) {
		SEM_LOCK(dcm);
		dcm->dcm_cr |= CR_MODM;
		SEM_UNLOCK(dcm);
		(void) splx(s);
	}
	return(bits);
}

dcm_setintrschm(dcm, request, brd)
     register struct dcmdevice *dcm;
     int request, brd;
{
	register int i;

#ifdef DEBUG
	if (dcmdebug & DDB_INTSCHM) {
		printf("dcm%d set intr schm request %d int state %x silo hist \n\t",
		       brd, request, dcmintschm[brd]);
		for (i = 0; i < 33; i++)  {
			printf("  %u", dcmrsize[i]);
			dcmrsize[i] = 0;
		}
		printf("\n");
	}
#endif /* DEBUG */

	/* if request true then we interrupt per char, else use card */
	/* polling interrupt hardware */
#ifdef DEBUG
	if (request == dcmintschm[brd]) { 
		printf("dcm%d setintrschm redundent request %x current %x\n",
		       brd, request, dcmintschm[brd]);
		return;
	}
#endif /* DEBUG */
	if (request) {
		for (i = 0; i < 256; i++)
			dcm->dcm_bmap[i].data_data = 0x0f;
		dcmintschm[brd] = 1;
	}
	else {
		for (i = 0; i < 256; i++) 
			dcm->dcm_bmap[i].data_data = 0x00;
		/*
		 * Don't slow down tandem mode, interrupt on these chars.
		 * XXX bad assumption, everyone uses ^Q, ^S for flow
		 */
		dcm->dcm_bmap[0x11].data_data = 0x0f;
		dcm->dcm_bmap[0x13].data_data = 0x0f;
		dcmintschm[brd] = 0;
	}
	while (dcm->dcm_cr & CR_TIMER) ;
	SEM_LOCK(dcm);
	dcm->dcm_cr |= CR_TIMER;	/* toggle card 16.7ms interrupts */
	SEM_UNLOCK(dcm);
}

#ifdef notdef
/*
 * Following are all routines needed for DCM to act as console
 */

struct tty *
dcmcninit(majordev)
	dev_t majordev;
{
	register struct dcmdevice *dcm;
	int unit, s;
	short stat;

	unit = CONUNIT;				/* XXX */
	dcm_addr[BOARD(CONUNIT)] = CONADDR;	/* XXX */

	dcm = dcm_addr[unit];
	s = splhigh();
	/* do something */
	splx(s);
	dcmconsole = unit;
	if (majordev)
		dcm_tty[unit].t_dev = makedev(majordev, unit);
	return(&dcm_tty[unit]);
}

dcmcngetc(dev)
{
	return(0);
}

/*
 * Console kernel output character routine.
 */
dcmcnputc(dev, c)
	dev_t dev;
	register int c;
{
	register struct dcmdevice *dcm = dcm_addr[BOARD(dev)];
	int port = PORT(dev);
	short stat;
	int head, tail, next;
	int s = splhigh();

	if (dcmconsole == -1)
		(void) dcmcninit(0);

	do {
		tail = dcm->dcm_ttail[port].ptr & TX_MASK;
		head = dcm->dcm_thead[port].ptr & TX_MASK;
	} while (tail != head);
	next = (dcm->dcm_ttail[port].ptr + 1) & TX_MASK;

	dcm->dcm_tfifos[3 - port][tail].data_char = c;
	dcm->dcm_ttail[port].ptr = next;

	dcm->dcm_cmdtab[port].dcm_data = CT_TX;
	SEM_LOCK(dcm);
	dcm->dcm_cr = (1 << port);
	SEM_UNLOCK(dcm);

	splx(s);
}
#endif
#endif
