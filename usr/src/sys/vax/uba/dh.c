/*	dh.c	3.2	%H%	*/

/*
 *	DH-11 driver
 *	This driver calls on the DHDM driver.
 *	If the DH has no DM11-BB, then the latter will
 *	be fake. To insure loading of the correct DM code,
 *	lib2 should have dhdm.o, dh.o and dhfdm.o in that order.
 */

#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"

#define	q3	tp->t_outq
#define	DHADDR	((struct device *)(UBA0_DEV + 0160020))
#define	NDH11	16	/* number of lines */
#define UBACVT(x) (cbase + (short)((x)-(char *)cfree))

struct cblock {
	struct cblock *c_next;
	char	c_info[CBSIZE];
};

struct	tty dh11[NDH11];
short	dhcc[NDH11];
int	dhchars[(NDH11+15)/16];
int	ndh11	= NDH11;
int	dhstart();
int	ttrstrt();
int cbase;
extern struct cblock cfree[];

/*
 * Hardware control bits
 */
#define	BITS6	01
#define	BITS7	02
#define	BITS8	03
#define	TWOSB	04
#define	PENABLE	020
/* DEC manuals incorrectly say this bit causes generation of even parity. */
#define	OPAR	040
#define	HDUPLX	040000

#define	IENAB	030100
#define	NXM	02000
#define	CLRNXM	0400
#define	PERROR	010000
#define	FRERROR	020000
#define	OVERRUN	040000
#define	XINT	0100000
#define	SSPEED	7	/* standard speed: 300 baud */

#ifdef ERNIE
#define	DHTIME	2		/* Since Berknet packets are only 100 chars */
#else
#define	DHTIME	6
#endif
extern int dhtimer();

/*
 * DM control bits
 */
#define	TURNON	03	/* CD lead + line enable */
#define	TURNOFF	01	/* line enable */
#define	RQS	04	/* request to send */

/*
 * Software copy of last dhbar
 */
short	dhsar[(NDH11+15)/16];

struct device
{
	union {
		short	dhcsr;
		char	dhcsrl;
	} un;
	short	dhnxch;
	short	dhlpr;
	unsigned short	dhcar;
	short	dhbcr;
	unsigned short	dhbar;
	short	dhbreak;
	short	dhsilo;
};

/*
 * Open a DH11 line.
 */
/*ARGSUSED*/
dhopen(dev, flag)
{
	register struct tty *tp;
	register d;
	register struct device *addr;
	static	timer_on;
	int s;

	d = minor(dev) & 0177;
	if (d >= NDH11) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dh11[d];
	addr = DHADDR;
	addr += d>>4;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dhstart;
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN;
	s = spl6();
	if (!timer_on) {
		timer_on++;
		timeout(dhtimer, (caddr_t)0, DHTIME);
		cbase = (short)uballoc((caddr_t)cfree, NCLIST*sizeof(struct cblock), 0);
	}
	splx(s);
	addr->un.dhcsr |= IENAB;
	if ((tp->t_state&ISOPEN) == 0) {
		ttychars(tp);
		tp->t_ispeed = SSPEED;
		tp->t_ospeed = SSPEED;
		tp->t_flags = ODDP|EVENP|ECHO;
		dhparam(d);
	}
	if (tp->t_state&XCLUDE && u.u_uid!=0) {
		u.u_error = EBUSY;
		return;
	}
	dmopen(dev);
	(*linesw[tp->t_line].l_open)(dev,tp);
}

/*
 * Close a DH11 line.
 */
/*ARGSUSED*/
dhclose(dev, flag)
dev_t dev;
int  flag;
{
	register struct tty *tp;
	register d;

	d = minor(dev) & 0177;
	tp = &dh11[d];
	(*linesw[tp->t_line].l_close)(tp);
	if (tp->t_state&HUPCLS || (tp->t_state&ISOPEN)==0)
		dmctl(d, TURNOFF);
	ttyclose(tp);
}

/*
 * Read from a DH11 line.
 */
dhread(dev)
{
register struct tty *tp;

	tp = &dh11[minor(dev) & 0177];
	(*linesw[tp->t_line].l_read)(tp);
}

/*
 * write on a DH11 line
 */
dhwrite(dev)
{
register struct tty *tp;

	tp = &dh11[minor(dev) & 0177];
	(*linesw[tp->t_line].l_write)(tp);
}

/*
 * DH11 receiver interrupt.
 */
dhrint(dev)
{
	register struct tty *tp;
	register short c;
	register struct device *addr;

	addr = DHADDR;
	addr += minor(dev) & 0177;
	while ((c = addr->dhnxch) < 0) {	/* char. present */
		tp = &dh11[((minor(dev)&0177)<<4) + ((c>>8)&017)];
		dhchars[minor(dev)&0177]++;
		if (tp >= &dh11[NDH11])
			continue;
		if((tp->t_state&ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if (c&PERROR)
			if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 || (tp->t_flags&(EVENP|ODDP))==ODDP )
				continue;
		if (c&OVERRUN)
			printf("O");
		if (c&FRERROR)		/* break */
			if (tp->t_flags&RAW)
				c = 0;	/* null (for getty) */
			else
				c = 0177;	/* DEL (intr) */
		(*linesw[tp->t_line].l_rint)(c,tp);
	}
}

/*
 * stty/gtty for DH11
 */
/*ARGSUSED*/
dhioctl(dev, cmd, addr, flag)
caddr_t addr;
{
	register struct tty *tp;

	tp = &dh11[minor(dev) & 0177];
	if (ttioccomm(cmd, tp, addr, dev)) {
		if (cmd==TIOCSETP||cmd==TIOCSETN)
			dhparam(dev);
	} else if (cmd==TIOCSBRK) {
		/* send a break */
		register int linebit = 1 << (dev&017);
		extern dhunbrk();

		wflushtty(tp);
		spl5();
		((struct device *)tp->t_addr)->dhbreak |= linebit;
		tp->t_state |= TIMEOUT;
		timeout(dhunbrk, (caddr_t)tp, 25);	/* 300-500 ms */
		while (((struct device *)tp->t_addr)->dhbreak & linebit)
			sleep((caddr_t)&tp->t_rawq, TTIPRI);
		tp->t_state &= ~TIMEOUT;
		spl0();
		flushtty(tp);
		return;
	} else
		u.u_error = ENOTTY;
}

dhunbrk(tp)
register struct tty *tp;
{

	((struct device *)tp->t_addr)->dhbreak &= ~ (1 << (minor(tp->t_dev)&017));
	wakeup((caddr_t)&tp->t_rawq);
}

/*
 * Set parameters from open or stty into the DH hardware
 * registers.
 */
dhparam(dev)
{
	register struct tty *tp;
	register struct device *addr;
	register d;

	d = minor(dev) & 0177;
	tp = &dh11[d];
	addr = (struct device *)tp->t_addr;
	spl5();
	addr->un.dhcsrl = (d&017) | IENAB;
	/*
	 * Hang up line?
	 */
	if ((tp->t_ispeed)==0) {
		tp->t_state |= HUPCLS;
		dmctl(d, TURNOFF);
		return;
	}
	d = ((tp->t_ospeed)<<10) | ((tp->t_ispeed)<<6);
	if ((tp->t_ispeed) == 4)		/* 134.5 baud */
		d |= BITS6|PENABLE|HDUPLX;
	else if (tp->t_flags&RAW)
		d |= BITS8;
	else
		d |= BITS7|PENABLE;
	if ((tp->t_flags&EVENP) == 0)
		d |= OPAR;
	if ((tp->t_ospeed) == 3)	/* 110 baud */
		d |= TWOSB;
	addr->dhlpr = d;
	spl0();
}

/*
 * DH11 transmitter interrupt.
 * Restart each line which used to be active but has
 * terminated transmission since the last interrupt.
 */
dhxint(dev)
{
	register struct tty *tp;
	register struct device *addr;
	register d;
	short ttybit, bar, *sbar;

	d = minor(dev) & 0177;
	addr = DHADDR + d;
	addr->un.dhcsr &= (short)~XINT;
	if (addr->un.dhcsr & NXM) {
		addr->un.dhcsr |= CLRNXM;
		printf("dh clr NXM\n");
	}
	sbar = &dhsar[d];
	bar = *sbar & ~addr->dhbar;
	d <<= 4; ttybit = 1;

	for(; bar; d++, ttybit <<= 1) {
		if(bar&ttybit) {
			*sbar &= ~ttybit;
			bar &= ~ttybit;
			tp = &dh11[d];
			if (tp->t_line) {
				(*linesw[tp->t_line].l_start)(tp);
			} else {
				addr->un.dhcsrl = (d&017)|IENAB;
				if (tp->t_state&FLUSH)
					tp->t_state &= ~FLUSH;
				else {
					ndflush(&q3, dhcc[d]);
				}
				tp->t_state &= ~BUSY;
				dhstart(tp);
			}
		}
	}
}

/*
 * Start (restart) transmission on the given DH11 line.
 */
dhstart(tp)
register struct tty *tp;
{
	register struct device *addr;
	register short nch;
	int s, d;

	/*
	 * If it's currently active, or delaying,
	 * no need to do anything.
	 */
	s = spl5();
	d = tp-dh11;
	addr = (struct device *)tp->t_addr;
	if (tp->t_state&(TIMEOUT|BUSY|TTSTOP))
		goto out;


	/*
	 * If the writer was sleeping on output overflow,
	 * wake him when low tide is reached.
	 */
	if (tp->t_state&ASLEEP && tp->t_outq.c_cc<=TTLOWAT) {
		tp->t_state &= ~ASLEEP;
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq); else
			wakeup((caddr_t)&tp->t_outq);
	}

	if (tp->t_outq.c_cc == 0)
		goto out;



	/*
	 * Find number of characters to transfer.
	 */
	if (tp->t_flags & RAW) {
		nch = ndqb(&tp->t_outq, 0);
	} else {
		nch = ndqb(&tp->t_outq, 0200);
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0177)+6);
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	/*
	 * If any characters were set up, start transmission;
	 */
	if (nch) {
		addr->un.dhcsrl = (d&017)|IENAB;
		addr->dhcar = UBACVT(tp->t_outq.c_cf);
		addr->dhbcr = -nch;
		dhcc[d] = nch;
		nch = 1<<(d&017);
		addr->dhbar |= nch;
		dhsar[d>>4] |= nch;
		tp->t_state |= BUSY;
	}
    out:
	splx(s);
}


/*
 * Stop output on a line.
 * Assume call is made at spl6.
 */
/*ARGSUSED*/
dhstop(tp, flag)
register struct tty *tp;
{
	register s;

	s = spl6();
	if (tp->t_state & BUSY)
		if ((tp->t_state&TTSTOP)==0)
			tp->t_state |= FLUSH;
	splx(s);
}

/*ARGSUSED*/
dhtimer(dev)
{
register d,cc;
register struct device *addr;
	addr = DHADDR; d = 0;
	do {
		cc = dhchars[d];
		dhchars[d] = 0;
		if (cc > 8*DHTIME)
			cc = 32; else
			if (cc > 3*DHTIME)
				cc = 16; else
				cc = 0;
		addr->dhsilo = cc;
		addr++;
		dhxint(d);		/* in case lost interrupt */
		dhrint(d++);
	} while (d < (NDH11+15)/16);
	timeout(dhtimer, (caddr_t)0, DHTIME);
}
