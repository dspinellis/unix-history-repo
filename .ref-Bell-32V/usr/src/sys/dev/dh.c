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
#include "../h/uba.h"

#define	DHADDR	((struct device *)(UBA0_DEV+0160020))
#define	NDH11	48	/* number of lines */
#define	DHNCH	8	/* max number of DMA chars */

struct	tty dh11[NDH11];
char	dh_clist[NDH11][DHNCH];
int	dhchars[(NDH11+15)/16];
int	ndh11	= NDH11;
int	dhstart();
int	ttrstrt();

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
#define	PERROR	010000
#define	FRERROR	020000
#define	XINT	0100000
#define	SSPEED	7	/* standard speed: 300 baud */
#define	NSILO	16
#define	DHTIME	6
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
int	dhsar[(NDH11+15)/16];

struct device
{
	union {
		short	dhcsr;
		char	dhcsrl;
	} un;
	short	dhnxch;
	short	dhlpr;
	unsigned short	*dhcar;
	short	dhbcr;
	short	dhbar;
	short	dhbreak;
	short	dhsilo;
};

/*
 * Open a DH11 line.
 */
dhopen(dev, flag)
{
	register struct tty *tp;
	register d;
	register struct device *addr;
	static	timer_on;
	int s;

	d = minor(dev);
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
		timeout(dhtimer,0,DHTIME);
	}
	splx(s);
	addr->un.dhcsr |= IENAB;
	if ((tp->t_state&ISOPEN) == 0) {
		tp->t_erase = CERASE;
		tp->t_kill = CKILL;
		tp->t_ispeed = SSPEED;
		tp->t_ospeed = SSPEED;
		tp->t_flags = ODDP|EVENP|ECHO;
		dhparam(d);
	}
	if (tp->t_state&XCLUDE && u.u_uid!=0) {
		u.u_error = EBUSY;
		return;
	}
	dmopen(d);
	(*linesw[tp->t_line].l_open)(dev,tp);
}

/*
 * Close a DH11 line.
 */
dhclose(dev)
{
	register struct tty *tp;
	register d;

	d = minor(dev);
	tp = &dh11[d];
	(*linesw[tp->t_line].l_close)(tp);
	if (tp->t_state&HUPCLS)
		dmctl(d, TURNOFF);
	ttyclose(tp);
}

/*
 * Read from a DH11 line.
 */
dhread(dev)
{
register struct tty *tp;

	tp = &dh11[minor(dev)];
	(*linesw[tp->t_line].l_read)(tp);
}

/*
 * write on a DH11 line
 */
dhwrite(dev)
{
register struct tty *tp;

	tp = &dh11[minor(dev)];
	(*linesw[tp->t_line].l_write)(tp);
}

/*
 * DH11 receiver interrupt.
 */
dhrint(dev)
{
	register struct tty *tp;
	register int c;
	register struct device *addr;

	addr = DHADDR;
	addr += minor(dev);
	while ((c = addr->dhnxch) < 0) {	/* char. present */
		tp = &dh11[(minor(dev)<<4) + ((c>>8)&017)];
		dhchars[minor(dev)]++;
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
dhioctl(dev, cmd, addr, flag)
caddr_t addr;
{
	register struct tty *tp;

	tp = &dh11[minor(dev)];
	if (ttioccomm(cmd, tp, addr, dev)) {
		if (cmd==TIOCSETP||cmd==TIOCSETN)
			dhparam(dev);
	} else
		u.u_error = ENOTTY;
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

	d = minor(dev);
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
	int ttybit, bar;

	d = minor(dev);
	addr = DHADDR;
	addr += d;
	bar = dhsar[d] & ~addr->dhbar;
	addr->un.dhcsr &= ~XINT;
	ttybit = 1;
	for (tp = &dh11[d<<4]; bar; tp++) {
		if(bar&ttybit) {
			dhsar[d] &= ~ttybit;
			bar &= ~ttybit;
			tp->t_state &= ~BUSY;
			if (tp->t_line)
				(*linesw[tp->t_line].l_start)(tp);
			else
				dhstart(tp);
		}
		ttybit <<= 1;
	}
}

/*
 * Start (restart) transmission on the given DH11 line.
 */
dhstart(tp)
register struct tty *tp;
{
	register struct device *addr;
	register c;
	int s, d, nch;
	char *cp;

	s = spl5();
	d = tp-dh11;
	addr = (struct device *)tp->t_addr;
	/*
	 * If it's currently active, or delaying,
	 * no need to do anything.
	 */
	if (tp->t_state&(TIMEOUT|BUSY|TTSTOP))
		goto out;
	/*
	 * t_char is a delay indicator which may have been
	 * left over from the last start.
	 * Arrange for the delay.
	 */
	if (c = tp->t_char) {
		tp->t_char = 0;
		timeout(ttrstrt, (caddr_t)tp, (c&0177)+6);
		tp->t_state |= TIMEOUT;
		goto out;
	}
	/*
	 * if outq.c_cc is negative, then c_cf is
	 * taken as the address of a contiguous 
	 * output buffer
	 */
	if ((nch=tp->t_outq.c_cc)<0) {
		if ((cp=tp->t_outq.c_cf)==NULL)
			goto out;
		addr->un.dhcsrl = (d&017) | IENAB;
		addr->dhcar = cp;
		addr->dhbcr = nch;
		c = 1<<(d&017);
		addr->dhbar |= c;
		dhsar[d>>4] |= c;
		tp->t_state |= BUSY;
		tp->t_outq.c_cc = 0;
		tp->t_outq.c_cf = NULL;
		goto out;
	}

	cp = dh_clist[d];
	nch = 0;
	/*
	 * Copy DHNCH characters, or up to a delay indicator,
	 * to the DMA area.
	 */
	while (nch > -DHNCH && (c = getc(&tp->t_outq))>=0) {
		if (c >= 0200 && (tp->t_flags&RAW)==0) {
			tp->t_char = c;
			break;
		}
		*cp++ = c;
		nch--;
	}
	/*
	 * If the writer was sleeping on output overflow,
	 * wake him when low tide is reached.
	 */
	if (tp->t_outq.c_cc<=TTLOWAT && tp->t_state&ASLEEP) {
		tp->t_state &= ~ASLEEP;
		wakeup((caddr_t)&tp->t_outq);
	}
	/*
	 * enable transmission when hooked to a channel
	 */
	if (tp->t_outq.c_cc<=TTLOWAT && tp->t_chan!=NULL) {
		mcttstart(tp);
	}
	/*
	 * If any characters were set up, start transmission;
	 * otherwise, check for possible delay.
	 */
	if (nch) {
		addr->un.dhcsrl = (d&017) | IENAB;
		addr->dhcar = cp+nch;
		addr->dhbcr = nch;
		c = 1<<(d&017);
		addr->dhbar |= c;
		dhsar[d>>4] |= c;
		tp->t_state |= BUSY;
	} else
	if (c = tp->t_char) {
		tp->t_char = 0;
		timeout(ttrstrt, (caddr_t)tp, (c&0177)+6);
		tp->t_state |= TIMEOUT;
	}
    out:
	splx(s);
}

dhtimer(dev)
{
register d,cc;
register struct device *addr;

	addr = DHADDR; d = 0;
	do {
		cc = dhchars[d];
		dhchars[d] = 0;
		if (cc > 50)
			cc = 32; else
			if (cc > 16)
				cc = 16; else
				cc = 0;
		addr->dhsilo = cc;
		addr += 1;
		dhrint(d++);
	} while (d < (NDH11+15)/16);
	timeout(dhtimer,0,DHTIME);
}
