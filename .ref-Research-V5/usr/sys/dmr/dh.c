#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 *	DH-11 driver
 *	This driver calls on the DHDM driver.
 *	If the DH has no DM11-BB, then this driver will
 *	be fake. To insure loading of the correct DM code,
 *	lib2 should have dhdm.o dh.o and dhfdm.o in that order.
 */

#include "../param.h"
#include "../conf.h"
#include "../user.h"
#include "../tty.h"
#include "../proc.h"

#define	DHADDR	0160020
#define	NDH11	16

struct	tty dh11[NDH11];
int	ndh11	NDH11;

#define	BITS6	01
#define	BITS7	02
#define	BITS8	03
#define	TWOSB	04
#define	PENABLE	020
#define	OPAR	040	/* beware DEC manuals */
#define	HDUPLX	040000

#define	IENABLE	030100
#define	PERROR	010000
#define	FRERROR	020000
#define	SSPEED	7	/* standard speed: 300 baud */

#define	PS	0177776

int	dhsar;
struct dhregs {
	int dhcsr;
	int dhnxch;
	int dhlpr;
	int dhcar;
	int dhbcr;
	int dhbar;
	int dhbreak;
	int dhsilo;
};

dhopen(dev, flag)
{
	register struct tty *tp;
	extern dhstart();

	if (dev.d_minor >= NDH11) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dh11[dev.d_minor];
	tp->t_addr = dhstart;
	DHADDR->dhcsr =| IENABLE;
	tp->t_state =| WOPEN|SSTART;
	if ((tp->t_state&ISOPEN) == 0) {
		tp->t_erase = CERASE;
		tp->t_kill = CKILL;
		tp->t_speeds = SSPEED | (SSPEED<<8);
		tp->t_flags = ODDP|EVENP|ECHO;
		dhparam(tp);
	}
	dmopen(dev);
	tp->t_state =& ~WOPEN;
	tp->t_state =| ISOPEN;
	if (u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = tp;
}

dhclose(dev)
{
	register struct tty *tp;

	tp = &dh11[dev.d_minor];
	dmclose(dev);
	tp->t_state =& (CARR_ON|SSTART);
	wflushtty(tp);
}

dhread(dev)
{
	register struct tty *tp;

	tp = &dh11[dev.d_minor];
	if ((tp->t_state&CARR_ON) != 0)
		ttread(tp);
}

dhwrite(dev)
{
	register struct tty *tp;

	tp = &dh11[dev.d_minor];
	if ((tp->t_state&CARR_ON) != 0)
		ttwrite(tp);
}

dhrint()
{
	register struct tty *tp;
	register int c;

	while ((c = DHADDR->dhnxch) < 0) {	/* char. present */
		tp = &dh11[(c>>8)&017];
		if (tp >= &dh11[NDH11])
			continue;
		if((tp->t_state&ISOPEN)==0 || (c&PERROR)) {
			wakeup(tp);
			continue;
		}
		if (c&FRERROR)		/* break */
			if (tp->t_flags&RAW)
				c = 0;		/* null (for getty) */
			else
				c = 0177;	/* DEL (intr) */
		ttyinput(c, tp);
	}
}

dhsgtty(dev, av)
int *av;
{
	register struct tty *tp;
	register r;

	tp = &dh11[dev.d_minor];
	if (ttystty(tp, av))
		return;
	dhparam(tp);
}

dhparam(atp)
struct tty *atp;
{
	register struct tty *tp;
	register int lpr;

	tp = atp;
	spl5();
	DHADDR->dhcsr.lobyte = (tp - dh11) | IENABLE;
	lpr = (tp->t_speeds.hibyte<<10) | (tp->t_speeds.lobyte<<6);
	if (tp->t_speeds.lobyte == 4)		/* 134.5 baud */
		lpr =| BITS6|PENABLE|HDUPLX; else
		if (tp->t_flags&EVENP)
			if (tp->t_flags&ODDP)
				lpr =| BITS8; else
				lpr =| BITS7|PENABLE; else
			lpr =| BITS7|OPAR|PENABLE;
	if (tp->t_speeds.lobyte == 3)	/* 110 baud */
		lpr =| TWOSB;
	DHADDR->dhlpr = lpr;
	spl0();
}

dhxint()
{
	register struct tty *tp;
	register ttybit, bar;

	bar = DHADDR->dhbar;
	DHADDR->dhcsr =& ~0101060;
	bar = (bar|dhsar)^bar;
	ttybit = 1;
	for (tp = dh11; tp < &dh11[NDH11]; tp++) {
		if(bar&ttybit) {
			dhsar =& ~ttybit;
			tp->t_state =& ~BUSY;
			dhstart(tp);
		}
		ttybit =<< 1;
	}
}

dhstart(atp)
struct tty *atp;
{
	extern ttrstrt();
	register lineno, c;
	register struct tty *tp;
	int sps;
	struct { int int; };

	sps = PS->int;
	spl5();
	tp = atp;
	if (tp->t_state&(TIMEOUT|BUSY) || (tp->t_outq.c_cc==0))
		return;
	if ((c = getc(&tp->t_outq))<=0177) {
		tp->t_char = c;
		lineno = tp-dh11;
		DHADDR->dhcsr.lobyte = lineno | IENABLE;
		DHADDR->dhcar = &tp->t_char;
		DHADDR->dhbcr = -1;
		lineno = 1<<lineno;
		DHADDR->dhbar =| lineno;
		dhsar =| lineno;
		tp->t_state =| BUSY;
	} else {
		timeout(ttrstrt, tp, (c&0177)+6);
		tp->t_state =| TIMEOUT;
	}
	if (tp->t_outq.c_cc == 0 || tp->t_outq.c_cc == TTLOWAT)
		wakeup(&tp->t_outq);
	PS->int = sps;
}
