/*	cons.c	1.1	85/07/21	*/
/*	Minor device 0 is the CP itself.
/*	  No real read/writes can be done to him.
/*	Minor 1 is the console terminal.
/*	Minor 2 is the remote line trminal.
/**/

/*
 * Tahoe console processor driver
 *
 */
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/ioctl.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/uio.h"
#include "../h/callout.h"
#include "../h/systm.h"
#include "../machine/cp.h"
#include "../machine/mtpr.h"

#define FALSE 0
#define TRUE  1

int	cnrestart() ;
int	timeout() ;

struct	tty cons[3];	/* One for each unit on CP */
struct	cons_info {
	char	last_one;	/* Last char sent - possibly repeat
				 * If zero, timeout has nothing to do
				 */
	int	active_timeout;	/* There is an active timeout for this line
				 * Set by 'cnputc' when a timeout is initiated.
				 * Reset by the routine  called at timeout.
			 	 */
	int	try_later;	/* If true, timeout has nothing to do.
				 * Set by 'cnputc' every time a char is sent.
				 * Reset by the timeout arrival. If next time
				 * the timeout comes it's zero, then it may
				 * have something to do.
				 */
	} cons_info[3];
struct	cpdcb_o consout[3] = { 
			/* 	unit,		cmd,count, buf */
			{(char)(CPTAKE | CPDONE),0,   0 },
			{(char)(CPTAKE | CPDONE),0,   0 },
			{(char)(CPTAKE | CPDONE),0,   0 }
			};
struct	cpdcb_i consin[3] = {
			/* 	unit,		cmd,count, buf */
			{(char)(CPTAKE | CPDONE),0,   0 },
			{(char)(CPTAKE | CPDONE),0,   0 },
			{(char)(CPTAKE | CPDONE),0,   0 }
			};
struct	cphdr *lasthdr;

int	cnstart();
int	ttrstrt();
char	partab[];

cnopen(dev, flag)
dev_t dev;
{
	register struct cpdcb_i *cin;
	register struct tty *tp;
	register int timo;

	if (minor(dev) > CPREMOT) 
		return EEXIST;
	tp = &cons[minor(dev)];
	if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return EBUSY;
	if (lasthdr != (struct cphdr *)0) {
		timo = 10000;
		uncache((char *)&lasthdr->cp_unit);
		while ((lasthdr->cp_unit & CPTAKE)==0 && --timo )
			uncache((char *)&lasthdr->cp_unit);
	}	/* Here we give up waiting */
	cin = &consin[minor(dev)];
	cin->cp_hdr.cp_unit = (char)(minor(dev));
	cin->cp_hdr.cp_comm = (char)CPREAD;
	cin->cp_hdr.cp_count = 1;	/* Get ready for input */
	mtpr (cin, CPMDCB);
	lasthdr = (struct cphdr *)cin;
	tp->t_oproc = cnstart;
	tp->t_dev = dev;
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_state = TS_ISOPEN|TS_CARR_ON;
		tp->t_flags = EVENP|ECHO|XTABS|CRMOD;
	}
	(*linesw[tp->t_line].l_open)(dev, tp);
}

cnclose(dev)
dev_t dev;
{
	register struct tty *tp = &cons[minor(dev)];

	(*linesw[tp->t_line].l_close)(tp);
	ttyclose(tp);
}

/*ARGSUSED*/
cnread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &cons[minor(dev)];

	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

/*ARGSUSED*/
cnwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &cons[minor(dev)];

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

	if (intenable == 0) return;
	/* make sure we dont take it from cache */
	uncache((char *)&consin[minor(dev)].cpi_buf[0]);
	c = consin[minor(dev)].cpi_buf[0];
/*
/* Wait about 5 milli for last CPMDCB to be read by CP,
/* otherwise give up
/**/
	timo = 10000;
	uncache((char *)&lasthdr->cp_unit);
	while ((lasthdr->cp_unit & CPTAKE)==0  && --timo  )
		uncache((char *)&lasthdr->cp_unit);
	uncache((char *)&lasthdr->cp_unit);
	if (lasthdr->cp_unit & CPTAKE)
	{
		consin[minor(dev)].cp_hdr.cp_unit = (char)(minor(dev));
			/* This resets status bits */
		mtpr (&consin[minor(dev)], CPMDCB); /* Ready for new character */
		lasthdr = (struct cphdr *)&consin[minor(dev)];
		tp = &cons[minor(dev)];
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
}

cnioctl(dev, cmd, addr, flag)
dev_t dev;
caddr_t addr;
{
	register struct tty *tp = &cons[minor(dev)];
	register error;
 
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (error >= 0)
		return error;
	if ((error = ttioctl(tp, cmd, addr, flag)) < 0)
		error = ENOTTY;
	else if(cmd==TIOCSETP || cmd==TIOCSETN)
		cnparams(tp);
	return error;
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
	register int line_no;

	if (intenable == 0 || consintr == 0) return;
#ifdef	CPPERF
	if (minor(dev)==CPCONS) scope_in(1);
	else scope_in(2);
#endif
	line_no = minor(dev);
	tp = &cons[line_no];
	tp->t_state &= ~TS_BUSY;
	cons_info[line_no].last_one = (char)0;
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		cnstart(tp);
}

cnstart(tp)
register struct tty *tp;
{
	register c;
	register s;

#ifdef	CPPERF
	if (minor(tp->t_dev)==CPCONS) scope_in(3);
	else scope_in(4);
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
		cnputc(c,tp);
	else if (c<=0177)
		cnputc ((c | (partab[c]&0200))&0xff,tp);
	else {
		timeout(ttrstrt, (caddr_t)tp, (c&0177));
		tp->t_state |= TS_TIMEOUT;
		goto out;
	}
	tp->t_state |= TS_BUSY;
    out:
	splx(s);
}

/*
 * Print a character on console.
 */
cnputc(c,tp)
register char c;
register struct tty *tp;
{
	register timo , line_no, s;
	register struct cpdcb_o *current;

	/* tp == 0 only in system error messages */
	if (tp == 0) {
		current = &consout[CPCONS];
		line_no = CPCONS;
		if(lasthdr == 0)	/* not done anythig yet */
			lasthdr = (struct cphdr *)current;
		c |= partab[c&0177]&0200;
	}
	else {
		current = &consout[minor(tp->t_dev)];
		line_no = minor(tp->t_dev);
	}
	timo = 30000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 * make sure we dont test this bit in cache!
	 */
	uncache((char *)&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit & CPDONE) == 0 && --timo )
		uncache((char *)&current->cp_hdr.cp_unit);
	current->cp_hdr.cp_comm = (char)CPWRITE;
	current->cp_hdr.cp_count = 1;
	current->cp_buf[0] = (char)(c & 0xff);
	timo = 10000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 */
	uncache((char *)&lasthdr->cp_unit);
	while ((lasthdr->cp_unit & CPTAKE)==0 && --timo )
		uncache((char *)&lasthdr->cp_unit);
	/* Reset done bit */
	current->cp_hdr.cp_unit = (char)line_no; 
	lasthdr = (struct cphdr *)current;
#ifdef	CPPERF
	if (intenable != 0) scope_in(5);
#endif
	cons_info[line_no].last_one = c;
	if ( !cons_info[line_no].active_timeout && clk_enable) {
		cons_info[line_no].active_timeout = TRUE;
		timeout (cnrestart, (caddr_t)tp, 10);
	}
	cons_info[line_no].try_later = TRUE; /* For timeout-means wait some more */
	mtpr (current, CPMDCB);
}

/*
 * Restart (if necessary) transfer to CP line.
 * This way, lost 'transmit' interrupts don't break the chain.
 */
cnrestart (tp)
struct tty *tp;
{
	register line_no, s;

	if (tp == 0) {
		line_no = CPCONS;
	} else
		line_no = minor(tp->t_dev);
	if (cons_info[line_no].try_later) {
		cons_info[line_no].try_later = FALSE;
		timeout (cnrestart, (caddr_t)tp, 10);
	}
	else {
		cons_info[line_no].active_timeout = FALSE;
		if (cons_info[line_no].last_one != (char)0)
			cnputc (cons_info[line_no].last_one, tp);
	}
}

/*
 * Set line parameters
 */
cnparams(tp)
register struct tty *tp;
{
	register timo ;
	register struct cpdcb_o *current;
	register struct cpdcb_i *cin;

	current = &consout[minor(tp->t_dev)];
	timo = 30000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 * make sure we dont test this bit in cache!
	 */
	uncache((char *)&current->cp_hdr.cp_unit);
	while ((current->cp_hdr.cp_unit & CPDONE) == 0 && --timo )
		uncache((char *)&current->cp_hdr.cp_unit);
	current->cp_hdr.cp_comm = (char)CPSTTY;
	current->cp_hdr.cp_count = 4;
	current->cp_buf[0] = tp->t_ispeed;
	/* the rest are defaults */
	current->cp_buf[1] = 0;	/* no parity */
	current->cp_buf[2] = 0; /* stop bits */
	current->cp_buf[3] = 8; /* data bits */
	timo = 10000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 */
	uncache((char *)&lasthdr->cp_unit);
	while ((lasthdr->cp_unit & CPTAKE)==0 && --timo )
		uncache((char *)&lasthdr->cp_unit);
	/* Reset done bit */
	current->cp_hdr.cp_unit = (char)minor(tp->t_dev); 
	lasthdr = (struct cphdr *)current;
	mtpr (current, CPMDCB);

	timo = 10000;
	uncache((char *)&lasthdr->cp_unit);
	while ((lasthdr->cp_unit & CPTAKE)==0 && --timo )
		uncache((char *)&lasthdr->cp_unit);
	cin = &consin[minor(tp->t_dev)];
	cin->cp_hdr.cp_unit = (char)(minor(tp->t_dev));
	cin->cp_hdr.cp_comm = (char)CPREAD;
	cin->cp_hdr.cp_count = 1;	/* Get ready for input */
	mtpr (cin, CPMDCB);
	lasthdr = (struct cphdr *)cin;

}
