/*	cons.c	4.7	81/03/09	*/

/*
 * Vax console driver and floppy interface
 *
 * WE AVOID USE THE READY BIT IN TXCS BECAUSE IT DOESN'T
 * WORK ON AN 11/750 WITH AN RDM PLUGGED IN.
 */
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/systm.h"
#include "../h/cons.h"
#include "../h/mtpr.h"
#include "../h/mx.h"
#include "../h/cpu.h"

#define	NL1	000400
#define	NL2	001000
#define	CR2	020000
#define	FF1	040000
#define	TAB1	002000

struct	tty cons;
int	cnstart();
int	ttrstrt();
char	partab[];

/*ARGSUSED*/
cnopen(dev, flag)
dev_t dev;
{
	register struct tty *tp;

	tp = &cons;
	tp->t_oproc = cnstart;
	tp->t_iproc = NULL;
	if ((tp->t_state&ISOPEN) == 0) {
		ttychars(tp);
		tp->t_state = ISOPEN|CARR_ON;
		tp->t_flags = EVENP|ECHO|XTABS|CRMOD;
	}
	if (tp->t_state&XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	mtpr(RXCS, mfpr(RXCS)|RXCS_IE);
	mtpr(TXCS, mfpr(TXCS)|TXCS_IE);
	(*linesw[tp->t_line].l_open)(dev, tp);
}

/*ARGSUSED*/
cnclose(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &cons;
	(*linesw[tp->t_line].l_close)(tp);
	ttyclose(tp);
}

/*ARGSUSED*/
cnread(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &cons;
	(*linesw[tp->t_line].l_read)(tp);
}

/*ARGSUSED*/
cnwrite(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &cons;
	(*linesw[tp->t_line].l_write)(tp);
}

/*
 * Got a level-20 receive interrupt -
 * the LSI wants to give us a character.
 * Catch the character, and see who it goes to.
 */
/*ARGSUSED*/
cnrint(dev)
dev_t dev;
{
	register int c;
	register struct tty *tp;

	c = mfpr(RXDB);
	if (c&RXDB_ID) {
#if VAX780
		if (cpu == VAX_780)
			cnrfl(c);
#endif
		return;
	}
	tp = &cons;
	(*linesw[tp->t_line].l_rint)(c, tp);
}

/*ARGSUSED*/
cnioctl(dev, cmd, addr, flag)
dev_t dev;
caddr_t addr;
{
	register struct tty *tp;
 
	tp = &cons;
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (cmd == 0)
		return;
	if (ttioctl(tp, cmd, addr, flag) == 0)
		u.u_error = ENOTTY;
}

int	consdone = 1;
/*
 * Got a level-20 transmission interrupt -
 * the LSI wants another character.  First,
 * see if we can send something to the typewriter.
 * If not, try the floppy.
 */
/*ARGSUSED*/
cnxint(dev)
dev_t dev;
{
	register struct tty *tp;

	consdone++;
	tp = &cons;
	tp->t_state &= ~BUSY;
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		cnstart(tp);
#if VAX780
	if (cpu==VAX_780 && (tp->t_state & BUSY) == 0)
		conxfl();
#endif
}

cnstart(tp)
register struct tty *tp;
{
	register c;
	register s;

	s = spl5();
	if (tp->t_state & (TIMEOUT|BUSY|TTSTOP))
		goto out;
	if (tp->t_state&ASLEEP && tp->t_outq.c_cc <= TTLOWAT(tp)) {
		tp->t_state &= ~ASLEEP;
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (consdone == 0)
		return;
	if ((c=getc(&tp->t_outq)) >= 0) {
		consdone = 0;
		if (tp->t_flags&RAW)
			mtpr(TXDB, c&0xff);
		else if (c<=0177)
			mtpr(TXDB, (c | (partab[c]&0200))&0xff);
		else {
			timeout(ttrstrt, (caddr_t)tp, (c&0177));
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= BUSY;
    out:
	splx(s);
}

/*
 * Print a character on console.
 * Attempts to save and restore device
 * status.
 */
cnputc(c)
register c;
{
	register s, timo;

	timo = 30000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 */
	while((mfpr(TXCS)&TXCS_RDY) == 0)
		if(--timo == 0)
			break;
	if(c == 0)
		return;
	s = mfpr(TXCS);
	mtpr(TXCS, 0);
	mtpr(TXDB, c&0xff);
	if(c == '\n')
		cnputc('\r');
	cnputc(0);
	mtpr(TXCS, s);
}
