/*	cons.c	3.4	%H%	*/

/*
 * Vax console driver and floppy interface
 */
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/systm.h"
#include "../h/cons.h"
#include "../h/mtpr.h"

/*
 * When running dz's using only SAE (silo alarm) on input
 * it is necessary to call dzrint() at clock interrupt time.
 * This is unsafe unless spl5()s in tty code are changed to
 * spl6()s to block clock interrupts.  Note that the dh driver
 * currently in use works the same way as the dz, even though
 * we could try to more intelligently manage its silo.
 * Thus don't take this out if you have no dz's unless you
 * change clock.c and dhtimer().
 */
#define	spl5	spl6

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
		cnrfl(c);
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
	if (ttioccom(cmd, tp, addr, dev) == 0)
		u.u_error = ENOTTY;
}

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

	tp = &cons;
	tp->t_state &= ~BUSY;
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		cnstart(tp);
	if ((tp->t_state & BUSY) == 0)
		conxfl();
}

cnstart(tp)
register struct tty *tp;
{
	register c;
	register s;

	s = spl5();
	if (tp->t_state & (TIMEOUT|BUSY|TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= TTLOWAT && tp->t_state&ASLEEP) {
		tp->t_state &= ~ASLEEP;
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	if ((mfpr(TXCS)&TXCS_RDY) == 0)
		return;
	if ((c=getc(&tp->t_outq)) >= 0) {
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

char	*msgbufp = msgbuf;	/* Next saved printf character */
/*
 * Print a character on console.
 * Attempts to save and restore device
 * status.
 * If the switches are 0, all
 * printing is inhibited.
 *
 * Whether or not printing is inhibited,
 * the last MSGBUFS characters
 * are saved in msgbuf for inspection later.
 */
putchar(c)
register c;
{
	register s, timo;

	if (c != '\0' && c != '\r' && c != 0177) {
		*msgbufp++ = c;
		if(msgbufp >= &msgbuf[MSGBUFS])
			msgbufp = msgbuf;
	}
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
	mtpr(TXCS,0);
	mtpr(TXDB, c&0xff);
	if(c == '\n')
		putchar('\r');
	putchar(0);
	mtpr(TXCS, s);
}
