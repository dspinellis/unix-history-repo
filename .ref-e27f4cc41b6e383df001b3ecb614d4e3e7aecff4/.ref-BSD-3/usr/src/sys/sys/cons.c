/*	cons.c	2.1	1/5/80	*/

/*
 *   Vax console driver and floppy interface
 */
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/systm.h"
#include "../h/cons.h"
#include "../h/mtpr.h"

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
	mtpr(RXCS, mfpr(RXCS)|RXCS_IE);
	mtpr(TXCS, mfpr(TXCS)|TXCS_IE);
	ttyopen(dev, tp);
}

/*ARGSUSED*/
cnclose(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &cons;
	wflushtty(tp);
	tp->t_state = 0;
}

/*ARGSUSED*/
cnread(dev)
dev_t dev;
{

	ttread(&cons);
}

/*ARGSUSED*/
cnwrite(dev)
dev_t dev;
{

	ttwrite(&cons);
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

	if (cnxtty() == 0)
		conxfl();
}

/*
 * Formerly cnxint -
 * called by new cnxint to see if there are
 * any characters to write to the console, and,
 * if there are, to do it.
 */
cnxtty()
{
	register struct tty *tp;

	tp = &cons;
	if (tp->t_outq.c_cc == 0)
		return(0);
	ttstart(tp);
	if (tp->t_outq.c_cc == 0 || tp->t_outq.c_cc == TTLOWAT)
		wakeup((caddr_t)&tp->t_outq);
	return(1);
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

	c = mfpr(RXDB);
	if ((c&RXDB_ID)==0)		/* look at source byte */
		ttyinput(c, &cons);	/* character from typewriter */
	else
		cnrfl(c);		/* character from floppy */
}

/*ARGSUSED*/
cnioctl(dev, cmd, addr, flag)
dev_t dev;
caddr_t addr;
{
	register struct tty *tp;
 
	tp = &cons;
	if (ttioccom(cmd, tp, addr, dev) ==0)
		u.u_error = ENOTTY;
}

cnstart(tp)
register struct tty *tp;
{
	register c;

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
		}
	}
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
