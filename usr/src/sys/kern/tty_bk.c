/*	tty_bk.c	4.1	82/07/13	*/

#include "bk.h"

#if NBK > 0
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/conf.h"
#include "../h/buf.h"

/*
 * Line discipline for Berkeley network.
 *
 * This supplies single lines to a user level program
 * with a minimum of fuss.  Lines are newline terminated.
 *
 * This discipline requires that tty device drivers call
 * the line specific l_ioctl routine from their ioctl routines,
 * assigning the result to cmd so that we can refuse most tty specific
 * ioctls which are unsafe because we have ambushed the
 * teletype input queues, overlaying them with other information.
 */

/*
 * Open as networked discipline.  Called when discipline changed
 * with ioctl, this assigns a buffer to the line for input, and
 * changing the interpretation of the information in the tty structure.
 */
/*ARGSUSED*/
bkopen(dev, tp)
dev_t dev;
register struct tty *tp;
{
	register struct buf *bp;

	if (u.u_error)
		return;		/* paranoia */
	if (tp->t_line == NETLDISC) {
		u.u_error = EBUSY;		/* sometimes the network */
		return;				/* ... opens /dev/tty */
	}
	bp = geteblk(1024);
	flushtty(tp, FREAD|FWRITE);
	tp->t_bufp = bp;
	tp->t_cp = (char *)bp->b_un.b_addr;
	tp->t_inbuf = 0;
	tp->t_rec = 0;
}

/*
 * Break down... called when discipline changed or from device
 * close routine.
 */
bkclose(tp)
register struct tty *tp;
{
	register s;

	s = spl5();
	wakeup((caddr_t)&tp->t_rawq);
	if (tp->t_bufp) {
		brelse(tp->t_bufp);
		tp->t_bufp = 0;
	} else
		printf("bkclose: no buf\n");
	tp->t_cp = 0;
	tp->t_inbuf = 0;
	tp->t_rec = 0;
	tp->t_line = 0;		/* paranoid: avoid races */
	splx(s);
}

/*
 * Read from a network line.
 * Characters have been buffered in a system buffer and are
 * now dumped back to the user in one fell swoop, and with a
 * minimum of fuss.  Note that no input is accepted when a record
 * is waiting.  Our clearing tp->t_rec here allows further input
 * to accumulate.
 */
bkread(tp)
register struct tty *tp;
{
	register int i;
	register s;

	if ((tp->t_state&TS_CARR_ON)==0)
		return (-1);
	s = spl5();
	while (tp->t_rec == 0 && tp->t_line == NETLDISC)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	splx(s);
	if (tp->t_line != NETLDISC)
		return (-1);
	i = MIN(tp->t_inbuf, (int)u.u_count);
	if (copyout(tp->t_bufp->b_un.b_addr, u.u_base, (unsigned)i)) {
		u.u_error = EFAULT;
		return (-1);
	}
	u.u_count -= i;
	u.u_base += i;
	u.u_offset += i;
	tp->t_cp = (char *)tp->t_bufp->b_un.b_addr;
	tp->t_inbuf = 0;
	tp->t_rec = 0;
	return (0);
}

/*
 * Low level character input routine.
 * Stuff the character in the buffer, and wake up the top
 * half after setting t_rec if this completes the record
 * or if the buffer is (ick!) full.
 *
 * Thisis where the formatting should get done to allow
 * 8 character data paths through escapes.
 *
 * This rutine should be expanded in-line in the receiver
 * interrupt routine of the dh-11 to make it run as fast as possible.
 */
bkinput(c, tp)
register c;
register struct tty *tp;
{

	if (tp->t_rec)
		return;
	*tp->t_cp++ = c;
	if (++tp->t_inbuf == 1024 || c == '\n') {
		tp->t_rec = 1;
		wakeup((caddr_t)&tp->t_rawq);
	}
}

/*
 * This routine is called whenever a ioctl is about to be performed
 * and gets a chance to reject the ioctl.  We reject all teletype
 * oriented ioctl's except those which set the discipline, and
 * those which get parameters (gtty and get special characters).
 */
/*ARGSUSED*/
bkioctl(tp, cmd, addr)
struct tty *tp;
caddr_t addr;
{

	if ((cmd>>8) != 't')
		return (cmd);
	switch (cmd) {

	case TIOCSETD:
	case TIOCGETD:
	case TIOCGETP:
	case TIOCGETC:
		return (cmd);
	}
	u.u_error = ENOTTY;
	return (0);
}
#endif
