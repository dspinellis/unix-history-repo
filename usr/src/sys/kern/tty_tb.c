/*	tty_tb.c	4.1	82/06/26	*/

#include "tb.h"
#if NTB > 0

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
 * Line discipline for RS232 tablets.
 *
 * This supplies binary coordinate data to a user level program
 * with a minimum of fuss.
 *
 * This discipline requires that tty device drivers call
 * the line specific l_ioctl routine from their ioctl routines,
 * assigning the result to cmd so that we can refuse most tty specific
 * ioctls which are unsafe because we have ambushed the
 * teletype input queues and other data, overlaying them with
 * the following information: the tty queue header, t_un.T_CTLQ,
 * is overlaid with a MTABCHAR character buffer -- the raw input
 * chars. The local characters (t_rocount on) are overlaid with
 * the current coordinate position.
 */

#define MTABCHAR 5
#define MNTABCHAR 6
struct tbposition {
	int xpos;
	int ypos;
	short int status;
	short int scount;
};

/*
 * Open as tablet discipline.  Called when discipline changed
 * with ioctl, and changes the interpretation of the information
 * in the tty structure.
 */
/*ARGSUSED*/
tbopen(dev, tp)
dev_t dev;
register struct tty *tp;
{
	register struct tbposition *tbp;

	if (u.u_error)
		return;		/* paranoia */
	if (tp->t_line == TABLDISC || tp->t_line == NTABLDISC) {
		u.u_error = EBUSY;
		return;	
	}
	wflushtty(tp);
	tp->t_cp = (char *) &tp->t_un.T_CTLQ;	/* overlay control queue */
	tp->t_inbuf = 0;
	tbp = (struct tbposition *) &tp->t_rocount;
	tbp->xpos = tbp->ypos = tbp->status = tbp->scount = 0;
}

/*
 * Break down... called when discipline changed or from device
 * close routine.
 */
tbclose(tp)
register struct tty *tp;
{
	register s;

	s = spl5();
	tp->t_cp = 0;
	tp->t_inbuf = 0;
	tp->t_rawq.c_cc = 0;		/* clear queues -- paranoid */
	tp->t_canq.c_cc = 0;
	tp->t_un.T_CTLQ.c_cc = 0;	/* clear overlaid queue status */
	tp->t_un.T_CTLQ.c_cf = tp->t_un.T_CTLQ.c_cl = NULL;
	tp->t_line = 0;		/* paranoid: avoid races */
	splx(s);
}

/*
 * Read from a tablet line.
 * Characters have been buffered in a buffer and
 * decoded. The coordinates are now sluffed back to the user.
 */
tbread(tp)
register struct tty *tp;
{
	register int i;
	register s;
	struct tbposition tbposition;

	if ((tp->t_state&TS_CARR_ON)==0)
		return (-1);
	if (copyout(&tp->t_rocount, u.u_base, (unsigned)(sizeof tbposition))) {
		u.u_error = EFAULT;
		return (-1);
	}
	u.u_count -= sizeof tbposition;
	u.u_base += sizeof tbposition;
	u.u_offset += sizeof tbposition;
	return (0);
}

/*
 * Low level character input routine.
 * Stuff the character in the buffer, and decode the it
 * if all the chars are there.
 *
 * This routine could be expanded in-line in the receiver
 * interrupt routine of the dh-11 to make it run as fast as possible.
 */
int LASTTABC;
tbinput(c, tp)
register c;
register struct tty *tp;
{

	if(tp->t_line == TABLDISC) {
		if((c&0200) || (tp->t_inbuf == MTABCHAR)) {
			tp->t_cp = (char *) &tp->t_un.T_CTLQ;
			tp->t_inbuf = 0;
		}
		*tp->t_cp++ = c&0177;
		if(++tp->t_inbuf == MTABCHAR)
			tbdecode((char *) &tp->t_un.T_CTLQ,
					(struct tbposition *) &tp->t_rocount);
	} else if(tp->t_line == NTABLDISC) {
		if((c&0200) || (tp->t_inbuf == MNTABCHAR)) {
			tp->t_cp = (char *) &tp->t_un.T_CTLQ;
			tp->t_inbuf = 0;
		}
		*tp->t_cp++ = c&0177;
		if(++tp->t_inbuf == MNTABCHAR)
			tbndecode((char *) &tp->t_un.T_CTLQ,
					(struct tbposition *) &tp->t_rocount);
	}
}

/*
 * Decode tablet coordinates from ascii to binary.
 *	(gtco 6 character format)
 */
tbndecode(cp, tbposition)
	register char *cp;
	register struct tbposition *tbposition;
{

	tbposition->status = *cp>>2;	/* this needs to be decoded */
	tbposition->xpos = ((*cp++)&03)<<14;
	tbposition->xpos |= (*cp++)<<7;
	tbposition->xpos |= (*cp++);
	tbposition->ypos = ((*cp++)&03)<<14;
	tbposition->ypos |= (*cp++)<<7;
	tbposition->ypos |= (*cp++);
	tbposition->scount++;
}

/*
 * Decode tablet coordinates from ascii to binary.
 *	(hitachi 5 character format)
 */
tbdecode(cp, tbposition)
	register char *cp;
	register struct tbposition *tbposition;
{
	register int status;
	register char byte;

	byte = *cp++;
	status = (byte&0100) ? 0100000 : 0;
	byte &= ~0100;
	if(byte > 036)
		status |= 1<<((byte-040)/2);
	tbposition->xpos = (*cp++)<<7;
	tbposition->xpos |= (*cp++);
	if(tbposition->xpos < 256)	/* tablet wraps around at 256 */
		status &= 077777;	/* make it out of proximity */
	tbposition->ypos = (*cp++)<<7;
	tbposition->ypos |= (*cp++);
	tbposition->status  = status;
	tbposition->scount++;
}

/*
 * This routine is called whenever a ioctl is about to be performed
 * and gets a chance to reject the ioctl.  We reject all teletype
 * oriented ioctl's except those which set the discipline, and
 * those which get parameters (gtty and get special characters).
 */
/*ARGSUSED*/
tbioctl(tp, cmd, addr)
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
