/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty_tb.c	6.5 (Berkeley) %G%
 */

#include "tb.h"
#if NTB > 0

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "inode.h"
#include "file.h"
#include "buf.h"
#include "uio.h"

/*
 * Line discipline for RS232 tablets.
 * Supplies binary coordinate data.
 *
 * MAKE TABLET TYPE AN ioctl TO AVOID HAVING ONE DISCIPLINE PER TABLET TYPE.
 */

#define NTBS		(16)
#define MALLTABCHAR	(8)
#define MTABCHAR 	(5)
#define MNTABCHAR	(6)

struct tb {
	short used;
	char cbuf[MALLTABCHAR];
	struct tbpos {
		int	xpos;
		int	ypos;
		short	status;
		short	scount;
	} tbpos;
} tb[NTBS];

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
	register struct tb *tbp;

	if (tp->t_line == TABLDISC || tp->t_line == NTABLDISC)
		return (ENODEV);
	ttywflush(tp);
	for (tbp = tb; tbp < &tb[NTBS]; tbp++)
		if (!tbp->used)
			break;
	if (tbp >= &tb[NTBS])
		return (EBUSY);
	tbp->used++;
	tp->t_cp = tbp->cbuf;
	tp->t_inbuf = 0;
	tbp->tbpos.xpos = tbp->tbpos.ypos = 0;
	tbp->tbpos.status = tbp->tbpos.scount = 0;
	tp->T_LINEP = (caddr_t) tbp;
	return (0);
}

/*
 * Break down... called when discipline changed or from device
 * close routine.
 */
tbclose(tp)
	register struct tty *tp;
{
	register int s = spl5();

	((struct tb *) tp->T_LINEP)->used = 0;
	tp->t_cp = 0;
	tp->t_inbuf = 0;
	tp->t_rawq.c_cc = 0;		/* clear queues -- paranoid */
	tp->t_canq.c_cc = 0;
	tp->t_line = 0;			/* paranoid: avoid races */
	splx(s);
}

/*
 * Read from a tablet line.
 * Characters have been buffered in a buffer and
 * decoded. The coordinates are now sluffed back to the user.
 */
tbread(tp, uio)
	register struct tty *tp;
	struct uio *uio;
{
	struct tbpos *tbpos;

	if ((tp->t_state&TS_CARR_ON)==0)
		return (EIO);
	tbpos = &(((struct tb *) (tp->T_LINEP))->tbpos);
	return (uiomove(tbpos, sizeof *tbpos, UIO_READ, uio));
}

/*
 * Low level character input routine.
 * Stuff the character in the buffer, and decode the it
 * if all the chars are there.
 *
 * This routine could be expanded in-line in the receiver
 * interrupt routine of the dh-11 to make it run as fast as possible.
 */
int	LASTTABC;

tbinput(c, tp)
	register int c;
	register struct tty *tp;
{
	register struct tb *tbp = (struct tb *) tp->T_LINEP;

	if (tp->t_line == TABLDISC) {
		if ((c&0200) || (tp->t_inbuf == MTABCHAR)) {
			tp->t_cp = tbp->cbuf;
			tp->t_inbuf = 0;
		}
		*tp->t_cp++ = c&0177;
		if (++tp->t_inbuf == MTABCHAR)
			tbdecode(tbp->cbuf, &tbp->tbpos);
	} else if (tp->t_line == NTABLDISC) {
		if ((c&0200) || (tp->t_inbuf == MNTABCHAR)) {
			tp->t_cp = tbp->cbuf;
			tp->t_inbuf = 0;
		}
		*tp->t_cp++ = c&0177;
		if (++tp->t_inbuf == MNTABCHAR)
			tbndecode(tbp->cbuf, &tbp->tbpos);
	}
}

/*
 * Decode tablet coordinates from ascii to binary.
 *	(gtco 6 character format)
 */
tbndecode(cp, tbpos)
	register char *cp;
	register struct tbpos *tbpos;
{

	tbpos->status = *cp>>2;	/* this needs to be decoded */
	tbpos->xpos = ((*cp++)&03)<<14;
	tbpos->xpos |= (*cp++)<<7;
	tbpos->xpos |= (*cp++);
	tbpos->ypos = ((*cp++)&03)<<14;
	tbpos->ypos |= (*cp++)<<7;
	tbpos->ypos |= (*cp++);
	tbpos->scount++;
}

/*
 * Decode tablet coordinates from ascii to binary.
 *	(hitachi 5 character format)
 */
tbdecode(cp, tbpos)
	register char *cp;
	register struct tbpos *tbpos;
{
	register int status;
	register char byte;

	byte = *cp++;
	status = (byte&0100) ? 0100000 : 0;
	byte &= ~0100;
	if (byte > 036)
		status |= 1<<((byte-040)/2);
	tbpos->xpos = (*cp++)<<7;
	tbpos->xpos |= (*cp++);
	if (tbpos->xpos < 256)		/* tablet wraps around at 256 */
		status &= 077777;	/* make it out of proximity */
	tbpos->ypos = (*cp++)<<7;
	tbpos->ypos |= (*cp++);
	tbpos->status  = status;
	tbpos->scount++;
}

/*
 * This routine is called whenever a ioctl is about to be performed
 * and gets a chance to reject the ioctl.  We reject all teletype
 * oriented ioctl's except those which set the discipline, and
 * those which get parameters (gtty and get special characters).
 */
/*ARGSUSED*/
tbioctl(tp, cmd, data, flag)
	struct tty *tp;
	caddr_t data;
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
