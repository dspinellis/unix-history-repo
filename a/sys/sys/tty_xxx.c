#ifndef NOCOMPAT
/*	tty_xxx.c	4.2	82/12/17	*/

#include "../machine/reg.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/ioctl.h"
#include "../h/tty.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/dk.h"
#include "../h/uio.h"
#include "../h/kernel.h"

ottioctl(tp, com, data, flag)
	struct tty *tp;
	caddr_t data;
{
	int s;

	switch (com) {

	/* set and fetch special characters */
	case TIOCSETC:
		bcopy(data, (caddr_t)&tp->t_intrc, sizeof (struct tchars));
		break;

	case TIOCGETC:
		bcopy((caddr_t)&tp->t_intrc, data, sizeof (struct tchars));
		break;

	/* set new parameters */
	case TIOCSETP:
	case TIOCSETN: {
		register struct sgttyb *sg = (struct sgttyb *)data;

		s = spl5();
		if (tp->t_flags&RAW || sg->sg_flags&RAW || com == TIOCSETP)
			wflushtty(tp);
		else if ((tp->t_flags&CBREAK) != (sg->sg_flags&CBREAK)) {
			if (sg->sg_flags&CBREAK) {
				struct clist tq;

				catq(&tp->t_rawq, &tp->t_canq);
				tq = tp->t_rawq;
				tp->t_rawq = tp->t_canq;
				tp->t_canq = tq;
			} else {
				tp->t_flags |= PENDIN;
				ttwakeup(tp);
			}
		}
		tp->t_ispeed = sg->sg_ispeed;
		tp->t_ospeed = sg->sg_ospeed;
		tp->t_erase = sg->sg_erase;
		tp->t_kill = sg->sg_kill;
		tp->t_flags &= ~0xffff;
		tp->t_flags |= sg->sg_flags;
		if (tp->t_flags&RAW) {
			tp->t_state &= ~TS_TTSTOP;
			ttstart(tp);
		}
		splx(s);
		break;
	}

	/* send current parameters to user */
	case TIOCGETP: {
		register struct sgttyb *sg = (struct sgttyb *)data;

		sg->sg_ispeed = tp->t_ispeed;
		sg->sg_ospeed = tp->t_ospeed;
		sg->sg_erase = tp->t_erase;
		sg->sg_kill = tp->t_kill;
		sg->sg_flags = tp->t_flags;
		break;
	}

	case FIONBIO:
		if (*(int *)data)
			tp->t_state |= TS_NBIO;
		else
			tp->t_state &= ~TS_NBIO;
		break;

	/* set/get local special characters */
	case TIOCSLTC:
		bcopy(data, (caddr_t)&tp->t_suspc, sizeof (struct ltchars));
		break;

	case TIOCGLTC:
		bcopy((caddr_t)&tp->t_suspc, data, sizeof (struct ltchars));
		break;

	/*
	 * Modify local mode word.
	 */
	case TIOCLBIS:
		tp->t_flags |= *(int *)data << 16;
		break;

	case TIOCLBIC:
		tp->t_flags &= ~(*(int *)data << 16);
		break;

	case TIOCLSET:
		tp->t_flags &= 0xffff;
		tp->t_flags |= *(int *)data << 16;
		break;

	case TIOCLGET:
		*(int *)data = tp->t_flags >> 16;
		break;

	/* should allow SPGRP and GPGRP only if tty open for reading */
	case TIOCSPGRP:
		tp->t_pgrp = *(int *)data;
		break;

	case TIOCGPGRP:
		*(int *)data = tp->t_pgrp;
		break;

	default:
		return (-1);
	}
	return (0);
}
#endif
