/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)bkmac.h	7.1 (Berkeley) 6/4/86
 */

/*
 * Macro definition of bk.c/netinput().
 * This is used to replace a call to
 *		(*linesw[tp->t_line].l_rint)(c,tp);
 * with
 *
 *		if (tp->t_line == NETLDISC)
 *			BKINPUT(c, tp);
 *		else
 *			(*linesw[tp->t_line].l_rint)(c,tp);
 */
#define	BKINPUT(c, tp) { \
	if ((tp)->t_rec == 0) { \
		*(tp)->t_cp++ = c; \
		if (++(tp)->t_inbuf == 1024 || (c) == '\n') { \
			(tp)->t_rec = 1; \
			wakeup((caddr_t)&(tp)->t_rawq); \
		} \
	} \
}
