/*	%M%	%I%	%E%	*/

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
