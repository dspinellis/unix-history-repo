/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hist.h	8.1 (Berkeley) %G%
 */

/*
 * el.hist.c: History functions
 */
#ifndef _h_el_hist
#define _h_el_hist

#include "histedit.h"

typedef const HistEvent *	(*hist_fun_t) __P((ptr_t, int, ...));

typedef struct el_history_t {
    char *buf;				/* The history buffer		*/
    char *last;				/* The last character		*/
    int eventno;			/* Event we are looking for	*/
    ptr_t ref;				/* Argument for history fcns	*/
    hist_fun_t fun;			/* Event access			*/
    const HistEvent *ev;		/* Event cookie			*/
} el_history_t;

#define HIST_FUN(el, fn, arg)	\
    ((((el)->el_history.ev = \
       (*(el)->el_history.fun)((el)->el_history.ref, fn, arg)) == NULL) ? \
     NULL : (el)->el_history.ev->str)

#define	HIST_NEXT(el)		HIST_FUN(el, H_NEXT, NULL)
#define	HIST_FIRST(el)		HIST_FUN(el, H_FIRST, NULL)
#define	HIST_LAST(el)		HIST_FUN(el, H_LAST, NULL)
#define	HIST_PREV(el)		HIST_FUN(el, H_PREV, NULL)
#define	HIST_EVENT(el, num)	HIST_FUN(el, H_EVENT, num)

protected int 		hist_init	__P((EditLine *));
protected void 		hist_end	__P((EditLine *));
protected el_action_t	hist_get	__P((EditLine *));
protected int		hist_set	__P((EditLine *, hist_fun_t, ptr_t));
protected int		hist_list	__P((EditLine *, int, char **));

#endif /* _h_el_hist */
