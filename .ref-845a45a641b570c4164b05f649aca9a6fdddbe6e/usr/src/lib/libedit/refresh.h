/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)refresh.h	8.1 (Berkeley) %G%
 */

/*
 * el.refresh.h: Screen refresh functions
 */
#ifndef _h_el_refresh
#define _h_el_refresh

#include "histedit.h"

typedef struct {
    coord_t 	 r_cursor;	/* Refresh cursor position	*/
    int r_oldcv, r_newcv;	/* Vertical locations		*/
} el_refresh_t;

protected void	re_putc 		__P((EditLine *, int));
protected void	re_clear_lines		__P((EditLine *));
protected void	re_clear_display	__P((EditLine *));
protected void	re_refresh		__P((EditLine *));
protected void	re_refresh_cursor	__P((EditLine *));
protected void	re_fastaddc		__P((EditLine *));
protected void	re_goto_bottom		__P((EditLine *));

#endif /* _h_el_refresh */
