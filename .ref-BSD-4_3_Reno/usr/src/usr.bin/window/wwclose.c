/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwclose.c	3.18 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"

wwclose(w)
register struct ww *w;
{
	wwindex[w->ww_index] = 0;
	if (w->ww_pty >= 0)
		(void) close(w->ww_pty);
	if (w->ww_socket >= 0)
		(void) close(w->ww_socket);
	wwfree((char **)w->ww_win, w->ww_w.t);
	wwfree((char **)w->ww_buf, w->ww_b.t);
	if (w->ww_fmap != 0)
		wwfree((char **)w->ww_fmap, w->ww_w.t);
	free((char *)(w->ww_nvis + w->ww_w.t));
	if (w->ww_ob != 0)
		free(w->ww_ob);
	free((char *)w);
}
