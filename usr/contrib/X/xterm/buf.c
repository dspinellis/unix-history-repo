#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* buf.c */

#ifndef lint
static char *rcsid_buf_c = "$Header: buf.c,v 10.8 86/02/01 16:05:37 tony Rel $";
#endif	lint

#include <stdio.h>
#include <errno.h>
#include <X/Xlib.h>
#include "ptyx.h"
#include <ctype.h>

extern int errno;
extern int am_slave;

/*
 * If no input available, returns -1. Else, reads characters from
 * trmbuf->fildes and puts them in the buffer, then returns first
 * character read.
 */
fill(trmbuf)
register Buffer *trmbuf;
{
#ifdef JUMPSCROLL
	extern Terminal term;
	register Screen *screen = &term.screen;

	if (screen->scroll_amt)
		FlushScroll(screen);
#endif JUMPSCROLL
	trmbuf->ptr = &trmbuf->buf[0];
	trmbuf->cnt = read(trmbuf->fildes, trmbuf->ptr, BUF_SIZE);

	if (trmbuf->cnt == -1) {
	   	if (errno != EWOULDBLOCK) {
			if (errno == EIO && am_slave) exit(0);	/* pty closed */
			else Panic("fill: read returned unexpected error (%d)\n",
				errno);
		}
		return(-1);
	}
	
	if (--trmbuf->cnt < 0)
		Panic("fill: read returned %d\n", trmbuf->cnt+1);
	return(*trmbuf->ptr++ & 0177);
}
