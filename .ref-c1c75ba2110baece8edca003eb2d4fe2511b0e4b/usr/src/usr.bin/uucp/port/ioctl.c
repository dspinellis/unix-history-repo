#ifndef lint
static char sccsid[] = "@(#)ioctl.c	5.2 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sgtty.h>

/*******
 *	ioctl(fn, com, ttbuf)	for machines without ioctl
 *	int fn, com;
 *	struct sgttyb *ttbuf;
 *
 *	return codes - same as stty and gtty
 */

ioctl(fn, com, ttbuf)
register int fn, com;
struct sgttyb *ttbuf;
{
	struct sgttyb tb;

	switch (com) {
	case TIOCHPCL:
		gtty(fn, &tb);
		tb.sg_flags |= 1;
		return(stty(fn, &tb));
	case TIOCGETP:
		return(gtty(fn, ttbuf));
	case TIOCSETP:
		return(stty(fn, ttbuf));
	case TIOCEXCL:
	case TIOCNXCL:
	default:
		return(-1);
	}
}
