/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ttydefaults.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/termios.h>

#include "extern.h"

void
set_ttydefaults(fd)
	int fd;
{
	struct termios term;

	tcgetattr(fd, &term);
	term.c_iflag = TTYDEF_IFLAG;
	term.c_oflag = TTYDEF_OFLAG;
	term.c_lflag = TTYDEF_LFLAG;
	term.c_cflag = TTYDEF_CFLAG;
	tcsetattr(fd, TCSAFLUSH, &term);
}
