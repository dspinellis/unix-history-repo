/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)pause.c	5.3 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/*
 * Backwards compatible pause.
 */
pause()
{

	sigpause(sigblock(0L));
}
