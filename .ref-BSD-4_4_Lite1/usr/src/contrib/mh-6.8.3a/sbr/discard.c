/* discard.c - discard output on a file pointer */
#ifndef	lint
static char ident[] = "@(#)$Id: discard.c,v 1.7 1993/02/26 21:56:04 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#ifdef POSIX
#include <termios.h>
#else
#ifndef	SYS5
#include <sgtty.h>
#else	/* SYS5 */
#include <sys/types.h>
#include <termio.h>
#ifndef	NOIOCTLH
#include <sys/ioctl.h>
#endif	/* NOIOCTLH */
#endif	/* SYS5 */
#endif	/* POSIX */


void	discard (io)
FILE   *io;
{
#ifndef POSIX
#ifndef	SYS5
    struct sgttyb   sg;
#else	/* SYS5 */
    struct termio   sg;
#endif	/* SYS5 */
#endif	/* POSIX */

    if (io == NULL)
	return;

#ifdef POSIX
    tcflush (fileno (io), TCOFLUSH);
#else
#ifndef	SYS5
    if (ioctl (fileno (io), TIOCGETP, (char *) &sg) != NOTOK)
	(void) ioctl (fileno (io), TIOCSETP, (char *) &sg);
#else	/* SYS5 */
    if (ioctl (fileno (io), TCGETA, &sg) != NOTOK)
	(void) ioctl (fileno (io), TCSETA, &sg);
#endif	/* SYS5 */
#endif	/* POSIX */

#ifdef _FSTDIO
    fpurge (io);
#else
    if (io -> _ptr = io -> _base)
	io -> _cnt = 0;
#endif
}
