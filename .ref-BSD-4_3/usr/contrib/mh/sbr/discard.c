/* discard.c - discard output on a file pointer */

#include "../h/mh.h"
#include <stdio.h>
#ifndef	SYS5
#include <sgtty.h>
#else	SYS5
#include <sys/types.h>
#include <termio.h>
#include <sys/ioctl.h>
#endif	SYS5


void	discard (io)
FILE   *io;
{
#ifndef	SYS5
    struct sgttyb   sg;
#else	SYS5
    struct termio   sg;
#endif	SYS5

    if (io == NULL)
	return;

#ifndef	SYS5
    if (ioctl (fileno (io), TIOCGETP, (char *) &sg) != NOTOK)
	(void) ioctl (fileno (io), TIOCSETP, (char *) &sg);
#else	SYS5
    if (ioctl (fileno (io), TCGETA, &sg) != NOTOK)
	(void) ioctl (fileno (io), TCSETA, &sg);
#endif	SYS5

    if (io -> _ptr = io -> _base)
	io -> _cnt = 0;
}
