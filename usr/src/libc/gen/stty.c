/* @(#)stty.c	4.1 (Berkeley) 12/21/80 */
/*
 * Writearound to old stty and gtty system calls
 */

#include <sgtty.h>

stty(fd, ap)
struct sgtty *ap;
{
	return(ioctl(fd, TIOCSETP, ap));
}

gtty(fd, ap)
struct sgtty *ap;
{
	return(ioctl(fd, TIOCGETP, ap));
}
