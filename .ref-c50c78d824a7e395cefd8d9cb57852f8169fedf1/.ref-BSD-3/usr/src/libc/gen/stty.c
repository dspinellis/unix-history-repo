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
