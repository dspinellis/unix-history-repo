/*	stty.c	4.2	83/07/04	*/

/*
 * Writearound to old stty system call.
 */

#include <sgtty.h>

stty(fd, ap)
	struct sgtty *ap;
{

	return(ioctl(fd, TIOCSETP, ap));
}
