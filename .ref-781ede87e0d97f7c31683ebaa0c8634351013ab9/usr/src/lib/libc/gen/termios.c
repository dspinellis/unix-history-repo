/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)termios.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/termios.h>
#include <stdio.h>

tcgetattr(fd, t)
	int fd;
	struct termios *t;
{
	extern errno;

	return(ioctl(fd, TIOCGETA, t));
}

tcsetattr(fd, opt, t)
	int fd, opt;
	struct termios *t;
{
	struct termios localterm;

	if (opt & TCSASOFT) {
		localterm = *t;
		localterm.c_cflag |= CIGNORE;
		t = &localterm;
		opt &= TCSASOFT;
	}
	if (opt == TCSANOW)
		return (ioctl(fd, TIOCSETA, t));
	else if (opt == TCSADRAIN)
		return (ioctl(fd, TIOCSETAW, t));
	else
		return (ioctl(fd, TIOCSETAF, t));
}

tcsetpgrp(fd, pgrp)
{
	return(ioctl(fd, TIOCSPGRP, &pgrp));
}

tcgetpgrp(fd)
{
	int pgrp;

	if (ioctl(fd, TIOCGPGRP, &pgrp) < 0)
		return(-1);
	return(pgrp);
}

cfgetospeed(t)
	struct termios *t;
{
	return(t->c_ospeed);
}

cfgetispeed(t)
	struct termios *t;
{
	return(t->c_ispeed);
}

cfsetospeed(t, speed)
	struct termios *t;
{
	t->c_ospeed = speed;
}

cfsetispeed(t, speed)
	struct termios *t;
{
	t->c_ispeed = speed;
}

cfsetspeed(t, speed)
	struct termios *t;
{
	t->c_ispeed = t->c_ospeed = speed;
}

cfmakeraw(t)
	struct termios *t;
{
	t->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INLCR|IGNCR|ICRNL|IXON);
	t->c_oflag &= ~(ONLCR|OXTABS);
	t->c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
	/* set MIN/TIME */
}
