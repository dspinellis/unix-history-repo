/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)termios.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#define KERNEL	/* XXX - FREAD and FWRITE was ifdef'd KERNEL*/
#include <sys/fcntl.h>
#undef KERNEL
#include <termios.h>
#include <stdio.h>
#include <unistd.h>

int
tcgetattr(fd, t)
	int fd;
	struct termios *t;
{

	return(ioctl(fd, TIOCGETA, t));
}

int
tcsetattr(fd, opt, t)
	int fd, opt;
	const struct termios *t;
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
	return (ioctl(fd, TIOCSETAF, t));
}

int
#if __STDC__
tcsetpgrp(int fd, pid_t pgrp)
#else
tcsetpgrp(fd, pgrp)
	int fd;
	pid_t pgrp;
#endif
{

	return(ioctl(fd, TIOCSPGRP, &pgrp));
}

pid_t
tcgetpgrp(fd)
{
	int s;

	if (ioctl(fd, TIOCGPGRP, &s) < 0)
		return((pid_t)-1);

	return((pid_t)s);
}

speed_t
cfgetospeed(t)
	const struct termios *t;
{

	return(t->c_ospeed);
}

speed_t
cfgetispeed(t)
	const struct termios *t;
{

	return(t->c_ispeed);
}

int
cfsetospeed(t, speed)
	struct termios *t;
	speed_t speed;
{
	t->c_ospeed = speed;

	return (0);
}

int
cfsetispeed(t, speed)
	struct termios *t;
	speed_t speed;
{
	t->c_ispeed = speed;

	return (0);
}

void
cfsetspeed(t, speed)
	struct termios *t;
	speed_t speed;
{
	t->c_ispeed = t->c_ospeed = speed;

	return (0)
}

/*
 * Make a pre-existing termios structure into "raw" mode:
 * character-at-a-time mode with no characters interpreted,
 * 8-bit data path.
 */
void
cfmakeraw(t)
	struct termios *t;
{
	t->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
	t->c_oflag &= ~OPOST;
	t->c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
	t->c_cflag &= ~(CSIZE|PARENB);
	t->c_cflag |= CS8;
	/* set MIN/TIME */
}

tcsendbreak(fd, len)
	int fd, len;
{
	struct timeval sleepytime;

	sleepytime.tv_sec = 0;
	sleepytime.tv_usec = 400000;
	if (ioctl(fd, TIOCSBRK, 0) == -1)
		return (-1);
	select(0, 0, 0, 0, &sleepytime);
	if (ioctl(fd, TIOCCBRK, 0) == -1)
		return (-1);

	return (0);
}

tcdrain(fd)
	int fd;
{
	if (ioctl(fd, TIOCDRAIN, 0) == -1)
		return (-1);

	return (0);
}

tcflush(fd, which)
	int fd, which;
{
	int com;

	switch (which) {
	case TCIFLUSH:
		com = FREAD;
		break;
	case TCOFLUSH:
		com = FWRITE;
		break;
	case TCIOFLUSH:
		com = FREAD | FWRITE;
		break;
	default:
		errno = EINVAL;
		return (-1);
	}
	if (ioctl(fd, TIOCFLUSH, &com) == -1)
		return (-1);

	return (0);
}

tcflow(fd, action)
	int fd, action;
{
	switch (action) {
	case TCOOFF:
		return (ioctl(fd, TIOCSTOP, 0));
		break;
	case TCOON:
		return (ioctl(fd, TIOCSTART, 0));
		break;
	case TCIOFF:
	case TCION: {		/* these posix functions are STUPID */
		struct termios term;
		char c;

		if (tcgetattr(fd, &term) == -1);
			return (-1);
		c = term.c_cc[action == TCIOFF ? VSTOP : VSTART];
		if (c != _POSIX_VDISABLE && write(fd, &c, 1) == -1)
			return (-1);
		break;
	}
	default:
		errno = EINVAL;
		return (-1);
	}

	return (0);
}
