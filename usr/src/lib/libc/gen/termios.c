/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)termios.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/errno.h>
#include <sys/termios.h>
#include <sys/tty.h>
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
	long code;
	int ret;
	extern errno;

	switch (opt) {
	case TCSANOW:
		code = TIOCSETA; 
		break;
	case TCSADRAIN: 
		code = TIOCSETAW; 
		break;
	case TCSADFLUSH: 
		code = TIOCSETAF; 
		break;
	case TCSANOW | TCSASOFT:
		code = TIOCSETAS; 
		break;
	case TCSADRAIN | TCSASOFT:
		code = TIOCSETAWS;
		break;
	case TCSADFLUSH | TCSASOFT:
		code = TIOCSETAFS;
		break;
	default:
		errno = EINVAL;
		return(-1);
	}
	return(ioctl(fd, code, t));
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
