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
	t->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INLCR|IGNCR|ICRNL|IXON|IEXTEN);
	t->c_oflag &= ~(ONLCR|OXTABS);
	t->c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG);
	/* set MIN/TIME */
}
