#include <stdio.h>
#include <unistd.h>
#include <compat.h>

int
fchmod(fd, mode)
	int fd;
	u_int mode;
{
	char *tty;

	/* Find the tty. */
	if ((tty = ttyname(fd)) == NULL)
		return (-1);

	return (chmod(tty, mode));
}
