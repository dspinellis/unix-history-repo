#include <X/mit-copyright.h>
/* Copyright 1986, Massachusetts Institute of Technology */

#ifndef lint
static char *rcsid_xcons_c = "$Header: xcons.c,v 10.1 86/02/02 13:10:50 jg Rel $";
#endif lint

#include <stdio.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/errno.h>

extern int errno;

main(argc, argv)
    int argc;
    char **argv;
{
	char fromdev[30], todev[30];
	int from, to, n;
	char buf[2048];

	if (argc != 3)
		exit(1);
	strcpy(fromdev, "/dev/");
	strcat(fromdev, argv[2]);
	strcpy(todev, "/dev/");
	strcat(todev, argv[1]);
	if (argc != 3 || (from = open(fromdev, O_RDONLY, 0)) < 0)
		exit(1);
	while (1) {
		if ((n = read(from, buf, sizeof(buf))) <= 0)
			exit(1);
		if (fcntl(from, F_SETFL, FNDELAY) < 0)
			exit(1);
		/* we only open the tty when we need to, because it won't
		 * be preserved across logins */
		if ((to = open(todev, O_WRONLY, 0)) < 0)
			exit(1);
		while (n > 0) {
			write(to, buf, n);
			n = read(from, buf, n);
		}
		close(to);
		if (errno != EWOULDBLOCK || fcntl(from, F_SETFL, 0) < 0)
			exit(1);
	}
}
