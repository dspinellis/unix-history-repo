static	char *sccsid = "@(#)halt.c	4.2 (Berkeley) 11/10/80";
/*
 * Halt
 */
#include <stdio.h>
#include <sys/reboot.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int howto;
	char *ttyn = (char *)ttyname(2);

	howto = RB_HALT;
	argc--, argv++;
	while (argc > 0) {
		if (!strcmp(*argv, "-n"))
			howto |= RB_NOSYNC;
		else if (!strcmp(*argv, "-y"))
			ttyn = 0;
		else {
			fprintf(stderr, "usage: halt [ -n ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	if (ttyn && *(ttyn+strlen("/dev/tty")) == 'd') {
		fprintf(stderr, "halt: dangerous on a dialup; use ``halt -y'' if you are really sure\n");
		exit(1);
	}
	syscall(55, howto);
	perror("reboot");
}
