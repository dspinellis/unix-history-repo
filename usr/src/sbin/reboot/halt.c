static	char *sccsid = "@(#)halt.c	4.1 (Berkeley) %G%";
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

	howto = RB_HALT;
	argc--, argv++;
	while (argc > 0) {
		if (!strcmp(*argv, "-n"))
			howto |= RB_NOSYNC;
		else {
			fprintf(stderr, "usage: halt [ -n ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	syscall(55, howto);
	perror("reboot");
}
