#include <stdio.h>
#include <sys/reboot.h>
/*
 * Reboot
 */
static	char *sccsid = "@(#)reboot.c	4.1 (Berkeley) %G%";

main(argc, argv)
	int argc;
	char **argv;
{
	int howto;
	register char *argp;

	argc--, argv++;
	howto = 0;
	while (argc > 0) {
		if (!strcmp(*argv, "-s"))
			howto |= RB_SINGLE;
		else if (!strcmp(*argv, "-n"))
			howto |= RB_NOSYNC;
		else if (!strcmp(*argv, "-a"))
			howto |= RB_ASKNAME;
		else {
			fprintf(stderr,
			    "usage: reboot [ -a ] [ -n ] [ -s ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	syscall(55, howto);
	perror("reboot");
}
