static char *sccsid = "@(#)swapon.c	4.1 (Berkeley) %G%";
#include <stdio.h>

#define	VSWAPON	85

main(argc, argv)
	int argc;
	char *argv[];
{
	int stat = 0;

	--argc, argv++;
	if (argc == 0) {
		fprintf(stderr, "usage: swapon name...\n");
		exit(1);
	}
	do {
		if (syscall(VSWAPON, *argv++) == -1) {
			stat = 1;
			perror(argv[-1]);
		}
		argc--;
	} while (argc > 0);
	exit(stat);
}
