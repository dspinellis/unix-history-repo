static char *sccsid = "@(#)rmdir.c	4.8 (Berkeley) %G%";
/*
 * Remove directory
 */
#include <stdio.h>

main(argc,argv)
	int argc;
	char **argv;
{
	int errors = 0;

	if (argc < 2) {
		fprintf(stderr, "usage: %s directory ...\n", argv[0]);
		exit(1);
	}
	while (--argc)
		if (rmdir(*++argv) < 0) {
			fprintf(stderr, "rmdir: ");
			perror(*argv);;
			errors++;
		}
	exit(errors != 0);
}
