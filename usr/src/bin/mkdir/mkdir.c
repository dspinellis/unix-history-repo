static char *sccsid = "@(#)mkdir.c	4.5 (Berkeley) %G%";
/*
 * make directory
 */
#include <stdio.h>

main(argc, argv)
	char *argv[];
{
	int errors = 0;

	if (argc < 2) {
		fprintf(stderr, "usage: %s directory ...\n", argv[0]);
		exit(1);
	}
	while (--argc)
		if (mkdir(*++argv, 0777) < 0) {
			fprintf(stderr, "mkdir: ");
			perror(*argv);
			errors++;
		}
	exit(errors != 0);
}
