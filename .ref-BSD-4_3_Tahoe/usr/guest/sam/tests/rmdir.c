static char *sccsid = "%W% (Berkeley) %G%";
/*
 * remove directory
 */
#include <stdio.h>

main(argc, argv)
	char *argv[];
{
	register int errors = 0;

	if(argc < 2) {
		fprintf(stderr, "rmdir: arg count\n");
		exit(1);
	}
	while(--argc)
		if (rmdir(*++argv) < 0)
			perror(*argv), errors++;
	exit(errors != 0);
}
