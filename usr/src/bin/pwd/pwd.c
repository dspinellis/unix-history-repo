static char *sccsid = "@(#)pwd.c	4.4 (Berkeley) %G%";
/*
 * Print working (current) directory
 */
#include <stdio.h>
#include <sys/param.h>

char *getwd();

main()
{
	char pathname[MAXPATHLEN + 1];

	if (getwd(pathname) == NULL) {
		fprintf(stderr, "pwd: %s\n", pathname);
		exit(1);
	}
	printf("%s\n", pathname);
	exit(0);
}
