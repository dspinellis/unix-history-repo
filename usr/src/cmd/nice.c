/* nice */

#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	int nicarg = 10;
	extern errno;
	extern char *sys_errlist[];

	if(argc > 1 && argv[1][0] == '-') {
		nicarg = atoi(&argv[1][1]);
		argc--;
		argv++;
	}
	if(argc < 2) {
		fputs("usage: nice [ -n ] command\n", stderr);
		exit(1);
	}
	nice(nicarg);
	execvp(argv[1], &argv[1]);
	fprintf(stderr, "%s: %s\n", sys_errlist[errno], argv[1]);
	exit(1);
}
