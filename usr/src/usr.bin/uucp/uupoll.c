#ifndef lint
static char sccsid[] = "@(#)uupoll.c	5.1 (Berkeley) 7/2/83";
#endif

/*
 * Poll named system(s).
 *
 * The poll occurs even if recent attempts have failed,
 * but not if L.sys prohibits the call (e.g. wrong time of day).
 *
 * AUTHOR
 *	Tom Truscott (rti!trt)
 */

#include "uucp.h"

main(argc, argv)
register int argc;
register char **argv;
{
	if (argc < 2) {
		fprintf(stderr, "usage: uupoll system ...\n");
		cleanup(1);
	}

	chdir(Spool);
	strcpy(Progname, "uupoll");
	uucpname(Myname);

	for (--argc, ++argv; argc > 0; --argc, ++argv) {
		if (strcmp(argv[0], Myname) == SAME) {
			fprintf(stderr, "This *is* %s!\n", Myname);
			continue;
		}

		if (versys(argv[0])) {
			fprintf(stderr, "%s: unknown system.\n", argv[0]);
			continue;
		}
		/* Remove any STST file that might stop the poll */
		rmstat(argv[0]);
		/* Attempt the call */
		xuucico(argv[0]);
	}
	cleanup(0);
}

cleanup(code)
int code;
{
	exit(code);
}
