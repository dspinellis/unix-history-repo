#ifndef lint
static char sccsid[] = "@(#)uupoll.c	5.2 (Berkeley) %G%";
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
	int ret;
	char wrkpre[MAXFULLNAME];
	char file[MAXFULLNAME];

	if (argc < 2) {
		fprintf(stderr, "usage: uupoll system ...\n");
		cleanup(1);
	}

	ret = chdir(Spool);
	ASSERT(ret >= 0, "CHDIR FAILED", Spool, ret);
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
		sprintf(wrkpre, "LCK..%.7s", argv[0]);
		if (access(wrkpre, 0) < 0)
			rmstat(argv[0]);
		sprintf(wrkpre, "%c.%.7s", CMDPRE, argv[0]);
		if (!iswrk(file, "chk", Spool, wrkpre)) {
			sprintf(file, "%s/%c.%.7szPOLL", subdir(Spool, CMDPRE),
				CMDPRE, argv[0]);
			close(creat(file, 0666));
		}
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
