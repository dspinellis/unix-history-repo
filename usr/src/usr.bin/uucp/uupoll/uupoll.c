#ifndef lint
static char sccsid[] = "@(#)uupoll.c	5.4 (Berkeley) %G%";
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

int TransferSucceeded = 1;

main(argc, argv)
register int argc;
register char **argv;
{
	int ret;
	char wrkpre[MAXFULLNAME];
	char file[MAXFULLNAME];
	char grade = 'A';
	int nocall = 0;

	if (argc < 2) {
		fprintf(stderr, "usage: uupoll [-gX] [-n] system ...\n");
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
		if (strncmp(argv[0],"-g",2) == SAME) {
			grade = argv[0][2];
			continue;
		}
		if (strcmp(argv[0],"-n") == SAME) {
			nocall++;
			continue;
		}

		if (versys(&argv[0])) {
			fprintf(stderr, "%s: unknown system.\n", argv[0]);
			continue;
		}
		/* Remove any STST file that might stop the poll */
		sprintf(wrkpre, "%s/LCK..%.*s", LOCKDIR, MAXBASENAME, argv[0]);
		if (access(wrkpre, 0) < 0)
			rmstat(argv[0]);
		sprintf(wrkpre, "%c.%.*s", CMDPRE, SYSNSIZE, argv[0]);
		if (!iswrk(file, "chk", Spool, wrkpre)) {
			sprintf(file, "%s/%c.%.*s%cPOLL", subdir(Spool, CMDPRE),
				CMDPRE, SYSNSIZE, argv[0], grade);
			close(creat(file, 0666));
		}
		/* Attempt the call */
		if (!nocall)
			xuucico(argv[0]);
	}
	cleanup(0);
}

cleanup(code)
int code;
{
	exit(code);
}
