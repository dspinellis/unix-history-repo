/*	@(#)test3.c	1.4 90/01/03 NFS Rev 2 Testsuite
 *	1.5 Lachman ONC Test Suite source
 *
 * Test lookup up and down across mount points
 *
 * Uses the following important system calls against the server:
 *
 *	chdir()
 *	getwd()
 *	stat()
 */

#include <sys/param.h>
#ifdef SVR3
#include <sys/types.h>
#include <sys/fs/nfs/time.h>
#else
#include <sys/vfs.h>
#include <sys/time.h>
#endif
#include <sys/stat.h>
#include <sys/errno.h>
#include <stdio.h>
#include "tests.h"

int Tflag = 0;		/* print timing */
int Hflag = 0;		/* print help message */
int Fflag = 0;		/* test function only;  set count to 1, negate -t */
int Nflag = 0;		/* Suppress directory operations */

usage()
{
	fprintf(stdout, "usage: %s [-htfn] [count]\n", Myname);
	fprintf(stdout, "  Flags:  h    Help - print this usage info\n");
	fprintf(stdout, "          t    Print execution time statistics\n");
	fprintf(stdout, "          f    Test function only (negate -t)\n");
	fprintf(stdout, "          n    Suppress test directory create operations\n");
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int count = 250;		/* times to do test */
	int ct;
	struct timeval time;
	struct stat statb;
	char *opts;
	char path[MAXPATHLEN];

	umask(0);
	setbuf(stdout, NULL);
	Myname = *argv++;
	argc--;
	while (argc && **argv == '-') {
		for (opts = &argv[0][1]; *opts; opts++) {
			switch (*opts) {
				case 'h':	/* help */
					usage();
					exit(1);

				case 't':	/* time */
					Tflag++;
					break;
				
				case 'f':	/* funtionality */
					Fflag++;
					break;
				
				case 'n':	/* No Test Directory create */
					Nflag++;
					break;

				default:
					error("unknown option '%c'", *opts);
					usage();
					exit(1);
			}
		}
		argc--;
		argv++;
	}

	if (argc) {
		count = getparm(*argv, 1, "count");
		argv++;
		argc--;
	}
	if (argc) {
		usage();
		exit(1);
	}

	if (Fflag) {
		Tflag = 0;
		count = 1;
	}

	fprintf(stdout, "%s: lookups across mount point\n", Myname);

	if (!Nflag)
		testdir(NULL);
	else
		mtestdir(NULL);

	if (Tflag) {
		starttime();
	}

	for (ct = 0; ct < count; ct++) {
#ifdef SVR3
		if (getcwd(path, sizeof(path)) == NULL) {
			fprintf(stderr, "%s: getcwd failed\n", Myname);
			exit(1);
		}
#else
		if (getwd(path) == NULL) {
			fprintf(stderr, "%s: getwd failed\n", Myname);
			exit(1);
		}
#endif
		if (stat(path, &statb) < 0) {
			error("can't stat %s after getwd", path);
			exit(1);
		}
	}

	if (Tflag) {
		endtime(&time);
	}
	fprintf(stdout, "\t%d getwd and stat calls", count * 2);
	if (Tflag) {
		fprintf(stdout, " in %d.%-2d seconds",
		    time.tv_sec, time.tv_usec / 10000);
	}
	fprintf(stdout, "\n");
	complete();
}
