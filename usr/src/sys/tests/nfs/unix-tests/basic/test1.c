/*	@(#)test1.c	1.4 90/01/03 NFS Rev 2 Testsuite
 *	1.4 Lachman ONC Test Suite source
 *
 * Test file and directory creation.
 * Builds a tree on the server.
 *
 * Uses the following important system calls against the server:
 *
 *	chdir()
 *	mkdir()		(if creating directories, level > 1)
 *	creat()
 */

#include <sys/param.h>
#ifndef major
#include <sys/types.h>
#endif
#ifdef SVR3
#include <sys/fs/nfs/time.h>
#else
#include <sys/time.h>
#endif
#include <sys/stat.h>
#include <stdio.h>
#include "tests.h"

int Tflag = 0;		/* print timing */
int Hflag = 0;		/* print help message */
int Sflag = 0;		/* don't print non-error messages */
int Fflag = 0;		/* test function only;  set count to 1, negate -t */
int Nflag = 0;		/* Suppress directory operations */

usage()
{
	fprintf(stdout, "usage: %s [-htfn] [levels files dirs fname dname]\n",
	    Myname);
	/* -s is a hidden option used by test2 */
	fprintf(stdout, "  Flags:  h    Help - print this usage info\n");
	fprintf(stdout, "          t    Print execution time statistics\n");
	fprintf(stdout, "          f    Test function only (negate -t)\n");
	fprintf(stdout, "          n    Suppress test directory create operations\n");
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int files = DFILS;	/* number of files in each dir */
	int totfiles = 0;
	int dirs = DDIRS;	/* directories in each dir */
	int totdirs = 0;
	int levels = DLEVS;	/* levels deep */
	char *fname = FNAME;
	char *dname = DNAME;
	struct timeval time;
	char command[MAXPATHLEN];
	struct stat statb;
	char *opts;

	setbuf(stdout, NULL);
	Myname = *argv++;
	argc--;
	while (argc && **argv == '-') {
		for (opts = &argv[0][1]; *opts; opts++) {
			switch (*opts) {
				case 'h':	/* help */
					usage();
					exit(1);

				case 's':	/* silent */
					Sflag++;
					break;

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
		levels = getparm(*argv, 1, "levels");
		argv++;
		argc--;
	}
	if (argc) {
		files = getparm(*argv, 0, "files");
		argv++;
		argc--;
	}
	if (argc) {
		dirs = getparm(*argv, 0, "dirs");
		if (dirs == 0 && levels != 1) {
			error("Illegal dirs parameter, must be at least 1");
			exit(1);
		}
		argv++;
		argc--;
	}
	if (argc) {
		fname = *argv;
		argc--;
		argv++;
	}
	if (argc) {
		dname = *argv;
		argc--;
		argv++;
	}
	if (argc != 0) {
		error("too many parameters");
		usage();
		exit(1);
	}

	if (Fflag) {
		Tflag = 0;
		levels = 2;
		files = 2;
		dirs = 2;
	}

	if (!Sflag) {
		fprintf(stdout, "%s: File and directory creation test\n",
		    Myname);
	}

	if (!Nflag)
		testdir(NULL);
	else
		mtestdir(NULL);

	if (Tflag && !Sflag) {
		starttime();
	}
	dirtree(levels, files, dirs, fname, dname, &totfiles, &totdirs);
	if (Tflag && !Sflag) {
		endtime(&time);
	}
	if (!Sflag) {
		fprintf(stdout,
		    "\tcreated %d files %d directories %d levels deep",
		    totfiles, totdirs, levels);
	}
	if (Tflag && !Sflag) {
		fprintf(stdout, " in %d.%-2d seconds",
		    time.tv_sec, time.tv_usec / 10000);
	}
	if (!Sflag) {
		fprintf(stdout, "\n");
	}
	complete();
}
