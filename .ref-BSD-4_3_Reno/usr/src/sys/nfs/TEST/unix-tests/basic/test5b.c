/*	@(#)test5b.c	1.3 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 * Test read - will read a file of specified size, contents not looked at
 *
 * Uses the following important system calls against the server:
 *
 *	chdir()
 *	mkdir()		(for initial directory creation if not -m)
 *	open()
 *	read()
 *	unlink()
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

#ifndef MIN
#define MIN(a, b)	((a) < (b) ? (a) : (b))
#endif

#define	BUFSZ	8192
#define	DSIZE	1048576

int Tflag = 0;		/* print timing */
int Hflag = 0;		/* print help message */
int Fflag = 0;		/* test function only;  set count to 1, negate -t */
int Nflag = 0;		/* Suppress directory operations */

usage()
{
	fprintf(stdout, "usage: %s [-htfn] [size count fname]\n", Myname);
	fprintf(stdout, "  Flags:  h    Help - print this usage info\n");
	fprintf(stdout, "          t    Print execution time statistics\n");
	fprintf(stdout, "          f    Test function only (negate -t)\n");
	fprintf(stdout, "          n    Suppress test directory create operations\n");
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int count = DCOUNT;	/* times to do each file */
	int ct;
	int size = DSIZE;
	int si;
	int i;
	int fd;
	int bytes;
	char *bigfile = "bigfile";
	struct timeval time;
	char str[MAXPATHLEN];
	struct stat statb;
	char *opts;
	char buf[BUFSZ];

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
                size = getparm(*argv, 1, "size");
		if (size <= 0) {
			usage();
			exit(1);
		}
		argv++;
		argc--;
	}
	if (argc) {
                count = getparm(*argv, 1, "count");
		if (count <= 0) {
			usage();
			exit(1);
		}
		argv++;
		argc--;
	}
	if (argc) {
                bigfile = *argv;
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

	fprintf(stdout, "%s: read\n", Myname);

	mtestdir(NULL);

	if (Tflag) {
		starttime();
	}

	for (ct = 0; ct < count; ct++) {
		if ((fd = open(bigfile, 0)) < 0) {
			error("can't open '%s'", bigfile);
			exit(1);
		}
		for (si = size; si > 0; si -= bytes) {
			bytes = MIN(BUFSZ, si);
			if (read(fd, buf, bytes) != bytes) {
				error("'%s' read failed", bigfile);
				exit(1);
			}
		}
		close(fd);
	}

	if (Tflag) {
		endtime(&time);
	}
	fprintf(stdout, "\tread %d byte file %d times", size, count);
	if (Tflag) {
		fprintf(stdout, " in %d.%-2d seconds (%d bytes/sec)",
		    time.tv_sec, time.tv_usec / 10000, size*count/time.tv_sec);
	}
	fprintf(stdout, "\n");

	if (unlink(bigfile) < 0) {
		error("can't unlink '%s'", bigfile);
		exit(1);
	}
	complete();
}
