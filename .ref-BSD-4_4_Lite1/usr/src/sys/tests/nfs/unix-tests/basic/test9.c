/*	@(#)test9.c	1.4 90/01/03 NFS Rev 2 Testsuite
 *	1.4 Lachman ONC Test Suite source
 *
 * Test statfs
 *
 * Uses the following important system calls against the server:
 *
 *	chdir()
 *	mkdir()		(for initial directory creation if not -m)
 *	statfs()
 */

#include <sys/param.h>
#ifdef SVR3
#include <sys/types.h>
#include <sys/fs/nfs/time.h>
#include <sys/statfs.h>
#else
#include <sys/vfs.h>
#include <sys/time.h>
#endif
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
	int count = 1500;	/* times to do statfs call */
	int ct;
	struct timeval time;
	struct statfs sfsb;
	char *opts;

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

	if (!Nflag)
		testdir(NULL);
	else
		mtestdir(NULL);

	fprintf(stdout, "%s: statfs\n", Myname);

	if (Tflag) {
		starttime();
	}

	for (ct = 0; ct < count; ct++) {
#ifdef SVR3
		if (statfs(".", &sfsb, sizeof(sfsb), 0) < 0) {
#else
		if (statfs(".", &sfsb) < 0) {
#endif
			error("can't do statfs on \".\"");
			exit(1);
		}
	}

	if (Tflag) {
		endtime(&time);
	}
#ifdef SVR3
#ifdef DEBUG
	fprintf(stdout, "\ttype=%d, bsize=%d, blocks=%d, bfree=%d\n\
\t  bavail=%d, files=%d, ffree=%d, vol=%s, pack=%s\n",
		sfsb.f_fstyp, sfsb.f_bsize, sfsb.f_blocks, sfsb.f_bfree,
		sfsb.f_bfree, sfsb.f_files, sfsb.f_ffree, sfsb.f_fname,
		sfsb.f_fpack);
#endif /* DEBUG */
#else /* SVR3 */
#ifdef DEBUG
	fprintf(stdout, "\ttype=%d, bsize=%d, blocks=%d, bfree=%d\n\
\t  bavail=%d, files=%d, ffree=%d, fsid=%d %d\n",
		sfsb.f_type, sfsb.f_bsize, sfsb.f_blocks, sfsb.f_bfree,
		sfsb.f_bavail, sfsb.f_files, sfsb.f_ffree,
#ifdef NFS3_2
		sfsb.f_fsid.val[0], sfsb.f_fsid.val[1]);
#else /* NFS3_2 */
		sfsb.f_fsid[0], sfsb.f_fsid[1]);
#endif /* NFS3_2 */
#endif /* DEBUG */
#endif /* SVR3 */
	fprintf(stdout, "\t%d statfs calls", count);
	if (Tflag) {
		fprintf(stdout, " in %d.%-2d seconds",
		    time.tv_sec, time.tv_usec / 10000);
	}
	fprintf(stdout, "\n");
	complete();
}
