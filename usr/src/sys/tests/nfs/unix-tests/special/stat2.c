/*	@(#)stat2.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 * create a bunch of files and stat them repeatedly
 */
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#ifdef SVR3
#include	<sys/fs/nfs/time.h>
#else
#include	<sys/time.h>
#endif

int stats = 0;

main(argc, argv)
	int argc;
	char *argv[];
{
	struct timeval stim, etim;
	float elapsed;
	int files, count;
	register int i, j;
	char name[256];
	struct stat statb;

	if (argc != 4) {
		fprintf(stderr, "usage: %s dir files count\n", argv[0]);
		exit(1);
	}

	if (mkdir(argv[1], 0777) < 0) {
		perror(argv[1]);
	}
	chdir(argv[1]);
	files = atoi(argv[2]);
	count = atoi(argv[3]);
	for (i=0; i<files; i++) {
		sprintf(name, "%d", i);
		close(creat(name, 0666));
	}

	gettimeofday(&stim, 0);
	for (i=0; i<count; i++) {
		for (j=0; j<files; j++) {
			sprintf(name, "%d", i);
			stat(name, &statb);
			stats++;
		}
	}
	gettimeofday(&etim, 0);
	elapsed = (float) (etim.tv_sec - stim.tv_sec) +
	    (float)(etim.tv_usec - stim.tv_usec) / 1000000.0;
	fprintf(stdout, "%d calls in %f seconds (%f calls/sec)\n",
	    stats, elapsed, (float)stats / elapsed);
	exit(0);
}
