/*
 * Batch: program to batch a list of news articles into an unbatch script.
 * Usage: /usr/lib/news/batch listfile
 *  where listfile is a file containing a list, one per line, of full
 *  path names of files containing articles, e.g. as produced by the F
 *  transmission option in the sys file.
 * Output is placed on standard output.
 *
 * Intended usage:
 *  uux -c -z -r rmt!rnews '<' =/usr/lib/news/batchnews_/usr/spool/outnews/rmt
 * where the -c option and the = notation require local mods to uucp.
 * This would be invoked every hour or two from crontab.
 *
 * Other possible usage:
 *  /usr/lib/news/batchnews /usr/spool/outnews/rmt | uux - -z -r rmt!rnews
 * Also invoked from crontab every hour or two.  This requires no changes
 * to your uucp, but eats up disk space storing copies of the articles
 * in the spool directory.  The method you choose is transparent to the
 * other end, but the other end must be expecting this batching format.
 */
static char *sccsid = "@(#)batch.c	1.3	4/3/83";
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
struct stat sbuf;

main(argc,argv)
char **argv;
{
	register FILE *fd, *nfd;
	register int c;
	register char *cp;
	char fname[512];
	char *index();

	if (argc != 2) {
		fprintf(stderr, "Usage: batchnews listfile\n");
		exit(1);
	}

	if (strncmp(BATCHDIR, argv[1], strlen(BATCHDIR))) {
		fprintf(stderr, "Permission denied - BATCHDIR mismatch\n");
		fprintf(stderr, "BATCHDIR %s, arg %s\n", BATCHDIR, argv[1]);
		exit(2);
	}

	fd = fopen(argv[1], "r");
	if (fd == NULL) {
		/*
		 * This is not necessarily an error condition, if the
		 * file doesn't exist perhaps there's just no news.
		 */
		perror(argv[1]);
		exit(2);
	}

	while (fgets(fname, sizeof fname, fd) != NULL) {
		cp = index(fname, '\n');
		if (cp)
			*cp = '\0';
		nfd = fopen(fname, "r");
		if (nfd == NULL) {
			printf(": cannot open %s\n", fname);
			continue;
		}
		fstat(fileno(nfd), &sbuf);
		printf("#! rnews %ld\n", sbuf.st_size);
		while ((c = getc(nfd)) != EOF)
			putchar(c);
		fclose(nfd);
	}
	fclose(fd);

	/*
	 * We have reached EOF.  We assume that even if more news
	 * came in while we were generating this, we got it.  So
	 * it's safe to truncate the file.
	 */
	close(creat(argv[1], 0666));
	exit(0);
}
