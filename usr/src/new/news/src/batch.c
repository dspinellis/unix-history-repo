
/*
 * This software is Copyright (c) 1985 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * Batch: program to batch a list of news articles into an unbatch script.
 * Usage: /usr/lib/news/batch listfile [bytecount]
 *  where listfile is a file containing a list, one per line, of full
 *  path names of files containing articles, e.g. as produced by the F
 *  transmission option in the sys file.
 *  bytecount is the maximum number of bytes to output before exiting
 * Output is placed on standard output.
 *
 * Intended usage:
 *
 *	With the shellfile "sendbatch", with machine names as arguments:
 * 		e.g
 *		sendbatch rlgvax seismo
 *
 * This would be invoked every hour or two from crontab.
 *
 */

#ifdef SCCSID
static char	*SccsId = "@(#)batch.c	1.19	10/7/87";
#endif /* SCCSID */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "defs.h"

#if defined(USG) || defined(BSD4_2)
#include <fcntl.h>
#endif

struct stat sbuf;

extern int errno;
extern char *sys_errlist[];

main(argc,argv)
char **argv;
{
	register FILE *fd, *nfd;
	register int c;
	register long n;
	register char *cp;
	char *fdstatus;
	long maxbytes, nbytes;
	long atol();
	char fname[512];
	char workfile[512];
	char cbuf[BUFSIZ];
	char *index(), *fgets();

	if (argc < 2) {
		fprintf(stderr, "Usage: batch listfile [bytecount]\n");
		exit(1);
	}

	/*
	 * Rename real file to a work name to avoid race conditions.
	 * If workfile exists skip the rename in order
	 * to recover from a crash w/o losing anything.
	 */
	(void) strcpy(workfile, argv[1]);
	(void) strcat(workfile, ".work");
	if (access(workfile, 0) < 0) {
		if (access(argv[1], 0) < 0 && errno == ENOENT)
			exit(0);	/* no news */
		if (rename(argv[1], workfile) < 0) {
			logerror("rename(%s,%s) %s", argv[1], workfile,
				sys_errlist[errno]);
			exit(1);
		}
	}
	fd = fopen(workfile, "r");
	if (fd == NULL) {
		logerror("fopen(%s,r) %s", workfile, sys_errlist[errno]);
		exit(1);
	}

	if (argc > 2)
		maxbytes = atol(argv[2]);
	else
		maxbytes = 100000000L; /* backwards compatible */
	nbytes = 0;
	while ((fdstatus = fgets(fname, sizeof fname, fd)) != NULL) {
		cp = index(fname, '\n');
		if (cp)
			*cp = '\0';
		if (fname[0] == '\0')
			continue;
		nfd = fopen(fname, "r");
		if (nfd == NULL) {
			perror(fname);
			continue;
		}
		(void) fstat(fileno(nfd), &sbuf);
		if (cp)
			*cp = '\n';
		if (sbuf.st_size == 0)
			continue;
		nbytes += sbuf.st_size;
		if (nbytes > maxbytes && nbytes != sbuf.st_size)
			break;
		printf("#! rnews %ld\n", (long)sbuf.st_size);
		/* guess length of #! rnews string */
		nbytes += 13;
		n = 0;
		while (c = fread(cbuf, 1, sizeof cbuf, nfd)) {
			fwrite(cbuf, 1, c, stdout);
			n += c;
		}
		(void) fclose(nfd);
		if (ferror(stdout)){
			logerror("stdout write %s", sys_errlist[errno]);
			exit(1);
		}
		(void) fflush(stdout);
		if (n != sbuf.st_size) { /* paranoia */
			logerror("%s, expected %ld bytes, got %ld", fname,
				n, sbuf.st_size);
			/* breaking out of this early will end up resyncing
			   the batch files (isn't serendipity wonderful?) */
			break;
		}
	}
	if (fdstatus != NULL) {		/* exceeded maxbytes */
		char tmpfile[512];

		(void) umask(2);
		(void) strcpy(tmpfile, argv[1]);
		(void) strcat(tmpfile, ".tmp");
	    	nfd = fopen(tmpfile, "w");
		if (nfd == NULL) {
			logerror("fopen(%s,w) %s", tmpfile, sys_errlist[errno]);
			exit(1);
		}
		do {
			fputs(fname, nfd);
		} while (fgets(fname, sizeof fname, fd) != NULL);
		if (ferror(nfd)) {
			logerror("write(%s) %s", tmpfile, sys_errlist[errno]);
			exit(1);
		}
		(void) fclose(nfd);
		(void) fclose(fd);
		/* will pick it up next time thru */
		if (rename(tmpfile, workfile) < 0) {
			logerror("rename(%s,%s) %s", tmpfile, workfile,
				sys_errlist[errno]);
			exit(1);
		}
	}		
	else
		(void) unlink(workfile);
	exit(0);
}

/*
 * Log the given message, with printf strings and parameters allowed,
 * on the log file, if it can be written.
 */
/* VARARGS1 */
logerror(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	FILE *logfile;
	char lfname[BUFLEN];		/* the log file */
	char bfr[BUFLEN];
	char *logtime, *ctime(); 
	time_t t;

	(void) time(&t);
	logtime = ctime(&t);
	logtime[16] = 0;
	logtime += 4;

#if defined(LOGDIR) || defined(HOME)
	(void) sprintf(lfname, "%s/%s/errlog", logdir(HOME), LIBDIR);
#else
	(void) sprintf(lfname, "%s/errlog", LIBDIR);
#endif

	(void) sprintf(bfr, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	fprintf(stderr, bfr);
	if (access(lfname, 0) == 0 && (logfile = fopen(lfname, "a")) != NULL) {
#if defined(USG) || defined(BSD4_2)
		int flags;
		flags = fcntl(fileno(logfile), F_GETFL, 0);
		(void) fcntl(fileno(logfile), F_SETFL, flags|O_APPEND);
#else /* v7 */
		(void) lseek(fileno(logfile), 0L, 2);
#endif /* v7 */
		fprintf(logfile, "%s\tbatch\t%s\n", logtime, bfr);
		(void) fclose(logfile);
	}
}

#if !defined(BSD4_2)
rename(from, to)
register char *from, *to;
{
	(void) unlink(to);
	if (link(from, to) < 0)
		return -1;

	(void) unlink(from);
	return 0;
}
#endif /* !BSD4_2 */
