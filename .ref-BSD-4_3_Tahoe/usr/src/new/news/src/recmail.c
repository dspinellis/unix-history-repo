/*
 * This software is Copyright (c) 1986 by Rick Adams.
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
 * recmail: read a mail message on stdin, grab all addresses in To and Cc
 * lines, and pass the full message to all addressees.  This is useful to
 * send the output of a recently edited mail message (with headers edited too).
 * It is similar to sendmail -t, but only assumes /bin/mail.
 * To use your own mailer, e. g. nmail, compile with -DMAILER=my_mailer.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)recmail.c	1.16	9/24/87";
#endif /* SCCSID */

#include "params.h"

#ifndef MAILER
#define MAILER "/bin/mail"
#endif
char mailer[] = MAILER;

#define MAXRECIPS 100
char *recips[MAXRECIPS];
int nrecips = 0;

main()
{
	FILE *fd;
	char *tmpf;
	FILE *errfd;
	char *errf;
	char linebuf[1024];
	int i, pid, wpid;
	int exstat;
	char *mypath;
	int goodcnt, badcnt;
	char *mktemp(), *getenv();

	tmpf = mktemp("/tmp/rmXXXXXX");
	(void) close(creat(tmpf,0666));
	fd = fopen(tmpf, "w");
	errf = mktemp("/tmp/rmXXXXXX");
	(void) close(creat(errf,0666));
	errfd = fopen(errf, "w");
	fprintf(errfd, "Subject: Returned mail\n");
	fprintf(errfd, "\n  ----- Transcript of session follows -----\n");
	(void) fflush(errfd);
	goodcnt = badcnt = 0;

	while (fgets(linebuf, sizeof linebuf, stdin) != NULL) {
	   	 if ((strncmp(linebuf, "Bcc: ", 5) == 0 ||
		    strncmp(linebuf, "bcc: ", 5) == 0 ||
		    strncmp(linebuf, "BCC: ", 5) == 0)) {
		    if (linebuf[5] != '\n')
			addrecips(linebuf+5);
		    }
		else if (fputs(linebuf, fd) == EOF)
			goto werror;
		if (linebuf[0] == '\n')
			break;
		if ((strncmp(linebuf, "To: ", 4) == 0 ||
		    strncmp(linebuf, "to: ", 4) == 0 ||
		    strncmp(linebuf, "TO: ", 4) == 0 ||
		    strncmp(linebuf, "Cc: ", 4) == 0 ||
		    strncmp(linebuf, "cc: ", 4) == 0 ||
		    strncmp(linebuf, "CC: ", 4) == 0) &&
		     linebuf[4] != '\n')
			addrecips(linebuf+4);
	}
	if (!feof(stdin)) {
		while (fgets(linebuf, sizeof linebuf, stdin) != NULL) {
			if (fputs(linebuf, fd) == EOF) {
werror:
				printf("write error on temp file\n");
				exit(2);
			}
		}
	}
	/*
	 * Append the contents of the .signature file (if it exists) to
	 * the end of the mail message
	 */
	{
		char sigbuf[BUFSIZ];
		register c;
		register char *p = getenv("HOME");
		FILE *infp;
			
		if (p) {
			(void) sprintf(sigbuf, "%s/%s", p, ".signature");
			if (infp = fopen(sigbuf, "r")) {
				fprintf(fd,"---\n");
				while ((c = getc(infp)) != EOF)
					putc(c,fd);
				(void) fclose(infp);
			}
		}
	}
	(void) fclose(fd);

	/*
	 * Force the path to only consider /bin and /usr/bin, since
	 * that's the version of mail we want (not /usr/ucb/mail)
	 */
	if (mailer[0] != '/') {
		register int e;
		extern char **environ;
		for (e = 0; environ[e] != NULL; ++e)
			if (strncmp(environ[e], "PATH=", 5) == 0) {
				environ[e] = "PATH=/bin:/usr/bin";
				break;
			}
	}
	mypath = getenv("PATH");
	if (mypath)
		strcpy(mypath, "/bin:/usr/bin");

	/*
	 * We send the copies out separately, because of a bug in
	 * USG's /bin/mail which will generate ANOTHER To: line,
	 * even though we already have one, if there are at least
	 * two recipients.
	 */
	for (i=0; i<nrecips; i++) {
		/*
		 * mail recips[i] < tmpf
		 */
		pid = mailto(tmpf, errfd, recips[i]);
		exstat = -1;
		while ((wpid = wait(&exstat)) >= 0 && wpid != pid)
			;
		if (exstat == 0)
			goodcnt++;
		else
			badcnt++;
	}
	if (badcnt) {
		mailback(errfd, tmpf, errf);
		(void) unlink(tmpf);
		(void) unlink(errf);
		exit(1);
	} else if (goodcnt == 0) {
		fprintf(errfd, "recmail: no 'To:' line\n");
		mailback(errfd, tmpf, errf);
		(void) unlink(tmpf);
		(void) unlink(errf);
		exit (1);
	}
	(void) unlink(tmpf);
	(void) unlink(errf);
	exit (0);
}

#define isok(c) (isprint(c) && (c) != ' ' && c != ',')
addrecips(line)
char *line;
{
	char *front, *back, *tail;
	char *malloc();

	tail = line + strlen(line);
	for (front=line; front < tail; ) {
		while (!isok(*front) && front < tail)
			front++;
		if (front >= tail)
			break;	/* skip end of line garbage */
		for (back=front; isok(*back); back++)
			;
		*back=0;
		if (nrecips >= MAXRECIPS) {
			printf("Too many destinations\n");
			exit(2);
		}
		if ((recips[nrecips] = malloc(strlen(front) + 1)) == NULL) {
			printf("Out of space\n");
			exit(2);
		}
		(void) strcpy(recips[nrecips], front);
		nrecips++;
		front = back+1;
	}
}

int
mailto(tmpf, errfd, recip)
char *tmpf;
FILE *errfd;
char *recip;
{
	register int pid;

	/*
	 * mail recips < tmpf
	 */
	while ((pid = vfork()) == -1) {
		fprintf(stderr, "fork failed, waiting...\r\n");
		sleep(60);
	}
	if (pid == 0) {
		(void) close(0);
		(void) open(tmpf, 0);
		if (errfd != NULL) {
			(void) close(1);
			(void) dup(fileno(errfd));
			(void) fclose(errfd);
			(void) close(2);
			(void) dup(1);
		}
		execlp(mailer, mailer, recip, (char *)0);
		perror(mailer);
		exit(1);
	}
	return pid;
}

mailback(errfd, tmpf, errf)
register FILE *errfd;
char *tmpf;
char *errf;
{
	register FILE *fd;
	register int c;
	int exstat;
	register int pid, wpid;
	char *logn;
	char *getlogin(), *getenv();
	register struct passwd *pwd;

	if ((fd = fopen(tmpf, "r")) != NULL) {
		fprintf(errfd, "\n   ----- Unsent message follows -----\n");
		while ((c = getc(fd)) != EOF)
			putc(c, errfd);
		(void) fclose(fd);
	}
	(void) fclose(errfd);
	if ((logn = getlogin()) == NULL && (logn = getenv("USER")) == NULL) {
		if ((pwd = getpwuid(getuid())) == NULL)
			return;
		logn = pwd->pw_name;
	}
	pid = mailto(errf, (FILE *)NULL, logn);
	while ((wpid = wait(&exstat)) >= 0 && wpid != pid)
		;
}
