/*
 * recmail: read a mail message on stdin, grab all addresses in To and Cc
 * lines, and pass the full message to all addressees.  This is useful to
 * send the output of a recently edited mail message (with headers edited too).
 * It is similar to sendmail -t, but only assumes /bin/mail.
 */
static char *sccsid = "@(#)recmail.c	1.2	3/30/83";

#include <stdio.h>
#include <ctype.h>

#define MAXRECIPS 100
char *recips[MAXRECIPS];
int nrecips = 0;

main(argc, argv)
char **argv;
{
	FILE *fd;
	char *tmpf;
	char linebuf[1024];
	char *mailer = "mail";	/* /bin/mail */
	int i, pid, wpid;
	int exstat;
	char *mypath;
	char *mktemp(), *getenv();

	tmpf = mktemp("/tmp/rmXXXXXX");
	fd = fopen(tmpf, "w");

	while (fgets(linebuf, sizeof linebuf, stdin) != NULL) {
		fputs(linebuf, fd);
		if (strncmp(linebuf, "To: ", 4) == 0 ||
		    strncmp(linebuf, "to: ", 4) == 0 ||
		    strncmp(linebuf, "TO: ", 4) == 0 ||
		    strncmp(linebuf, "Cc: ", 4) == 0 ||
		    strncmp(linebuf, "cc: ", 4) == 0 ||
		    strncmp(linebuf, "CC: ", 4) == 0)
			addrecips(linebuf+4);
	}
	fclose(fd);

	/*
	 * Force the path to only consider /bin and /usr/bin, since
	 * that's the version of mail we want (not /usr/ucb/mail)
	 */
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
		while ((pid = fork()) == -1) {
			fprintf(stderr, "fork failed, waiting...\r\n");
			sleep(60);
		}
		if (pid == 0) {
			close(0);
			open(tmpf, 0);
			execlp("mail", "mail", recips[i], 0);
			perror("mail");
			exit(1);
		}
		while ((wpid = wait(&exstat)) >= 0 && wpid != pid)
			;
	}
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
		for (back=front; isok(*back); back++)
			;
		*back=0;
		recips[nrecips] = malloc(strlen(front) + 1);
		strcpy(recips[nrecips], front);
		nrecips++;
		front = back+1;
	}
}
