/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)wall.c	5.4 (Berkeley) %G%";
#endif not lint

/*
 * wall.c - Broadcast a message to all users.
 *
 * This program is not related to David Wall, whose Stanford Ph.D. thesis
 * is entitled "Mechanisms for Broadcast and Selective Broadcast".
 */

#include <stdio.h>
#include <utmp.h>
#include <errno.h>
#include <signal.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#define IGNOREUSER	"sleeper"

char	hostname[32];
char	mesg[3000];
int	msize,sline;
struct	utmp *utmp;
char	*strcpy();
char	*strcat();
char	*malloc();
char	who[9] = "???";
long	clock, time();
struct tm *localtime();
struct tm *localclock;

extern	errno;

main(argc, argv)
char *argv[];
{
	register int i, c;
	register struct utmp *p;
	int f;
	struct stat statb;

	(void) gethostname(hostname, sizeof (hostname));
	if ((f = open("/etc/utmp", O_RDONLY, 0)) < 0) {
		fprintf(stderr, "Cannot open /etc/utmp\n");
		exit(1);
	}
	clock = time( 0 );
	localclock = localtime( &clock );
	sline = ttyslot();	/* 'utmp' slot no. of sender */
	(void) fstat(f, &statb);
	utmp = (struct utmp *)malloc(statb.st_size);
	c = read(f, (char *)utmp, statb.st_size);
	(void) close(f);
	c /= sizeof(struct utmp);
	if (sline)
		strncpy(who, utmp[sline].ut_name, sizeof(utmp[sline].ut_name));
	(void)sprintf(mesg,
	    "\n\007\007Broadcast Message from %s@%s (%.*s) at %d:%02d ...\r\n\n"
		, who
		, hostname
		, sizeof(utmp[sline].ut_line)
		, utmp[sline].ut_line
		, localclock -> tm_hour
		, localclock -> tm_min
	);
	msize = strlen(mesg);
	if (argc >= 2) {
		/* take message from unix file instead of standard input */
		if (freopen(argv[1], "r", stdin) == NULL) {
			perror(argv[1]);
			exit(1);
		}
	}
	while ((i = getchar()) != EOF) {
		if (i == '\n')
			mesg[msize++] = '\r';
		if (msize >= sizeof mesg) {
			fprintf(stderr, "Message too long\n");
			exit(1);
		}
		mesg[msize++] = i;
	}
	fclose(stdin);
	for (i=0; i<c; i++) {
		p = &utmp[i];
		if (p->ut_name[0] == 0 ||
		    strncmp(p->ut_name, IGNOREUSER, sizeof(p->ut_name)) == 0)
			continue;
		sendmes(p->ut_line);
	}
	exit(0);
}

sendmes(tty)
char *tty;
{
	register f, flags;
	static char t[50] = "/dev/";
	int e, i;

	strcpy(t + 5, tty);

	if ((f = open(t, O_WRONLY|O_NDELAY)) < 0) {
		if (errno != EWOULDBLOCK)
			perror(t);
		return;
	}
	if ((flags = fcntl(f, F_GETFL, 0)) == -1) {
		perror(t);
		return;
	}
	if (fcntl(f, F_SETFL, flags | FNDELAY) == -1)
		goto oldway;
	i = write(f, mesg, msize);
	e = errno;
	(void) fcntl(f, F_SETFL, flags);
	if (i == msize) {
		(void) close(f);
		return;
	}
	if (e != EWOULDBLOCK) {
		errno = e;
		perror(t);
		(void) close(f);
		return;
	}
oldway:
	while ((i = fork()) == -1)
		if (wait((int *)0) == -1) {
			fprintf(stderr, "Try again\n");
			return;
		}
	if (i) {
		(void) close(f);
		return;
	}

	(void) write(f, mesg, msize);
	exit(0);
}
