/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cron.c	4.18 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/resource.h>
#include <pwd.h>
#include <fcntl.h>
#include <syslog.h>
#include <stdio.h>
#include <ctype.h>
#include "pathnames.h"

#define	LISTS	(2*BUFSIZ)
#define	MAXLIN	BUFSIZ

#define	EXACT	100
#define	ANY	101
#define	LIST	102
#define	RANGE	103
#define	EOS	104

char	crontab[]	= _PATH_CRON;
char	loc_crontab[]   = _PATH_LCRON;
time_t	itime, time();
struct	tm *loct;
struct	tm *localtime();
char	*malloc();
char	*realloc();
void	reapchild();
int	flag;
char	*list;
char	*listend;
unsigned listsize;

FILE	*debug;
#define dprintf if (debug) fprintf

main(argc, argv)
	int argc;
	char **argv;
{
	register char *cp;
	char *cmp();
	time_t filetime = 0;
	time_t lfiletime = 0;
	char c;
	extern char *optarg;

	if (geteuid()) {
		fprintf(stderr, "cron: NOT super-user\n");
		exit(1);
	}

	openlog("cron", LOG_PID | LOG_CONS | LOG_NOWAIT, LOG_DAEMON);
	switch (fork()) {

	case -1:
		syslog(LOG_ERR, "fork: %m");
		exit(1);
		/* NOTREACHED */
	case 0:
		break;
	default:
		exit(0);
		/* NOTREACHED */
	}

	c = getopt(argc, argv, "d:");
	if (c == 'd') {
		debug = fopen(optarg, "w");
		if (debug == NULL)
			exit(1);
		(void) fcntl(fileno(debug), F_SETFL, FAPPEND);
	}
	daemon(0, 0);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGCHLD, reapchild);
	(void) time(&itime);
	itime -= localtime(&itime)->tm_sec;

	for (;; itime+=60, slp()) {
		struct stat cstat, lcstat;
		int newcron, newloc;
		
		newcron = 0;
		if (stat(crontab, &cstat) < 0)
		    cstat.st_mtime = 1;
		if (cstat.st_mtime != filetime) {
			filetime = cstat.st_mtime;
			newcron++;
		}

		newloc  = 0;
		if (stat(loc_crontab, &lcstat) < 0)
		    lcstat.st_mtime = 1;
		if (lcstat.st_mtime != lfiletime) {
			lfiletime = lcstat.st_mtime;
			newloc++;
		}

		if (newcron || newloc) {
			init();
			append(crontab);
			append(loc_crontab);
			*listend++ = EOS;
			*listend++ = EOS;
		}

		loct = localtime(&itime);
		loct->tm_mon++;		 /* 1-12 for month */
		if (loct->tm_wday == 0)
			loct->tm_wday = 7;	/* sunday is 7, not 0 */
		for(cp = list; *cp != EOS;) {
			flag = 0;
			cp = cmp(cp, loct->tm_min);
			cp = cmp(cp, loct->tm_hour);
			cp = cmp(cp, loct->tm_mday);
			cp = cmp(cp, loct->tm_mon);
			cp = cmp(cp, loct->tm_wday);
			if(flag == 0)
				ex(cp);
			while(*cp++ != 0)
				;
		}
	}
}

char *
cmp(p, v)
char *p;
{
	register char *cp;

	cp = p;
	switch(*cp++) {

	case EXACT:
		if (*cp++ != v)
			flag++;
		return(cp);

	case ANY:
		return(cp);

	case LIST:
		while(*cp != LIST)
			if(*cp++ == v) {
				while(*cp++ != LIST)
					;
				return(cp);
			}
		flag++;
		return(cp+1);

	case RANGE:
		if(*cp > v || cp[1] < v)
			flag++;
		return(cp+2);
	}
	if(cp[-1] != v)
		flag++;
	return(cp);
}

slp()
{
	register i;
	time_t t;

	(void) time(&t);
	i = itime - t;
	if(i < -60 * 60 || i > 60 * 60) {
		itime = t;
		i = 60 - localtime(&itime)->tm_sec;
		itime += i;
	}
	if(i > 0)
		sleep((u_int)i);
}

ex(s)
char *s;
{
	register struct passwd *pwd;
	char user[BUFSIZ];
	char *c = user;
	int pid;

	switch (fork()) {
	case 0:
		break;
	case -1:
		syslog(LOG_ERR, "cannot fork: %m (running %.40s%s)",
			s, strlen(s) > 40 ? "..." : "");
		/*FALLTHROUGH*/
	default:
		return;
	}
	pid = getpid();
	while(*s != ' ' && *s != '\t')
		*c++ = *s++;
	*c = '\0';
	s++;
	if ((pwd = getpwnam(user)) == NULL) {
		syslog(LOG_ERR, "invalid user name \"%s\"", user);
		dprintf(debug, "%d: cannot find %s\n", pid, user),
			fflush(debug);
		exit(1);
	}
	(void) setgid(pwd->pw_gid);
	(void) initgroups(pwd->pw_name, pwd->pw_gid);
	(void) setuid(pwd->pw_uid);
	(void) freopen("/", "r", stdin);
	closelog();
	dprintf(debug, "%d: executing %s", pid, s), fflush (debug);
	execl(_PATH_BSHELL, "sh", "-c", s, 0);
	syslog(LOG_ERR, "cannot exec %s: %m");
	dprintf(debug, "%d: cannot execute sh\n", pid), fflush (debug);
	exit(0);
}

init()
{
	/*
	 * Don't free in case was longer than LISTS.  Trades off
	 * the rare case of crontab shrinking vs. the common case of
	 * extra realloc's needed in append() for a large crontab.
	 */
	if (list == 0) {
		list = malloc(LISTS);
		listsize = LISTS;
	}
	listend = list;
}

append(fn)
char *fn;
{
	register i, c;
	register char *cp;
	register char *ocp;
	register int n;

	if (freopen(fn, "r", stdin) == NULL)
		return;
	cp = listend;
loop:
	if(cp > list+listsize-MAXLIN) {
		int length = cp - list;

		listsize += LISTS;
		list = realloc(list, listsize);
		cp = list + length;
	}
	ocp = cp;
	for(i=0;; i++) {
		do
			c = getchar();
		while(c == ' ' || c == '\t')
			;
		if(c == EOF || c == '\n')
			goto ignore;
		if(i == 5)
			break;
		if(c == '*') {
			*cp++ = ANY;
			continue;
		}
		if ((n = number(c)) < 0)
			goto ignore;
		c = getchar();
		if(c == ',')
			goto mlist;
		if(c == '-')
			goto mrange;
		if(c != '\t' && c != ' ')
			goto ignore;
		*cp++ = EXACT;
		*cp++ = n;
		continue;

	mlist:
		*cp++ = LIST;
		*cp++ = n;
		do {
			if ((n = number(getchar())) < 0)
				goto ignore;
			*cp++ = n;
			c = getchar();
		} while (c==',');
		if(c != '\t' && c != ' ')
			goto ignore;
		*cp++ = LIST;
		continue;

	mrange:
		*cp++ = RANGE;
		*cp++ = n;
		if ((n = number(getchar())) < 0)
			goto ignore;
		c = getchar();
		if(c != '\t' && c != ' ')
			goto ignore;
		*cp++ = n;
	}
	while(c != '\n') {
		if(c == EOF)
			goto ignore;
		if(c == '%')
			c = '\n';
		*cp++ = c;
		c = getchar();
	}
	*cp++ = '\n';
	*cp++ = 0;
	goto loop;

ignore:
	cp = ocp;
	while(c != '\n') {
		if(c == EOF) {
			(void) fclose(stdin);
			listend = cp;
			return;
		}
		c = getchar();
	}
	goto loop;
}

number(c)
register c;
{
	register n = 0;

	while (isdigit(c)) {
		n = n*10 + c - '0';
		c = getchar();
	}
	(void) ungetc(c, stdin);
	if (n>=100)
		return(-1);
	return(n);
}

void
reapchild()
{
	union wait status;
	int pid;

	while ((pid = wait3((int *)&status, WNOHANG, (struct rusage *)0)) > 0)
		dprintf(debug, "%d: child exits with signal %d status %d\n",
			pid, status.w_termsig, status.w_retcode),
			fflush (debug);
}
