#ifndef lint
static char sccsid[] = "@(#)mailst.c	5.5 (Berkeley) %G%";
#endif

#include <signal.h>
#include "uucp.h"
#ifdef USG
#include <fcntl.h>
#endif USG

/*LINTLIBRARY*/

/*
 *	mailst  -  this routine will fork and execute
 *	a mail command sending string (str) to user (user).
 *	If file is non-null, the file is also sent.
 *	(this is used for mail returned to sender.)
 */

mailst(user, str, file)
char *user, *str, *file;
{
	register FILE *fp, *fi;
	char buf[BUFSIZ];
	register int c;

	sprintf(buf, "%s '%s'", MAIL, user);
	if ((fp = rpopen(buf, "w")) != NULL) {
		fprintf(fp, "From: uucp\nTo: %s\nSubject: %s\n\n", user, str);
		if (file && *file != '\0' && (fi = fopen(subfile(file), "r")) != NULL) {
			while ((c = getc(fi)) != EOF)
				putc(c, fp);
			putc('\n', fp);
			fclose(fi);
		}
		rpclose(fp);
	}
}

/*
 * 'reverting' version of popen
 * which runs process with permissions of real gid/uid
 * rather than the effective gid/uid.
 */
#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	popen_pid[20];

FILE *
rpopen(cmd,mode)
char	*cmd;
char	*mode;
{
	int p[2];
	register myside, hisside, pid;

	if(pipe(p) < 0)
		return NULL;
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	if((pid = fork()) == 0) {
		/* myside and hisside reverse roles in child */
		close(myside);
#ifdef USG
		close(tst(0, 1);
		fcntl(hisside, F_DUPFD, tst(0, 1));
#else !USG
		dup2(hisside, tst(0, 1));
#endif !USG
		close(hisside);
		/* revert permissions */
		setgid(getgid());
		setuid(getuid());
		execl("/bin/sh", "sh", "-c", cmd, (char *)0);
		_exit(1);
	}
	if(pid == -1)
		return NULL;
	popen_pid[myside] = pid;
	close(hisside);
	return(fdopen(myside, mode));
}

rpclose(ptr)
FILE *ptr;
{
	register f, r, (*hstat)(), (*istat)(), (*qstat)();
	int status;

	f = fileno(ptr);
	fclose(ptr);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while((r = wait(&status)) != popen_pid[f] && r != -1)
		;
	if(r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
	return status;
}
