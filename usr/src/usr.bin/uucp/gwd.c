#ifndef lint
static char sccsid[] = "@(#)gwd.c	5.1 (Berkeley) 7/2/83";
#endif

#include "uucp.h"

/*******
 *	gwd(wkdir)	get working directory
 *
 *	return codes  0 | FAIL
 */

gwd(wkdir)
register char *wkdir;
{
	register FILE *fp;
	extern FILE *rpopen();
	extern int rpclose();
	register char *c;

	*wkdir = '\0';
	/* PATH added to rpopen.  Suggested by Henry Spencer (utzoo!henry) */
	if ((fp = rpopen("PATH=/bin:/usr/bin;pwd 2>&-", "r")) == NULL)
		return(FAIL);
	if (fgets(wkdir, 100, fp) == NULL) {
		pclose(fp);
		return(FAIL);
	}
	if (*(c = wkdir + strlen(wkdir) - 1) == '\n')
		*c = '\0';
	rpclose(fp);
	return(0);
}

/*
 * rti!trt: gwd uses 'reverting' version of popen
 * which runs process with permissions of real gid/uid
 * rather than the effective gid/uid.
 * Bug noted by we13!rjk  (Randy King).
 */
/* @(#)popen.c	4.1 (Berkeley) 12/21/80 */
#include <signal.h>
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
		dup2(hisside, tst(0, 1));
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
	return(status);
}
