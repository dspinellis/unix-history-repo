#ifndef lint
static char sccsid[] = "@(#)getwd.c	5.3 (Berkeley) %G%";
#endif

#include "uucp.h"

/*
 *	get working directory
 *
 *	return codes  SUCCESS | FAIL
 */

gwd(wkdir)
register char *wkdir;
{
	register FILE *fp;
	extern FILE *rpopen();
	extern int rpclose();
	register char *c;

#ifdef BSD4_2
	if (getwd(wkdir) == 0)
		return FAIL;
#else !BSD4_2
# ifdef VMS
	getwd(wkdir);	/* Call Eunice C library version instead */
#else !VMS
	*wkdir = '\0';
	if ((fp = rpopen("PATH=/bin:/usr/bin:/usr/ucb;pwd 2>&-", "r")) == NULL)
		return FAIL;
	if (fgets(wkdir, 100, fp) == NULL) {
		rpclose(fp);
		return FAIL;
	}
	if (*(c = wkdir + strlen(wkdir) - 1) == '\n')
		*c = '\0';
	rpclose(fp);
# endif !VMS
#endif !BSD4_2
	return SUCCESS;
}

/*
 * gwd uses 'reverting' version of popen
 * which runs process with permissions of real gid/uid
 * rather than the effective gid/uid.
 */
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
	return status;
}
