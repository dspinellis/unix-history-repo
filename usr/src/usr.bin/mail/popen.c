#include <stdio.h>
#include <signal.h>
#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	popen_pid[20];
static	char	*sccsid = "@(#)popen.c	1.2.1.1 %G%";

FILE *
popen(cmd,mode)
char	*cmd;
char	*mode;
{
	int p[2];
	register myside, hisside, pid;

	if(pipe(p) < 0)
		return NULL;
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	if((pid = vfork()) == 0) {
		/* myside and hisside reverse roles in child */
		close(myside);
		dup2(hisside, tst(0, 1));
		close(hisside);
		execl("/bin/csh", "sh", "-c", cmd, 0);
		_exit(1);
	}
	if(pid == -1)
		return NULL;
	popen_pid[myside] = pid;
	close(hisside);
	return(fdopen(myside, mode));
}

pclose(ptr)
FILE *ptr;
{
	register f, r;
	int status;

	f = fileno(ptr);
	fclose(ptr);
	sighold(SIGINT);
	sighold(SIGQUIT);
	sighold(SIGHUP);
	while((r = wait(&status)) != popen_pid[f] && r != -1)
		;
	if(r == -1)
		status = -1;
	sigrelse(SIGINT);
	sigrelse(SIGQUIT);
	sigrelse(SIGHUP);
	return(status);
}
