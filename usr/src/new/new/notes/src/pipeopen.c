/*
 * pipeopen, pipeclose
 * kludged from popen.c for notes
 *
 * Lou Salkind, NYU
 */

#include "parms.h"
#include "structs.h"
#include <stdio.h>
#include <signal.h>
#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	p_pid;
static	FILE	*fptr;
extern	char	*myshell;
static	int (*hstat)(), (*istat)(), (*qstat)();
#ifdef	SIGTSTP
static	int (*tstat)();
#endif
static	int brokpipe();

FILE *
pipeopen(cmd,mode)
char	*cmd;
char	*mode;
{
	int p[2];
	register myside, hisside;

	if(pipe(p) < 0)
		return NULL;
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	ttystop();
	if((p_pid = fork()) == 0) {
		close(myside);
		dup2(hisside, tst(0, 1));
		close(hisside);
		setuid(globuid);
		umask(msk);
		uncatchem();
		execl(myshell, "sh", "-c", cmd, 0);
		_exit(1);
	}
	if(p_pid == -1) {
		ttystrt();
		return NULL;
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
#ifdef	SIGTSTP
	tstat = signal(SIGTSTP, SIG_DFL);
#endif
	signal(SIGPIPE, brokpipe);
	close(hisside);
	fptr = fdopen(myside, mode);
	return(fptr);
}

pipeclose()
{
	register r;
	int status;

	fclose(fptr);
	while((r = wait(&status)) != p_pid && r != -1)
		;
	if(r == -1)
		status = -1;
	ttystrt();
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
#ifdef	SIGTSTP
	signal(SIGTSTP, tstat);
#endif
	signal(SIGPIPE, SIG_DFL);
	return(status);
}

static
brokpipe()
{
	pipeclose();
	wfchar();
	longjmp(jenv, 1);
}
