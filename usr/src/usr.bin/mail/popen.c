#include <stdio.h>
#include <signal.h>
#include <errno.h>
#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	popen_pid[20];
static	char	*sccsid = "@(#)popen.c	1.2.1.2 %G%";

# ifndef VMUNIX
# define vfork	fork
# endif VMUNIX

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
	extern int errno;

	f = fileno(ptr);
	fclose(ptr);
# ifdef VMUNIX
	sighold(SIGINT);
	sighold(SIGQUIT);
	sighold(SIGHUP);
# endif VMUNIX
	while((r = wait(&status)) != popen_pid[f] && r != -1 && errno != EINTR)
		;
	if(r == -1)
		status = -1;
# ifdef VMUNIX
	sigrelse(SIGINT);
	sigrelse(SIGQUIT);
	sigrelse(SIGHUP);
# endif VMUNIX
	return(status);
}
