/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* NOTE WELL:
 * This file is "included" into iproc.c -- it is not compiled separately!
 */

#include <signal.h>
#include <sgtty.h>
#include "wait.h"

#define DEAD	1	/* Dead but haven't informed user yet */
#define STOPPED	2	/* Job stopped */
#define RUNNING	3	/* Just running */
#define NEW	4	/* This process is brand new */

/* If process is dead, flags says how. */
#define EXITED	1
#define KILLED	2

#define isdead(p)	((p) == NULL || proc_state((p)) == DEAD || (p)->p_toproc == -1)
#define makedead(p)	{ proc_state((p)) = DEAD; }

#define proc_buf(p)	((p)->p_buffer->b_name)
#define proc_cmd(p)	((p)->p_name)
#define proc_state(p)	((p)->p_state)

private Process	*procs = NULL;

File	*ProcInput;
int	ProcOutput,
	kbd_pid = 0,
	NumProcs = 0;

private Process *
proc_pid(pid)
int	pid;
{
	register Process	*p;

	for (p = procs; p != NULL; p = p->p_next)
		if (p->p_portpid == pid)
			break;

	return p;
}

void
read_proc(pid, nbytes)
int	pid;
register int	nbytes;
{
	register Process	*p;
	char	ibuf[512];

	if ((p = proc_pid(pid)) == NULL) {
		writef("\riproc: unknown pid (%d)", pid);
		return;
	}

	if (proc_state(p) == NEW) {
		int	rpid;
		/* pid of real child, not of portsrv */

		(void) f_readn(ProcInput, (char *) &rpid, sizeof (int));
		p->p_pid = rpid;
		p->p_state = RUNNING;
		return;
	}

	if (nbytes == EOF) {		/* okay to clean up this process */
		union wait	status;
		int	pid;

		(void) f_readn(ProcInput, &status.w_status, sizeof (int));
		do {
			pid = wait((int *)NULL);
			if (pid < 0)
				break;
			kill_off(pid, status);
		} while (pid != p->p_portpid);
		proc_close(p);
		makedead(p);
		return;
	}

	while (nbytes > 0) {
		size_t n = f_readn(ProcInput, ibuf, min((sizeof ibuf) - 1, nbytes));

		ibuf[n] = '\0';	/* Null terminate for convenience */
		nbytes -= n;
		proc_rec(p, ibuf);
	}
}

void
ProcKill()
{
	proc_kill(curbuf->b_process, SIGKILL);
}

void
ProcInt()
{
	proc_kill(curbuf->b_process, SIGINT);
}

void
ProcQuit()
{
	proc_kill(curbuf->b_process, SIGQUIT);
}

private void
proc_close(p)
Process	*p;
{
	if (p->p_toproc >= 0) {
		(void) close(p->p_toproc);
		p->p_toproc = -1;	/* writes will fail */
		NumProcs -= 1;
	}
}

void
proc_write(p, buf, nbytes)
Process	*p;
char	*buf;
size_t	nbytes;
{
	(void) write(p->p_toproc, buf, nbytes);
}


#ifdef	STDARGS
private void
proc_strt(char *bufname, int clobber, ...)
#else
private /*VARARGS3*/ void
proc_strt(bufname, clobber, va_alist)
	char	*bufname;
	int	clobber;
	va_dcl
#endif
{
	Window	*owind = curwind;
	int	toproc[2],
		pid;
	Process	*newp;
	Buffer	*newbuf;
	char	*argv[32],
		*cp,
		foo[10],
		cmdbuf[LBSIZE];
	int	i;
	va_list	ap;

	isprocbuf(bufname);	/* make sure BUFNAME is either nonexistant
				   or is of type B_PROCESS */
	if (access(Portsrv, X_OK) < 0) {
		complain("[Couldn't access %s: %s]", Portsrv, strerror(errno));
		/* NOTREACHED */
	}
	dopipe(toproc);

	switch (pid = fork()) {
	case -1:
		pipeclose(toproc);
		complain("[Fork failed: %s]", strerror(errno));

	case 0:
		argv[0] = "portsrv";
		va_init(ap, clobber);
		make_argv(&argv[1], ap);
		va_end(ap);
		(void) dup2(toproc[0], 0);
		(void) dup2(ProcOutput, 1);
		(void) dup2(ProcOutput, 2);
		pipeclose(toproc);
		jcloseall();
		execv(Portsrv, argv);
		raw_complain("execl failed: %s\n", strerror(errno));
		_exit(1);
	}

	newp = (Process *) malloc(sizeof *newp);
	/* ??? better check for newp == NULL -- DHR */
	newp->p_next = procs;
	newp->p_state = NEW;

	cmdbuf[0] = '\0';
	va_init(ap, clobber);
	while (cp = va_arg(ap, char *)) {
		size_t	pl = strlen(cmdbuf);

		swritef(&cmdbuf[pl], sizeof(cmdbuf)-pl, "%s ", cp);
	}
	va_end(ap);
	va_init(ap, clobber);
	newp->p_name = copystr(cmdbuf);
	procs = newp;
	newp->p_portpid = pid;
	newp->p_pid = -1;

	newbuf = do_select((Window *)NULL, bufname);
	newbuf->b_type = B_PROCESS;
	newp->p_buffer = newbuf;
	newbuf->b_process = newp;	/* sorta circular, eh? */
	pop_wind(bufname, clobber, B_PROCESS);
	ToLast();
	if (!bolp())
		LineInsert(1);
	/* Pop_wind() after everything is set up; important!
	   Bindings won't work right unless newbuf->b_process is already
	   set up BEFORE NEWBUF is first SetBuf()'d. */
	newp->p_mark = MakeMark(curline, curchar, M_FLOATER);
	newp->p_dbx_mode = NO;

	newp->p_toproc = toproc[1];
	newp->p_reason = 0;
	NumProcs += 1;
	if (NumProcs == 1)
		(void) kbd_strt();
	(void) close(toproc[0]);
	SetWind(owind);
}

void
pinit()
{
	int	p[2];

	(void) pipe(p);
	ProcInput = fd_open("process-input", F_READ|F_LOCKED, p[0],
			    (char *)NULL, 512);
	ProcOutput = p[1];
	if ((kbd_pid = fork()) == -1) {
		raw_complain("Cannot fork kbd process! %s\n", strerror(errno));
		finish(SIGHUP);
	}
	if (kbd_pid == 0) {
		signal(SIGINT, SIG_IGN);
		signal(SIGALRM, SIG_IGN);
		close(1);
		dup(ProcOutput);
		jcloseall();
		execl(Kbd_Proc, "kbd", (char *)NULL);
		raw_complain("kbd exec failed: %s\n", strerror(errno));
		exit(-1);
	}
}

private int	kbd_state = OFF;

/* kbd_strt() and kbd_stop() return true if they changed the state
   of the keyboard process.  E.g., kbd_strt() returns TRUE if the
   kbd process was previously stopped.  This is so kbd starting and
   stopping in pairs works - see finish() in jove.c. */

kbd_strt()
{
	if (kbd_state == OFF) {
		kbd_state = ON;
		kill(kbd_pid, KBDSIG);
		return TRUE;
	}
	return FALSE;
}

kbd_stop()
{
	if (kbd_state == ON) {
		kbd_state = OFF;
		kill(kbd_pid, KBDSIG);
		return TRUE;
	}
	return FALSE;
}

kbd_kill()
{
	if (kbd_pid != 0) {
		kill(kbd_pid, SIGKILL);
		kbd_pid = 0;
	}
}
