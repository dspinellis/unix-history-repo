/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#ifdef BSD4_2
#   include <sys/wait.h>
#else
#   include <wait.h>
#endif
#include <signal.h>
#include <sgtty.h>

#define DEAD	1	/* Dead but haven't informed user yet */
#define STOPPED	2	/* Job stopped */
#define RUNNING	3	/* Just running */
#define NEW	4	/* This process is brand new */

/* If process is dead, flags says how. */
#define EXITED	1
#define KILLED	2

#define isdead(p)	(p == 0 || proc_state(p) == DEAD || p->p_toproc == -1)
#define makedead(p)	(proc_state(p) = DEAD)

#define proc_buf(p)	(p->p_buffer->b_name)
#define proc_cmd(p)	(p->p_name)
#define proc_state(p)	(p->p_state)

private Process	*procs = 0;

int	ProcInput,
	ProcOutput,
	NumProcs = 0;

char *
pstate(p)
Process	*p;
{
	switch (proc_state(p)) {
	case NEW:
		return "Pre-birth";

	case STOPPED:
		return "Stopped";

	case RUNNING:
		return "Running";

	case DEAD:
		if (p->p_howdied == EXITED) {
			if (p->p_reason == 0)
				return "Done";
			return sprint("Exit %d", p->p_reason);
		}
		return sprint("Killed %d", p->p_reason);

	default:
		return "Unknown state";
	}
}

static Process *
proc_pid(pid)
{
	register Process	*p;

	for (p = procs; p != 0; p = p->p_next)
		if (p->p_portpid == pid)
			break;

	return p;
}

procs_read()
{
	struct header {
		int	pid;
		int	nbytes;
	} header;
	int	n;
	long	nbytes;
	static int	here = NO;

	if (here)	
		return;
	sighold(SIGCHLD);	/* block any other children */
	here = YES;
	for (;;) {
		(void) ioctl(ProcInput, FIONREAD, (struct sgttyb *) &nbytes);
		if (nbytes < sizeof header)
			break;
		n = read(ProcInput, (char *) &header, sizeof header);
		if (n != sizeof header)
			finish(1);
		read_proc(header.pid, header.nbytes);
	}
	here = NO;
	sigrelse(SIGCHLD);
}

read_proc(pid, nbytes)
int	pid;
register int	nbytes;
{
	register Process	*p;
	int	n;
	char	ibuf[512];

	if ((p = proc_pid(pid)) == 0) {
		printf("\riproc: unknown pid (%d)", pid);
		return;
	}
	if (proc_state(p) == NEW) {
		int	rpid;
		/* pid of real child, not of portsrv */

		doread(ProcInput, (char *) &rpid, nbytes);
		nbytes -= sizeof rpid;
		p->p_pid = rpid;
		p->p_state = RUNNING;
	}

	if (nbytes == EOF) {		/* okay to clean up this process */
		proc_close(p);
		makedead(p);
		return;
	}

	while (nbytes > 0) {
		n = min((sizeof ibuf) - 1, nbytes);
		doread(ProcInput, ibuf, n);
		ibuf[n] = 0;	/* Null terminate for convenience */
		nbytes -= n;
		proc_rec(p, ibuf);
	}
}

ProcKill()
{
	proc_kill(curbuf->b_process, SIGKILL);
}

ProcInt()
{
	proc_kill(curbuf->b_process, SIGINT);
}

ProcQuit()
{
	proc_kill(curbuf->b_process, SIGQUIT);
}

private
proc_close(p)
Process	*p;
{
	sighold(SIGCHLD);

	if (p->p_toproc >= 0) {
		(void) close(p->p_toproc);
		p->p_toproc = -1;	/* writes will fail */
		NumProcs -= 1;
	}

	sigrelse(SIGCHLD);
}

do_rtp(mp)
register Mark	*mp;
{
	register Process	*p = curbuf->b_process;
	Line	*line1 = curline,
		*line2 = mp->m_line;
	int	char1 = curchar,
		char2 = mp->m_char;
	char	*gp;

	if (isdead(p) || p->p_buffer != curbuf)
		return;

	(void) fixorder(&line1, &char1, &line2, &char2);
	while (line1 != line2->l_next) {
		gp = ltobuf(line1, genbuf) + char1;
		if (line1 == line2)
			gp[char2] = '\0';
		else
			strcat(gp, "\n");
		(void) write(p->p_toproc, gp, strlen(gp));
		line1 = line1->l_next;
		char1 = 0;
	}
}

/* VARARGS3 */

private
proc_strt(bufname, clobber, va_alist)
char	*bufname;
va_dcl
{
	Window	*owind = curwind;
	int	toproc[2],
		pid;
	Process	*newp;
	Buffer	*newbuf;
    	char	*argv[32],
    		*cp,
    		foo[10],
		cmdbuf[128];
    	int	i;
	va_list	ap;

	isprocbuf(bufname);	/* make sure BUFNAME is either nonexistant
				   or is of type B_PROCESS */
	dopipe(toproc);

	sighold(SIGCHLD);
#ifdef SIGWINCH
	sighold(SIGWINCH);
#endif
	switch (pid = fork()) {
	case -1:
		pclose(toproc);
		complain("[Fork failed.]");

	case 0:
		sigrelse(SIGCHLD);
#ifdef SIGWINCH
		sigrelse(SIGWINCH);
#endif
	    	argv[0] = "portsrv";
	    	argv[1] = foo;
		sprintf(foo, "%d", ProcInput);
		va_start(ap);
		make_argv(&argv[2], ap);
		va_end(ap);
		(void) dup2(toproc[0], 0);
		(void) dup2(ProcOutput, 1);
		(void) dup2(ProcOutput, 2);
		pclose(toproc);
		execv(Portsrv, argv);
		printf("execl failed\n");
		_exit(1);
	}

	newp = (Process *) malloc(sizeof *newp);
	newp->p_next = procs;
	newp->p_state = NEW;
	newp->p_cmd = 0;

	cmdbuf[0] = '\0';
	va_start(ap);
	while (cp = va_arg(ap, char *))
		sprintf(&cmdbuf[strlen(cmdbuf)], "%s ", cp);
	va_end(ap);
	newp->p_name = copystr(cmdbuf);
	procs = newp;
	newp->p_portpid = pid;
	newp->p_pid = -1;

	newbuf = do_select((Window *) 0, bufname);
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

	newp->p_toproc = toproc[1];
	newp->p_reason = 0;
	NumProcs += 1;
	(void) close(toproc[0]);
	SetWind(owind);
	sigrelse(SIGCHLD);
#ifdef SIGWINCH
	sigrelse(SIGWINCH);
#endif
}

pinit()
{
	int	p[2];

	(void) signal(SIGCHLD, proc_child);
	(void) pipe(p);
	ProcInput = p[0];
	ProcOutput = p[1];
	(void) signal(INPUT_SIG, procs_read);
	sighold(INPUT_SIG);	/* Released during terminal read */
}

doread(fd, buf, n)
char	*buf;
{
	int	nread;

	if ((nread = read(fd, buf, n)) != n)
		complain("Cannot read %d (got %d) bytes.", n, nread);
}
