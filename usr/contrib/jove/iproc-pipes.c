/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#ifdef BSD4_2
#   include <sys/wait.h>
#else
#   include <wait.h>
#endif
#include <signal.h>
#include <sgtty.h>

typedef struct process	Process;

#define DEAD	1	/* Dead but haven't informed user yet */
#define STOPPED	2	/* Job stopped */
#define RUNNING	3	/* Just running */
#define NEW	4	/* This process is brand new */

/* If process is dead, flags says how. */
#define EXITED	1
#define KILLED	2

#define isdead(p)	(p == 0 || proc_state(p) == DEAD || p->p_toproc == -1)

#define proc_buf(p)	(p->p_buffer->b_name)
#define proc_cmd(p)	(p->p_name)
#define proc_state(p)	(p->p_state)

struct process {
	Process	*p_next;
	int	p_toproc,	/* read p_fromproc and write p_toproc */
		p_portpid,	/* Pid of child (the portsrv) */
		p_pid;		/* Pid of real child i.e. not portsrv */
	Buffer	*p_buffer;	/* Add output to end of this buffer */
	char	*p_name;	/* ... */
	char	p_state,	/* State */
		p_howdied,	/* Killed? or Exited? */
		p_reason,	/* If signaled, p_reason is the signal; else
				   it is the the exit code */
		p_eof;		/* Received EOF, so can be free'd up */
	Mark	*p_mark;	/* Where output left us. */
	data_obj
		*p_cmd;		/* Command to call when process dies */
} *procs = 0,
  *cur_proc = 0;

char	proc_prompt[80] = "% ";

int	ProcInput,
	ProcOutput,
	NumProcs = 0;

static char *
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
			return sprint("[Exit %d]", p->p_reason);
		}
		return sprint("[Killed %d]", p->p_reason);

	default:
		return "Unknown state.";
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
	static int	here = 0;

	if (here)	
		return;
	sighold(SIGCHLD);	/* Block any other children. */
	here++;
	for (;;) {
		(void) ioctl(ProcInput, FIONREAD, (struct sgttyb *) &nbytes);
		if (nbytes < sizeof header)
			break;
		n = read(ProcInput, (char *) &header, sizeof header);
		if (n != sizeof header)
			finish(1);
		read_proc(header.pid, header.nbytes);
	}
	redisplay();
	here = 0;
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
		/* Pid of real child, not of portsrv. */

		doread(ProcInput, (char *) &rpid, nbytes);
		nbytes -= sizeof rpid;
		p->p_pid = rpid;
		p->p_state = RUNNING;
	}

	if (nbytes == EOF) {		/* Okay to clean up this process */
		p->p_eof = 1;
		NumProcs--;	/* As far as getch() in main is concerned */
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
	proc_kill(cur_proc, SIGKILL);
}

ProcInt()
{
	proc_kill(cur_proc, SIGINT);
}

ProcQuit()
{
	proc_kill(cur_proc, SIGQUIT);
}

static
proc_close(p)
Process	*p;
{
	(void) close(p->p_toproc);
	p->p_toproc = -1;	/* Writes will fail. */
}

do_rtp(mp)
register Mark	*mp;
{
	register Process	*p = cur_proc;
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

/* VARARGS2 */

static
proc_strt(bufname, procname, cmd)
char	*bufname,
	*procname,
	*cmd;
{
	Window	*owind = curwind;
	int	toproc[2],
		pid;
	Process	*newp;
	Buffer	*bp;
    	char	*args[25],
    		**cp,
    		foo[10],
		cmdbuf[128];
    	int	i;

	bp = buf_exists(bufname);
	if (bp != 0 && IsModified(bp) && bp->b_type != B_IPROCESS && bp->b_type != B_PROCESS)
		complain("Command would over-write buffer %s.", procname, bufname);
	pop_wind(bufname, 1, B_IPROCESS);

	dopipe(toproc);

	switch (pid = fork()) {
	case -1:
		pclose(toproc);
		complain("[Fork failed.]");

	case 0:
	    	args[0] = "portsrv";
	    	args[1] = foo;
		sprintf(foo, "%d", ProcInput);
	    	for (i = 0, cp = &cmd; cp[i] != 0; i++)
	    		args[2 + i] = cp[i];
	    	args[2 + i] = 0;
		(void) dup2(toproc[0], 0);
		(void) dup2(ProcOutput, 1);
		(void) dup2(ProcOutput, 2);
		pclose(toproc);
		execv(PORTSRV, args);
		printf("Execl failed.\n");
		_exit(1);
	}

	sighold(SIGCHLD);
	cur_proc = newp = (Process *) malloc(sizeof *newp);
	newp->p_next = procs;
	newp->p_state = NEW;
	newp->p_mark = MakeMark(curline, curchar, FLOATER);
	newp->p_cmd = 0;

	cp = &cmd + 1;
	cmdbuf[0] = '\0';
	while (*cp != 0)
		sprintf(&cmdbuf[strlen(cmdbuf)], "%s ", *cp++);
	newp->p_name = copystr(cmdbuf);
	procs = newp;
	newp->p_portpid = pid;
	newp->p_pid = -1;
	newp->p_buffer = curbuf;
	newp->p_toproc = toproc[1];
	newp->p_reason = 0;
	newp->p_eof = 0;
	NumProcs++;
	(void) close(toproc[0]);
	sigrelse(SIGCHLD);
	SetWind(owind);
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
