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

#define isdead(p)	(p == 0 || proc_state(p) == DEAD || p->p_fd == -1)

#define proc_buf(p)	(p->p_buffer->b_name)
#define proc_cmd(p)	(p->p_name)
#define proc_state(p)	(p->p_state)

struct process {
	Process	*p_next;
	int	p_fd,		/* File descriptor of ptyp? opened r/w */
		p_pid;		/* pid of child (the shell) */
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

int	global_fd = 1,
	NumProcs = 0;

#ifdef BRLUNIX
	extern struct sg_brl sg1;
#else
	extern struct sgttyb sg1;
#endif

extern struct tchars tc1;

#ifdef TIOCSLTC
	extern struct ltchars ls1;
#endif

static char *
pstate(p)
Process	*p;
{
	switch (proc_state(p)) {
	case STOPPED:
		return "Stopped";

	case RUNNING:
		return "Running";

	case DEAD:
		if (p->p_howdied == EXITED) {
			if (p->p_reason == 0)
				return "Done";
			return sprint("exit(%d)", p->p_reason);
		}
		return sprint("Killed(%d)", p->p_reason);

	default:
		return "Unknown state.";
	}
}

static Process *
proc_pid(pid)
{
	register Process	*p;

	for (p = procs; p != 0; p = p->p_next)
		if (p->p_pid == pid)
			break;

	return p;
}

read_proc(fd)
register int	fd;
{
	register Process	*p;
	unsigned int	n;
	char	ibuf[1024];

	for (p = procs; p != 0; p = p->p_next)
		if (p->p_fd == fd)
			break;

	if (p == 0) {
		printf("\riproc: unknown fd %d", fd);
		return;
	}

	n = read(fd, ibuf, sizeof(ibuf) - 1);

	if (n == 0) {
		proc_close(p);
		NumProcs--;
		return;
	}

	ibuf[n] = '\0';
	proc_rec(p, ibuf);
	redisplay();
}

ProcKill()
{
	register Buffer	*b;
	register Process	*p;
	Process	*buf_to_proc();
	char	*bname;

	bname = ask_buf(cur_proc ? cur_proc->p_buffer : (Buffer *) 0);

	if ((b = buf_exists(bname)) == 0)
		complain("[No such buffer]");
	if ((p = buf_to_proc(b)) == 0)
		complain("%s not tied to a process.", bname);
	proc_kill(p, SIGKILL);
}

Process *
buf_to_proc(b)
register Buffer *b;
{
	register Process *p;

	for (p = procs; p != 0; p = p->p_next)
		if (p->p_buffer == b)
			return p;
	return 0;
}

ProcCont()
{
	if (cur_proc == 0)
		complain("[No processes]");
	if (cur_proc->p_state != DEAD) {
		proc_kill(cur_proc, SIGCONT);
		cur_proc->p_state = RUNNING;
	}		
}

ProcEof()
{
	send_p(tc1.t_eofc);
}

ProcInt()
{
	send_p(tc1.t_intrc);
}

ProcQuit()
{
	send_p(tc1.t_quitc);
}

#ifdef TIOCSLTC

ProcStop()
{
	send_p(ls1.t_suspc);
}

ProcDStop()
{
	send_p(ls1.t_dsuspc);
}

#endif

send_p(c)
char	c;
{
	if (cur_proc == 0)
		complain("[No processes]");
	if (cur_proc->p_buffer == curbuf)
		ToLast();
	(void) write(cur_proc->p_fd, &c, 1);
}

static
proc_close(p)
Process *p;
{
	(void) close(p->p_fd);
	global_fd &= ~(1 << p->p_fd);
	p->p_eof++;
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
		(void) write(p->p_fd, gp, strlen(gp));
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
	Window *owind	= curwind;
	int	pid;
	Process	*newp;
	Buffer 	*bp;
	char	**cp;
	int	i,
		f,
		ttyfd;
	long 	ldisc,
		lmode;
	register char	*s,
			*t;
	extern int	errno;
	extern char 	**environ;
	static char	ttybuf[11],
			ptybuf[11];
	char	cmdbuf[128];
#ifdef BRLUNIX
	struct sg_brl sg;
#else
	struct sgttyb sg;
#endif

#ifdef TIOCGWINSZ
	struct winsize win;
#else
#  ifdef BTL_BLIT
#  include <sys/jioctl.h>
	struct jwinsize jwin;
#  endif
#endif

	bp = buf_exists(bufname);
	if (bp != 0 && IsModified(bp) && bp->b_type != B_IPROCESS && bp->b_type != B_PROCESS)
		complain("Command would over-write buffer %s.", procname, bufname);
	pop_wind(bufname, 1, B_IPROCESS);

	for (s = "pqrs"; *s; s++) {
		for (t = "0123456789abcdef"; *t; t++) {
			sprintf(ptybuf, "/dev/pty%c%c", *s, *t);
			if ((ttyfd = open(ptybuf, 2)) >= 0)
				goto out;
		}
	}

out:	if (s == 0 && t == 0)
		complain("[Out of ptys!]");

	strcpy(ttybuf, ptybuf);
	ttybuf[5] = 't';

#ifdef TIOCGETD
	(void) ioctl(0, TIOCGETD, (struct sgttyb *) &ldisc);
#endif
#ifdef TIOCLGET
	(void) ioctl(0, TIOCLGET, (struct sgttyb *) &lmode);
#endif
#ifdef TIOCGWINSZ
	(void) ioctl(0, TIOCGWINSZ, (struct sgttyb *) &win);
#else
#  ifdef BTL_BLIT
	(void) ioctl(0, JWINSIZE, (struct sgttyb *) &jwin);
#  endif BTL_BLIT
#endif

	switch (pid = fork()) {
	case -1:
		(void) close(ttyfd);
		complain("[Fork failed!]");

	case 0:
		cp = &cmd;

		for (i = 0; i < 32; i++)
			(void) close(i);

#ifdef TIOCNOTTY
		if ((i = open("/dev/tty", 2)) >= 0) {
			(void) ioctl(i, TIOCNOTTY, (struct sgttyb *) 0);
			(void) close(i);
		}
#endif
		i = open(ttybuf, 2);
		for (f = 0; f <= 2; f++)
			(void) dup2(i, f);

#ifdef TIOCSETD
		(void) ioctl(0, TIOCSETD, (struct sgttyb *) &ldisc);
#endif
#ifdef TIOCLSET
		(void) ioctl(0, TIOCLSET, (struct sgttyb *) &lmode);
#endif
#ifdef TIOCSETC
		(void) ioctl(0, TIOCSETC, (struct sgttyb *) &tc1);
#endif
#ifdef TIOCSLTC
		(void) ioctl(0, TIOCSLTC, (struct sgttyb *) &ls1);
#endif

#ifdef TIOCGWINSZ
#    ifdef SIGWINCH
		(void) signal(SIGWINCH, SIG_IGN);
#    endif
		win.ws_row = curwind->w_height;
		(void) ioctl(0, TIOCSWINSZ, (struct sgttyb *) &win);
#else
#  ifdef BTL_BLIT
		jwin.bytesy = curwind->w_height;
		(void) ioctl(0, JSWINSIZE, (struct sgttyb *) &jwin);
#  endif
#endif

		sg = sg1;
		sg.sg_flags &= ~(ECHO | CRMOD);
		(void) stty(0, &sg);

		i = getpid();
		(void) ioctl(0, TIOCSPGRP, (struct sgttyb *) &i);
		(void) setpgrp(0, i);
		execve(cp[0], &cp[1], environ);
		(void) write(1, "execve failed!\n", 15);
		_exit(errno + 1);
	}

	sighold(SIGCHLD);
#ifdef SIGWINCH
	sighold(SIGWINCH);
#endif
	cur_proc = newp = (Process *) emalloc(sizeof *newp);

	newp->p_fd = ttyfd;
	newp->p_pid = pid;
	newp->p_eof = 0;
	newp->p_buffer = curbuf;

	cp = &cmd + 1;
	cmdbuf[0] = '\0';
	while (*cp != 0)
		(void) sprintf(&cmdbuf[strlen(cmdbuf)], "%s ", *cp++);

	newp->p_name = copystr(cmdbuf);
	newp->p_state = RUNNING;
	newp->p_reason = 0;
	newp->p_mark = MakeMark(curline, curchar, FLOATER);

	newp->p_next = procs;
	procs = newp;
	NumProcs++;
	global_fd |= 1 << newp->p_fd;
	sigrelse(SIGCHLD);
	SetWind(owind);
}
	
pinit()
{
	(void) signal(SIGCHLD, proc_child);
}
