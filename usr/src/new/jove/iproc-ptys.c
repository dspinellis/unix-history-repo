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
#include <errno.h>

#define DEAD	1	/* dead but haven't informed user yet */
#define STOPPED	2	/* job stopped */
#define RUNNING	3	/* just running */
#define NEW	4	/* brand new, never been ... received no input */

/* If process is dead, flags says how. */
#define EXITED	1
#define KILLED	2

#define isdead(p)	(p == 0 || proc_state(p) == DEAD || p->p_fd == -1)
#define makedead(p)	(proc_state(p) = DEAD)

#define proc_buf(p)	(p->p_buffer->b_name)
#define proc_cmd(p)	(p->p_name)
#define proc_state(p)	(p->p_state)

private Process	*procs = 0;

long	global_fd = 1;
int	NumProcs = 0;

#ifdef BRLUNIX
	extern struct sg_brl sg1;
#else
	extern struct sgttyb sg1;
#endif

extern struct tchars tc1;

#ifdef TIOCSLTC
	extern struct ltchars ls1;
#endif

char *
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
			return sprint("Exit %d", p->p_reason);
		}
		return sprint("Killed %d", p->p_reason);

	case NEW:
		return "New";

	default:
		return "Unknown state";
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
	if (n == -1 && errno == EIO) {
		if (proc_state(p) == NEW)
			return;
		proc_close(p);
		makedead(p);
		return;
	} else {
		if (proc_state(p) != RUNNING) {
			proc_state(p) = RUNNING;
			UpdModLine = YES;
		}
	}
	if (n <= 0) {
		if (n == 0)
			strcpy(ibuf, "[Process EOF]");
		else
			sprintf(ibuf, "\n[pty read error: %d]\n", errno);
	} else
		ibuf[n] = '\0';
	proc_rec(p, ibuf);
}

ProcKill()
{
	register Buffer	*b;
	Process	*buf_to_proc();
	char	*bname;

	bname = ask_buf(curbuf);

	if ((b = buf_exists(bname)) == 0)
		complain("[No such buffer]");
	if (b->b_process == 0)
		complain("%s not tied to a process.", bname);
	proc_kill(b->b_process, SIGKILL);
}

ProcCont()
{
	Process	*p;

	if ((p = curbuf->b_process) == 0)
		complain("[No process]");
	if (p->p_state != DEAD) {
		proc_kill(p, SIGCONT);
		p->p_state = RUNNING;
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

ProcStop()
{
	send_p(ls1.t_suspc);
}

ProcDStop()
{
	send_p(ls1.t_dsuspc);
}

send_p(c)
char	c;
{
	Process	*p;
	char	buf[2];

	if ((p = curbuf->b_process) == 0)
		complain("[No process]");
	ToLast();
	buf[0] = c;
	buf[1] = '\0';
	proc_rec(p, buf);
	(void) write(p->p_fd, &c, 1);
}

private
proc_close(p)
Process *p;
{
	sighold(SIGCHLD);	/* be mutually exclusive */

	if (p->p_fd >= 0) {
		(void) close(p->p_fd);
		global_fd &= ~(1L << p->p_fd);
		NumProcs -= 1;
		p->p_fd = -1;
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
	int	nbytes;

	if (isdead(p) || p->p_buffer != curbuf)
		return;

	(void) fixorder(&line1, &char1, &line2, &char2);
	while (line1 != line2->l_next) {
		gp = ltobuf(line1, genbuf) + char1;
		if (line1 == line2)
			gp[char2] = '\0';
		else
			strcat(gp, "\n");
		if (nbytes = strlen(gp))
			(void) write(p->p_fd, gp, nbytes);
		line1 = line1->l_next;
		char1 = 0;
	}
}

/* VARARGS2 */

private
proc_strt(bufname, clobber, va_alist)
char	*bufname;
va_dcl
{
	va_list	ap;
	char	*argv[32],
		*cp;
	Window *owind = curwind;
	int	pid;
	Process	*newp;
	Buffer 	*newbuf;
	int	i,
		ptyfd,
		ttyfd,
	 	ldisc,
		lmode;
	register char	*s,
			*t;
	extern int	errno;
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

	isprocbuf(bufname);	/* make sure BUFNAME is either nonexistant
				   or is of type B_PROCESS */
	for (s = "pqrs"; *s; s++) {
		for (t = "0123456789abcdef"; *t; t++) {
			sprintf(ptybuf, "/dev/pty%c%c", *s, *t);
			if ((ptyfd = open(ptybuf, 2)) >= 0) {
				strcpy(ttybuf, ptybuf);
				ttybuf[5] = 't';
				/* make sure both ends are available */
				if ((i = open(ttybuf, 2)) < 0)
					continue;
				(void) close(i);
				goto out;
			}
		}
	}

out:	if (s == 0 && t == 0)
		complain("[Out of ptys!]");

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
#  endif /* BTL_BLIT */
#endif

	sighold(SIGCHLD);
#ifdef SIGWINCH
	sighold(SIGWINCH);
#endif
	switch (pid = fork()) {
	case -1:
		(void) close(ptyfd);
		message("[Fork failed!]");
		goto fail;

	case 0:
		sigrelse(SIGCHLD);
#ifdef SIGWINCH
		sigrelse(SIGWINCH);
#endif
		for (i = 0; i < 32; i++)
			(void) close(i);

#ifdef TIOCNOTTY
		if ((i = open("/dev/tty", 2)) >= 0) {
			(void) ioctl(i, TIOCNOTTY, (struct sgttyb *) 0);
			(void) close(i);
		}
#endif
		if ((ttyfd = open(ttybuf, 2)) < 0)
			exit(-1);
		(void) dup2(ttyfd, 1);
		(void) dup2(ttyfd, 2);

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
		va_start(ap);
		make_argv(argv, ap);
		va_end(ap);
		execv(argv[0], &argv[1]);
		(void) write(1, "execve failed!\n", 15);
		_exit(errno + 1);
	}

	newp = (Process *) emalloc(sizeof *newp);

	newp->p_fd = ptyfd;
	newp->p_pid = pid;

	newbuf = do_select((Window *) 0, bufname);
	newbuf->b_type = B_PROCESS;
	newp->p_buffer = newbuf;
	newbuf->b_process = newp;	/* sorta circular, eh? */
	pop_wind(bufname, clobber, B_PROCESS);
	/* Pop_wind() after everything is set up; important!
	   Bindings won't work right unless newbuf->b_process is already
	   set up BEFORE NEWBUF is first SetBuf()'d. */
	ToLast();
	if (!bolp())
		LineInsert(1);

	cmdbuf[0] = '\0';
	va_start(ap);
	while (cp = va_arg(ap, char *))
		sprintf(&cmdbuf[strlen(cmdbuf)], "%s ", cp++);
	va_end(ap);

	newp->p_name = copystr(cmdbuf);
	newp->p_state = NEW;
	newp->p_reason = 0;
	newp->p_mark = MakeMark(curline, curchar, M_FLOATER);

	newp->p_next = procs;
	procs = newp;
	NumProcs += 1;
	global_fd |= 1L << newp->p_fd;
	SetWind(owind);

fail:	sigrelse(SIGCHLD);
#ifdef SIGWINCH
	sigrelse(SIGWINCH);
#endif
}
	
pinit()
{
	(void) signal(SIGCHLD, proc_child);
}

