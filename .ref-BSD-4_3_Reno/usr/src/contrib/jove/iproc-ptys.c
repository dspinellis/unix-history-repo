/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* NOTE WELL:
 * This file is "included" into iproc.c -- it is not compiled separately!
 */

#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include <sgtty.h>
#include <errno.h>
#include "wait.h"

#ifdef USE_OLD_TTY
# define gtty(fd, buf)	ioctl(fd, TIOCGETP, buf)
# define stty(fd, buf)	ioctl(fd, TIOCSETP, buf)
#endif

#define DEAD	1	/* dead but haven't informed user yet */
#define STOPPED	2	/* job stopped */
#define RUNNING	3	/* just running */
#define NEW	4	/* brand new, never been ... received no input */

/* If process is dead, flags says how. */
#define EXITED	1
#define KILLED	2

#define isdead(p)	((p) == NULL || proc_state((p)) == DEAD || (p)->p_fd == -1)
#define makedead(p)	{ proc_state((p)) = DEAD; }

#define proc_buf(p)	((p)->p_buffer->b_name)
#define proc_cmd(p)	((p)->p_name)
#define proc_state(p)	((p)->p_state)

private Process	*procs = 0;

long	global_fd = 1;
int	NumProcs = 0;

#include "ttystate.h"

private Process *
proc_pid(pid)
int	pid;
{
	register Process	*p;

	for (p = procs; p != 0; p = p->p_next)
		if (p->p_pid == pid)
			break;

	return p;
}

void
read_proc(fd)
register int	fd;
{
	register Process	*p;
	int	n;
	char	ibuf[1024];

	for (p = procs; p != 0; p = p->p_next)
		if (p->p_fd == fd)
			break;

	if (p == 0) {
		writef("\riproc: unknown fd %d", fd);
		return;
	}

	n = read(fd, ibuf, sizeof(ibuf) - 1);
	if (n == -1 && (errno == EIO || errno == EWOULDBLOCK)) {
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
			swritef(ibuf, "\n[pty read error: %d]\n", errno);
	} else
		ibuf[n] = '\0';
	proc_rec(p, ibuf);
}

void
ProcKill()
{
	register Buffer	*b;
	char	*bname;

	bname = ask_buf(curbuf);

	if ((b = buf_exists(bname)) == 0)
		complain("[No such buffer]");
	if (b->b_process == 0)
		complain("%s not tied to a process.", bname);
	proc_kill(b->b_process, SIGKILL);
}

void
ProcCont()
{
	Process	*p;

	if ((p = curbuf->b_process) == 0)
		complain("[No process]");
	if (p->p_state != DEAD) {
		proc_kill(p, SIGCONT);
		UpdModLine = YES;
		p->p_state = RUNNING;
	}
}

private void
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
	(void) write(p->p_fd, &c, (size_t) 1);
}

void
ProcEof()
{
	send_p(tc[OFF].t_eofc);
}

void
ProcInt()
{
	send_p(tc[OFF].t_intrc);
}

void
ProcQuit()
{
	send_p(tc[OFF].t_quitc);
}

void
ProcStop()
{
	send_p(ls[OFF].t_suspc);
}

void
ProcDStop()
{
	send_p(ls[OFF].t_dsuspc);
}

private void
proc_close(p)
Process *p;
{
	SigHold(SIGCHLD);	/* be mutually exclusive */

	if (p->p_fd >= 0) {
		(void) close(p->p_fd);
		global_fd &= ~(1L << p->p_fd);
		NumProcs -= 1;
		p->p_fd = -1;
	}

	SigRelse(SIGCHLD);
}

private void
proc_write(p, buf, nbytes)
Process *p;
char	*buf;
size_t	nbytes;
{
	long	mask = (1 << p->p_fd);

	while (write(p->p_fd, buf, nbytes) <  0)
		select(p->p_fd + 1, (long *) 0, &mask, (long *) 0, (struct timeval *) 0);
}

#ifdef	STDARGS
	private void
proc_strt(char *bufname, int clobber, ...)
#else
	private /*VARARGS2*/ void
proc_strt(bufname, clobber, va_alist)
	char	*bufname;
	int	clobber;
	va_dcl
#endif
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
	char	ttybuf[11],
		ptybuf[11];
	char	cmdbuf[128];
#ifdef BRLUNIX
	struct sg_brl sgt;
#else
	struct sgttyb sgt;
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
			swritef(ptybuf, "/dev/pty%c%c", *s, *t);
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
	(void) ioctl(0, TIOCGETD, (UnivPtr) &ldisc);
#endif
#ifdef TIOCLGET
	(void) ioctl(0, TIOCLGET, (UnivPtr) &lmode);
#endif
#ifdef TIOCGWINSZ
	(void) ioctl(0, TIOCGWINSZ, (UnivPtr) &win);
#else
#  ifdef BTL_BLIT
	(void) ioctl(0, JWINSIZE, (UnivPtr) &jwin);
#  endif /* BTL_BLIT */
#endif

	SigHold(SIGCHLD);
#ifdef SIGWINCH
	SigHold(SIGWINCH);
#endif
	switch (pid = fork()) {
	case -1:
		(void) close(ptyfd);
		message("[Fork failed!]");
		goto fail;

	case 0:
		SigRelse(SIGCHLD);
#ifdef SIGWINCH
		SigRelse(SIGWINCH);
#endif
		for (i = 0; i < 32; i++)
			(void) close(i);

#ifdef TIOCNOTTY
		if ((i = open("/dev/tty", 2)) >= 0) {
			(void) ioctl(i, TIOCNOTTY, (UnivPtr) 0);
			(void) close(i);
		}
#endif
		if ((ttyfd = open(ttybuf, 2)) < 0)
			exit(-1);
		(void) dup2(ttyfd, 1);
		(void) dup2(ttyfd, 2);

#ifdef TIOCSETD
		(void) ioctl(0, TIOCSETD, (UnivPtr) &ldisc);
#endif
#ifdef TIOCLSET
		(void) ioctl(0, TIOCLSET, (UnivPtr) &lmode);
#endif
#ifdef TIOCSETC
		(void) ioctl(0, TIOCSETC, (UnivPtr) &tc[OFF]);
#endif
#ifdef TIOCSLTC
		(void) ioctl(0, TIOCSLTC, (UnivPtr) &ls[OFF]);
#endif

#ifdef TIOCGWINSZ
#    ifdef SIGWINCH
		(void) signal(SIGWINCH, SIG_IGN);
#    endif
		win.ws_row = curwind->w_height;
		(void) ioctl(0, TIOCSWINSZ, (UnivPtr) &win);
#else
#  ifdef BTL_BLIT
		jwin.bytesy = curwind->w_height;
		(void) ioctl(0, JSWINSIZE, (UnivPtr) &jwin);
#  endif
#endif

		sgt = sg[OFF];
		sgt.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
		(void) stty(0, &sgt);

		{
			int	on = 1;

			(void) ioctl(0, TIOCREMOTE, (UnivPtr) &on);
		}

		i = getpid();
		(void) ioctl(0, TIOCSPGRP, (UnivPtr) &i);
		(void) setpgrp(0, i);
		va_init(ap, clobber);
		make_argv(argv, ap);
		va_end(ap);
		execv(argv[0], (const char **) &argv[1]);
		(void) write(1, "execve failed!\n", (size_t) 15);
		_exit(errno + 1);
	}

	newp = (Process *) emalloc(sizeof *newp);

#ifdef O_NDELAY
	fcntl (ptyfd, F_SETFL, O_NDELAY);
#endif
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
	va_init(ap, clobber);
	while ((cp = va_arg(ap, char *)) != NIL)
		swritef(&cmdbuf[strlen(cmdbuf)], "%s ", cp++);
	va_end(ap);

	newp->p_name = copystr(cmdbuf);
	newp->p_state = NEW;
	newp->p_reason = 0;
	newp->p_mark = MakeMark(curline, curchar, M_FLOATER);
	newp->p_dbx_mode = NO;

	newp->p_next = procs;
	procs = newp;
	NumProcs += 1;
	global_fd |= 1L << newp->p_fd;
	SetWind(owind);

fail:	SigRelse(SIGCHLD);
#ifdef SIGWINCH
	SigRelse(SIGWINCH);
#endif
}

void
pinit()
{
	(void) signal(SIGCHLD, proc_child);
}
