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
#include "ttystate.h"
#include "wait.h"
#ifdef BSD386
#include <sys/ioctl.h>
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

private Process	*procs = NULL;

fd_set	global_fd;
int	NumProcs = 0;

private Process *
proc_pid(pid)
int	pid;
{
	register Process	*p;

	for (p = procs; p != NULL; p = p->p_next)
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

	for (p = procs; p != NULL; p = p->p_next)
		if (p->p_fd == fd)
			break;

	if (p == NULL) {
		writef("\riproc: unknown fd %d", fd);
		return;
	}

	n = read(fd, (UnivPtr) ibuf, sizeof(ibuf) - 1);
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
			swritef(ibuf, sizeof(ibuf),
				"\n[pty read error: %d]\n", errno);
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

	if ((b = buf_exists(bname)) == NULL)
		complain("[No such buffer]");
	if (b->b_process == NULL)
		complain("%s not tied to a process.", bname);
	proc_kill(b->b_process, SIGKILL);
}

void
ProcCont()
{
	Process	*p;

	if ((p = curbuf->b_process) == NULL)
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

	if ((p = curbuf->b_process) == NULL)
		complain("[No process]");
	ToLast();
	buf[0] = c;
	buf[1] = '\0';
	proc_rec(p, buf);
	(void) write(p->p_fd, (UnivPtr) &c, (size_t) 1);
}

void
ProcEof()
{
#ifdef SGTTY
	send_p(tc[OFF].t_eofc);
#else
	send_p(sg[OFF].c_cc[VEOF]);
#endif
}

void
ProcInt()
{
#ifdef SGTTY
	send_p(tc[OFF].t_intrc);
#else
	send_p(sg[OFF].c_cc[VINTR]);
#endif

}

void
ProcQuit()
{
#ifdef SGTTY
	send_p(tc[OFF].t_quitc);
#else
	send_p(sg[OFF].c_cc[VQUIT]);
#endif
}

void
ProcStop()
{
#ifdef SGTTY
	send_p(ls[OFF].t_suspc);
#else
	send_p(sg[OFF].c_cc[VSUSP]);
#endif
}

void
ProcDStop()
{
#ifdef SGTTY
	send_p(ls[OFF].t_dsuspc);
#else
	send_p(sg[OFF].c_cc[VDSUSP]);
#endif
}

private void
proc_close(p)
Process *p;
{
	SigHold(SIGCHLD);	/* be mutually exclusive */

	if (p->p_fd >= 0) {
		(void) close(p->p_fd);
		FD_CLR(p->p_fd, &global_fd);
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
	fd_set	mask;

	FD_ZERO(&mask);
	FD_SET(p->p_fd, &mask);

	while (write(p->p_fd, (UnivPtr) buf, nbytes) <  0)
		select(p->p_fd + 1, (fd_set *)0, &mask, (fd_set *)0, (struct timeval *)0);
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
		ptyfd = -1,
		ttyfd,
		ldisc,
		lmode;
	register char	*s,
			*t;
	char	ttybuf[11],
		ptybuf[11];
	char	cmdbuf[LBSIZE];
#if defined(SGTTY) || defined(BRLUNIX)
#ifdef	BRLUNIX
	struct sg_brl sgt;
#else
	struct sgttyb sgt;
#endif
#endif
#ifdef TERMIO
	struct termio	sgt;
#endif
#ifdef TERMIOS
	struct termios	sgt;
#endif

#ifdef	TIOCGWINSZ
	struct winsize win;
#else
#  ifdef	BTL_BLIT
#  include <sys/jioctl.h>
	struct jwinsize jwin;
#  endif
#endif

	isprocbuf(bufname);	/* make sure BUFNAME is either nonexistant
				   or is of type B_PROCESS */
	va_init(ap, clobber);
	make_argv(argv, ap);
	va_end(ap);
	if (access(argv[0], X_OK) != 0) {
		complain("[Couldn't access %s: %s]", argv[0], strerror(errno));
		/* NOTREACHED */
	}
	for (s = "pqrs"; ptyfd<0; s++) {
		if (*s == '\0')
			complain("[Out of ptys!]");
		for (t = "0123456789abcdef"; *t; t++) {
			swritef(ptybuf, sizeof(ptybuf), "/dev/pty%c%c", *s, *t);
			if ((ptyfd = open(ptybuf, 2)) >= 0) {
				strcpy(ttybuf, ptybuf);
				ttybuf[5] = 't';
				/* make sure both ends are available */
				if ((i = open(ttybuf, 2)) < 0) {
					(void) close(ptyfd);
					ptyfd = -1;
				} else {
					(void) close(i);
					break;
				}
			}
		}
	}

#ifdef	TIOCGETD
	(void) ioctl(0, TIOCGETD, (UnivPtr) &ldisc);
#endif
#ifdef	TIOCLGET
	(void) ioctl(0, TIOCLGET, (UnivPtr) &lmode);
#endif
#ifdef	TIOCGWINSZ
	(void) ioctl(0, TIOCGWINSZ, (UnivPtr) &win);
#else
#  ifdef	BTL_BLIT
	(void) ioctl(0, JWINSIZE, (UnivPtr) &jwin);
#  endif	/* BTL_BLIT */
#endif

	SigHold(SIGCHLD);
#ifdef	SIGWINCH
	SigHold(SIGWINCH);
#endif
	switch (pid = fork()) {
	case -1:
		(void) close(ptyfd);
		s_mess("[Fork failed! %s]", strerror(errno));
		goto fail;

	case 0:
		SigRelse(SIGCHLD);
#ifdef	SIGWINCH
		SigRelse(SIGWINCH);
#endif
		for (i = 0; i < 32; i++)
			(void) close(i);

#ifdef	TIOCNOTTY
		if ((i = open("/dev/tty", 2)) >= 0) {
			(void) ioctl(i, TIOCNOTTY, (UnivPtr)NULL);
			(void) close(i);
		}
#endif
		if ((ttyfd = open(ttybuf, 2)) < 0)
			exit(-1);
		(void) dup2(ttyfd, 1);
		(void) dup2(ttyfd, 2);

#ifdef	TIOCSETD
		(void) ioctl(0, TIOCSETD, (UnivPtr) &ldisc);
#endif
#ifdef	TIOCLSET
		(void) ioctl(0, TIOCLSET, (UnivPtr) &lmode);
#endif
#ifdef	TIOCSETC
		(void) ioctl(0, TIOCSETC, (UnivPtr) &tc[OFF]);
#endif
#ifdef	TIOCSLTC
		(void) ioctl(0, TIOCSLTC, (UnivPtr) &ls[OFF]);
#endif

#ifdef	TIOCGWINSZ
#    ifdef	SIGWINCH
		(void) signal(SIGWINCH, SIG_IGN);
#    endif
		win.ws_row = curwind->w_height;
		(void) ioctl(0, TIOCSWINSZ, (UnivPtr) &win);
#else
#  ifdef	BTL_BLIT
		jwin.bytesy = curwind->w_height;
		(void) ioctl(0, JSWINSIZE, (UnivPtr) &jwin);
#  endif
#endif

		sgt = sg[OFF];
#ifdef SGTTY
		sgt.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
		(void) stty(0, &sgt);
#else
		sgt.c_lflag &= ~(ISIG|ICANON|ECHO);
		sgt.c_lflag |= ISIG|ICANON|IEXTEN;
#ifndef OCRNL
#define OCRNL 0
#endif
		sgt.c_oflag &= ~(OCRNL|ONLCR);
# ifdef	TERMIO
		(void) ioctl(0, TCSETAW, (UnivPtr) &sgt);
# endif	/* TERMIO */

# ifdef	TERMIOS
		(void) tcsetattr(0, TCSADRAIN, &sgt);
# endif /* TERMIOS */
#endif

		{
			int	on = 1;

			(void) ioctl(0, TIOCREMOTE, (UnivPtr) &on);
		}

#ifdef TIOCSCTTY
		(void) setsid();
		(void) ioctl(0, TIOCSCTTY, 0);
#else
		i = getpid();
		(void) ioctl(0, TIOCSPGRP, (UnivPtr) &i);
		(void) setpgrp(0, i);
#endif
		execv(argv[0], (const char **) &argv[1]);
		raw_scream("execve failed! %s\n");
		_exit(errno + 1);
	}

	newp = (Process *) emalloc(sizeof *newp);

#ifdef	O_NDELAY
	fcntl (ptyfd, F_SETFL, O_NDELAY);
#endif
	newp->p_fd = ptyfd;
	newp->p_pid = pid;

	newbuf = do_select((Window *)NULL, bufname);
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
	while ((cp = va_arg(ap, char *)) != NULL) {
		size_t	pl = strlen(cmdbuf);

		swritef(&cmdbuf[pl], sizeof(cmdbuf)-pl, "%s ", cp);
	}
	va_end(ap);

	newp->p_name = copystr(cmdbuf);
	newp->p_state = NEW;
	newp->p_reason = 0;
	newp->p_mark = MakeMark(curline, curchar, M_FLOATER);
	newp->p_dbx_mode = NO;

	newp->p_next = procs;
	procs = newp;
	NumProcs += 1;
	FD_SET(newp->p_fd, &global_fd);
	SetWind(owind);

fail:
	SigRelse(SIGCHLD);
#ifdef	SIGWINCH
	SigRelse(SIGWINCH);
#endif
}

void
pinit()
{
	FD_ZERO(&global_fd);
	FD_SET(0, &global_fd);
	(void) signal(SIGCHLD, proc_child);
}
