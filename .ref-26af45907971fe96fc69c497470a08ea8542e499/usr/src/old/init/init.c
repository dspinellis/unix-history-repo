/*
 * Copyright (c) 1980,1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)init.c	5.19 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/reboot.h>
#include <sys/syslog.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <setjmp.h>
#include <utmp.h>
#include <errno.h>
#include <ttyent.h>
#include "pathnames.h"

#define	CMDSIZ	200	/* max string length for getty or window command*/
#define	ALL	p = itab; p ; p = p->next
#define	EVER	;;
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

char	shell[]	= _PATH_BSHELL;
char	minus[]	= "-";
char	runc[]	= _PATH_RC;
char	ctty[]	= _PATH_CONSOLE;

struct	tab
{
	char	line[UT_LINESIZE];
	char	comn[CMDSIZ];
	char	xflag;
	int	pid;
	int	wpid;		/* window system pid for SIGHUP	*/
	char	wcmd[CMDSIZ];	/* command to start window system process */
	time_t	gettytime;
	int	gettycnt;
	time_t	windtime;
	int	windcnt;
	struct	tab *next;
} *itab;

int	fi;
int	mergflag;
char	tty[20];
jmp_buf	sjbuf, shutpass;

char	*strcpy(), *strcat();
long	lseek();
void	idle(), merge(), reset();

struct	sigvec rvec = { reset, sigmask(SIGHUP), 0 };

main(argc, argv)
	char **argv;
{
	/* insure proper semantics for setjmp/longjmp */
	static int howto, oldhowto, started = 0;
#if defined(tahoe)
	register int r12;		/* make sure r11 gets bootflags */
#endif
#if defined(vax) || defined(tahoe) || defined(hp300)
	/* howto passed in high-order register XXX */
	register int r11;		/* passed thru from boot */
#ifdef __GNUC__
#ifdef hp300
	asm("movl d7,%0" : "=rm" (howto));
#else
	asm("movl r11,%0" : "=rm" (howto));
#endif
#else
	howto = r11;
#endif /* __GNUC__ */
#else  /* vax || tahoe || hp300 */
	/* howto passed as argument */
	howto = 0;
#endif  /* ! (vax || tahoe || hp300) */

	/*
	 * We expect a single options argument from the kernel.
	 * If it is present, we ignore anything in registers from above.
	 */
	if (argc > 1 && argv[1][0] == '-') {
		char *cp;

		howto = 0;
		cp = &argv[1][1];
		while (*cp) switch (*cp++) {
#ifdef notyet
		case 'f':
			howto |= RB_FASTBOOT;
			break;
#endif
		case 's':
			howto |= RB_SINGLE;
			break;
		}
	}
	if (getuid() != 0)
		exit(1);
	openlog("init", LOG_CONS|LOG_ODELAY, LOG_AUTH);
	if (setsid() < 0)
		syslog(LOG_ERR, "setsid failed (initial) %m");
	sigvec(SIGTERM, &rvec, (struct sigvec *)0);
	signal(SIGTSTP, idle);
	signal(SIGTTIN, SIG_IGN);
	signal(SIGTTOU, SIG_IGN);
	(void) setjmp(sjbuf);
	for (; ; ) {
		oldhowto = howto;
		howto = RB_SINGLE;
		if (started && setjmp(shutpass) == 0)
			shutdown();
		started = 1;
		if (oldhowto & RB_SINGLE)
			single();
		if (runcom(oldhowto) == 0) 
			continue;
		merge();
		multiple();
	}
}

void	shutreset();

shutdown()
{
	register i;
	register struct tab *p, *p1;

	signal(SIGHUP, SIG_IGN);
	for (p = itab; p ; ) {
		term(p);
		p1 = p->next;
		free(p);
		p = p1;
	}
	itab = (struct tab *)0;
	signal(SIGALRM, shutreset);
	(void) kill(-1, SIGTERM);	/* one chance to catch it */
	sleep(5);
	alarm(30);
	for (i = 0; i < 5; i++)
		kill(-1, SIGKILL);
	while (wait((int *)0) != -1)
		;
	alarm(0);
	shutend();
}

char shutfailm[] = "init: WARNING: something is hung (won't die); ps axl advised\n";

void
shutreset()
{
	int status;

	if (fork() == 0) {
		int ct = open(ctty, 1);
		write(ct, shutfailm, sizeof (shutfailm));
		sleep(5);
		exit(1);
	}
	sleep(5);
	shutend();
	longjmp(shutpass, 1);
}

shutend()
{
	register i;

	acct(0);
	signal(SIGALRM, SIG_DFL);
	for (i = 0; i < 10; i++)
		close(i);
	logwtmp("~", "shutdown", "");
}

single()
{
	register pid;
	register xpid;
	int fd;
	extern int errno;

	do {
		pid = fork();
		if (pid == 0) {
			signal(SIGTERM, SIG_DFL);
			signal(SIGHUP, SIG_DFL);
			signal(SIGALRM, SIG_DFL);
			signal(SIGTSTP, SIG_IGN);
			if (setsid() < 0)
				syslog(LOG_ERR, "setsid failed (single): %m");
			(void) revoke(ctty);
			if ((fd = open(ctty, O_RDWR)) < 0) {
				syslog(LOG_ERR, "open %s: %m", ctty);
				exit(1);
			}
			if (ioctl(fd, TIOCSCTTY, 0) < 0)
				syslog(LOG_ERR, "TIOCSCTTY failed: %m");
			dup2(fd, 0);
			dup2(fd, 1);
			dup2(fd, 2);
			if (fd > 2)
				close(fd);
			execl(shell, minus, (char *)0);
			perror(shell);
			exit(0);
		}
		while ((xpid = wait((int *)0)) != pid)
			if (xpid == -1 && errno == ECHILD)
				break;
	} while (xpid == -1);
}

runcom(oldhowto)
	int oldhowto;
{
	register pid;
	int fd, status;

	pid = fork();
	if (pid == 0) {
		signal(SIGTSTP, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		if ((fd = open(ctty, O_RDWR)) < 0)
			syslog(LOG_ERR, "open %s: %m", ctty);
		else {
			dup2(fd, 0);
			dup2(fd, 1);
			dup2(fd, 2);
			if (fd > 2)
				close(fd);
		}
		if (setsid() < 0)
			syslog(LOG_ERR, "setsid failed (runcom) %m");
		if (ioctl(0, TIOCSCTTY, 0) < 0) 
			syslog(LOG_ERR, "TIOCSCTTY failed (runcom) %m");
		if (oldhowto & RB_SINGLE)
			execl(shell, shell, runc, (char *)0);
		else
			execl(shell, shell, runc, "autoboot", (char *)0);
		exit(1);
	}
	while (wait(&status) != pid)
		;
	if (status)
		return (0);
	logwtmp("~", "reboot", "");
	return (1);
}

struct	sigvec	mvec = { merge, sigmask(SIGTERM), 0 };
/*
 * Multi-user.  Listen for users leaving, SIGHUP's
 * which indicate ttys has changed, and SIGTERM's which
 * are used to shutdown the system.
 */
multiple()
{
	extern int errno;
	register struct tab *p;
	register pid;
	int omask;

	sigvec(SIGHUP, &mvec, (struct sigvec *)0);
	for (EVER) {
		pid = wait((int *)0);
/* SHOULD FIX THIS IN THE KERNEL */
		if (pid == -1 && errno != EINTR)
			return;
		omask = sigblock(sigmask(SIGHUP));
		for (ALL) {
			/* must restart window system BEFORE emulator */
			if (p->wpid == pid || p->wpid == -1)
				wstart(p);
			if (p->pid == pid || p->pid == -1) {
				/* disown the window system */
				if (p->wpid)
					kill(p->wpid, SIGHUP);
				cleanutmp(p);
				dfork(p);
			}
		}
		sigsetmask(omask);
	}
}

/*
 * Merge current contents of ttys file
 * into in-core table of configured tty lines.
 * Entered as signal handler for SIGHUP.
 */
#define	FOUND	1
#define	CHANGE	2
#define WCHANGE 4

void
merge()
{
	register struct tab *p;
	register struct ttyent *t;
	register struct tab *p1;

	for (ALL)
		p->xflag = 0;
	setttyent();
	while (t = getttyent()) {
		if ((t->ty_status & TTY_ON) == 0)
			continue;
		for (ALL) {
			if (SCMPN(p->line, t->ty_name))
				continue;
			p->xflag |= FOUND;
			if (SCMPN(p->comn, t->ty_getty)) {
				p->xflag |= CHANGE;
				SCPYN(p->comn, t->ty_getty);
			}
			if (SCMPN(p->wcmd, t->ty_window ? t->ty_window : "")) {
				p->xflag |= WCHANGE|CHANGE;
				SCPYN(p->wcmd, t->ty_window);
			}
			goto contin1;
		}

		/*
		 * Make space for a new one
		 */
		p1 = (struct tab *)calloc(1, sizeof(*p1));
		if (!p1) {
			syslog(LOG_ERR, "no space for '%s' !?!", t->ty_name);
			goto contin1;
		}
		/*
		 * Put new terminal at the end of the linked list.
		 */
		if (itab) {
			for (p = itab; p->next ; p = p->next)
				;
			p->next = p1;
		} else
			itab = p1;

		p = p1;
		SCPYN(p->line, t->ty_name);
		p->xflag |= FOUND|CHANGE;
		SCPYN(p->comn, t->ty_getty);
		if (t->ty_window && strcmp(t->ty_window, "") != 0) {
			p->xflag |= WCHANGE;
			SCPYN(p->wcmd, t->ty_window);
		}
	contin1:
		;
	}
	endttyent();
	p1 = (struct tab *)0;
	for (ALL) {
		if ((p->xflag&FOUND) == 0) {
			term(p);
			wterm(p);
			if (p1)
				p1->next = p->next;
			else
				itab = p->next;
			free(p);
			p = p1 ? p1 : itab;
		} else {
			/* window system should be started first */
			if (p->xflag&WCHANGE) {
				wterm(p);
				wstart(p);
			}
			if (p->xflag&CHANGE) {
				term(p);
				dfork(p);
			}
		}
		p1 = p;
	}
}

term(p)
	register struct tab *p;
{

	if (p->pid != 0) {
		cleanutmp(p);
		kill(p->pid, SIGKILL);
	}
	p->pid = 0;
	/* send SIGHUP to get rid of connections */
	if (p->wpid > 0)
		kill(p->wpid, SIGHUP);
}

dfork(p)
	struct tab *p;
{
	register pid;
	time_t t;
	int dowait = 0;

	time(&t);
	p->gettycnt++;
	if ((t - p->gettytime) >= 60) {
		p->gettytime = t;
		p->gettycnt = 1;
	} else if (p->gettycnt >= 5) {
		dowait = 1;
		p->gettytime = t;
		p->gettycnt = 1;
	}
	pid = fork();
	if (pid == 0) {
		signal(SIGTERM, SIG_DFL);
		signal(SIGHUP, SIG_IGN);
		sigsetmask(0);	/* since can be called from masked code */
		if (dowait) {
			syslog(LOG_ERR, "'%s %s' failing, sleeping", p->comn, p->line);
			closelog();
			sleep(30);
		}
		if (setsid() < 0)
			syslog(LOG_ERR, "setsid failed(dfork) %m");
		execit(p->comn, p->line);
		exit(0);
	}
	p->pid = pid;
}

cleanutmp(p)
	register struct tab *p;
{
	if (logout(p->line)) {
		logwtmp(p->line, "", "");
		/*
		 * After a proper login force reset
		 * of error detection code in dfork.
		 */
		p->gettytime = 0;
		p->windtime = 0;
	}
}

void
reset()
{
	longjmp(sjbuf, 1);
}

jmp_buf	idlebuf;

void
idlehup()
{
	longjmp(idlebuf, 1);
}

void
idle()
{
	register struct tab *p;
	register pid;

	signal(SIGHUP, idlehup);
	for (EVER) {
		if (setjmp(idlebuf))
			return;
		pid = wait((int *) 0);
		if (pid == -1) {
			sigpause(0);
			continue;
		}
		for (ALL) {
			/* if window system dies, mark it for restart */
			if (p->wpid == pid)
				p->wpid = -1;
			if (p->pid == pid) {
				cleanutmp(p);
				p->pid = -1;
			}
		}
	}
}

wterm(p)
	register struct tab *p;
{
	if (p->wpid != 0) {
		kill(p->wpid, SIGKILL);
	}
	p->wpid = 0;
}

wstart(p)
	register struct tab *p;
{
	register pid;
	time_t t;
	int dowait = 0;

	time(&t);
	p->windcnt++;
	if ((t - p->windtime) >= 60) {
		p->windtime = t;
		p->windcnt = 1;
	} else if (p->windcnt >= 5) {
		dowait = 1;
		p->windtime = t;
		p->windcnt = 1;
	}

	pid = fork();

	if (pid == 0) {
		signal(SIGTERM, SIG_DFL);
		signal(SIGHUP,  SIG_IGN);
		sigsetmask(0);	/* since can be called from masked code */
		if (dowait) {
			syslog(LOG_ERR, "'%s %s' failing, sleeping", p->wcmd, p->line);
			closelog();
			sleep(30);
		}
		if (setsid() < 0)
			syslog(LOG_ERR, "setsid failed (window) %m");
		execit(p->wcmd, p->line);
		exit(0);
	}
	p->wpid = pid;
}

#define NARGS	20	/* must be at least 4 */
#define ARGLEN	512	/* total size for all the argument strings */

execit(s, arg)
	char *s;
	char *arg;	/* last argument on line */
{
	char *argv[NARGS], args[ARGLEN], *envp[1];
	register char *sp = s;
	register char *ap = args;
	register char c;
	register int i;

	/*
	 * First we have to set up the argument vector.
	 * "prog arg1 arg2" maps to exec("prog", "-", "arg1", "arg2"). 
	 */
	for (i = 1; i < NARGS - 2; i++) {
		argv[i] = ap;
		for (EVER) {
			if ((c = *sp++) == '\0' || ap >= &args[ARGLEN-1]) {
				*ap = '\0';
				goto done;
			}
			if (c == ' ') {
				*ap++ = '\0';
				while (*sp == ' ')
					sp++;
				if (*sp == '\0')
					goto done;
				break;
			}
			*ap++ = c;
		}
	}
done:
	argv[0] = argv[1];
	argv[1] = "-";
	argv[i+1] = arg;
	argv[i+2] = 0;
	envp[0] = 0;
	execve(argv[0], &argv[1], envp);
	/* report failure of exec */
	syslog(LOG_ERR, "%s: %m", argv[0]);
	closelog();
	sleep(10);	/* prevent failures from eating machine */
}
