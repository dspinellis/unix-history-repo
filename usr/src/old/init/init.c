
/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)init.c	5.1 (Berkeley) %G%";
#endif not lint

#include <signal.h>
#include <sys/types.h>
#include <utmp.h>
#include <setjmp.h>
#include <sys/reboot.h>
#include <errno.h>
#include <sys/file.h>
#include <ttyent.h>
#include <syslog.h>

#define	LINSIZ	sizeof(wtmp.ut_line)
#define	CMDSIZ	70	/* max string length for getty or window command*/
#define	TABSIZ	100
#define	ALL	p = &itab[0]; p < &itab[TABSIZ]; p++
#define	EVER	;;
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

char	shell[]	= "/bin/sh";
char	minus[]	= "-";
char	runc[]	= "/etc/rc";
char	utmp[]	= "/etc/utmp";
char	wtmpf[]	= "/usr/adm/wtmp";
char	ctty[]	= "/dev/console";

struct utmp wtmp;
struct	tab
{
	char	line[LINSIZ];
	char	comn[CMDSIZ];
	char	xflag;
	int	pid;
	int	wpid;		/* window system pid for SIGHUP	*/
	char	wcmd[CMDSIZ];	/* command to start window system process */
	time_t	gettytime;
	int	gettycnt;
} itab[TABSIZ];

int	fi;
int	mergflag;
char	tty[20];
jmp_buf	sjbuf, shutpass;
time_t	time0;

int	reset();
int	idle();
char	*strcpy(), *strcat();
long	lseek();

struct	sigvec rvec = { reset, sigmask(SIGHUP), 0 };

#ifdef vax
main()
{
	register int r11;		/* passed thru from boot */
#else
main(argc, argv)
	char **argv;
{
#endif
	int howto, oldhowto;

	time0 = time(0);
#ifdef vax
	howto = r11;
#else
	if (argc > 1 && argv[1][0] == '-') {
		char *cp;

		howto = 0;
		cp = &argv[1][1];
		while (*cp) switch (*cp++) {
		case 'a':
			howto |= RB_ASKNAME;
			break;
		case 's':
			howto |= RB_SINGLE;
			break;
		}
	} else {
		howto = RB_SINGLE;
	}
#endif
	openlog("init", LOG_CONS|LOG_ODELAY, 0);
	sigvec(SIGTERM, &rvec, (struct sigvec *)0);
	signal(SIGTSTP, idle);
	signal(SIGSTOP, SIG_IGN);
	signal(SIGTTIN, SIG_IGN);
	signal(SIGTTOU, SIG_IGN);
	(void) setjmp(sjbuf);
	for (EVER) {
		oldhowto = howto;
		howto = RB_SINGLE;
		if (setjmp(shutpass) == 0)
			shutdown();
		if (oldhowto & RB_SINGLE)
			single();
		if (runcom(oldhowto) == 0) 
			continue;
		merge();
		multiple();
	}
}

int	shutreset();

shutdown()
{
	register i;
	register struct tab *p;

	close(creat(utmp, 0644));
	signal(SIGHUP, SIG_IGN);
	for (ALL) {
		term(p);
		p->line[0] = 0;
	}
	signal(SIGALRM, shutreset);
	alarm(30);
	for (i = 0; i < 5; i++)
		kill(-1, SIGKILL);
	while (wait((int *)0) != -1)
		;
	alarm(0);
	shutend();
}

char shutfailm[] = "WARNING: Something is hung (wont die); ps axl advised\n";

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
	register i, f;

	acct(0);
	signal(SIGALRM, SIG_DFL);
	for (i = 0; i < 10; i++)
		close(i);
	f = open(wtmpf, O_WRONLY|O_APPEND);
	if (f >= 0) {
		SCPYN(wtmp.ut_line, "~");
		SCPYN(wtmp.ut_name, "shutdown");
		SCPYN(wtmp.ut_host, "");
		time(&wtmp.ut_time);
		write(f, (char *)&wtmp, sizeof(wtmp));
		close(f);
	}
	return (1);
}

single()
{
	register pid;
	register xpid;
	extern	errno;

	do {
		pid = fork();
		if (pid == 0) {
			signal(SIGTERM, SIG_DFL);
			signal(SIGHUP, SIG_DFL);
			signal(SIGALRM, SIG_DFL);
			signal(SIGTSTP, SIG_IGN);
			(void) open(ctty, O_RDWR);
			dup2(0, 1);
			dup2(0, 2);
			execl(shell, minus, (char *)0);
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
	register pid, f;
	int status;

	pid = fork();
	if (pid == 0) {
		(void) open("/", O_RDONLY);
		dup2(0, 1);
		dup2(0, 2);
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
	f = open(wtmpf, O_WRONLY|O_APPEND);
	if (f >= 0) {
		SCPYN(wtmp.ut_line, "~");
		SCPYN(wtmp.ut_name, "reboot");
		SCPYN(wtmp.ut_host, "");
		if (time0) {
			wtmp.ut_time = time0;
			time0 = 0;
		} else
			time(&wtmp.ut_time);
		write(f, (char *)&wtmp, sizeof(wtmp));
		close(f);
	}
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
	register struct tab *p;
	register pid;

	sigvec(SIGHUP, &mvec, (struct sigvec *)0);
	for (EVER) {
		pid = wait((int *)0);
		if (pid == -1)
			return;
		for (ALL) {
			/* must restart window system BEFORE emulator */
			if (p->wpid == pid || p->wpid == -1)
				wstart(p);
			if (p->pid == pid || p->pid == -1) {
				/* disown the window system */
				if (p->wpid)
					kill(p->wpid, SIGHUP);
				rmut(p);
				dfork(p);
			}
		}
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

merge()
{
	register struct tab *p;
	register struct ttyent *t;

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
			if (SCMPN(p->wcmd, t->ty_window)) {
				p->xflag |= WCHANGE|CHANGE;
				SCPYN(p->wcmd, t->ty_window);
			}
			goto contin1;
		}

		for (ALL) {
			if (p->line[0] != 0)
				continue;
			SCPYN(p->line, t->ty_name);
			p->xflag |= FOUND|CHANGE;
			SCPYN(p->comn, t->ty_getty);
			if (strcmp(t->ty_window, "") != 0) {
				p->xflag |= WCHANGE;
				SCPYN(p->wcmd, t->ty_window);
			}
			goto contin1;
		}
	contin1:
		;
	}
	endttyent();
	for (ALL) {
		if ((p->xflag&FOUND) == 0) {
			term(p);
			p->line[0] = 0;
			wterm(p);
		}
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
}

term(p)
	register struct tab *p;
{

	if (p->pid != 0) {
		rmut(p);
		kill(p->pid, SIGKILL);
	}
	p->pid = 0;
	/* send SIGHUP to get rid of connections */
	if (p->wpid > 0)
		kill(p->wpid, SIGHUP);
}

#include <sys/ioctl.h>

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
		if (dowait) {
			syslog(LOG_ERR, "'%s %s' failing, sleeping", p->comn, p->line);
			closelog();
			sleep(30);
		}
		execit(p->comn, p->line);
		exit(0);
	}
	p->pid = pid;
}

/*
 * Remove utmp entry.
 */
rmut(p)
	register struct tab *p;
{
	register f;
	int found = 0;

	f = open(utmp, O_RDWR);
	if (f >= 0) {
		while (read(f, (char *)&wtmp, sizeof(wtmp)) == sizeof(wtmp)) {
			if (SCMPN(wtmp.ut_line, p->line) || wtmp.ut_name[0]==0)
				continue;
			lseek(f, -(long)sizeof(wtmp), 1);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			found++;
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, O_WRONLY|O_APPEND);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, p->line);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			close(f);
		}
		/*
		 * After a proper login force reset
		 * of error detection code in dfork.
		 */
		p->gettytime = 0;
	}
}

reset()
{

	longjmp(sjbuf, 1);
}

jmp_buf	idlebuf;

idlehup()
{

	longjmp(idlebuf, 1);
}

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
				rmut(p);
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
	int npid = fork();

	if (npid == 0) {
/*
		signal(SIGTERM, SIG_DFL);
		signal(SIGHUP,  SIG_DFL);
		signal(SIGALRM, SIG_DFL);
		signal(SIGTSTP, SIG_IGN);
*/
		execit(p->wcmd, p->line);
		exit(0);
	}
	p->wpid = npid;
}

#define NARGS	20	/* must be at lease 4 */
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
