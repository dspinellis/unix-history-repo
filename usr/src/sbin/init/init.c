static	char *sccsid = "@(#)init.c	4.8 (Berkeley) %G%";
#include <signal.h>
#include <sys/types.h>
#include <utmp.h>
#include <setjmp.h>
#include <sys/reboot.h>
#include <errno.h>

#define	LINSIZ	sizeof(wtmp.ut_line)
#define	TABSIZ	100
#define	ALL	p = &itab[0]; p < &itab[TABSIZ]; p++
#define	EVER	;;
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

char	shell[]	= "/bin/sh";
char	getty[]	 = "/etc/getty";
char	minus[]	= "-";
char	runc[]	= "/etc/rc";
char	ifile[]	= "/etc/ttys";
char	utmp[]	= "/etc/utmp";
char	wtmpf[]	= "/usr/adm/wtmp";
char	ctty[]	= "/dev/console";
char	dev[]	= "/dev/";

struct utmp wtmp;
struct
{
	char	line[LINSIZ];
	char	comn;
	char	flag;
} line;
struct	tab
{
	char	line[LINSIZ];
	char	comn;
	char	xflag;
	int	pid;
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

main()
{
	register int r11;		/* passed thru from boot */
	int howto, oldhowto;

	time0 = time(0);
	howto = r11;
	setjmp(sjbuf);
	signal(SIGTERM, reset);
	signal(SIGTSTP, idle);
	signal(SIGSTOP, SIG_IGN);
	signal(SIGTTIN, SIG_IGN);
	signal(SIGTTOU, SIG_IGN);
	for(EVER) {
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
	for(ALL) {
		term(p);
		p->line[0] = 0;
	}
	signal(SIGALRM, shutreset);
	alarm(30);
	for(i=0; i<5; i++)
		kill(-1, SIGKILL);
	while(wait((int *)0) != -1)
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
	for(i=0; i<10; i++)
		close(i);
	f = open(wtmpf, 1);
	if (f >= 0) {
		lseek(f, 0L, 2);
		SCPYN(wtmp.ut_line, "~");
		SCPYN(wtmp.ut_name, "shutdown");
		time(&wtmp.ut_time);
		write(f, (char *)&wtmp, sizeof(wtmp));
		close(f);
	}
	return(1);
}

single()
{
	register pid;
	register xpid;
	extern	errno;

   do {
	pid = fork();
	if(pid == 0) {
/*
		alarm(300);
*/
		signal(SIGTERM, SIG_DFL);
		signal(SIGHUP, SIG_DFL);
		signal(SIGALRM, SIG_DFL);
		open(ctty, 2);
		dup(0);
		dup(0);
		execl(shell, minus, (char *)0);
		exit(0);
	}
	while((xpid = wait((int *)0)) != pid)
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
	if(pid == 0) {
		open("/", 0);
		dup(0);
		dup(0);
		if (oldhowto & RB_SINGLE)
			execl(shell, shell, runc, (char *)0);
		else
			execl(shell, shell, runc, "autoboot", (char *)0);
		exit(1);
	}
	while(wait(&status) != pid)
		;
	if(status)
		return(0);
	f = open(wtmpf, 1);
	if (f >= 0) {
		lseek(f, 0L, 2);
		SCPYN(wtmp.ut_line, "~");
		SCPYN(wtmp.ut_name, "reboot");
		if (time0) {
			wtmp.ut_time = time0;
			time0 = 0;
		} else
			time(&wtmp.ut_time);
		write(f, (char *)&wtmp, sizeof(wtmp));
		close(f);
	}
	return(1);
}

setmerge()
{

	signal(SIGHUP, setmerge);
	mergflag = 1;
}

multiple()
{
	register struct tab *p;
	register pid;

loop:
	mergflag = 0;
	signal(SIGHUP, setmerge);
	for(EVER) {
		pid = wait((int *)0);
		if(mergflag) {
			merge();
			goto loop;
		}
		if(pid == -1)
			return;
		for(ALL)
			if(p->pid == pid || p->pid == -1) {
				rmut(p);
				dfork(p);
			}
	}
}

term(p)
register struct tab *p;
{

	if(p->pid != 0) {
		rmut(p);
		kill(p->pid, SIGKILL);
	}
	p->pid = 0;
}

rline()
{
	register c, i;

loop:
	c = get();
	if(c < 0)
		return(0);
	if(c == 0)
		goto loop;
	line.flag = c;
	c = get();
	if(c <= 0)
		goto loop;
	line.comn = c;
	SCPYN(line.line, "");
	for (i=0; i<LINSIZ; i++) {
		c = get();
		if(c <= 0)
			break;
		line.line[i] = c;
	}
	while(c > 0)
		c = get();
	if(line.line[0] == 0)
		goto loop;
	if(line.flag == '0')
		goto loop;
	strcpy(tty, dev);
	strncat(tty, line.line, LINSIZ);
	if(access(tty, 06) < 0)
		goto loop;
	return(1);
}

get()
{
	char b;

	if(read(fi, &b, 1) != 1)
		return(-1);
	if(b == '\n')
		return(0);
	return(b);
}

#define	FOUND	1
#define	CHANGE	2

merge()
{
	register struct tab *p;

	fi = open(ifile, 0);
	if(fi < 0)
		return;
	for(ALL)
		p->xflag = 0;
	while(rline()) {
		for(ALL) {
			if (SCMPN(p->line, line.line))
				continue;
			p->xflag |= FOUND;
			if(line.comn != p->comn) {
				p->xflag |= CHANGE;
				p->comn = line.comn;
			}
			goto contin1;
		}
		for(ALL) {
			if(p->line[0] != 0)
				continue;
			SCPYN(p->line, line.line);
			p->xflag |= FOUND|CHANGE;
			p->comn = line.comn;
			goto contin1;
		}
	contin1:
		;
	}
	close(fi);
	for(ALL) {
		if((p->xflag&FOUND) == 0) {
			term(p);
			p->line[0] = 0;
		}
		if((p->xflag&CHANGE) != 0) {
			term(p);
			dfork(p);
		}
	}
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
	} else {
		if (p->gettycnt >= 5) {
			dowait = 1;
			p->gettytime = t;
			p->gettycnt = 1;
		}
	}
	pid = fork();
	if(pid == 0) {
		int oerrno, f;
		extern int errno;

		signal(SIGTERM, SIG_DFL);
		signal(SIGHUP, SIG_IGN);
		if (dowait) {
			f = open("/dev/console", 1);
			write(f, "init: ", 6);
			write(f, tty, strlen(tty));
			write(f, ": getty failing, sleeping\n\r", 27);
			close(f);
			sleep(30);
			if ((f = open("/dev/tty", 2)) >= 0) {
				ioctl(f, TIOCNOTTY, 0);
				close(f);
			}
		}
		strcpy(tty, dev);
		strncat(tty, p->line, LINSIZ);
		chown(tty, 0, 0);
		chmod(tty, 0622);
		if (open(tty, 2) < 0) {
			int repcnt = 0;
			do {
				oerrno = errno;
				if (repcnt % 10 == 0) {
					f = open("/dev/console", 1);
					write(f, "init: ", 6);
					errno = oerrno, perror(tty);
					close(f);
					if ((f = open("/dev/tty", 2)) >= 0) {
						ioctl(f, TIOCNOTTY, 0);
						close(f);
					}
				}
				repcnt++;
				sleep(60);
			} while (open(tty, 2) < 0);
			exit(0);	/* have wrong control tty, start over */
		}
		vhangup();
		signal(SIGHUP, SIG_DFL);
		open(tty, 2);
		close(0);
		dup(1);
		dup(0);
		tty[0] = p->comn;
		tty[1] = 0;
		execl(getty, minus, tty, (char *)0);
		exit(0);
	}
	p->pid = pid;
}

rmut(p)
register struct tab *p;
{
	register f;
	int found = 0;

	f = open(utmp, 2);
	if(f >= 0) {
		while(read(f, (char *)&wtmp, sizeof(wtmp)) == sizeof(wtmp)) {
			if (SCMPN(wtmp.ut_line, p->line) || wtmp.ut_name[0]==0)
				continue;
			lseek(f, -(long)sizeof(wtmp), 1);
			SCPYN(wtmp.ut_name, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			found++;
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, 1);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, p->line);
			SCPYN(wtmp.ut_name, "");
			time(&wtmp.ut_time);
			lseek(f, (long)0, 2);
			write(f, (char *)&wtmp, sizeof(wtmp));
			close(f);
		}
	}
}

reset()
{
	longjmp(sjbuf, 1);
}

idle()
{
	register struct tab *p;
	register pid;

	signal(SIGTSTP, idle);
	for (;;) {
		pid = wait((int *) 0);
		if (mergflag)
			return;
		if (pid == -1)
			pause();
		else {
			for (ALL)
				if (p->pid == pid) {
					rmut(p);
					p->pid = -1;
				}
		}
	}
}
