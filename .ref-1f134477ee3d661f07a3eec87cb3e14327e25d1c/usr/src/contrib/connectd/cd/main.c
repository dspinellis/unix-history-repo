/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "main.h"
#include <sys/time.h>
#include <sys/ioctl.h>

#include <signal.h>
#include <sys/types.h>
#include <utmp.h>
#include <setjmp.h>
#include <sys/reboot.h>
#include <errno.h>
#include <sys/file.h>
#include <ttyent.h>
#include <sys/syslog.h>
#include <sys/stat.h>

#define	LINSIZ	sizeof(wtmp.ut_line)
#define	CMDSIZ	200	/* max string length for getty or window command*/
#define	ALL	p = itab; p ; p = p->next
#define	EVER	;;
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

char	shell[]	= "/bin/sh";
char	minus[]	= "-";
char	runc[]	= "/etc/rc";
char	utmpf[]	= "/etc/utmp";
char	wtmpf[]	= "/usr/adm/wtmp";
char	ctty[]	= "/dev/console";


struct utmp wtmp;
struct	tab
{
	char	line[LINSIZ];
	char	comn[CMDSIZ];
	int		baud;		/* outgoing baud rate */
	int		sock;		/* socket if outgoing connected */
	int		fd;		/* file desc if we have, else -1 */
	int		errfd;		/* error file desc if we have, else -1 */
	char	xflag;
	int		gpid;		/* pid of getty or connector */
	time_t	gettytime;
	int	gettycnt;
	struct	tab *next;
} *itab;

char	tty3[LINSIZ] = { "tty3" } ;
char	tty2[LINSIZ] = { "tty2" } ;
int	fi;
int	mergflag;
char	tty[20];
jmp_buf	sjbuf, shutpass;
time_t	time0;

int	reset();
int	idle();
char	*strcpy(), *strcat();
long	lseek();

struct conversation  convers[MAXCONNECTS] ;
jmp_buf terminate;

fd_set rd_fdset, wr_fdset ;
int term(), sigpipe();
int debug;
extern int errno;
struct timeval tv_2th = { 0, 500000 } ;
main (argc,argv) char *argv[]; {
	int sock, msgsock, rval, rv ;
	struct sockaddr_un rqsts;
	int err;
	struct iovec iov[4];
	int constatus, rqst, resp;
	int optlen ;
	char *optp;
	struct connectdomain *cdp;
	int i,n,afd ;
	struct conversation *cd ;
	struct tab *p ;

	/* initialize */
	n = 0;
	FD_ZERO(&rd_fdset) ;
	FD_ZERO(&wr_fdset) ;
	signal (SIGTTOU, SIG_IGN) ;
	signal (SIGTTIN, SIG_IGN) ;
	signal (SIGINT, term) ;
	signal (SIGTERM, term) ;
	signal (SIGPIPE, sigpipe) ;
	if(setjmp(terminate)) goto on_error;
	openlog("connectd", LOG_CONS|LOG_ODELAY, LOG_AUTH);
	/* disassociate from tty pgrp */
	n = open ("/dev/tty", 2) ;
	ioctl(n,TIOCNOTTY, 0) ;
	close(n) ;

	/* make incoming request socket */
	sock = socket (AF_UNIX, SOCK_STREAM, 0) ;
	if (sock < 0) {
		perror("stream socket") ;
		exit(1) ;
	}
	rqsts.sun_family = AF_UNIX ;
	strcpy (rqsts.sun_path,"/dev/connect") ;
	if (bind (sock, &rqsts, sizeof (rqsts))) {
		perror ("bind /dev/connect") ;
		exit (1) ;
	}

	/* debugging ? */
	if ((argc <= 1) || strcmp(argv[1],"-d") != 0) {
		if (fork()) exit(0) ;
		close (0); close (1); close (2);
	} else debug = 1;

	/* build tables */
	itab = (struct tab *)calloc(1, sizeof(*itab));
	SCPYN(itab->line, tty3);
	itab->fd = -1;	/* do we have a fd open */
	itab->sock = -1; /* does someone else have this fd? */
	itab->gpid = 0; /* does getty have this fd? */

	itab->next = (struct tab *)calloc(1, sizeof(*itab));
	itab->next->fd = -1;	/* do we have a fd open */
	itab->next->sock = -1; /* does someone else have this fd? */
	itab->next->gpid = 0; /* does getty else have this fd? */
	SCPYN(itab->next->line, tty2);
	p = itab ;

	/* accept connection requests on socket */
	listen (sock, 5) ;
	FD_SET (sock, &rd_fdset) ;

	/* add requests from other lines */
	for (ALL) openline(p) ;

	/* service requests as they come in */
	for (;;) {
	    int s, ctrl, n;
	    fd_set readable, writeable;

	    readable = rd_fdset;
	    writeable = wr_fdset;
#ifdef notdef
for (i=0; i <20 ; i++) {
if (FD_ISSET(i,&readable)) fprintf(stderr, "rd%d ", i) ;
if (FD_ISSET(i,&writeable)) fprintf(stderr, "wr%d ", i) ;
}
fprintf(stderr, "?\n") ;
#endif
	    if ((n = select(21, &readable, &writeable,
		(fd_set *)0, &tv_2th )) <= 0) {
		    if (n < 0 && errno != EINTR)
				if (debug)
					perror ("select") ;
				else
					syslog(LOG_WARNING, "select: %m\n");
		    sleep(1);
		    continue;
	    }

		/* got a request, see who it is */
fprintf(stderr, "select %d\n", n) ;
for (i=0; i <20 ; i++) {
if (FD_ISSET(i,&readable))
fprintf(stderr, "rdsel%d ", i) ;
if (FD_ISSET(i,&writeable))
fprintf(stderr, "wrsel%d ", i) ;
}
fprintf(stderr, "\n") ;

		/* have we a new connection request? */
	    if (FD_ISSET(sock, &readable)) {
		msgsock = accept(sock, 0, 0) ;
		if (msgsock == -1) {
			perror ("accept") ;
			continue ;
		}
/*allocate a connection */
		convers[msgsock].co_sock = msgsock ;
		FD_SET (msgsock, &rd_fdset) ;
fprintf(stderr, "accept %d\n", msgsock) ;
	    }
		/* have we a incoming request */
for (p = itab; p ; p = p->next)
	    if (FD_ISSET(p->fd, &writeable)) {
		/* fork off getty after setting up line */
printf("do a getty on fd%d\n", p->fd) ;
if(p->sock >= 0) {  printf("on a conn?\n") ; continue; }
			i = fork ();
			if (i < 0) {
				perror("cd:fork") ;
				exit(1);
			}
			if (!i) {
				dup2(p->fd, 0);
				dup2(p->fd, 1);
				dup2(p->fd, 2);
				i = getdtablesize();
				for (n=3 ; n < i ; n++) close (n) ;
ioctl(0,TIOCYESTTY, 0) ;
			execl("/etc/getty", "getty", "std.1200", "-", 0) ;
				perror("exec");
				exit(1);
			} else {
p->gpid = i ;
				i = wait(&n) ;
printf("cd: waitgetty %d %d\n", i, n) ;
p->gpid = 0 ;
	closeline (p, 0) ;
rmut(p) ;
	sleep (1) ;
	openline (p) ;
			}
	    } ;
		;
		/* have we an existing socket request */
	    for (cd = convers; cd - convers < MAXCONNECTS ; cd++) {
		cdp = &cd->co_cd ;
	    	if (FD_ISSET(cd->co_sock, &readable)) {
fprintf(stderr, "recv %d\n", cd->co_sock) ;
			/* recieve connnection request message */
			rqst = rcvrequest(cd->co_sock, cd, &optp,
					&optlen, &afd) ;

/*fprintf(stderr, "rqst %d\n", rqst) ;*/
			if (rqst < 0) goto end_session ;

/*printf("cd:request %d ", rqst) ;*/

			/* process request */
			switch (rqst) {
		case CDNEWREQUEST:
				cd->co_rqst = rqst ;
/*printf("cd_family %d, cd_address %s, cd_alen %d\n",
	cdp->cd_family, cdp->cd_address, cdp->cd_alen) ;*/
/*if (optlen)
	printf("option:%s\n", optp);*/
			if (afd>= 0) {
				cd->co_errfd = afd ;
			} else cd->co_errfd = -1 ;

			cd->co_constatus = -1;
for (p = itab; p ; p = p->next)
	if (p->gpid == 0 && p->fd >= 0 && p->sock < 0) break;
if (!p) exit(0); /* return error can't find a line */
fprintf(stderr, "allocate fd%d line %s\n", p->fd, p->line) ;
ioctl(p->fd, TIOCSIGNCAR, 0) ;
p->errfd = cd->co_errfd;
p->sock = cd->co_sock ;
			i = fork ();
			if (i < 0) {
				perror("cd:fork") ;
				exit(1);
			}
			if (!i) {
				dup2(p->fd, 0);
				dup2(p->fd, 1);
				if (cd->co_errfd) dup2(cd->co_errfd,2);
				else close(2) ;
				i = getdtablesize();
				for (n=3 ; n < i ; n++) close (n) ;
				execl("con", "con", cdp->cd_address, 0) ;
				perror("exec");
				exit(1);
			} else {
				cd->co_pid = i ;
				i = wait(&n) ;
/*printf("cd: wait %d %d\n", i, n) ;*/
				if (n == 0) cd->co_constatus = n;
				else	{
	fprintf(stderr, "cd: con fails status %d\n", n) ;
					cd->co_constatus = (-1 <<16) | n ;
					closeline (p, 0) ;
				}
			}
if (p->fd >= 0) {
	fprintf(stderr, "cd: sending fd %d \n", p->fd) ;
	ioctl(p->fd, TIOCCIGNCAR, 0) ;
}
optlen = 0;

			resp = CDNEWRESPONSE ;
		/* send connnection response message */
		err = sendrequest(cd->co_sock, resp, cd, optp, optlen, p->fd) ;
		if(p->fd >= 0) closeline (p, 1) ;
		if (cd->co_constatus) goto end_session ;
		break ;

	case CDFINISHREQUEST:
for (p = itab; p ; p = p->next)
	if (p->sock == cd->co_sock) break;
if(!p) exit(0); /* return no such connection */
		p->fd = afd ;
fprintf(stderr, "cd: received fd %d \n", p->fd) ;
			cd->co_constatus = -1;
ioctl(p->fd, TIOCSIGNCAR, 0) ;
			i = fork ();
			if (i < 0) {
				perror("cd:fork") ;
				exit(1);
			}
			if (!i) {
				dup2(p->fd, 0);
				dup2(p->fd, 1);
				if (cd->co_errfd <= 0) dup2(cd->co_errfd,2);
				else close(2) ;
				i = getdtablesize();
				for (n=3 ; n < i ; n++) close (n) ;
				execl("con", "con", "drop" , 0) ;
				perror("exec");
				exit(1);
			} else {
				cd->co_pid = i ;
				i = wait(&n) ;
fprintf(stderr,"cd: wait %d %d\n", i, n) ;
				if (n == 0) cd->co_constatus = n;
				else	cd->co_constatus = (-1 <<16) | n ;
			}
fprintf(stderr,"cd: dropped \n") ;

		cd->co_rqst = resp = CDFINISHRESPONSE ;
		/* send connnection response message */
		err = sendrequest(cd->co_sock, resp, cd, 0, 0, 0) ;
		goto end_session;
		} 	/* end of switch */
		} ;  continue; 	/* end of if */

	end_session:
/*fprintf(stderr, "end_session\n") ;*/
		close (cd->co_errfd) ;
		FD_CLR (cd->co_sock, &rd_fdset) ;
		close (cd->co_sock) ;
		cd->co_sock = 0 ;
		if (p->fd >= 0) closeline (p, 0) ;
		sleep(1) ;
		openline (p) ;
	} /* end of conv for */
	}	/*end of foerever */
on_error:
	close (sock) ;
	unlink ("/dev/connect") ;
}

term() { struct tab *p;

	for (p = itab ; p ; p = p->next)
		if (p->gpid) kill (p->gpid, SIGHUP);
	longjmp (terminate);
}

sigpipe() { printf("SIGPIPE\n") ; fflush (stdout) ; longjmp (terminate); }

/*
 * open and prepare line for connectd
 */
openline(p) struct tab *p; {
	char ttyn[32]; int n;
	
	/* open the bastard */
	strcpy (ttyn, "/dev/");
	strcat (ttyn, p->line) ;
	p->fd = open (ttyn, O_RDWR|O_NDELAY) ;

	/* disassociate from our pgrp */
	n = open ("/dev/tty", O_RDWR|O_NDELAY) ;
	ioctl (n, TIOCNOTTY, 0) ;
	close(n) ;

	/* mark 'em to be watched for carrier */
	FD_SET (p->fd, &wr_fdset) ;

	/* if set in a still open state */
	ioctl(p->fd, TIOCCIGNCAR, 0) ;

	if (debug) fprintf(stderr, "openline: %s: fd %d\n", p->line,  p->fd) ;
}

closeline(p, i) struct tab *p; {

	if (debug) fprintf(stderr, "closeline: %s: fd %d ", p->line,  p->fd) ;

	close (p->fd) ;
	FD_CLR (p->fd, &wr_fdset) ;
	p->fd = -1 ;
	if (i) {
		if (debug) fprintf(stderr, "remove from use\n") ;
		return ;
	}

	if (debug) fprintf(stderr, "entirely\n") ;
	if (p->gpid) kill (p->gpid, SIGKILL); /* no mercy */
	p->gpid = 0 ;

	if(p->sock >= 0) close (p->sock);
	p->sock = -1 ;
}

#ifdef notdef
struct	sigvec rvec = { reset, sigmask(SIGHUP), 0 };


main(argc, argv)
	char **argv;
{
	int howto, oldhowto;

	time0 = time(0);
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
	openlog("connectd", LOG_CONS|LOG_ODELAY, LOG_AUTH);
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
	register struct tab *p, *p1;

	close(creat(utmpf, 0644));
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
	int omask;

	sigvec(SIGHUP, &mvec, (struct sigvec *)0);
	for (EVER) {
		pid = wait((int *)0);
		if (pid == -1)
			return;
		omask = sigblock(SIGHUP);
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

merge()
{
	register struct tab *p;
	register struct ttyent *t;
	register struct tab *p1;

	for (ALL)
		p->xflag = 0;
	setttyent();
	while (t = getttyent()) {
		/* is it init's responsibility?
		if ((t->ty_status & TTY_ON)) continue; */
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
		if (strcmp(t->ty_window, "") != 0) {
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
		sigsetmask(0);	/* since can be called from masked code */
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
#endif
/*
 * Remove utmp entry.
 */
rmut(p)
	register struct tab *p;
{
	register f;
	int found = 0;
	static unsigned utmpsize;
	static struct utmp *utmp;
	register struct utmp *u;
	int nutmp;
	struct stat statbf;

	f = open(utmpf, O_RDWR);
	if (f >= 0) {
		fstat(f, &statbf);
		if (utmpsize < statbf.st_size) {
			utmpsize = statbf.st_size + 10 * sizeof(struct utmp);
			if (utmp)
				utmp = (struct utmp *)realloc(utmp, utmpsize);
			else
				utmp = (struct utmp *)malloc(utmpsize);
			if (!utmp)
				syslog(LOG_ERR, "utmp malloc failed");
		}
		if (statbf.st_size && utmp) {
			nutmp = read(f, utmp, statbf.st_size);
			nutmp /= sizeof(struct utmp);
			for (u = utmp ; u < &utmp[nutmp] ; u++) {
				if (SCMPN(u->ut_line, p->line) ||
				    u->ut_name[0]==0)
					continue;
				lseek(f, ((long)u)-((long)utmp), L_SET);
				SCPYN(u->ut_name, "");
				SCPYN(u->ut_host, "");
				time(&u->ut_time);
				write(f, (char *)u, sizeof(*u));
				found++;
			}
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
#ifdef notdef
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
#endif
