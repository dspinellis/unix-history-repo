/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)sliplogin.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * sliplogin.c
 * [MUST BE RUN SUID, SLOPEN DOES A SUSER()!]
 *
 * This program initializes its own tty port to be an async TCP/IP interface.
 * It sets the line discipline to slip, invokes a shell script to initialize
 * the network interface, then pauses forever waiting for hangup.
 *
 * It is a remote descendant of several similar programs with incestuous ties:
 * - Kirk Smith's slipconf, modified by Richard Johnsson @ DEC WRL.
 * - slattach, probably by Rick Adams but touched by countless hordes.
 * - the original sliplogin for 4.2bsd, Doug Kingston the mover behind it.
 *
 * There are two forms of usage:
 *
 * "sliplogin"
 * Invoked simply as "sliplogin" and a realuid != 0, the program looks up
 * the uid in /etc/passwd, and then the username in the file /etc/hosts.slip.
 * If and entry is found, the line on fd0 is configured for SLIP operation
 * as specified in the file.
 *
 * "sliplogin IPhost1 </dev/ttyb"
 * Invoked by root with a username, the name is looked up in the
 * /etc/hosts.slip file and if found fd0 is configured as in case 1.
 */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <sys/syslog.h>
#include <netdb.h>

#if defined(BSD4_4)
#define TERMIOS
#endif
#ifdef TERMIOS
#include <sys/termios.h>
#include <ttyent.h>
#endif
#include <netinet/in.h>
#include <net/if.h>
#include <net/if_slvar.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include "pathnames.h"

int	unit;
int	slip_mode;
int	speed;
char	loginargs[BUFSIZ];
char	loginfile[BUFSIZ];
char	logoutfile[BUFSIZ];
char	loginname[BUFSIZ];

struct slip_modes {
	char	*sm_name;
	int	sm_value;
}	 modes[] = {
	"normal",	0,              
	"compress",	SC_COMPRESS,   
	"noicmp",	SC_NOICMP,
	"autocomp",	SC_AUTOCOMP
};

void
findid(name)
	char *name;
{
	FILE *fp;
	static char slopt[5][16];
	static char laddr[16];
	static char raddr[16];
	static char mask[16];
	char user[16];
	int i, j, n;

	(void)strcpy(loginname, name);
	if ((fp = fopen(_PATH_ACCESS, "r")) == NULL) {
		(void)fprintf(stderr, "sliplogin: %s: %s\n",
		    _PATH_ACCESS, strerror(errno));
		syslog(LOG_ERR, "%s: %m\n", _PATH_ACCESS);
		exit(1);
	}
	while (fgets(loginargs, sizeof(loginargs) - 1, fp)) {
		if (ferror(fp))
			break;
		n = sscanf(loginargs, "%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s\n",
                        user, laddr, raddr, mask, slopt[0], slopt[1], 
			slopt[2], slopt[3], slopt[4]);
		if (user[0] == '#' || isspace(user[0]))
			continue;
		if (strcmp(user, name) != 0)
			continue;

		slip_mode = 0;
		for (i = 0; i < n - 4; i++) {
			for (j = 0; j < sizeof(modes)/sizeof(struct slip_modes);
				j++) {
				if (strcmp(modes[j].sm_name, slopt[i]) == 0) {
					slip_mode |= modes[j].sm_value;
					break;
				}
			}
		}

		/*
		 * we've found the guy we're looking for -- see if
		 * there's a login file we can use.  First check for
		 * one specific to this host.  If none found, try for
		 * a generic one.
		 */
		(void)sprintf(loginfile, "%s.%s", _PATH_LOGIN, name);
		if (access(loginfile, R_OK|X_OK)) {
			(void)strcpy(loginfile, _PATH_LOGIN);
			(void)strcpy(logoutfile, _PATH_LOGOUT);
			if (access(loginfile, R_OK|X_OK)) {
				fputs("access denied - no login file\n",
				      stderr);
				syslog(LOG_ERR,
				       "access denied for %s - no %s\n",
				       name, _PATH_LOGIN);
				exit(5);
			}
		} else
			(void)sprintf(logoutfile, "%s.%s", _PATH_LOGOUT, name);

		(void) fclose(fp);
		return;
	}
	(void)fprintf(stderr, "SLIP access denied for %s\n", name);
	syslog(LOG_ERR, "SLIP access denied for %s\n", name);
	exit(4);
	/* NOTREACHED */
}

char *
sigstr(s)
	int s;
{
	static char buf[32];

	switch (s) {
	case SIGHUP:	return("HUP");
	case SIGINT:	return("INT");
	case SIGQUIT:	return("QUIT");
	case SIGILL:	return("ILL");
	case SIGTRAP:	return("TRAP");
	case SIGIOT:	return("IOT");
	case SIGEMT:	return("EMT");
	case SIGFPE:	return("FPE");
	case SIGKILL:	return("KILL");
	case SIGBUS:	return("BUS");
	case SIGSEGV:	return("SEGV");
	case SIGSYS:	return("SYS");
	case SIGPIPE:	return("PIPE");
	case SIGALRM:	return("ALRM");
	case SIGTERM:	return("TERM");
	case SIGURG:	return("URG");
	case SIGSTOP:	return("STOP");
	case SIGTSTP:	return("TSTP");
	case SIGCONT:	return("CONT");
	case SIGCHLD:	return("CHLD");
	case SIGTTIN:	return("TTIN");
	case SIGTTOU:	return("TTOU");
	case SIGIO:	return("IO");
	case SIGXCPU:	return("XCPU");
	case SIGXFSZ:	return("XFSZ");
	case SIGVTALRM:	return("VTALRM");
	case SIGPROF:	return("PROF");
	case SIGWINCH:	return("WINCH");
#ifdef SIGLOST
	case SIGLOST:	return("LOST");
#endif
	case SIGUSR1:	return("USR1");
	case SIGUSR2:	return("USR2");
	}
	(void)sprintf(buf, "sig %d", s);
	return(buf);
}

int
hup_handler(s)
	int s;
{
	if (access(logoutfile, R_OK|X_OK) == 0) {
		char logincmd[2*BUFSIZ+32];

		(void)sprintf(logincmd, "%s %d %d %s", logoutfile, unit, speed,
			      loginargs);
		(void)system(logincmd);
	}
	(void)close(0);
	syslog(LOG_INFO, "closed %s slip unit %d (%s)\n", loginname, unit,
	       sigstr(s));
	exit(1);
	/* NOTREACHED */
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int	fd, s, ldisc, odisc;
	char *name;
#ifdef TERMIOS
	struct	termios tios, otios;
#else
	struct	sgttyb tty, otty;
#endif
	char logincmd[2*BUFSIZ+32];
	extern uid_t getuid();

	if ((name = strrchr(argv[0], '/')) == NULL)
		name = argv[0];
	s = getdtablesize();
	for (fd = 3 ; fd < s ; fd++)
		(void) close(fd);
	openlog(name, LOG_PID, LOG_DAEMON);
	if (argc > 1) {
		if (argc > 2) {
			(void)fprintf(stderr, "Usage: %s loginname\n", argv[0]);
			exit(1);
		}
		findid(argv[1]);

		/*
		 * Disassociate from current controlling terminal, if any,
		 * and ensure that the slip line is our controlling terminal.
		 */
#ifdef TERMIOS
		(void) setsid();
		(void) ioctl(0, TIOCSCTTY, (caddr_t)0);
#else
		if ((fd = open("/dev/tty", O_RDONLY, 0)) >= 0) {
			extern char *ttyname();

			(void) ioctl(fd, TIOCNOTTY, (caddr_t)0);
			(void) close(fd);
			/* open slip tty again to acquire as controlling tty? */
			fd = open(ttyname(0), O_RDWR, 0);
			if (fd >= 0)
				(void) close(fd);
		}
		(void) setpgrp(0, getpid());
#endif
	} else {
		extern char *getenv();

		if ((name = getenv("USER")) == NULL) {
			(void) fprintf(stderr, "access denied - no username\n");
			syslog(LOG_ERR, "access denied - no username\n");
			exit(1);
		}
		findid(name);
	}
	(void) fchmod(0, 0600);
	(void) fprintf(stderr, "starting slip login for %s\n", loginname);
#ifdef TERMIOS
	/* set up the line parameters */
	if (ioctl(0, TIOCGETA, (caddr_t)&tios) < 0) {
		syslog(LOG_ERR, "ioctl (TIOCGETA): %m");
		exit(1);
	}
	otios = tios;
	tios.c_cflag = CS8|CREAD|HUPCL;
	tios.c_iflag = IGNBRK;
	tios.c_oflag = tios.c_lflag = 0;
	if (ioctl(0, TIOCSETA, (caddr_t)&tios) < 0) {
		syslog(LOG_ERR, "ioctl (TIOCSETA) (1): %m");
		exit(1);
	}
#else
	/* set up the line parameters */
	if (ioctl(0, TIOCGETP, (caddr_t)&tty) < 0) {
		syslog(LOG_ERR, "ioctl (TIOCGETP): %m");
		exit(1);
	}
	otty = tty;
	speed = tty.sg_ispeed;
	tty.sg_flags = RAW | ANYP;
	if (ioctl(0, TIOCSETP, (caddr_t)&tty) < 0) {
		syslog(LOG_ERR, "ioctl (TIOCSETP): %m");
		exit(1);
	}
#endif
	/* find out what ldisc we started with */
	if (ioctl(0, TIOCGETD, (caddr_t)&odisc) < 0) {
		syslog(LOG_ERR, "ioctl(TIOCGETD) (1): %m");
		exit(1);
	}
	ldisc = SLIPDISC;
	if (ioctl(0, TIOCSETD, (caddr_t)&ldisc) < 0) {
		syslog(LOG_ERR, "ioctl(TIOCSETD): %m");
		exit(1);
	}
	/* find out what unit number we were assigned */
	if (ioctl(0, TIOCGETD, (caddr_t)&unit) < 0) {
		syslog(LOG_ERR, "ioctl (TIOCGETD) (2): %m");
		exit(1);
	}
	(void) signal(SIGHUP, hup_handler);
	(void) signal(SIGTERM, hup_handler);

	syslog(LOG_INFO, "attaching slip unit %d for %s\n", unit, loginname);
	(void)sprintf(logincmd, "%s %d %d %s", loginfile, unit, speed,
		      loginargs);
	/*
	 * aim stdout and errout at /dev/null so logincmd output won't
	 * babble into the slip tty line.
	 */
	(void)close(1);
	if ((fd = open("/dev/null", O_WRONLY, 0)) != 1) {
		if (fd < 0) {
			syslog(LOG_ERR, "open /dev/null: %m");
			exit(1);
		}
		(void)dup2(fd, 1);
		(void)close(fd);
	}
	(void)dup2(1,2);
	if (s = system(logincmd)) {
		syslog(LOG_ERR, "%s login failed: exit status %d from %s",
		       loginname, s, loginfile);
		(void) ioctl(0, TIOCSETD, (caddr_t)&odisc);
		exit(6);
	}
	if (ioctl(0, SLIOCSFLAGS, (caddr_t)&slip_mode) < 0) {
		syslog(LOG_ERR, "ioctl (SLIOCSFLAGS): %m");
		exit(1);
	}

	/* twiddle thumbs until we get a signal */
	while (1)
		sigpause(0);

	/* NOTREACHED */
}
