/*-
 * Copyright (c) 1980, 1987, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987, 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)login.c	5.64 (Berkeley) %G%";
#endif /* not lint */

/*
 * login [ name ]
 * login -r hostname	(for rlogind)
 * login -h hostname	(for telnetd, etc.)
 * login -f name	(for pre-authenticated login: datakit, xterm, etc.)
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/file.h>
#include <sgtty.h>

#include <utmp.h>
#include <signal.h>
#include <errno.h>
#include <ttyent.h>
#include <syslog.h>
#include <grp.h>
#include <pwd.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include <tzfile.h>
#include "pathnames.h"

#define	TTYGRPNAME	"tty"		/* name of group to own ttys */

/*
 * This bounds the time given to login.  Not a define so it can
 * be patched on machines where it's too small.
 */
int	timeout = 300;
#ifdef KERBEROS
int	notickets = 1;
#endif

struct	passwd *pwd;
int	failures;
char	term[64], *envinit[1], *hostname, *username, *tty;

char *months[] =
	{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
	  "Sep", "Oct", "Nov", "Dec" };

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg, **environ;
	struct timeval tp;
	struct tm *ttp;
	struct timeval tp;
	struct group *gr;
	register int ch;
	register char *p;
	int ask, fflag, hflag, pflag, rflag, cnt;
	int quietlog, rval;
	char *domain, *salt, *ttyn;
	char tbuf[MAXPATHLEN + 2], tname[sizeof(_PATH_TTY) + 10];
	char localhost[MAXHOSTNAMELEN];
	char *ctime(), *ttyname(), *stypeof(), *crypt(), *getpass();
	time_t time();
	off_t lseek();
	void timedout();

	(void)signal(SIGALRM, timedout);
	(void)alarm((u_int)timeout);
	(void)signal(SIGQUIT, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)setpriority(PRIO_PROCESS, 0, 0);

	openlog("login", LOG_ODELAY, LOG_AUTH);

	/*
	 * -p is used by getty to tell login not to destroy the environment
	 * -r is used by rlogind to cause the autologin protocol;
 	 * -f is used to skip a second login authentication 
	 * -h is used by other servers to pass the name of the remote
	 *    host to login so that it may be placed in utmp and wtmp
	 */
	domain = NULL;
	if (gethostname(localhost, sizeof(localhost)) < 0)
		syslog(LOG_ERR, "couldn't get local hostname: %m");
	else
		domain = index(localhost, '.');

	fflag = hflag = pflag = rflag = 0;
	uid = getuid();
	while ((ch = getopt(argc, argv, "fh:pr:")) != EOF)
		switch (ch) {
		case 'f':
			if (rflag) {
				fprintf(stderr,
				    "login: only one of -r and -f allowed.\n");
				exit(1);
			}
			fflag = 1;
			break;
		case 'h':
			if (uid) {
				(void)fprintf(stderr,
				    "login: -h for super-user only.\n");
				exit(1);
			}
			if (rflag) {
				fprintf(stderr,
				    "login: only one of -r and -h allowed.\n");
				exit(1);
			}
			hflag = 1;
			if (domain && (p = index(optarg, '.')) &&
			    strcasecmp(p, domain) == 0)
				*p = 0;
			hostname = optarg;
			break;
		case 'p':
			pflag = 1;
			break;
		case 'r':
			if (hflag || fflag) {
				fprintf(stderr,
				    "login: -f and -h not allowed with -r.\n");
				exit(1);
			}
			if (getuid()) {
				fprintf(stderr,
				    "login: -r for super-user only.\n");
				exit(1);
			}
			/* "-r hostname" must be last args */
			if (optind != argc) {
				fprintf(stderr, "Syntax error.\n");
				exit(1);
			}
			rflag = 1;
			passwd_req = (doremotelogin(optarg) == -1);
			if (domain && (p = index(optarg, '.')) &&
			    !strcmp(p, domain))
				*p = '\0';
			hostname = optarg;
			break;
		case '?':
		default:
			if (!uid)
				syslog(LOG_ERR, "invalid flag %c", ch);
			(void)fprintf(stderr,
			    "usage: login [-fp] [username]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;
	if (*argv) {
		username = *argv;
		if (strlen(username) > UT_NAMESIZE)
			username[UT_NAMESIZE] = '\0';
		ask = 0;
	} else
		ask = 1;
	if (rflag)
		ask = 0;

	for (cnt = getdtablesize(); cnt > 2; cnt--)
		close(cnt);

	ttyn = ttyname(0);
	if (ttyn == NULL || *ttyn == '\0') {
		(void)sprintf(tname, "%s??", _PATH_TTY);
		ttyn = tname;
	}
	if (tty = rindex(ttyn, '/'))
		++tty;
	else
		tty = ttyn;

	for (cnt = 0;; ask = 1) {
		if (ask) {
			fflag = 0;
			getloginname();
		}
		/*
		 * Note if trying multiple user names; log failures for
		 * previous user name, but don't bother logging one failure
		 * for nonexistent name (mistyped username).
		 */
		if (failures && strcmp(tbuf, username)) {
			if (failures > (pwd ? 0 : 1))
				badlogin(tbuf);
			failures = 0;
		}
		(void)strcpy(tbuf, username);

		if (pwd = getpwnam(username))
			salt = pwd->pw_passwd;
		else
			salt = "xx";

		/*
		 * if we have a valid account name, and it doesn't have a
		 * password, or the -f option was specified and the caller
		 * is root or the caller isn't changing their uid, don't
		 * authenticate.
		 */
		if (pwd && (*pwd->pw_passwd == '\0' ||
		    fflag && (uid == 0 || uid == pwd->pw_uid)))
			break;
		fflag = 0;

		/*
		 * If trying to log in as root, but with insecure terminal,
		 * refuse the login attempt.
		 */
		if (pwd && pwd->pw_uid == 0 && !rootterm(tty)) {
			(void)fprintf(stderr,
			    "%s login refused on this terminal.\n",
			    pwd->pw_name);
			if (hostname)
				syslog(LOG_NOTICE,
				    "LOGIN %s REFUSED FROM %s ON TTY %s",
				    pwd->pw_name, hostname, tty);
			else
				syslog(LOG_NOTICE,
				    "LOGIN %s REFUSED ON TTY %s",
				     pwd->pw_name, tty);
			continue;
		}

		(void)setpriority(PRIO_PROCESS, 0, -4);

		p = getpass("Password:");

		if (pwd) {
		bzero(p, strlen(p));

		(void)setpriority(PRIO_PROCESS, 0, 0);

		if (pwd && !rval)
			break;

		(void)printf("Login incorrect\n");
		failures++;
		/* we allow 10 tries, but after 3 we start backing off */
		if (++cnt > 3) {
			if (cnt >= 10) {
				badlogin(username);
				sleepexit(1);
			}
			sleep((u_int)((cnt - 3) * 5));
		}
	}

	/* committed to login -- turn off timeout */
	(void)alarm((u_int)0);

	/* paranoia... */
	endpwent();

	/* if user not super-user, check for disabled logins */
	if (pwd->pw_uid)
		checknologin();

	if (chdir(pwd->pw_dir) < 0) {
		(void)printf("No directory %s!\n", pwd->pw_dir);
		if (chdir("/"))
			exit(0);
		pwd->pw_dir = "/";
		(void)printf("Logging in with home = \"/\".\n");
	}
#define	TWOWEEKS	(14*24*60*60)
	if (pwd->pw_change || pwd->pw_expire)
		(void)gettimeofday(&tp, (struct timezone *)NULL);
	if (pwd->pw_change)
		if (tp.tv_sec >= pwd->pw_change) {
			printf("Sorry -- your password has expired.\n");
			sleepexit(1);
		}
		else if (tp.tv_sec - pwd->pw_change < TWOWEEKS) {
			ttp = localtime(&pwd->pw_change);
			printf("Warning: your password expires on %s %d, 19%d\n",
			    months[ttp->tm_mon], ttp->tm_mday, ttp->tm_year);
		}
	if (pwd->pw_expire)
		if (tp.tv_sec >= pwd->pw_expire) {
			printf("Sorry -- your account has expired.\n");
			sleepexit(1);
		}
		else if (tp.tv_sec - pwd->pw_expire < TWOWEEKS) {
			ttp = localtime(&pwd->pw_expire);
			printf("Warning: your account expires on %s %d, 19%d\n",
			    months[ttp->tm_mon], ttp->tm_mday, ttp->tm_year);
		}

	if (pwd->pw_change || pwd->pw_expire)
		(void)gettimeofday(&tp, (struct timezone *)NULL);
	if (pwd->pw_change)
		if (tp.tv_sec >= pwd->pw_change) {
			(void)printf("Sorry -- your password has expired.\n");
			sleepexit(1);
		}
		else if (pwd->pw_change - tp.tv_sec <
		    2 * DAYSPERWEEK * SECSPERDAY && !quietlog)
			(void)printf("Warning: your password expires on %s",
			    ctime(&pwd->pw_expire));
	if (pwd->pw_expire)
		if (tp.tv_sec >= pwd->pw_expire) {
			(void)printf("Sorry -- your account has expired.\n");
			sleepexit(1);
		}
		else if (pwd->pw_expire - tp.tv_sec <
		    2 * DAYSPERWEEK * SECSPERDAY && !quietlog)
			(void)printf("Warning: your account expires on %s",
			    ctime(&pwd->pw_expire));

	/* nothing else left to fail -- really log in */
	{
		struct utmp utmp;

		bzero((void *)&utmp, sizeof(utmp));
		(void)time(&utmp.ut_time);
		strncpy(utmp.ut_name, username, sizeof(utmp.ut_name));
		if (hostname)
			strncpy(utmp.ut_host, hostname, sizeof(utmp.ut_host));
		strncpy(utmp.ut_line, tty, sizeof(utmp.ut_line));
		login(&utmp);
	}

	dolastlog(quietlog);

	(void)chown(ttyn, pwd->pw_uid,
	    (gr = getgrnam(TTYGRPNAME)) ? gr->gr_gid : pwd->pw_gid);
	(void)chmod(ttyn, 0620);
	(void)setgid(pwd->pw_gid);

	initgroups(username, pwd->pw_gid);

	if (*pwd->pw_shell == '\0')
		pwd->pw_shell = _PATH_BSHELL;

	/* destroy environment unless user has requested preservation */
	if (!pflag)
		environ = envinit;
	(void)setenv("HOME", pwd->pw_dir, 1);
	(void)setenv("SHELL", pwd->pw_shell, 1);
	if (term[0] == '\0')
		strncpy(term, stypeof(tty), sizeof(term));
	(void)setenv("TERM", term, 0);
	(void)setenv("USER", pwd->pw_name, 1);
	(void)setenv("PATH", _PATH_DEFPATH, 0);

	if (tty[sizeof("tty")-1] == 'd')
		syslog(LOG_INFO, "DIALUP %s, %s", tty, pwd->pw_name);
	/* if fflag is on, assume caller/authenticator has logged root login */
	if (pwd->pw_uid == 0 && fflag == 0)
		if (hostname)
			syslog(LOG_NOTICE, "ROOT LOGIN ON %s FROM %s",
			    tty, hostname);
		else
			syslog(LOG_NOTICE, "ROOT LOGIN ON %s", tty);

#ifdef KERBEROS
	if (!quietlog && notickets == 1)
		(void)printf("Warning: no Kerberos tickets issued.\n");
#endif

	if (!quietlog) {
		struct stat st;

		motd();
		(void)sprintf(tbuf, "%s/%s", _PATH_MAILDIR, pwd->pw_name);
		if (stat(tbuf, &st) == 0 && st.st_size != 0)
			(void)printf("You have %smail.\n",
			    (st.st_mtime > st.st_atime) ? "new " : "");
	}

	(void)signal(SIGALRM, SIG_DFL);
	(void)signal(SIGQUIT, SIG_DFL);
	(void)signal(SIGINT, SIG_DFL);
	(void)signal(SIGTSTP, SIG_IGN);

	tbuf[0] = '-';
	strcpy(tbuf + 1, (p = rindex(pwd->pw_shell, '/')) ?
	    p + 1 : pwd->pw_shell);

	if (setlogin(pwd->pw_name) < 0)
		syslog(LOG_ERR, "setlogin() failure: %m");

	/* discard permissions last so can't get killed and drop core */
	(void)setuid(pwd->pw_uid);

	execlp(pwd->pw_shell, tbuf, 0);
	(void)fprintf(stderr, "login: no shell: %s.\n", strerror(errno));
	exit(0);
}

getloginname()
{
	register int ch;
	register char *p;
	static char nbuf[UT_NAMESIZE + 1];

	for (;;) {
		(void)printf("login: ");
		for (p = nbuf; (ch = getchar()) != '\n'; ) {
			if (ch == EOF) {
				badlogin(username);
				exit(0);
			}
			if (p < nbuf + UT_NAMESIZE)
				*p++ = ch;
		}
		if (p > nbuf)
			if (nbuf[0] == '-')
				(void)fprintf(stderr,
				    "login names may not start with '-'.\n");
			else {
				*p = '\0';
				username = nbuf;
				break;
			}
	}
}

void
timedout()
{
	(void)fprintf(stderr, "Login timed out after %d seconds\n", timeout);
	exit(0);
}

rootterm(ttyn)
	char *ttyn;
{
	struct ttyent *t;

	return((t = getttynam(ttyn)) && t->ty_status&TTY_SECURE);
}

jmp_buf motdinterrupt;

motd()
{
	register int fd, nchars;
	sig_t oldint;
	int sigint();
	char tbuf[8192];

	if ((fd = open(_PATH_MOTDFILE, O_RDONLY, 0)) < 0)
		return;
	oldint = signal(SIGINT, sigint);
	if (setjmp(motdinterrupt) == 0)
		while ((nchars = read(fd, tbuf, sizeof(tbuf))) > 0)
			(void)write(fileno(stdout), tbuf, nchars);
	(void)signal(SIGINT, oldint);
	(void)close(fd);
}

sigint()
{
	longjmp(motdinterrupt, 1);
}

checknologin()
{
	register int fd, nchars;
	char tbuf[8192];

	if ((fd = open(_PATH_NOLOGIN, O_RDONLY, 0)) >= 0) {
		while ((nchars = read(fd, tbuf, sizeof(tbuf))) > 0)
			(void)write(fileno(stdout), tbuf, nchars);
		sleepexit(0);
	}
}

dolastlog(quiet)
	int quiet;
{
	struct lastlog ll;
	int fd;
	char *ctime();
	char *ctime();

	if ((fd = open(_PATH_LASTLOG, O_RDWR, 0)) >= 0) {
		(void)lseek(fd, (off_t)pwd->pw_uid * sizeof(ll), L_SET);
		if (!quiet) {
			if (read(fd, (char *)&ll, sizeof(ll)) == sizeof(ll) &&
			    ll.ll_time != 0) {
				(void)printf("Last login: %.*s ",
				    24-5, (char *)ctime(&ll.ll_time));
				if (*ll.ll_host != '\0')
					(void)printf("from %.*s\n",
					    sizeof(ll.ll_host), ll.ll_host);
				else
					(void)printf("on %.*s\n",
					    sizeof(ll.ll_line), ll.ll_line);
			}
			(void)lseek(fd, (off_t)pwd->pw_uid * sizeof(ll), L_SET);
		}
		bzero((void *)&ll, sizeof(ll));
		(void)time(&ll.ll_time);
		strncpy(ll.ll_line, tty, sizeof(ll.ll_line));
		if (hostname)
			strncpy(ll.ll_host, hostname, sizeof(ll.ll_host));
		(void)write(fd, (char *)&ll, sizeof(ll));
		(void)close(fd);
	}
}

badlogin(name)
	char *name;
{
	if (failures == 0)
		return;
	if (hostname) {
		syslog(LOG_NOTICE, "%d LOGIN FAILURE%s FROM %s",
		    failures, failures > 1 ? "S" : "", hostname);
		syslog(LOG_AUTHPRIV|LOG_NOTICE,
		    "%d LOGIN FAILURE%s FROM %s, %s",
		    failures, failures > 1 ? "S" : "", hostname, name);
	} else {
		syslog(LOG_NOTICE, "%d LOGIN FAILURE%s ON %s",
		    failures, failures > 1 ? "S" : "", tty);
		syslog(LOG_AUTHPRIV|LOG_NOTICE,
		    "%d LOGIN FAILURE%s ON %s, %s",
		    failures, failures > 1 ? "S" : "", tty, name);
	}
}

#undef	UNKNOWN
#define	UNKNOWN	"su"

char *
stypeof(ttyid)
	char *ttyid;
{
	struct ttyent *t;

	return(ttyid && (t = getttynam(ttyid)) ? t->ty_type : UNKNOWN);
}

sleepexit(eval)
	int eval;
{
	sleep((u_int)5);
	exit(eval);
}

doremotelogin(host)
	char *host;
{
	static char lusername[UT_NAMESIZE+1];
	char rusername[UT_NAMESIZE+1];

	getstr(rusername, sizeof(rusername), "remuser");
	getstr(lusername, sizeof(lusername), "locuser");
	getstr(term, sizeof(term), "Terminal type");
	username = lusername;
	pwd = getpwnam(username);
	if (pwd == NULL)
		return(-1);
	return(ruserok(host, (pwd->pw_uid == 0), rusername, username));
}

char *speeds[] = {
	"0", "50", "75", "110", "134", "150", "200", "300", "600",
	"1200", "1800", "2400", "4800", "9600", "19200", "38400",
};
#define	NSPEEDS	(sizeof(speeds) / sizeof(speeds[0]))

doremoteterm(tp)
	struct sgttyb *tp;
{
	register char *cp = index(term, '/'), **cpp;
	char *speed;

	if (cp) {
		*cp++ = '\0';
		speed = cp;
		cp = index(speed, '/');
		if (cp)
			*cp++ = '\0';
		for (cpp = speeds; cpp < &speeds[NSPEEDS]; cpp++)
			if (strcmp(*cpp, speed) == 0) {
				tp->sg_ispeed = tp->sg_ospeed = cpp-speeds;
				break;
			}
	}
	tp->sg_flags = ECHO|CRMOD|ANYP|XTABS;
}
