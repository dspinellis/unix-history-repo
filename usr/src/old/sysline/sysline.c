/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)sysline.c	5.18 (Berkeley) %G%";
#endif /* not lint */

/*
 * sysline - system status display on 25th line of terminal
 * j.k.foderaro
 *
 * Prints a variety of information on the special status line of terminals
 * that have a status display capability.  Cursor motions, status commands,
 * etc. are gleamed from /etc/termcap.
 * By default, all information is printed, and flags are given on the command
 * line to disable the printing of information.  The information and
 * disabling flags are:
 *
 *  flag	what
 *  -----	----
 *		time of day
 *		load average and change in load average in the last 5 mins
 *		number of user logged on
 *   -p		# of processes the users owns which are runnable and the
 *		  number which are suspended.  Processes whose parent is 1
 *		  are not counted.
 *   -l		users who've logged on and off.
 *   -m		summarize new mail which has arrived
 *
 *  <other flags>
 *   -r		use non reverse video
 *   -c		turn off 25th line for 5 seconds before redisplaying.
 *   -b		beep once one the half hour, twice on the hour
 *   +N		refresh display every N seconds.
 *   -i		print pid first thing
 *   -e		do simple print designed for an emacs buffer line
 *   -w		do the right things for a window
 *   -h		print hostname between time and load average
 *   -D		print day/date before time of day
 *   -d		debug mode - print status line data in human readable format
 *   -q		quiet mode - don't output diagnostic messages
 *   -s		print Short (left-justified) line if escapes not allowed
 *   -j		Print left Justified line regardless
 */

#define BSD4_2			/* for 4.2 BSD */
#define WHO			/* turn this on always */
#define RWHO			/* 4.1a or greater, with rwho */
#define NEW_BOOTTIME		/* 4.1c or greater */

#define NETPREFIX "ucb"
#define DEFDELAY 60		/* update status once per minute */
/*
 * if MAXLOAD is defined, then if the load average exceeded MAXLOAD
 * then the process table will not be scanned and the log in/out data
 * will not be checked.   The purpose of this is to reduced the load
 * on the system when it is loaded.
 */
#define MAXLOAD 6.0

#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/vtimes.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <fcntl.h>
#include <utmp.h>
#include <nlist.h>
#include <curses.h>
#undef nl
#ifdef TERMINFO
#include <term.h>
#endif
#include <stdarg.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#ifdef RWHO
#include <protocols/rwhod.h>

#define	DOWN_THRESHOLD	(11 * 60)

struct remotehost {
	char *rh_host;
	int rh_file;
} remotehost[10];
int nremotes = 0;
#endif RWHO

#include "pathnames.h"

struct nlist nl[] = {
	{ "_boottime" },
#define	NL_BOOT 0
	{ "_proc" },
#define NL_PROC 1
	{ "_nproc" },
#define NL_NPROC 2
	0
};

	/* stuff for the kernel */
int kmem;			/* file descriptor for _PATH_KMEM */
struct proc *proc, *procNPROC;
int nproc;
int procadr;
double avenrun[3];		/* used for storing load averages */

/*
 * In order to determine how many people are logged on and who has
 * logged in or out, we read in the /etc/utmp file. We also keep track of
 * the previous utmp file.
 */
int ut = -1;			/* the file descriptor */
struct utmp *new, *old;	
char *status;			/* per tty status bits, see below */
int nentries;			/* number of utmp entries */
	/* string lengths for printing */
#define LINESIZE (sizeof old->ut_line)
#define NAMESIZE (sizeof old->ut_name)
/*
 * Status codes to say what has happened to a particular entry in utmp.
 * NOCH means no change, ON means new person logged on,
 * OFF means person logged off.
 */
#define NOCH	0
#define ON	0x1
#define OFF	0x2

#ifdef WHO
char whofilename[100];
char whofilename2[100];
#endif

char hostname[MAXHOSTNAMELEN+1];	/* one more for null termination */
char lockfilename[100];		/* if exists, will prevent us from running */

	/* flags which determine which info is printed */
int mailcheck = 1;	/* m - do biff like checking of mail */
int proccheck = 1;	/* p - give information on processes */
int logcheck = 1; 	/* l - tell who logs in and out */
int hostprint = 0;	/* h - print out hostname */
int dateprint = 0;	/* h - print out day/date */
int quiet = 0;		/* q - hush diagnostic messages */

	/* flags which determine how things are printed */
int clr_bet_ref = 0;	/* c - clear line between refeshes */
int reverse = 1;	/* r - use reverse video */
int shortline = 0;	/* s - short (left-justified) if escapes not allowed */
int leftline = 0;	/* j - left-justified even if escapes allowed */

	/* flags which have terminal do random things	*/
int beep = 0;		/* b - beep every half hour and twice every hour */
int printid = 0;	/* i - print pid of this process at startup */
int synch = 1;		/* synchronize with clock */

	/* select output device (status display or straight output) */
int emacs = 0;		/* e - assume status display */
int window = 0;		/* w - window mode */
int dbug = 0;		/* d - debug */

/*
 * used to turn off reverse video every REVOFF times
 * in an attempt to not wear out the phospher.
 */
#define REVOFF 5
int revtime = 1;

	/* used by mail checker */
off_t mailsize = 0;
off_t linebeg = 0;		/* place where we last left off reading */

	/* things used by the string routines */
int chars;			/* number of printable characters */
char *sp;
char strarr[512];		/* big enough now? */
	/* flags to stringdump() */
char sawmail;			/* remember mail was seen to print bells */
char mustclear;			/* status line messed up */

	/* strings which control status line display */
#ifdef TERMINFO
char	*rev_out, *rev_end, *arrows;
char	*tparm();
#else
char	to_status_line[64];
char	from_status_line[64];
char	dis_status_line[64];
char	clr_eol[64];
char	rev_out[20], rev_end[20];
char	*arrows, *bell = "\007";
int	eslok;	/* escapes on status line okay (reverse, cursor addressing) */
int	hasws = 0;	/* is "ws" explicitly defined? */
int	columns;
#define tparm(cap, parm) tgoto((cap), 0, (parm))
char	*tgoto();
#endif

	/* to deal with window size changes */
#ifdef SIGWINCH
void sigwinch();
char winchanged;	/* window size has changed since last update */
#endif

	/* random globals */
char *username;
char *ourtty;			/* keep track of what tty we're on */
struct stat stbuf, mstbuf;	/* mstbuf for mail check only */
unsigned delay = DEFDELAY;
uid_t uid;
double loadavg = 0.0;		/* current load average */
int users = 0;

char *strcpy1();
char *sysrup();
int outc();
int erroutc();

main(argc, argv)
	int argc;
	register char **argv;
{
	register char *cp;
	char *home;
	void clearbotl();

	gethostname(hostname, sizeof hostname - 1);
	if ((cp = index(hostname, '.')) != NULL)
		*cp = '\0';
	(void)setvbuf(stdout, (char *)NULL, _IOFBF, 0);

	for (argv++; *argv != 0; argv++)
		switch (**argv) {
		case '-':
			for (cp = *argv + 1; *cp; cp++) {
				switch(*cp) {
				case 'r' :	/* turn off reverse video */
					reverse = 0;
					break;
				case 'c':
					clr_bet_ref = 1;
					break;
				case 'h':
					hostprint = 1;
					break;
				case 'D':
					dateprint = 1;
					break;
#ifdef RWHO
				case 'H':
					if (argv[1] == 0)
						break;
					argv++;
					if (strcmp(hostname, *argv) &&
					    strcmp(&hostname[sizeof NETPREFIX - 1], *argv))
						remotehost[nremotes++].rh_host = *argv;
					break;
#endif RWHO
				case 'm':
					mailcheck = 0;
					break;
				case 'p':
					proccheck = 0;
					break;
				case 'l':
					logcheck = 0;
					break;
				case 'b':
					beep = 1;
					break;
				case 'i':
					printid = 1;
					break;
				case 'w':
					window = 1;
					break;
				case 'e':
					emacs = 1;
					break;
				case 'd':
					dbug = 1;
					break;
				case 'q':
					quiet = 1;
					break;
				case 's':
					shortline = 1;
					break;
				case 'j':
					leftline = 1;
					break;
				default:
					fprintf(stderr,
						"sysline: bad flag: %c\n", *cp);
				}
			}
			break;
		case '+':
			delay = atoi(*argv + 1);
			if (delay < 10)
				delay = 10;
			else if (delay > 500)
				delay = 500;
			synch = 0;	/* no more sync */
			break;
		default:
			fprintf(stderr, "sysline: illegal argument %s\n",
				argv[0]);
		}
	if (emacs) {
		reverse = 0;
		columns = 79;
	} else	/* if not to emacs window, initialize terminal dependent info */
		initterm();
#ifdef SIGWINCH
	/*
	 * When the window size changes and we are the foreground
	 * process (true if -w), we get this signal.
	 */
	signal(SIGWINCH, sigwinch);
#endif
	getwinsize();		/* get window size from ioctl */

	/* immediately fork and let the parent die if not emacs mode */
	if (!emacs && !window && !dbug) {
		if (fork())
			exit(0);
		/* pgrp should take care of things, but ignore them anyway */
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGTTOU, SIG_IGN);
	}
	/*
	 * When we logoff, init will do a "vhangup()" on this
	 * tty which turns off I/O access and sends a SIGHUP
	 * signal.  We catch this and thereby clear the status
	 * display.  Note that a bug in 4.1bsd caused the SIGHUP
	 * signal to be sent to the wrong process, so you had to
	 * `kill -HUP' yourself in your .logout file.
	 * Do the same thing for SIGTERM, which is the default kill
	 * signal.
	 */
	signal(SIGHUP, clearbotl);
	signal(SIGTERM, clearbotl);
	/*
	 * This is so kill -ALRM to force update won't screw us up..
	 */
	signal(SIGALRM, SIG_IGN);

	uid = getuid();
	ourtty = ttyname(2);	/* remember what tty we are on */
	if (printid) {
		printf("%d\n", getpid());
		fflush(stdout);
	}
	dup2(2, 1);

	if ((home = getenv("HOME")) == 0)
		home = "";
	strcpy1(strcpy1(whofilename, home), "/.who");
	strcpy1(strcpy1(whofilename2, home), "/.sysline");
	strcpy1(strcpy1(lockfilename, home), "/.syslinelock");

	if ((kmem = open(_PATH_KMEM,0)) < 0) {
		fprintf(stderr, "Can't open %s\n", _PATH_KMEM);
		exit(1);
	}
	readnamelist();
	if (proccheck)
		initprocread();
	if (mailcheck)
		if ((username = getenv("USER")) == 0)
			mailcheck = 0;
		else {
			chdir(_PATH_MAILDIR);
			if (stat(username, &mstbuf) >= 0)
				mailsize = mstbuf.st_size;
			else
				mailsize = 0;
		}

	while (emacs || window || isloggedin())
		if (access(lockfilename, 0) >= 0)
			sleep(60);
		else {
			prtinfo();
			sleep(delay);
			if (clr_bet_ref) {
				tputs(dis_status_line, 1, outc);
				fflush(stdout);
				sleep(5);
			}
			revtime = (1 + revtime) % REVOFF;
		}
	clearbotl();
	/*NOTREACHED*/
}

isloggedin()
{
	/*
	 * you can tell if a person has logged out if the owner of
	 * the tty has changed
	 */
	struct stat statbuf;

	return fstat(2, &statbuf) == 0 && statbuf.st_uid == uid;
}

readnamelist()
{
	time_t bootime, clock, nintv, time();

	nlist(_PATH_UNIX, nl);
	if (nl[0].n_value == 0) {
		if (!quiet)
			fprintf(stderr, "No namelist\n");
		return;
	}
	lseek(kmem, (long)nl[NL_BOOT].n_value, 0);
	read(kmem, &bootime, sizeof(bootime));
	(void) time(&clock);
	nintv = clock - bootime;
	if (nintv <= 0L || nintv > 60L*60L*24L*365L) {
		if (!quiet)
			fprintf(stderr,
			"Time makes no sense... namelist must be wrong\n");
		nl[NL_PROC].n_value = 0;
	}
}

readutmp(nflag)
	char nflag;
{
	static time_t lastmod;		/* initially zero */
	static off_t utmpsize;		/* ditto */
	struct stat st;

	if (ut < 0 && (ut = open(_PATH_UTMP, 0)) < 0) {
		fprintf(stderr, "sysline: Can't open %s.\n", _PATH_UTMP);
		exit(1);
	}
	if (fstat(ut, &st) < 0 || st.st_mtime == lastmod)
		return 0;
	lastmod = st.st_mtime;
	if (utmpsize != st.st_size) {
		utmpsize = st.st_size;
		nentries = utmpsize / sizeof (struct utmp);
		if (old == 0) {
			old = (struct utmp *)calloc(utmpsize, 1);
			new = (struct utmp *)calloc(utmpsize, 1);
		} else {
			old = (struct utmp *)realloc((char *)old, utmpsize);
			new = (struct utmp *)realloc((char *)new, utmpsize);
			free(status);
		}
		status = malloc(nentries * sizeof *status);
		if (old == 0 || new == 0 || status == 0) {
			fprintf(stderr, "sysline: Out of memory.\n");
			exit(1);
		}
	}
	lseek(ut, 0L, 0);
	(void) read(ut, (char *) (nflag ? new : old), utmpsize);
	return 1;
}

/*
 * read in the process table locations and sizes, and allocate space
 * for storing the process table.  This is done only once.
 */
initprocread()
{

	if (nl[NL_PROC].n_value == 0)
		return;
	lseek(kmem, (long)nl[NL_PROC].n_value, 0);
	read(kmem, &procadr, sizeof procadr);
	lseek(kmem, (long)nl[NL_NPROC].n_value, 0);
	read(kmem, &nproc, sizeof nproc);
	if ((proc = (struct proc *) calloc(nproc, sizeof (struct proc))) == 0) {
		fprintf(stderr, "Out of memory.\n");
		exit(1);
	}
	procNPROC = proc + nproc;
}

/*
 * read in the process table.  This assumes that initprocread has alread been
 * called to set up storage.
 */
readproctab()
{

	if (nl[NL_PROC].n_value == 0)
		return (0);
	lseek(kmem, (long)procadr, 0);
	read(kmem, (char *)proc, nproc * sizeof (struct proc));
	return (1);
}

prtinfo()
{
	int on, off;
	register i;
	char fullprocess;

	stringinit();
#ifdef SIGWINCH
	if (winchanged) {
		winchanged = 0;
		getwinsize();
		mustclear = 1;
	}
#endif
#ifdef WHO
	/* check for file named .who in the home directory */
	whocheck();
#endif
	timeprint();
	/*
	 * if mail is seen, don't print rest of info, just the mail
	 * reverse new and old so that next time we run, we won't lose log
	 * in and out information
	 */
	if (mailcheck && (sawmail = mailseen()))
		goto bottom;
#ifdef RWHO
	for (i = 0; i < nremotes; i++) {
		char *tmp;

		stringspace();
		tmp = sysrup(remotehost + i);
		stringcat(tmp, strlen(tmp));
	}
#endif
	/*
	 * print hostname info if requested
	 */
	if (hostprint) {
		stringspace();
		stringcat(hostname, -1);
	}
	/*
	 * print load average and difference between current load average
	 * and the load average 5 minutes ago
	 */
	if (getloadavg(avenrun, 3) > 0) {
		double diff;

		stringspace();

		if ((diff = avenrun[0] - avenrun[1]) < 0.0)
			stringprt("%.1f %.1f", avenrun[0],  diff);
		else
			stringprt("%.1f +%.1f", avenrun[0], diff);
		loadavg = avenrun[0];		/* remember load average */
	}
	/*
	 * print log on and off information
	 */
	stringspace();
	fullprocess = 1;
#ifdef MAXLOAD
	if (loadavg > MAXLOAD)
		fullprocess = 0;	/* too loaded to run */
#endif
	/*
	 * Read utmp file (logged in data) only if we are doing a full
	 * process, or if this is the first time and we are calculating
	 * the number of users.
	 */
	on = off = 0;
	if (users == 0) {		/* first time */
		if (readutmp(0))
			for (i = 0; i < nentries; i++)
				if (old[i].ut_name[0])
					users++;
	} else if (fullprocess && readutmp(1)) {
		struct utmp *tmp;

		users = 0;
		for (i = 0; i < nentries; i++) {
			if (strncmp(old[i].ut_name,
			    new[i].ut_name, NAMESIZE) == 0)
				status[i] = NOCH;
			else if (old[i].ut_name[0] == '\0') {
				status[i] = ON;
				on++;
			} else if (new[i].ut_name[0] == '\0') {
				status[i] = OFF;
				off++;
			} else {
				status[i] = ON | OFF;
				on++;
				off++;
			}
			if (new[i].ut_name[0])
				users++;
		}
		tmp = new;
		new = old;
		old = tmp;
	}
	/*
	 * Print:
	 * 	1.  number of users
	 *	2.  a * for unread mail
	 *	3.  a - if load is too high
	 *	4.  number of processes running and stopped
	 */
	stringprt("%du", users);
	if (mailsize > 0 && mstbuf.st_mtime >= mstbuf.st_atime)
		stringcat("*", -1);
	if (!fullprocess && (proccheck || logcheck))
		stringcat("-", -1);
	if (fullprocess && proccheck && readproctab()) {
		register struct proc *p;
		int procrun, procstop;

		/*
		 * We are only interested in processes which have the same
		 * uid as us, and whose parent process id is not 1.
		 */
		procrun = procstop = 0;
		for (p = proc; p < procNPROC; p++) {
			if (p->p_stat == 0 || p->p_pgrp == 0 ||
			    p->p_uid != uid || p->p_ppid == 1)
				continue;
			switch (p->p_stat) {
			case SSTOP:
				procstop++;
				break;
			case SSLEEP:
				/*
				 * Sleep can mean waiting for a signal or just
				 * in a disk or page wait queue ready to run.
				 * We can tell if it is the later by the pri
				 * being negative.
				 */
				if (p->p_pri < PZERO)
					procrun++;
				break;
			case SWAIT:
			case SRUN:
			case SIDL:
				procrun++;
			}
		}
		if (procrun > 0 || procstop > 0) {
			stringspace();
			if (procrun > 0 && procstop > 0)
				stringprt("%dr %ds", procrun, procstop);
			else if (procrun > 0)
				stringprt("%dr", procrun);
			else
				stringprt("%ds", procstop);
		}
	}
	/*
	 * If anyone has logged on or off, and we are interested in it,
	 * print it out.
	 */
	if (logcheck) {
		/* old and new have already been swapped */
		if (on) {
			stringspace();
			stringcat("on:", -1);
			for (i = 0; i < nentries; i++)
				if (status[i] & ON) {
					stringprt(" %.8s", old[i].ut_name);
					ttyprint(old[i].ut_line);
				}
		}
		if (off) {
			stringspace();
			stringcat("off:", -1);
			for (i = 0; i < nentries; i++)
				if (status[i] & OFF) {
					stringprt(" %.8s", new[i].ut_name);
					ttyprint(new[i].ut_line);
				}
		}
	}
bottom:
		/* dump out what we know */
	stringdump();
}

timeprint()
{
	long curtime;
	struct tm *tp, *localtime();
	static int beepable = 1;

		/* always print time */
	time(&curtime);
	tp = localtime(&curtime);
	if (dateprint)
		stringprt("%.11s", ctime(&curtime));
	stringprt("%d:%02d", tp->tm_hour > 12 ? tp->tm_hour - 12 :
		(tp->tm_hour == 0 ? 12 : tp->tm_hour), tp->tm_min);
	if (synch)			/* sync with clock */
		delay = 60 - tp->tm_sec;
	/*
	 * Beepable is used to insure that we get at most one set of beeps
	 * every half hour.
	 */
	if (beep)
		if (beepable) {
			if (tp->tm_min == 30) {
				tputs(bell, 1, outc);
				fflush(stdout);
				beepable = 0;
			} else if (tp->tm_min == 0) {
				tputs(bell, 1, outc);
				fflush(stdout);
				sleep(2);
				tputs(bell, 1, outc);
				fflush(stdout);
				beepable = 0;
			}
		} else
			if (tp->tm_min != 0 && tp->tm_min != 30)
				beepable = 1;
}

/*
 * whocheck -- check for file named .who and print it on the who line first
 */
whocheck()
{
	int chss;
	register char *p;
	char buff[81];
	int whofile;

	if ((whofile = open(whofilename, 0)) < 0 &&
	    (whofile = open(whofilename2, 0)) < 0)
		return;
	chss = read(whofile, buff, sizeof buff - 1);
	close(whofile);
	if (chss <= 0)
		return;
	buff[chss] = '\0';
	/*
	 * Remove all line feeds, and replace by spaces if they are within
	 * the message, else replace them by nulls.
	 */
	for (p = buff; *p;)
		if (*p == '\n')
			if (p[1])
				*p++ = ' ';
			else
				*p = '\0';
		else
			p++;
	stringcat(buff, p - buff);
	stringspace();
}

/*
 * ttyprint -- given the name of a tty, print in the string buffer its
 * short name surrounded by parenthesis.
 * ttyxx is printed as (xx)
 * console is printed as (cty)
 */
ttyprint(name)
	char *name;
{
	char buff[11];

	if (strncmp(name, "tty", 3) == 0)
		stringprt("(%.*s)", LINESIZE - 3, name + 3);
	else if (strcmp(name, "console") == 0)
		stringcat("(cty)", -1);
	else
		stringprt("(%.*s)", LINESIZE, name);
}

/*
 * mail checking function
 * returns 0 if no mail seen
 */
mailseen()
{
	FILE *mfd;
	register n;
	register char *cp;
	char lbuf[100], sendbuf[100], *bufend;
	char seenspace;
	int retval = 0;

	if (stat(username, &mstbuf) < 0) {
		mailsize = 0;
		return 0;
	}
	if (mstbuf.st_size <= mailsize || (mfd = fopen(username,"r")) == NULL) {
		mailsize = mstbuf.st_size;
		return 0;
	}
	fseek(mfd, mailsize, 0);
	while ((n = readline(mfd, lbuf, sizeof lbuf)) >= 0 &&
	       strncmp(lbuf, "From ", 5) != 0)
		;
	if (n < 0) {
		stringcat("Mail has just arrived", -1);
		goto out;
	}
	retval = 1;
	/*
	 * Found a From line, get second word, which is the sender,
	 * and print it.
	 */
	for (cp = lbuf + 5; *cp && *cp != ' '; cp++)	/* skip to blank */
		;
	*cp = '\0';					/* terminate name */
	stringspace();
	stringprt("Mail from %s ", lbuf + 5);
	/*
	 * Print subject, and skip over header.
	 */
	while ((n = readline(mfd, lbuf, sizeof lbuf)) > 0)
		if (strncmp(lbuf, "Subject:", 8) == 0)
			stringprt("on %s ", lbuf + 9);
	if (!emacs)
		stringcat(arrows, 2);
	else
		stringcat(": ", 2);
	if (n < 0)			/* already at eof */
		goto out;
	/*
	 * Print as much of the letter as we can.
	 */
	cp = sendbuf;
	if ((n = columns - chars) > sizeof sendbuf - 1)
		n = sizeof sendbuf - 1;
	bufend = cp + n;
	seenspace = 0;
	while ((n = readline(mfd, lbuf, sizeof lbuf)) >= 0) {
		register char *rp;

		if (strncmp(lbuf, "From ", 5) == 0)
			break;
		if (cp >= bufend)
			continue;
		if (!seenspace) {
			*cp++ = ' ';		/* space before lines */
			seenspace = 1;
		}
		rp = lbuf;
		while (*rp && cp < bufend)
			if (isspace(*rp)) {
				if (!seenspace) {
					*cp++ = ' ';
					seenspace = 1;
				}
				rp++;
			} else {
				*cp++ = *rp++;
				seenspace = 0;
			}
	}
	*cp = 0;
	stringcat(sendbuf, -1);
	/*
	 * Want to update write time so a star will
	 * appear after the number of users until the
	 * user reads his mail.
	 */
out:
	mailsize = linebeg;
	fclose(mfd);
	touch(username);
	return retval;
}

/*
 * readline -- read a line from fp and store it in buf.
 * return the number of characters read.
 */
readline(fp, buf, n)
	register FILE *fp;
	char *buf;
	register n;
{
	register c;
	register char *cp = buf;

	linebeg = ftell(fp);		/* remember loc where line begins */
	cp = buf;
	while (--n > 0 && (c = getc(fp)) != EOF && c != '\n')
		*cp++ = c;
	*cp = 0;
	if (c == EOF && cp - buf == 0)
		return -1;
	return cp - buf;
}


/*
 * string hacking functions
 */

stringinit()
{
	sp = strarr;
	chars = 0;
}

/*VARARGS1*/
stringprt(fmt)
	char *fmt;
{
	va_list ap;
	char tempbuf[150];

	va_start(ap, fmt);
	(void)vsnprintf(tempbuf, sizeof(tempbuf), fmt, ap);
	va_end(ap);
	stringcat(tempbuf, -1);
}

stringdump()
{
	char bigbuf[sizeof strarr + 200];
	register char *bp = bigbuf;
	register int i;

	if (!emacs) {
		if (sawmail)
			bp = strcpy1(bp, bell);
		if (eslok)
			bp = strcpy1(bp, tparm(to_status_line,
				leftline ? 0 : columns - chars));
		else {
			bp = strcpy1(bp, to_status_line);
			if (!shortline && !leftline)
				for (i = columns - chars; --i >= 0;)
					*bp++ = ' ';
		}
		if (reverse && revtime != 0)
			bp = strcpy1(bp, rev_out);
	}
	*sp = 0;
	bp = strcpy1(bp, strarr);
	if (!emacs) {
		if (reverse)
			bp = strcpy1(bp, rev_end);
		bp = strcpy1(bp, from_status_line);
		if (sawmail)
			bp = strcpy1(strcpy1(bp, bell), bell);
		*bp = 0;
		tputs(bigbuf, 1, outc);
		if (mustclear) {
			mustclear = 0;
			tputs(clr_eol, 1, outc);
		}
		if (dbug)
			putchar('\n');
		fflush(stdout);
	} else
		write(2, bigbuf, bp - bigbuf);
}

stringspace()
{
	if (reverse && revtime != 0) {
#ifdef TERMINFO
		stringcat(rev_end,
			magic_cookie_glitch <= 0 ? 0 : magic_cookie_glitch);
		stringcat(" ", 1);
		stringcat(rev_out,
			magic_cookie_glitch <= 0 ? 0 : magic_cookie_glitch);
#else
		stringcat(rev_end, 0);
		stringcat(" ", 1);
		stringcat(rev_out, 0);
#endif TERMINFO
	} else
		stringcat(" ", 1);
}

/*
 * stringcat :: concatenate the characters in string str to the list we are
 * 	        building to send out.
 * str - the string to print. may contain funny (terminal control) chars.
 * n  - the number of printable characters in the string
 *	or if -1 then str is all printable so we can truncate it,
 *	otherwise don't print only half a string.
 */
stringcat(str, n)
	register char *str;
	register n;
{
	register char *p = sp;

	if (n < 0) {				/* truncate */
		n = columns - chars;
		while ((*p++ = *str++) && --n >= 0)
			;
		p--;
		chars += p - sp;
		sp = p;
	} else if (chars + n <= columns) {	/* don't truncate */
		while (*p++ = *str++)
			;
		chars += n;
		sp = p - 1;
	}
}

/*
 * touch :: update the modify time of a file.
 */
touch(name)
	char *name;		/* name of file */
{
	register fd;
	char buf;

	if ((fd = open(name, 2)) >= 0) {
		read(fd, &buf, 1);		/* get first byte */
		lseek(fd, 0L, 0);		/* go to beginning */
		write(fd, &buf, 1);		/* and rewrite first byte */
		close(fd);
	}
}


/*
 * clearbotl :: clear bottom line.
 * called when process quits or is killed.
 * it clears the bottom line of the terminal.
 */
void
clearbotl()
{
	register int fd;
	void sigexit();

	signal(SIGALRM, sigexit);
	alarm(30);	/* if can't open in 30 secs, just die */
	if (!emacs && (fd = open(ourtty, 1)) >= 0) {
		write(fd, dis_status_line, strlen(dis_status_line));
		close(fd);
	}
#ifdef PROF
	if (chdir(_PATH_SYSLINE) < 0)
		(void) chdir(_PATH_TMP);
#endif
	exit(0);
}

#ifdef TERMINFO
initterm()
{
	static char standbuf[40];

	setupterm(0, 1, 0);
	if (!window && !has_status_line) {
		/* not an appropriate terminal */
		if (!quiet)
		   fprintf(stderr, "sysline: no status capability for %s\n",
			getenv("TERM"));
		exit(1);
	}
	if (window || status_line_esc_ok) {
		if (set_attributes) {
			/* reverse video mode */
			strcpy(standbuf,
				tparm(set_attributes,0,0,1,0,0,0,0,0,0));
			rev_out = standbuf;
			rev_end = exit_attribute_mode;
		} else if (enter_standout_mode && exit_standout_mode) {
			rev_out = enter_standout_mode;
			rev_end = exit_standout_mode;
		} else
			rev_out = rev_end = "";
	} else
		rev_out = rev_end = "";
	columns--;	/* avoid cursor wraparound */
}

#else	/* TERMCAP */

initterm()
{
	char *term, *cp;
	static char tbuf[1024];
	char is2[40];
	extern char *UP;

	if ((term = getenv("TERM")) == NULL) {
		if (!quiet)
			fprintf(stderr,
				"sysline: No TERM variable in enviroment\n");
		exit(1);
	}
	if (tgetent(tbuf, term) <= 0) {
		if (!quiet)
			fprintf(stderr,
				"sysline: Unknown terminal type: %s\n", term);
		exit(1);
	}
	if (!window && tgetflag("hs") <= 0) {
		if (!strncmp(term, "h19", 3)) {
			/* for upward compatability with h19sys */
			strcpy(to_status_line,
				"\033j\033x5\033x1\033Y8%+ \033o");
			strcpy(from_status_line, "\033k\033y5");
			strcpy(dis_status_line, "\033y1");
			strcpy(rev_out, "\033p");
			strcpy(rev_end, "\033q");
			arrows = "\033Fhh\033G";
			columns = 80;
			UP = "\b";
			return;
		}
		if (!quiet)
			fprintf(stderr,
				"sysline: No status capability for %s\n", term);
		exit(1);
	}
	cp = is2;
	if (tgetstr("i2", &cp) != NULL) {
		/* someday tset will do this */
		tputs(is2, 1, erroutc);
		fflush(stdout);
	}

	/* the "-1" below is to avoid cursor wraparound problems */
	columns = tgetnum("ws");
	hasws = columns >= 0;
	if (!hasws)
		columns = tgetnum("co");
	columns -= 1;
	if (window) {
		strcpy(to_status_line, "\r");
		cp = dis_status_line;	/* use the clear line sequence */
		*cp++ = '\r';
		tgetstr("ce", &cp);
		if (leftline)
			strcpy(from_status_line, dis_status_line + 1);
		else
			strcpy(from_status_line, "");
	} else {
		cp = to_status_line;
		tgetstr("ts", &cp);
		cp = from_status_line;
		tgetstr("fs", &cp);
		cp = dis_status_line;
		tgetstr("ds", &cp);
		eslok = tgetflag("es");
	}
	if (eslok || window) {
		cp = rev_out;
		tgetstr("so", &cp);
		cp = rev_end;
		tgetstr("se", &cp);
		cp = clr_eol;
		tgetstr("ce", &cp);
	} else
		reverse = 0;	/* turn off reverse video */
	UP = "\b";
	if (!strncmp(term, "h19", 3))
		arrows = "\033Fhh\033G";	/* "two tiny graphic arrows" */
	else
		arrows = "->";
}
#endif TERMINFO

#ifdef RWHO
char *
sysrup(hp)
	register struct remotehost *hp;
{
	char filename[100];
	struct whod wd;
#define WHOD_HDR_SIZE (sizeof (wd) - sizeof (wd.wd_we))
	static char buffer[50];
	time_t now;

	/*
	 * rh_file is initially 0.
	 * This is ok since standard input is assumed to exist.
	 */
	if (hp->rh_file == 0) {
		/*
		 * Try rwho hostname file, and if that fails try ucbhostname.
		 */
		(void) strcpy1(strcpy1(filename, _PATH_RWHO), hp->rh_host);
		if ((hp->rh_file = open(filename, 0)) < 0) {
			(void) strcpy1(strcpy1(strcpy1(filename, _PATH_RWHO),
				NETPREFIX), hp->rh_host);
			hp->rh_file = open(filename, 0);
		}
	}
	if (hp->rh_file < 0) {
		(void) sprintf(buffer, "%s?", hp->rh_host);
		return(buffer);
	}
	(void) lseek(hp->rh_file, (off_t)0, 0);
	if (read(hp->rh_file, (char *)&wd, WHOD_HDR_SIZE) != WHOD_HDR_SIZE) {
		(void) sprintf(buffer, "%s ?", hp->rh_host);
		return(buffer);
	}
	(void) time(&now);
	if (now - wd.wd_recvtime > DOWN_THRESHOLD) {
		long interval;
		long days, hours, minutes;

		interval = now - wd.wd_recvtime;
		minutes = (interval + 59) / 60;	/* round to minutes */
		hours = minutes / 60;		/* extract hours from minutes */
		minutes %= 60;			/* remove hours from minutes */
		days = hours / 24;		/* extract days from hours */
		hours %= 24;			/* remove days from hours */
		if (days > 7 || days < 0)
			(void) sprintf(buffer, "%s down", hp->rh_host);
		else if (days > 0)
			(void) sprintf(buffer, "%s %d+%d:%02d",
				hp->rh_host, days, hours, minutes);
		else
			(void) sprintf(buffer, "%s %d:%02d",
				hp->rh_host, hours, minutes);
	} else
		(void) sprintf(buffer, "%s %.1f",
			hp->rh_host, wd.wd_loadav[0]/100.0);
	return buffer;
}
#endif RWHO

getwinsize()
{
#ifdef TIOCGWINSZ
	struct winsize winsize;

	/* the "-1" below is to avoid cursor wraparound problems */
	if (!hasws && ioctl(2, TIOCGWINSZ, (char *)&winsize) >= 0 &&
		winsize.ws_col != 0)
		columns = winsize.ws_col - 1;
#endif
}

#ifdef SIGWINCH
void
sigwinch()
{
	winchanged++;
}
#endif

void
sigexit()
{
	exit(1);
}

char *
strcpy1(p, q)
	register char *p, *q;
{

	while (*p++ = *q++)
		;
	return p - 1;
}

outc(c)
	char c;
{
	if (dbug)
		printf("%s", unctrl(c));
	else
		putchar(c);
}

erroutc(c)
	char c;
{
	if (dbug)
		fprintf(stderr, "%s", unctrl(c));
	else
		putc(c, stderr);
}
