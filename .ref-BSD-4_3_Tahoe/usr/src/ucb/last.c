/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)last.c	5.11 (Berkeley) 6/29/88";
#endif /* not lint */

/*
 * last
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <signal.h>
#include <time.h>
#include <utmp.h>
#include <stdio.h>

#define	SECDAY	(24*60*60)			/* seconds in a day */
#define	NO	0				/* false/no */
#define	YES	1				/* true/yes */

static struct utmp	buf[1024];		/* utmp read buffer */

#define	HMAX	sizeof(buf[0].ut_host)		/* size of utmp host field */
#define	LMAX	sizeof(buf[0].ut_line)		/* size of utmp tty field */
#define	NMAX	sizeof(buf[0].ut_name)		/* size of utmp name field */

typedef struct arg {
	char	*name;				/* argument */
#define	HOST_TYPE	-2
#define	TTY_TYPE	-3
#define	USER_TYPE	-4
	int	type;				/* type of arg */
	struct arg	*next;			/* linked list pointer */
} ARG;
ARG	*arglist;				/* head of linked list */

typedef struct ttytab {
	long	logout;				/* log out time */
	char	tty[LMAX + 1];			/* terminal name */
	struct ttytab	*next;			/* linked list pointer */
} TTY;
TTY	*ttylist;				/* head of linked list */

static long	currentout,			/* current logout value */
		maxrec;				/* records to display */
static char	*file = "/usr/adm/wtmp";	/* wtmp file */

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern int	optind;
	extern char	*optarg;
	int	ch;
	long	atol();
	char	*ttyconv();

	while ((ch = getopt(argc, argv, "0123456789f:h:t:")) != EOF)
		switch((char)ch) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/*
			 * kludge: last was originally designed to take
			 * a number after a dash.
			 */
			if (!maxrec)
				maxrec = atol(argv[optind - 1] + 1);
			break;
		case 'f':
			file = optarg;
			break;
		case 'h':
			hostconv(optarg);
			addarg(HOST_TYPE, optarg);
			break;
		case 't':
			addarg(TTY_TYPE, ttyconv(optarg));
			break;
		case '?':
		default:
			fputs("usage: last [-#] [-f file] [-t tty] [-h hostname] [user ...]\n", stderr);
			exit(1);
		}
	for (argv += optind; *argv; ++argv) {
#define	COMPATIBILITY
#ifdef	COMPATIBILITY
		/* code to allow "last p5" to work */
		addarg(TTY_TYPE, ttyconv(*argv));
#endif
		addarg(USER_TYPE, *argv);
	}
	wtmp();
	exit(0);
}

/*
 * wtmp --
 *	read through the wtmp file
 */
static
wtmp()
{
	register struct utmp	*bp;		/* current structure */
	register TTY	*T;			/* tty list entry */
	struct stat	stb;			/* stat of file for size */
	long	bl, delta,			/* time difference */
		lseek(), time();
	int	bytes, wfd,
		onintr();
	char	*ct, *crmsg,
		*asctime(), *ctime(), *strcpy();
	TTY	*addtty();

	if ((wfd = open(file, O_RDONLY, 0)) < 0 || fstat(wfd, &stb) == -1) {
		perror(file);
		exit(1);
	}
	bl = (stb.st_size + sizeof(buf) - 1) / sizeof(buf);

	(void)time(&buf[0].ut_time);
	(void)signal(SIGINT, onintr);
	(void)signal(SIGQUIT, onintr);

	while (--bl >= 0) {
		if (lseek(wfd, (long)(bl * sizeof(buf)), L_SET) == -1 ||
		    (bytes = read(wfd, (char *)buf, sizeof(buf))) == -1) {
			fprintf(stderr, "last: %s: ", file);
			perror((char *)NULL);
			exit(1);
		}
		for (bp = &buf[bytes / sizeof(buf[0]) - 1]; bp >= buf; --bp) {
			/*
			 * if the terminal line is '~', the machine stopped.
			 * see utmp(5) for more info.
			 */
			if (!strncmp(bp->ut_line, "~", LMAX)) {
				/* everybody just logged out */
				for (T = ttylist; T; T = T->next)
					T->logout = -bp->ut_time;
				currentout = -bp->ut_time;
				crmsg = strncmp(bp->ut_name, "shutdown", NMAX) ? "crash" : "down ";
				if (!bp->ut_name[0])
					(void)strcpy(bp->ut_name, "reboot");
				if (want(bp, NO)) {
					ct = ctime(&bp->ut_time);
					printf("%-*.*s  %-*.*s %-*.*s %10.10s %5.5s \n", NMAX, NMAX, bp->ut_name, LMAX, LMAX, bp->ut_line, HMAX, HMAX, bp->ut_host, ct, ct + 11);
					if (maxrec && !--maxrec)
						return;
				}
				continue;
			}
			/* find associated tty */
			for (T = ttylist;; T = T->next) {
				if (!T) {
					/* add new one */
					T = addtty(bp->ut_line);
					break;
				}
				if (!strncmp(T->tty, bp->ut_line, LMAX))
					break;
			}
			if (bp->ut_name[0] && want(bp, YES)) {
				ct = ctime(&bp->ut_time);
				printf("%-*.*s  %-*.*s %-*.*s %10.10s %5.5s ", NMAX, NMAX, bp->ut_name, LMAX, LMAX, bp->ut_line, HMAX, HMAX, bp->ut_host, ct, ct + 11);
				if (!T->logout)
					puts("  still logged in");
				else {
					if (T->logout < 0) {
						T->logout = -T->logout;
						printf("- %s", crmsg);
					}
					else
						printf("- %5.5s", ctime(&T->logout)+11);
					delta = T->logout - bp->ut_time;
					if (delta < SECDAY)
						printf("  (%5.5s)\n", asctime(gmtime(&delta))+11);
					else
						printf(" (%ld+%5.5s)\n", delta / SECDAY, asctime(gmtime(&delta))+11);
				}
				if (maxrec != -1 && !--maxrec)
					return;
			}
			T->logout = bp->ut_time;
		}
	}
	ct = ctime(&buf[0].ut_time);
	printf("\nwtmp begins %10.10s %5.5s \n", ct, ct + 11);
}

/*
 * want --
 *	see if want this entry
 */
static
want(bp, check)
	register struct utmp	*bp;
	int	check;
{
	register ARG	*step;

	if (check)
		/*
		 * when uucp and ftp log in over a network, the entry in
		 * the utmp file is the name plus their process id.  See
		 * etc/ftpd.c and usr.bin/uucp/uucpd.c for more information.
		 */
		if (!strncmp(bp->ut_line, "ftp", sizeof("ftp") - 1))
			bp->ut_line[3] = '\0';
		else if (!strncmp(bp->ut_line, "uucp", sizeof("uucp") - 1))
			bp->ut_line[4] = '\0';
	if (!arglist)
		return(YES);

	for (step = arglist; step; step = step->next)
		switch(step->type) {
		case HOST_TYPE:
			if (!strncasecmp(step->name, bp->ut_host, HMAX))
				return(YES);
			break;
		case TTY_TYPE:
			if (!strncmp(step->name, bp->ut_line, LMAX))
				return(YES);
			break;
		case USER_TYPE:
			if (!strncmp(step->name, bp->ut_name, NMAX))
				return(YES);
			break;
	}
	return(NO);
}

/*
 * addarg --
 *	add an entry to a linked list of arguments
 */
static
addarg(type, arg)
	int	type;
	char	*arg;
{
	register ARG	*cur;
	char	*malloc();

	if (!(cur = (ARG *)malloc((u_int)sizeof(ARG)))) {
		fputs("last: malloc failure.\n", stderr);
		exit(1);
	}
	cur->next = arglist;
	cur->type = type;
	cur->name = arg;
	arglist = cur;
}

/*
 * addtty --
 *	add an entry to a linked list of ttys
 */
static TTY *
addtty(ttyname)
	char	*ttyname;
{
	register TTY	*cur;
	char	*malloc();

	if (!(cur = (TTY *)malloc((u_int)sizeof(TTY)))) {
		fputs("last: malloc failure.\n", stderr);
		exit(1);
	}
	cur->next = ttylist;
	cur->logout = currentout;
	bcopy(ttyname, cur->tty, LMAX);
	return(ttylist = cur);
}

/*
 * hostconv --
 *	convert the hostname to search pattern; if the supplied host name
 *	has a domain attached that is the same as the current domain, rip
 *	off the domain suffix since that's what login(1) does.
 */
static
hostconv(arg)
	char	*arg;
{
	static int	first = 1;
	static char	*hostdot,
			name[MAXHOSTNAMELEN];
	char	*argdot, *index();

	if (!(argdot = index(arg, '.')))
		return;
	if (first) {
		first = 0;
		if (gethostname(name, sizeof(name))) {
			perror("last: gethostname");
			exit(1);
		}
		hostdot = index(name, '.');
	}
	if (hostdot && !strcasecmp(hostdot, argdot))
		*argdot = '\0';
}

/*
 * ttyconv --
 *	convert tty to correct name.
 */
static char *
ttyconv(arg)
	char	*arg;
{
	char	*mval,
		*malloc(), *strcpy();

	/*
	 * kludge -- we assume that all tty's end with
	 * a two character suffix.
	 */
	if (strlen(arg) == 2) {
		/* either 6 for "ttyxx" or 8 for "console" */
		if (!(mval = malloc((u_int)8))) {
			fputs("last: malloc failure.\n", stderr);
			exit(1);
		}
		if (!strcmp(arg, "co"))
			(void)strcpy(mval, "console");
		else {
			(void)strcpy(mval, "tty");
			(void)strcpy(mval + 3, arg);
		}
		return(mval);
	}
	if (!strncmp(arg, "/dev/", sizeof("/dev/") - 1))
		return(arg + 5);
	return(arg);
}

/*
 * onintr --
 *	on interrupt, we inform the user how far we've gotten
 */
static
onintr(signo)
	int	signo;
{
	char	*ct, *ctime();

	ct = ctime(&buf[0].ut_time);
	printf("\ninterrupted %10.10s %5.5s \n", ct, ct + 11);
	if (signo == SIGINT)
		exit(1);
	(void)fflush(stdout);			/* fix required for rsh */
}
