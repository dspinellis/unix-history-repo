/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)dm.c	5.3 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <pwd.h>
#include <utmp.h>
#include <nlist.h>
#include <stdio.h>
#include <ctype.h>

#define	GAMEHIDE	"/usr/games/hide"
#define	NOGAMING	"/usr/games/nogames"
#define	CONTROL		"/usr/games/dm.config"
#ifdef LOG
#define	LOGFILE		"/usr/adm/dm.log"
#endif

static FILE	*cfp;
static time_t	now;			/* current time value */
static int	priority = 0;		/* priority game runs at */
static char	*game,			/* requested game */
		*gametty;		/* from tty? */

main(argc, argv)
	int	argc;
	char	**argv;
{
	char	*C, *rindex();
	double	load();

	nogamefile();

	if (!strcmp(*argv, "dm"))
		exit(0);

	if (!(cfp = fopen(CONTROL, "r"))) {
		fprintf(stderr, "dm: unable to read %s.\n", CONTROL);
		exit(1);
	}

	read_days();
	read_ttys();

	game = (C = rindex(*argv, '/')) ? ++C : *argv;
	read_games();

#ifdef LOG
	logfile();
#endif
	play(argv);
/*NOTREACHED*/
}

/*
 * play --
 *	play the game
 */
static
play(args)
	char	**args;
{
	if (chdir(GAMEHIDE)) {
		perror("dm: chdir");
		exit(1);
	}
	if (priority > 0 && setpriority(PRIO_PROCESS, 0, priority) < 0)
		fputs("dm: unable to set priority!\n", stderr);
	setgid(getgid());	/* we run setgid kmem; lose it */
	execv(game, args);
	perror("dm");
	exit(1);
}

#define	lcontrol(buf)	(buf[0] == '%' && buf[1] == '%')
#define	lignore(buf)	(buf[0] == '\n' || buf[0] == '#')
/*
 * read_days --
 *	read through days listed in the control file, decide
 *	if current time is an okay play time.
 */
static
read_days()
{
	static char	*days[] = {
		"sunday", "monday", "tuesday", "wednesday",
		"thursday", "friday", "saturday",
	};
	struct tm	*ct;
	register char	*dp;
	int	start, stop;
	char	lbuf[BUFSIZ], f1[40], f2[40], f3[40];
	time_t	time();

	(void)time(&now);
	ct = localtime(&now);
	dp = days[ct->tm_wday];

	while (fgets(lbuf, sizeof(lbuf), cfp)) {
		if (lignore(lbuf))
			continue;
		if (lcontrol(lbuf))
			return;
		if (sscanf(lbuf, "%s%s%s", f1, f2, f3) != 3)
			continue;
		if (!strcasecmp(dp, f1)) {
			if (!isdigit(*f2) || !isdigit(*f3))
				continue;
			start = atoi(f2);
			stop = atoi(f3);
			if (ct->tm_hour >= start && ct->tm_hour <= stop) {
				fputs("dm: Sorry, games are not available from ", stderr);
				hour(start);
				fputs(" to ", stderr);
				hour(stop);
				fputs(" today.\n", stderr);
				exit(0);
			}
			continue;
		}
	}
}

/*
 * read_ttys --
 *	read through ttys listed in the control file, decide if this
 *	tty can be used for games.
 */
static
read_ttys()
{
	register char	*p_tty;
	char	lbuf[BUFSIZ], f1[40], f2[40],
		*ttyname(), *rindex();

	gametty = ttyname(0);
	if (p_tty = rindex(gametty, '/'))
		++p_tty;
	while (fgets(lbuf, sizeof(lbuf), cfp)) {
		if (lignore(lbuf))
			continue;
		if (lcontrol(lbuf))
			return;
		if (sscanf(lbuf, "%s%s", f1, f2) != 2)
			continue;
		if (strcasecmp("badtty", f1))
			continue;
		if (!strcmp(gametty, f2) || p_tty && !strcmp(p_tty, f2)) {
			fprintf(stderr, "dm: Sorry, you may not play games on %s.\n", gametty);
			exit(0);
		}
	}
}

/*
 * read_games --
 *	read through games listed in the control file, decide if this
 *	game can be played now.
 */
static
read_games()
{
	char	lbuf[BUFSIZ], f1[40], f2[40], f3[40], f4[40];

	while (fgets(lbuf, sizeof(lbuf), cfp)) {
		if (lignore(lbuf))
			continue;
		if (lcontrol(lbuf))
			return;
		if (sscanf(lbuf, "%s%s%s%s", f1, f2, f3, f4) != 4)
			break;
		if (!strcmp(game, f1) || !strcasecmp("default", f1)) {
			if (isdigit(*f2) && atoi(f2) < load()) {
				fputs("dm: Sorry, the load average is too high right now.\n", stderr);
				exit(0);
			}
			if (isdigit(*f3) && atoi(f3) <= users()) {
				fputs("dm: Sorry, there are too many users logged on right now.\n", stderr);
				exit(0);
			}
			if (isdigit(*f4))
				priority = atoi(f3);
			return;
		}
	}
}

static struct	nlist nl[] = {
	{ "_avenrun" },
#define	X_AVENRUN	0
	{ "" },
};

/*
 * load --
 *	return 15 minute load average
 */
static double
load()
{
	double	avenrun[3];
	int	kmem;
	long	lseek();

	if (nlist("/vmunix", nl)) {
		fputs("dm: nlist of /vmunix failed.\n", stderr);
		exit(1);
	}
	if ((kmem = open("/dev/kmem", O_RDONLY, 0)) < 0) {
		perror("dm: /dev/kmem");
		exit(1);
	}
	(void)lseek(kmem, (long)nl[X_AVENRUN].n_value, L_SET);
	(void)read(kmem, avenrun, sizeof(avenrun));
	return(avenrun[2]);
}

/*
 * users --
 *	return current number of users
 */
static
users()
{
	register int	nusers,
			utmp;
	struct utmp	buf;

	if ((utmp = open("/etc/utmp", O_RDONLY, 0)) < 0) {
		perror("dm: /etc/utmp");
		exit(1);
	}
	for (nusers = 0; read(utmp, (char *)&buf, sizeof(struct utmp)) > 0;)
		if (buf.ut_name[0] != '\0')
			++nusers;
	return(nusers);
}

/*
 * nogamefile --
 *	if the file NOGAMING exists, no games allowed.
 *	file may also contain a message for the user.
 */
static
nogamefile()
{
	register int	fd, n;
	char	buf[BUFSIZ];

	if ((fd = open(NOGAMING, O_RDONLY, 0)) >= 0) {
#define	MESG	"Sorry, no games right now.\n\n"
		(void)write(2, MESG, sizeof(MESG) - 1);
		while ((n = read(fd, buf, sizeof(buf))) > 0)
			(void)write(2, buf, n);
		exit(1);
	}
}

/*
 * hour --
 *	print out the hour in human form
 */
static
hour(h)
	int	h;
{
	switch(h) {
	case 0:
		fputs("midnight", stderr);
		break;
	case 12:
		fputs("noon", stderr);
		break;
	default:
		if (h > 12)
			fprintf(stderr, "%dpm", h - 12);
		else
			fprintf(stderr, "%dam", h);
	}
}

#ifdef LOG
static
logfile()
{
	struct passwd	*pw, *getpwuid();
	FILE	*lp;
	uid_t	uid;
	int	lock_cnt;
	char	*ctime();

	if (lp = fopen(LOGFILE, "a")) {
		for (lock_cnt = 0;; ++lock_cnt) {
			if (!flock(fileno(lp), LOCK_EX))
				break;
			if (lock_cnt == 4) {
				perror("dm: log lock");
				(void)fclose(lp);
				return;
			}
			sleep((u_int)1);
		}
		if (pw = getpwuid(uid = getuid()))
			fputs(pw->pw_name, lp);
		else
			fprintf(lp, "%u", uid);
		fprintf(lp, "\t%s\t%s\t%s", game, gametty, ctime(&now));
		(void)fclose(lp);
		(void)flock(fileno(lp), LOCK_UN);
	}
}
#endif
