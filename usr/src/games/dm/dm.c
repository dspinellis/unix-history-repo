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
static char sccsid[] = "@(#)dm.c	5.1 (Berkeley) %G%";
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

#define	LOG			/* if want game usage logged */

#define	GAMEHIDE	"/usr/games/hide"
#define	NOGAMING	"/etc/nogames"
#define	CONTROL		"/usr/games/game.control"
#ifdef LOG
#define	LOGFILE		"/usr/adm/gamelog"
#endif

static struct tm	*ct;		/* current time structure */
static time_t	now;			/* current time value */
static double	maxload = -1;		/* max load for game */
static int	maxusers = -1;		/* max users for game */
		priority = 0;		/* priority game runs at */
static char	*game;			/* requested game */

typedef struct dow {
	char	*day;
	int	start, stop;
} DOW;

static DOW	days[] = {
	"sunday",	25, -1,
	"monday",	25, -1,
	"tuesday",	25, -1,
	"wednesday",	25, -1,
	"thursday",	25, -1,
	"friday",	25, -1,
	"saturday",	25, -1,
	0,		0,  0,
};

main(argc, argv)
	int	argc;
	char	**argv;
{
	FILE	*fp;
	char	*C, *hour(), *rindex();
	struct tm	*localtime();
	time_t	time();
	double	load();

	nogamefile();

	if (argc == 1) {
		fputs("dm: game\n", stderr);
		exit(1);
	}

	if (!(fp = fopen(CONTROL, "r"))) {
		fprintf(stderr, "dm: unable to read %s.\n", CONTROL);
		exit(1);
	}

	read_days(fp);
	(void)time(&now);
	ct = localtime(&now);
	if (ct->tm_hour >= days[ct->tm_wday].start && ct->tm_hour <= days[ct->tm_wday].stop) {
		fprintf(stderr, "dm: Sorry, games are only available from %s to %s today.\n", hour(days[ct->tm_wday].start), hour(days[ct->tm_wday].stop));
		exit(0);
	}

	++argv;
	game = (C = rindex(*argv, '/')) ? ++C : *argv;

	read_games(fp);
	if (maxusers >= 0 && maxusers <= users()) {
		fputs("dm: Sorry, there are too many users logged on right now.\n", stderr);
		exit(0);
	}
	if (maxload >= 0 && maxload < load()) {
		fputs("dm: Sorry, the load average is too high right now.\n", stderr);
		exit(0);
	}
	(void)fclose(fp);

#ifdef LOG
	logfile();
#endif
	play(argv);
}

/*
 * play --
 *	play the game
 */
static
play(args)
	char	**args;
{
	uid_t	uid;
	gid_t	gid;

	if (chdir(GAMEHIDE)) {
		perror("dm: chdir");
		exit(1);
	}
	if (priority && setpriority(PRIO_PROCESS, 0, priority) < 0)
		fputs("dm: unable to set priority!\n", stderr);
	uid = getuid();
	setreuid(uid, uid);
	gid = getgid();
	setregid(gid, gid);
	execv(game, args);
	fprintf(stderr, "dm: can't find %s.\n", game);
	exit(1);
}

/*
 * read_days --
 *	read through days listed in the control file
 */
static
read_days(fp)
	register FILE	*fp;
{
	register DOW	*dp;
	char	lbuf[BUFSIZ], f1[20], f2[20], f3[20];

	while (fgets(lbuf, sizeof(lbuf), fp)) {
		if (lbuf[0] == '\n' || lbuf[0] == '#')
			continue;
		/* special line separates days from game names */
		if (lbuf[0] == '%' && lbuf[1] == '%')
			return;
		if (sscanf(lbuf, "%s%s%s\n", f1, f2, f3) != 3)
			continue;
		for (dp = days; dp->day; ++dp)
			if (dp->start <= 0 && !strcasecmp(dp->day, f1)) {
				if (isdigit(*f2))
					dp->start = atoi(f2);
				if (isdigit(*f3))
					dp->stop = atoi(f3);
				break;
			}
	}
}

/*
 * read_games --
 *	read through games listed in the control file
 */
static
read_games(fp)
	register FILE	*fp;
{
	register char	*C;
	char	lbuf[BUFSIZ], f1[20], f2[20], f3[20];
	double	atof();

	while (fgets(lbuf, sizeof(lbuf), fp)) {
		if (lbuf[0] == '\n' || lbuf[0] == '#')
			continue;
		for (C = lbuf; *C && !isspace(*C); ++C);
		*C = '\0';
		if (!strcmp(game, lbuf) || !strcasecmp("default", lbuf)) {
			if (sscanf(++C, "%s%s%s\n", f1, f2, f3) != 3)
				break;
			if (isdigit(*f1))
				maxload = atof(f1);
			if (isdigit(*f2))
				maxusers = atoi(f2);
			if (isdigit(*f3) || *f3 == '-' || *f3 == '+')
				priority = atoi(f3);
			break;
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
static char *
hour(h)
	int	h;
{
	static char	buf[20];

	if (!h)
		return("midnight");
	if (h == 12)
		return("noon");
	if (h < 12)
		(void)sprintf(buf, "%d pm", h - 12);
	else
		(void)sprintf(buf, "%d am", h);
	return(buf);
}

#ifdef LOG
static
logfile()
{
	struct passwd	*pw, *getpwuid();
	FILE	*lp;
	uid_t	uid;
	int	lock_cnt;
	char	*ctime(), *ttyname();

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
		fprintf(lp, "\t%s\t%s\t%s", game, ttyname(0), ctime(&now));
		(void)fclose(lp);
		(void)flock(fileno(lp), LOCK_UN);
	}
}
#endif
