/*
**  Vacation
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983, 1987 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
static char	SccsId[] = "@(#)vacation.c	5.5 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/file.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include <sysexits.h>
#include <syslog.h>

/*
**  VACATION -- return a message to the sender when on vacation.
**
**	This program could be invoked as a message receiver when someone is
**	on vacation.  It returns a message specified by the user to whoever
**	sent the mail, taking care not to return a message too often to
**	prevent "I am on vacation" loops.
*/

#define	NO	0			/* no/false */
#define	YES	1			/* yes/true */
#define	EOS	'\0'			/* end of string */
#define	MAXLINE	500			/* max line from mail header */
#define	PERIOD	(60L*60L*24L*7L)	/* week between notifications */
#define	VACAT	".vacation"		/* dbm's database prefix */
#define	VDIR	".vacation.dir"		/* dbm's DB prefix, part 1 */
#define	VIGN	".vacation.ignore"	/* addresses never replied to */
#define	VLOG	".vacation.log"		/* log action and errors */
#define	VMSG	".vacation.msg"		/* vacation message */
#define	VPAG	".vacation.pag"		/* dbm's DB prefix, part 2 */

typedef struct {
	char	*dptr;
	int	dsize;
} DATUM;

typedef struct {
	time_t	sentdate;
} DBREC;

static int	debug;			/* debugging flag */

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern int	optind;
	struct passwd	*pw;
	int	ch, iflag = NO;
	char	*from,
		*getfrom();
	uid_t	getuid();

	while ((ch = getopt(argc, argv, "Idi")) != EOF)
		switch((char)ch) {
		case 'i':			/* init the database */
		case 'I':			/* backward compatible */
			iflag = YES;
			break;
		case 'd':			/* debugging */
			debug = YES;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

	/* find and move to user's home directory */
	if (argc != 1) {
		if (!iflag)
			usage();
		if (!(pw = getpwuid(getuid())) || chdir(pw->pw_dir)) {
			fprintf(stderr, "vacation: no such user uid %u.\n", getuid());
			syslog(LOG_ERR, "vacation: no such user uid %u.\n", getuid());
			exit(EX_USAGE);
		}
		*argv = pw->pw_name;
	}
	else if (!(pw = getpwnam(*argv)) || chdir(pw->pw_dir)) {
		fprintf(stderr, "vacation: no such user %s.\n", *argv);
		syslog(LOG_ERR, "vacation: no such user %s.\n", *argv);
		exit(EX_USAGE);
	}

	/* iflag cleans out the database */
	if (iflag) {
		initialize();
		exit(EX_OK);
	}

	if (debug) {
		time_t	now,
			time();
		char	*ctime();

		if (!freopen(VLOG, "a", stderr)) {
			fprintf(stderr, "vacation: can't append ~%s/%s\n", *argv, VLOG);
			syslog(LOG_ERR, "vacation: can't append ~%s/%s\n", *argv, VLOG);
			exit(EX_CANTCREAT);
		}
		(void)time(&now);
		fprintf(stderr, "===== %s", ctime(&now));
	}

	/*
	 * if database missing, we create it and do a dbminit;
	 * otherwise, just do the dbminit.
	 */
	if (access(VDIR, F_OK))
		initialize();
	else
		dbminit(VACAT);

	/* find out who sent us mail */
	from = getfrom(&shortfrom);
#ifdef VDEBUG
	printf("from='%s'\nshortfrom='%s'\n", from, shortfrom);
	exit(0);
#endif

		setknows(shortfrom);

	/*
	 * ignore if recently replied to this address,
	 * else note the time and send a reply
	 */
	if (!knows(from)) {
		setknows(from, NO);
		sendmessage(from, *argv);
	}
	exit(EX_OK);
}

/*
 * getfrom --
 *	read mail "From" line and return sender's address
 */
static char *
getfrom(shortp)
char **shortp;
{
	register char	*p;
	char	buf[MAXLINE],
		*malloc(), *strcpy();

	/* read the from line */
	if (!gets(buf) || strncmp(buf, "From ", 5)) {
		fputs("vacation: no initial From line.\n", stderr);
		exit(EX_DATAERR);
	}

	/* find the end of the sender address and terminate it */
	for (p = &buf[5]; *p && *p != ' '; ++p);
	if (!*p) {
		fprintf(stderr, "vacation: address terminated From line '%s'\n", buf);
		exit(EX_DATAERR);
	}
	*p = EOS;
	if (!(p = malloc((u_int)(strlen(&buf[5]) + 1)))) {
		fputs("vacation: out of space.\n", stderr);
		exit(EX_OSERR);
	}
	/*
	 * Strip all but the rightmost UUCP host
	 * to prevent loops due to forwarding.
	 * Start searching leftward from the leftmost '@'.
	 *	a!b!c!d yields a short name of c!d
	 *	a!b!c!d@e yields a short name of c!d@e
	 *	e@a!b!c yields the same short name
	 */
#ifdef VDEBUG
printf("start='%s'\n", start);
#endif
	*shortp = start;			/* assume whole addr */
	if ((at = index(start, '@')) == NULL)	/* leftmost '@' */
		at = p;				/* if none, use end of addr */
	saveat = *at;
	*at = '\0';
	if ((bang = rindex(start, '!')) != NULL) {	/* rightmost '!' */
		char *bang2;

		*bang = '\0';
		if ((bang2 = rindex(start, '!')) != NULL) /* 2nd rightmost '!' */
			*shortp = bang2 + 1;		/* move past ! */
		*bang = '!';
	}
	*at = saveat;
#ifdef VDEBUG
printf("place='%s'\n", *shortp);
#endif

	/* return the sender address */
	return start;
}

/*
 * junkmail --
 *	read the header and return if automagic/junk/bulk mail
 */
static
junkmail(from)
	char	*from;
{
	static struct ignore {
		char	*name;
		int	len;
	} ignore[] = {
		"-REQUEST", 8,		"Postmaster", 10,
		"uucp", 4,		"MAILER-DAEMON", 13,
		"MAILER", 6,		NULL, NULL,
	};
	register struct ignore	*I;
	register int	len;
	register char	*p;
	char	buf[MAXLINE],
		*index(), *rindex();

	/*
	 * This is mildly amusing, and I'm not positive it's right; what
	 * we're trying to do is find the "real" name of the sender.  I'm
	 * assuming that addresses will be some variant of:
	 *
	 * From SENDER
	 * From SENDER@seismo.css.gov
	 * From SENDER%site.BITNET@seismo.css.gov
	 * From site1!site2!SENDER@seismo.css.gov
	 *
	 * Therefore, the following search order:
	 */
	if (!(p = index(from, '%')))
		if (!(p = index(from, '@'))) {
			if (p = rindex(from, '!'))
				++p;
			for (p = from; *p; ++p);
		}
	len = p - from;
	for (I = ignore; I->name; ++I)
		if (len >= I->len && !strcasencmp(I->name, p - I->len, I->len)) {
			if (debug)
				fprintf(stderr, "not sending to %s {%s}\n", from, I->name);
			return(YES);
		}

	/* read the header looking for a "Precedence:" line */
	while (gets(buf) && *buf) {
		if (strcasencmp(buf, "Precedence", 10) ||
		   buf[10] != ':' && buf[10] != ' ' && buf[10] != '\t')
			continue;

		/* find the value of this field */
		if (!(p = index(buf, ':')))
			continue;
		while (*++p && isspace(*p));
		if (!*p)
			continue;

		/* see if it is "junk" or "bulk" */
		if (!strcasencmp(p, "junk", 4) || !strcasecmp(p, "bulk", 4)) {
			if (debug)
				fprintf(stderr, "not sending to %s {junk/bulk}\n", from);
			return(YES);
		}
	}
	return(NO);
}

/*
 * knows --
 *	find out if user has gotten a vacation message recently.
 */
static
knows(user)
char *user;
{
	DATUM	k, d,
		fetch();
	time_t	now, then,
		time();

	k.dptr = user;
	k.dsize = strlen(user) + 1;
	d = fetch(k);
	if (d.dptr == NULL)
		return FALSE;
	bcopy(d.dptr, (char *)&ldbrec, sizeof ldbrec);	/* realign data */
	return ldbrec.sentdate + Timeout >= time((time_t *)0);
}

#ifndef VMUNIX
bcopy(from, to, size)
register char *to, *from;
register unsigned size;
{
	while (size-- > 0)
		*to++ = *from++;
}
#endif

/*
 * setknows --
 *	store that this user knows about the vacation.
 */
static
setknows(user, forever)
	char	*user;
	int	forever;
{
	DBREC	xrec;
	DATUM	k, d;
	time_t	time();

	k.dptr = user;
	k.dsize = strlen(user) + 1;
	if (forever)
		/* zero is the flag value meaning *never* reply */
		xrec.sentdate = 0;
	else
		(void)time(&xrec.sentdate);
	d.dptr = (char *)&xrec;
	d.dsize = sizeof(xrec);
	store(k, d);
}

/*
 * sendmessage --
 *	exec sendmail to send the vacation file to "user".
 */
static
sendmessage(user, myname)
	char	*user, *myname;
{
	if (debug) {
		fprintf(stderr, "sending {%s} to {%s}\n", VMSG, user);
		return;
	}
	if (!freopen(VMSG, "r", stdin)) {
		fprintf(stderr, "vacation: no ~%s/%s file.\n", myname, VMSG);
		syslog(LOG_ERR, "vacation: no ~%s/%s file.\n", myname, VMSG);
		exit(EX_NOINPUT);
	}
	execl("/usr/lib/sendmail", "sendmail", "-f", myname, user, NULL);
	fprintf(stderr, "vacation: can't exec /usr/lib/sendmail.\n");
	syslog(LOG_ERR, "vacation: can't exec /usr/lib/sendmail.\n");
	exit(EX_OSERR);
}

/*
 * initialize --
 *	initialize the database
 */
static
initialize()
{
	extern int	errno;
	extern char	*sys_errlist[];
	FILE	*fp;
	int	fd;

	if ((fd = open(VDIR, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		fprintf(stderr, "vacation: %s: %s\n", VDIR, sys_errlist[errno]);
		syslog(LOG_ERR, "vacation: %s: %s\n", VDIR, sys_errlist[errno]);
		exit(EX_OSERR);
	}
	(void)close(fd);
	if ((fd = open(VPAG, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		fprintf(stderr, "vacation: %s: %s\n", VPAG, sys_errlist[errno]);
		syslog(LOG_ERR, "vacation: %s: %s\n", VPAG, sys_errlist[errno]);
		exit(EX_OSERR);
	}
	(void)close(fd);
	dbminit(VACAT);
	if (fp = fopen(VIGN, "r")) {
		char	buf[MAXLINE],
			*index();

/* VARARGS 1 */
		while (fgets(buf, sizeof(buf), fp)) {
			*(index(buf, '\n')) = EOS;
			setknows(buf, YES);
		}
		(void)fclose(fp);
	}
	return p;
}

/*
 * usage --
 *	print out a usage message and die
 */
static
usage()
{
	uid_t	getuid();

	fputs("usage: vacation [-i] login\n", stderr);
	syslog(LOG_ERR, "uid %u: usage: vacation [-i] login\n", getuid());
	exit(EX_USAGE);
}
