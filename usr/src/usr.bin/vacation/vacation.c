/*
 * Copyright (c) 1983, 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1983, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vacation.c	5.12 (Berkeley) %G%";
#endif /* not lint */

/*
**  Vacation
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
*/

#include <sys/param.h>
#include <sys/file.h>
#include <pwd.h>
#include <ndbm.h>
#include <syslog.h>
#include <tzfile.h>
#include <stdio.h>
#include <ctype.h>

/*
 *  VACATION -- return a message to the sender when on vacation.
 *
 *	This program is invoked as a message receiver.  It returns a
 *	message specified by the user to whomever sent the mail, taking
 *	care not to return a message too often to prevent "I am on
 *	vacation" loops.
 */

#define	MAXLINE	500			/* max line from mail header */
#define	VMSG	".vacation.msg"		/* vacation message */
#define	VACAT	".vacation"		/* dbm's database prefix */
#define	VDIR	".vacation.dir"		/* dbm's DB prefix, part 1 */
#define	VPAG	".vacation.pag"		/* dbm's DB prefix, part 2 */

typedef struct alias {
	struct alias *next;
	char *name;
} ALIAS;
ALIAS *names;

static DBM *db;

extern int errno;

#define	VIT	"__VACATION__INTERVAL__TIMER__"
static char from[MAXLINE];

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind, opterr;
	extern char *optarg;
	struct passwd *pw;
	ALIAS *cur;
	time_t interval;
	int ch, iflag;
	char *malloc();
	uid_t getuid();
	long atol();

	opterr = iflag = 0;
	interval = -1;
	while ((ch = getopt(argc, argv, "a:Iir:")) != EOF)
		switch((char)ch) {
		case 'a':			/* alias */
			if (!(cur = (ALIAS *)malloc((u_int)sizeof(ALIAS))))
				break;
			cur->name = optarg;
			cur->next = names;
			names = cur;
			break;
		case 'I':			/* backward compatible */
		case 'i':			/* init the database */
			iflag = 1;
			break;
		case 'r':
			if (isdigit(*optarg)) {
				interval = atol(optarg) * SECS_PER_DAY;
				if (interval < 0)
					goto usage;
			}
			else
				interval = LONG_MAX;
			break;
		case '?':
		default:
			goto usage;
		}
	argc -= optind;
	argv += optind;

	if (argc != 1) {
		if (!iflag) {
usage:			syslog(LOG_NOTICE, "uid %u: usage: vacation [-i] [-a alias] login\n", getuid());
			myexit(1);
		}
		if (!(pw = getpwuid(getuid()))) {
			syslog(LOG_ERR, "vacation: no such user uid %u.\n", getuid());
			myexit(1);
		}
	}
	else if (!(pw = getpwnam(*argv))) {
		syslog(LOG_ERR, "vacation: no such user %s.\n", *argv);
		myexit(1);
	}
	if (chdir(pw->pw_dir)) {
		syslog(LOG_NOTICE, "vacation: no such directory %s.\n", pw->pw_dir);
		myexit(1);
	}

	if (iflag || access(VDIR, F_OK))
		initialize();
	if (!(db = dbm_open(VACAT, O_RDWR, 0))) {
		syslog(LOG_NOTICE, "vacation: %s: %s\n", VACAT,
		    strerror(errno));
		myexit(1);
	}

	if (interval != -1)
		setinterval(interval);
	if (iflag)
		myexit(0);

	if (!(cur = (ALIAS *)malloc((u_int)sizeof(ALIAS))))
		myexit(1);
	cur->name = pw->pw_name;
	cur->next = names;
	names = cur;

	readheaders();

	if (!recent()) {
		setreply();
		sendmessage(pw->pw_name);
	}
	myexit(0);
	/* NOTREACHED */
}

/*
 * readheaders --
 *	read mail headers
 */
getfrom(shortp)
char **shortp;
{
	register ALIAS *cur;
	register char *p;
	int tome, cont;
	char buf[MAXLINE], *strcpy(), *index();

	cont = tome = 0;
	while (fgets(buf, sizeof(buf), stdin) && *buf != '\n')
		switch(*buf) {
		case 'F':		/* "From " */
			cont = 0;
			if (!strncmp(buf, "From ", 5)) {
				for (p = buf + 5; *p && *p != ' '; ++p);
				*p = '\0';
				(void)strcpy(from, buf + 5);
				if (p = index(from, '\n'))
					*p = '\0';
				if (junkmail())
					myexit(0);
			}
			break;
		case 'P':		/* "Precedence:" */
			cont = 0;
			if (strncasecmp(buf, "Precedence", 10) || buf[10] != ':' && buf[10] != ' ' && buf[10] != '\t')
				break;
			if (!(p = index(buf, ':')))
				break;
			while (*++p && isspace(*p));
			if (!*p)
				break;
			if (!strncasecmp(p, "junk", 4) || !strncasecmp(p, "bulk", 4))
				myexit(0);
			break;
		case 'C':		/* "Cc:" */
			if (strncmp(buf, "Cc:", 3))
				break;
			cont = 1;
			goto findme;
		case 'T':		/* "To:" */
			if (strncmp(buf, "To:", 3))
				break;
			cont = 1;
			goto findme;
		default:
			if (!isspace(*buf) || !cont || tome) {
				cont = 0;
				break;
			}
findme:			for (cur = names; !tome && cur; cur = cur->next)
				tome += nsearch(cur->name, buf);
		}
	if (!tome)
		myexit(0);
	if (!*from) {
		syslog(LOG_NOTICE, "vacation: no initial \"From\" line.\n");
		myexit(1);
	}
}

	return start;
}

/*
 * junkmail --
 *	read the header and return if automagic/junk/bulk mail
 */
junkmail()
{
	static struct ignore {
		char	*name;
		int	len;
	} ignore[] = {
		"-request", 8,		"postmaster", 10,	"uucp", 4,
		"mailer-daemon", 13,	"mailer", 6,		"-relay", 6,
		NULL, NULL,
	};
	register struct ignore *cur;
	register int len;
	register char *p;
	char *index(), *rindex();

	/*
	 * This is mildly amusing, and I'm not positive it's right; trying
	 * to find the "real" name of the sender, assuming that addresses
	 * will be some variant of:
	 *
	 * From site!site!SENDER%site.domain%site.domain@site.domain
	 */
	if (!(p = index(from, '%')))
		if (!(p = index(from, '@'))) {
			if (p = rindex(from, '!'))
				++p;
			else
				p = from;
			for (; *p; ++p);
		}
	len = p - from;
	for (cur = ignore; cur->name; ++cur)
		if (len >= cur->len && !strncasecmp(cur->name, p - cur->len, cur->len))
			return(1);
	return(0);
}

/*
 * recent --
 *	find out if user has gotten a vacation message recently.
 *	use bcopy for machines with alignment restrictions
 */
char *user;
{
	datum key, data;
	time_t then, next, time();

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
 * setinterval --
 *	store the reply interval
 */
setinterval(interval)
	time_t interval;
{
	datum key, data;

	key.dptr = VIT;
	key.dsize = sizeof(VIT) - 1;
	data.dptr = (char *)&interval;
	data.dsize = sizeof(interval);
	dbm_store(db, key, data, DBM_REPLACE);
}

/*
 * setreply --
 *	store that this user knows about the vacation.
 */
setreply()
{
	datum key, data;
	time_t now, time();

	key.dptr = from;
	key.dsize = strlen(from);
	(void)time(&now);
	data.dptr = (char *)&now;
	data.dsize = sizeof(now);
	dbm_store(db, key, data, DBM_REPLACE);
}

/*
 * sendmessage --
 *	exec sendmail to send the vacation file to sender
 */
sendmessage(myname)
	char *myname;
{
	if (!freopen(VMSG, "r", stdin)) {
		syslog(LOG_NOTICE, "vacation: no ~%s/%s file.\n", myname, VMSG);
		myexit(1);
	}
	execl("/usr/lib/sendmail", "sendmail", "-f", myname, from, NULL);
	syslog(LOG_ERR, "vacation: can't exec /usr/lib/sendmail.\n");
	myexit(1);
}

/*
 * initialize --
 *	initialize the dbm database
 */
initialize()
{
	int fd;

	if ((fd = open(VDIR, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		syslog(LOG_NOTICE, "vacation: %s: %s\n", VDIR, strerror(errno));
		exit(1);
	}
	(void)close(fd);
	if ((fd = open(VPAG, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		syslog(LOG_NOTICE, "vacation: %s: %s\n", VPAG, strerror(errno));
		exit(1);
	}
	(void)close(fd);
}

/*
 * myexit --
 *	we're outta here...
 */
myexit(eval)
	int eval;
{
	if (db)
		dbm_close(db);
	exit(eval);
}
